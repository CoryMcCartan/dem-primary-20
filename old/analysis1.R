library(tidyverse)
library(usmap)

d.p = read_csv("data/quinnipiac_primaries.csv")
d.h = read_csv("data/quinnipiac_hth.csv")

# primary demographic plot
d.p %>%
    select(-date, -firm) %>%
    group_by(candidate) %>%
    summarize_at(vars(-total, -n), ~ round(weighted.mean((. - total)/total, n, na.rm=T), 2)) %>%
    gather("group", "pct_diff", -candidate) %>%
    #spread(candidate, pct_diff)
ggplot(aes(candidate, group, fill=pct_diff)) +
    geom_tile() + 
    scale_fill_gradient2()

# head-to-head demographic data
cand_totals = d.h %>%
    select(-date, -firm) %>%
    group_by(candidate, hth) %>%
    summarize(total = weighted.mean(total, n, na.rm=T)) %>%
    ungroup %>%
    mutate(candidate = case_when(
        candidate == "Trump" ~ "trump",
        candidate == "[lost]" ~ "lost",
        T ~ "dem")) %>%
    pivot_wider(names_from=candidate, values_from=total) %>%
    rename(candidate=hth)

# main goal is to calculate where dem candidates over/under preform their national
# polling average
hth_demg = d.h %>%
    select(-date, -firm) %>%
    group_by(candidate, hth) %>%
    mutate(total = weighted.mean(total, n, na.rm=T)) %>%
    gather("group", "pct", -candidate, -hth, -n, -total) %>%
    group_by(candidate, hth, group, total) %>%
    summarize(pct = weighted.mean(pct, n, na.rm=T)) %>%
    ungroup %>%
    mutate(candidate = case_when(
        candidate == "Trump" ~ "trump",
        candidate == "[lost]" ~ "lost",
        T ~ "dem"),
        diff = qlogis(pmax(pct, 0)) - qlogis(total)) %>%
    select(-total) %>%
    pivot_wider(names_from=candidate, values_from=c(pct, diff)) %>%
    rename(candidate=hth) %>%
    mutate(test = diff_dem+diff_trump)

# visualize
hth_demg %>%
    #filter(group %in% c("dem", "ind", "gop")) %>%
    #filter(str_detect(group, "age_")) %>%
    filter(group %in% c("white", "black", "hisp")) %>%
    #filter(str_detect(group, "white")) %>%
ggplot(aes(candidate, diff_dem, fill=pct_dem)) +
    facet_wrap(vars(group)) +
    geom_col(color="black") +
    scale_fill_gradient2(midpoint=0.5)


# try creating fake voting dataset
electorate = read_csv("data/pop_sample.csv")
pop_2016_dem = qlogis(0.482)
pop_2016_trump = qlogis(0.461)
pop_2012_dem = qlogis(0.511)
pop_2012_gop = qlogis(0.472)
pop_avg = read_csv("data/state_data_combined.csv") %>%
    filter(year >= 2000) %>%
    group_by(year) %>%
    mutate(pop_dem = weighted.mean(dem, votes),
           pop_gop = weighted.mean(gop, votes)) %>%
    ungroup() %>%
    select(abbr, contains("dem"), contains("gop")) %>%
    rename(state = abbr) %>%
    group_by(state) %>%
    summarize(dem_adj = mean(qlogis(dem) - qlogis(pop_dem)), 
              trump_adj = mean(qlogis(gop) - qlogis(pop_gop)))
pop_avg %>%
    mutate(dem_adj = plogis(dem_adj)) %>%
plot_usmap(data=., values="dem_adj") +
    scale_fill_gradient2(midpoint=0.5, limits=c(NA, 0.7))

pop_2012 = read_csv("data/state_data_combined.csv") %>%
    filter(year == 2012) %>%
    transmute(state = abbr, 
              dem_adj = qlogis(dem) - pop_2012_dem, 
              trump_adj = qlogis(gop) - pop_2012_gop)
pop_2016 = read_csv("data/2016_pres_states.csv") %>%
    transmute(state = abbr, 
              dem_adj = qlogis(dem) - pop_2016_dem, 
              trump_adj = qlogis(gop) - pop_2016_trump)

state.ev = read_csv("data/state_ev.csv") %>%
    transmute(state=state, ev=ev.2020) %>%
    left_join(read_csv("data/states.csv"), by=c("state"="name")) %>%
    select(state=abbr, ev) %>%
    spread(state, ev) %>%
    as_vector

tot_dem_vote = with(cand_totals, dem[candidate=="Biden"])
tot_trump_vote = with(cand_totals, trump[candidate=="Biden"])
tot_2way_vote = tot_dem_vote + tot_trump_vote

groups = c("men", "women", "white", "black", "hisp", "age_18_34", "age_35_49",
           "age_50_64", "age_60_inf", "white_coll", "white_nocoll", 
           "white_men", "white_women")

p = electorate %>%
    left_join(pop_avg, by="state") %>%
    mutate(dem_vote = w_vote * plogis(qlogis(tot_dem_vote) + dem_adj),
           trump_vote = w_vote * plogis(qlogis(tot_trump_vote) + trump_adj)) %>%
    select(state, w, w_vote, dem_vote, trump_vote, everything())
p

# given a target dem/trump general polling numbers, estimate the electoral
# college votes and popular vote
est_ev = function (p, tot_dem_vote, tot_trump_vote, lname, progress=F) {
    for (i in 1:(10*length(groups))) {
        idx = ((i-1) %% length(groups)) + 1
        grp = groups[idx]
        grp_vec = p[[grp]]
        
        exp_dem_pct = plogis(qlogis(tot_dem_vote) 
                         + filter(hth_demg, candidate==lname, group==grp)$diff_dem)
        exp_trump_pct = plogis(qlogis(tot_trump_vote) 
                         + filter(hth_demg, candidate==lname, group==grp)$diff_trump)
        
        obs_dem_pct = with(p[grp_vec,], sum(dem_vote)/sum(w_vote))
        obs_trump_pct = with(p[grp_vec,], sum(trump_vote)/sum(w_vote))
        
        p[grp_vec,] = p[grp_vec,] %>%
            mutate(dem_vote = dem_vote * exp_dem_pct/obs_dem_pct,
                   trump_vote = trump_vote * exp_trump_pct/obs_trump_pct,
                   obs_2way = sum(dem_vote + trump_vote) / sum(w_vote)) %>%
            #mutate(dem_vote = dem_vote * tot_2way_vote / obs_2way,
            #       trump_vote = trump_vote * tot_2way_vote / obs_2way) %>%
            select(-obs_2way)
    }
    
    if (progress) cat("|")
    
    p %>%
        group_by(state) %>%
        summarize(total_votes = sum(dem_vote + trump_vote),
                  dem_pct = sum(dem_vote) / total_votes,
                  gop_pct = sum(trump_vote) / total_votes,
                  winner = if_else(dem_pct > gop_pct, "DEM", "GOP"),
                  dem_ev = ifelse(winner == "DEM", state.ev[state], 0)) %>%
        summarize(dem_ev = sum(dem_ev),
                  gop_ev = 538 - dem_ev,
                  dem_pct = weighted.mean(dem_pct, total_votes),
                  gop_pct = weighted.mean(gop_pct, total_votes),
                  win_WI = sum((winner == "DEM") & (state == "WI")),
                  win_MI = sum((winner == "DEM") & (state == "MI")),
                  win_OH = sum((winner == "DEM") & (state == "OH")),
                  win_PA = sum((winner == "DEM") & (state == "PA")))
}

scenarios = crossing(dem = seq(0.48, 0.52, 0.001),
         gop = seq(0.46, 0.51, 0.001),
         candidate = c("Biden", "Sanders", "Warren", "Buttigieg", "Harris")) %>%
    #filter(dem + gop == 0.98)
    filter(dem + gop == 0.98 | dem + gop == 0.96 | dem + gop == 0.94)

ests = scenarios %>%
    group_by(dem, gop, candidate) %>%
    group_modify(~ est_ev(p, .y$dem, .y$gop, .y$candidate, T))


# visualization
ests %>%
    mutate(not_voting = round(1 - dem - gop, 2)) %>%
    filter(not_voting < 0.05) %>%
ggplot(aes(dem_pct-gop_pct, dem_ev, color=candidate)) +
    facet_wrap("not_voting") +
    geom_hline(yintercept=270, lty="dashed") +
    scale_color_viridis_d() +
    geom_line(lwd=0.8) + 
    scale_x_continuous(labels=scales::percent, name="Poll margin") +
    labs(y="Dem. EV")

ests %>%
ggplot(aes(dem_pct-gop_pct, win_PA, color=candidate)) +
    scale_color_viridis_d() +
    geom_line(lwd=0.8) 

ests_2016 %>%
ggplot(aes(dem_pct-gop_pct, dem_ev, color=candidate)) +
    geom_hline(yintercept=270, lty="dashed") +
    scale_color_viridis_d() +
    geom_line(lwd=0.8) + 
    scale_x_continuous(labels=scales::percent, name="Poll margin") +
    labs(y="Dem. EV")

saveRDS(ests, "ev_ests_2000_2016_avg.Rdata")

# check
{
for (i in 1:length(groups)) {
    grp = groups[i]
    grp_vec = p[[grp]]
    
    exp_dem_pct = plogis(qlogis(tot_dem_vote) 
                     + filter(hth_demg, candidate==lname, group==grp)$diff_dem)
    exp_trump_pct = plogis(qlogis(tot_trump_vote) 
                     + filter(hth_demg, candidate==lname, group==grp)$diff_trump)
    
    obs_dem_pct = with(p[grp_vec,], sum(dem_vote)/sum(w_vote))
    obs_trump_pct = with(p[grp_vec,], sum(trump_vote)/sum(w_vote))
    
    cat(str_glue("{str_pad(grp, 13, 'right')}:\tExp. {round(100*exp_dem_pct)}/",
    "{round(100*exp_trump_pct)},\tObs. {round(100*obs_dem_pct)}/",
    "{round(100*obs_trump_pct)}\n\n"))
}

obs_dem_pct = with(p, sum(dem_vote)/sum(w_vote))
obs_trump_pct = with(p, sum(trump_vote)/sum(w_vote))
cat(str_glue("{str_pad('Overall', 13, 'right')}:\tExp. {round(100*tot_dem_vote,1)}/",
"{round(100*tot_trump_vote,1)},\tObs. {round(100*obs_dem_pct,1)}/",
"{round(100*obs_trump_pct,1)}\n\n"))
}

p %>%
    group_by(state) %>%
    summarize(total_votes = sum(dem_vote + trump_vote),
              dem_pct = sum(dem_vote) / total_votes,
              gop_pct = sum(trump_vote) / total_votes,
              winner = if_else(dem_pct > gop_pct, "DEM", "GOP"),
              dem_ev = ifelse(winner == "DEM", state.ev[state], 0)) %>%
#plot_usmap(data=., values="dem_pct") +
#    scale_fill_gradient2(midpoint = 0.5, limits=c(NA, 0.75), 
#                         labels=scales::percent, name="Dem. Pct.")
plot_usmap(data=., values="winner") +
    scale_fill_manual(values=c("#5577ee", "#dd4422"))
