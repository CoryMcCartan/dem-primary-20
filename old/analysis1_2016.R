library(tidyverse)
library(usmap)

d.h = read_csv("data/quinnipiac_hth_2016.csv")

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
    rename(coll=white_coll, nocoll=white_nocoll) %>%
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
    #filter(group %in% c("white", "black", "hisp")) %>%
    filter(str_detect(group, "white")) %>%
ggplot(aes(candidate, diff_dem, fill=pct_dem)) +
    facet_wrap(vars(group)) +
    geom_col(color="black") +
    scale_fill_gradient2(midpoint=0.5)


# try creating fake voting dataset
electorate = read_rds("data/voters_samp.rds") %>%
    group_by_at(vars(-w, -population, -white_coll, -white_nocoll)) %>%
    summarize(w = sum(w)) %>%
    select(w, everything())

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
act_2016 = read_csv("data/state_data_combined.csv") %>%
    filter(year == 2016) %>%
    select(state = abbr, dem_act=dem, gop_act=gop)
act_2012 = read_csv("data/state_data_combined.csv") %>%
    filter(year == 2012) %>%
    select(state = abbr, dem_act=dem, gop_act=gop)

state.ev = read_csv("data/state_ev.csv") %>%
    transmute(state=state, ev=ev.2020) %>%
    left_join(read_csv("data/states.csv"), by=c("state"="name")) %>%
    select(state=abbr, ev) %>%
    spread(state, ev) %>%
    as_vector

tot_dem_vote = with(cand_totals, dem[candidate=="Clinton"])
tot_trump_vote = with(cand_totals, trump[candidate=="Clinton"])
tot_2way_vote = tot_dem_vote + tot_trump_vote

groups = c("men", "women", "white", "black", "hisp", "age_18_34", "age_35_49",
           "age_50_64", "age_65_inf", "coll", "nocoll", 
           "white_men", "white_women")

p = electorate %>%
    left_join(pop_2012, by="state") %>%
    mutate(dem_vote = w * plogis(qlogis(tot_dem_vote) + dem_adj),
           trump_vote = w * plogis(qlogis(tot_trump_vote) + trump_adj)) %>%
    select(state, w, dem_vote, trump_vote, everything())
p

# given a target dem/trump general polling numbers, estimate the electoral
# college votes and popular vote
est_ev = function (p, tot_dem_vote, tot_trump_vote, lname, progress=F, raw=F) {
    for (i in 1:(10*length(groups))) {
        idx = ((i-1) %% length(groups)) + 1
        grp = groups[idx]
        grp_vec = p[[grp]]
        
        exp_dem_pct = plogis(qlogis(tot_dem_vote) 
                         + filter(hth_demg, candidate==lname, group==grp)$diff_dem)
        exp_trump_pct = plogis(qlogis(tot_trump_vote) 
                         + filter(hth_demg, candidate==lname, group==grp)$diff_trump)
        
        obs_dem_pct = with(p[grp_vec,], sum(dem_vote)/sum(w))
        obs_trump_pct = with(p[grp_vec,], sum(trump_vote)/sum(w))
        
        p[grp_vec,] = p[grp_vec,] %>%
            mutate(dem_vote = dem_vote * exp_dem_pct/obs_dem_pct,
                   trump_vote = trump_vote * exp_trump_pct/obs_trump_pct,
                   obs_2way = sum(dem_vote + trump_vote) / sum(w)) %>%
            #mutate(dem_vote = dem_vote * tot_2way_vote / obs_2way,
            #       trump_vote = trump_vote * tot_2way_vote / obs_2way) %>%
            select(-obs_2way)
    }
    
    if (progress) cat("|")
    if (raw) return(p)
    
    p %>%
        group_by(state) %>%
        summarize(total_votes = sum(dem_vote + trump_vote),
                  dem_pct = sum(dem_vote) / total_votes,
                  gop_pct = sum(trump_vote) / total_votes,
                  winner = if_else(dem_pct > gop_pct, "DEM", "GOP"),
                  dem_ev = ifelse(winner == "DEM", state.ev[state], 0)) %>%
        mutate(dem_ev = sum(dem_ev),
                  gop_ev = 538 - dem_ev,
                  dem_pct_total = weighted.mean(dem_pct, total_votes),
                  gop_pct_total = weighted.mean(gop_pct, total_votes))
                  #win_WI = sum((winner == "DEM") & (state == "WI")),
                  #win_MI = sum((winner == "DEM") & (state == "MI")),
                  #win_OH = sum((winner == "DEM") & (state == "OH")),
                  #win_PA = sum((winner == "DEM") & (state == "PA")))
}

d.m = electorate %>%
    select(-w, -state, -population) %>%
    as.matrix
cor.m = cov.wt(d.m, electorate$w, cor=T)$cor
cor.m %>%
    as.data.frame %>%
    rownames_to_column("group1") %>%
    gather("group2", "cor", -group1) %>%
ggplot(aes(group1, group2, fill=abs(cor))) +
    geom_tile() +
    scale_fill_gradient2()

est1 = est_ev(p, 0.47, 0.479, "Sanders") 
p2 = est_ev(p, 0.47, 0.479, "Sanders", raw=T) 
plot_usmap(data=est1, values="winner") + scale_fill_manual(values=c("blue", "red"))

est1 %>%
    left_join(act_2016) %>%
    #lm(dem_pct ~ dem_act, data=.) %>%
    #summary
ggplot(aes(dem_pct, dem_act, label=state)) + 
    geom_hline(yintercept=0.5) + geom_vline(xintercept=0.5) +
    geom_text(size=2) +
    geom_smooth(method=lm) +
    lims(x=c(0.2, 1), y=c(0.2, 1))

act_2012 %>%
    left_join(act_2016, by="state", suffix=c("_12", "_16")) %>%
    lm(dem_act_16 ~ dem_act_12, data=.) %>%
    summary
ggplot(aes(dem_act_12, dem_act_16, label=state)) + 
    geom_hline(yintercept=0.5) + geom_vline(xintercept=0.5) +
    geom_text(size=2) +
    geom_smooth(method=lm) +
    lims(x=c(0.2, 1), y=c(0.2, 1))

scenarios = crossing(dem = seq(0.48, 0.48, 0.001),
         gop = seq(0.46, 0.46, 0.001),
         candidate = c("Clinton", "Sanders"))

scenarios = crossing(dem = seq(0.47, 0.50, 0.002),
         gop = seq(0.46, 0.50, 0.002),
         candidate = c("Clinton", "Sanders")) %>%
    filter(dem + gop == 0.97) %>%
    mutate(lead = dem - gop)
    #filter(dem + gop == 0.98 | dem + gop == 0.96 | dem + gop == 0.94)

ests = scenarios %>%
    group_by(dem, gop, candidate) %>%
    group_modify(~ est_ev(p, .y$dem, .y$gop, .y$candidate, T))


# visualization
ests %>%
    select(dem_pct=dem_pct_total, gop_pct=gop_pct_total, dem_ev, candidate) %>%
    distinct%>%
ggplot(aes(dem_pct-gop_pct, dem_ev, color=candidate)) +
    geom_hline(yintercept=270, lty="dashed") +
    scale_color_viridis_d() +
    geom_line(lwd=0.8) + 
    geom_point() +
    scale_x_continuous(labels=scales::percent, name="Poll margin") +
    labs(y="Dem. EV")

ests %>%
    filter(dem==0.498, candidate=="Clinton") %>%
plot_usmap(data=., region="states", values="winner") +
    scale_fill_manual(values=c("blue", "red"))
    #scale_fill_gradient2(midpoint=0.5)

#saveRDS(ests, "ev_ests_2000_2016_avg.Rdata")

# check
{
tot_dem_vote_2 = 0.48
tot_trump_vote_2 = 0.47
for (i in 1:length(groups)) {
    grp = groups[i]
    grp_vec = p[[grp]]
    
    exp_dem_pct = plogis(qlogis(tot_dem_vote_2) 
                     + filter(hth_demg, candidate==lname, group==grp)$diff_dem)
    exp_trump_pct = plogis(qlogis(tot_trump_vote_2) 
                     + filter(hth_demg, candidate==lname, group==grp)$diff_trump)
    
    obs_dem_pct = with(p2[grp_vec,], sum(dem_vote)/sum(w))
    obs_trump_pct = with(p2[grp_vec,], sum(trump_vote)/sum(w))
    
    cat(str_glue("{str_pad(grp, 13, 'right')}:\tExp. {round(100*exp_dem_pct)}/",
    "{round(100*exp_trump_pct)},\tObs. {round(100*obs_dem_pct)}/",
    "{round(100*obs_trump_pct)}\n\n"))
}

obs_dem_pct = with(p2, sum(dem_vote)/sum(w))
obs_trump_pct = with(p2, sum(trump_vote)/sum(w))
cat(str_glue("{str_pad('Overall', 13, 'right')}:\tExp. {round(100*tot_dem_vote_2,1)}/",
"{round(100*tot_trump_vote_2,1)},\tObs. {round(100*obs_dem_pct,1)}/",
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
