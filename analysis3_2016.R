library(tidyverse)
library(usmap)
source("IPF.R")
source("calc_plot.R")


################################################################################
# Load 2012 electorate and adjust to 2016 estimates

elec = read_rds("data/electorate_2012.rds")
elec_adj = read_rds("data/chg_2012_2016.rds") %>%
    mutate_if(is_logical, ~ as.numeric(.))

elec = elec %>%
    left_join(elec_adj) %>%
    mutate(w = w * (1 + pct_change + coll*0.06 + nocoll*(-0.06)),
           w_vote = w_vote * (1 + pct_change + coll*0.06 + nocoll*(-0.06)))

# print proportion in each category
elec %>%
    mutate(total = sum(w)) %>%
    group_by_at(vars(men:nocoll)) %>%
    summarize(total = sum(w) / mean(total)) %>%
    ungroup %>%
    mutate(white_men = white & men,
           white_women = white & women,
           other = !white & !black & !hisp) %>%
    mutate_at(vars(-total), ~ . * total) %>%
    summarize_all(sum) %>%
    select(-total) %>%
    gather(group, total)
    
################################################################################
# Estimate necessary adjustment per group to reflect candidate strengths and
# weaknesses

grps_old = read_csv("data/demg_2012.csv") %>%
    transmute(group=group, pct_dem = 0.5+margin_dem/200) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf) %>%
    mutate(total = 0.52) %>% 
    mutate_at(vars(-total), ~ qlogis(.) - qlogis(total)) %>%
    select(-total) %>%
    pivot_longer(everything(), names_to="group", values_to="diff_old")

grps_now = read_csv("data/quinnipiac_hth_2016.csv") %>%
    rename(coll=white_coll, nocoll=white_nocoll) %>%
    group_by(candidate, hth) %>%
    summarize_at(vars(total:hisp), ~ weighted.mean(., n, na.rm=T)) %>%
    select(-gop:-ind) %>%
    filter(candidate != "[lost]") %>%
    pivot_longer(men:hisp, names_to="group", values_to="pct") %>%
    ungroup %>%
    mutate(candidate = if_else(candidate == "Trump", "gop", "dem")) %>%
    pivot_wider(names_from=candidate, values_from=c(pct, total)) %>%
    rename(candidate=hth) %>%
    mutate(dem = pct_dem/(pct_dem + pct_gop),
           total = total_dem/(total_dem + total_gop),
           diff_now = qlogis(dem) - qlogis(total)) %>%
    select(candidate, group, diff_now)

chg = left_join(grps_now, grps_old, by="group") %>%
    mutate(adj = diff_now - diff_old) %>%
    select(-diff_now, -diff_old) 
    #candidate = str_to_lower(str_sub(candidate, 1, 4))) %>%
    #pivot_wider(names_from=candidate, values_from=adj, names_prefix="adj_")


################################################################################
# Apply calculated adjustments and look at scenarios across popular vote range

apply_adj = function(elec, chg, lname) {
    elec %>%
        rownames_to_column("id") %>%
        pivot_longer(men:nocoll, names_to="group", values_to="val") %>%
        left_join(filter(chg, candidate==lname), by="group") %>%
        group_by(id) %>%
        mutate(adj = sum(adj*val)) %>%
        ungroup %>%
        select(-starts_with("adj_")) %>%
        pivot_wider(names_from="group", values_from="val") %>%
        select(-id) %>%
        mutate(pred = pred + adj,
               w_vote = plogis(pred) * w)
}

hyp_elec_c = elec %>%
    apply_adj(chg, "Clinton") %>%
    adj_overall(0.511, n=6)

hyp_elec_s = elec %>%
    apply_adj(chg, "Sanders") %>%
    adj_overall(0.511, n=6)

hyp_elec_o = elec %>%
    adj_overall(0.511, n=6)

# Scenario simulation
scenarios = crossing(overall = seq(0.47, 0.53, 0.002),
                     candidate = c("Clinton", "Sanders", "Obama"))

ests = scenarios %>%
    group_by_all() %>%
    group_modify(function(.x, .y) {
        cat("|")
        
        if (.y$candidate == "Obama") 
            elec %>%
                adj_overall(.y$overall, n=6) %>%
                calc_states
        else 
            elec %>%
                apply_adj(chg, .y$candidate) %>%
                adj_overall(.y$overall, n=6) %>%
                calc_states
    })

# Pop vote vs. EVs
ests %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
ggplot(aes(overall, dem_ev, color=candidate)) +
    geom_hline(yintercept=270, lty="dashed") +
    geom_line(lwd=0.6) +
    scale_color_viridis_d(labels=c("Clinton", "Obama (hypothetical)", "Sanders")) +
    labs(x="Two-party popular vote", y="Electoral College", color="Candidate",
         title="Clinton's Systematic Disadvantage in 2016") +
    scale_x_continuous(labels=scales::percent)
ggsave("img/2016_early_indicators.png", width=8, height=4, dpi=300)

# Maps for each candidate
filter(ests, overall == 0.496) %>% 
    group_by(candidate) %>%
    group_map(~ calc_ec(.x))

ex.d = filter(ests, overall == 0.496) %>% select(candidate, state, dem_win)
p_c = map_win(filter(ex.d, candidate=="Clinton"), "Clinton @ 49.6% (223 EV)")
p_o = map_win(filter(ex.d, candidate=="Obama"), "Obama @ 49.6% (253 EV)")
p_s = map_win(filter(ex.d, candidate=="Sanders"), "Sanders @ 49.6% (279 EV)")
gridExtra::grid.arrange(p_c, p_o, p_s, nrow=2, ncol=2)

# rust belt
ests %>%
    filter(state %in% c("PA", "WI", "MI", "OH")) %>%
    #filter(state %in% c("OH")) %>%
    arrange(candidate, state, overall) %>%
    group_by(candidate, overall) %>%
    summarize(won_all = min(dem_pct > 0.5)) %>%
    group_by(candidate) %>%
    summarize(win = overall[match(1, won_all)]) %>%
    ggplot(aes(reorder(candidate, -win), win, fill=candidate)) +
    geom_col() +
    geom_text(aes(label=scales::percent(win, accuracy=0.1)), nudge_y=0.002, fontface="bold") +
    scale_fill_viridis_d() +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), expand=c(0, 0.0001)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(0.47, 0.53)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="National popular vote needed to win MI, PA, WI",
         caption="Source: Analysis of Quinnipiac polling data") +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())

# avg EVs
ests %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
    ungroup %>%
    group_by(candidate) %>%
    summarize(avg_ev = mean(dem_ev,)) %>%
    ggplot(aes(reorder(candidate, avg_ev), avg_ev, fill=candidate)) + 
    geom_col() +
    geom_text(aes(label=round(avg_ev)), nudge_y=3, fontface="bold") +
    scale_fill_viridis_d() +
    scale_y_continuous(expand=c(0, 1)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(220, 270)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="Average electoral votes",
         caption=str_c("Averages taken across a range of the national popular ",
                       "vote from 47% to 53%.\n———\n",
                       "Source: Analysis of Quinnipiac poll data")) +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())



# Compare to actual demographic groups
grps_act = read_csv("data/demg_2016.csv") %>%
    transmute(group=group, pct_dem = 0.5+margin_dem/200) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf) %>%
    pivot_longer(everything(), names_to="group", values_to="dem_pct_act")

comp = bind_rows(Obama=calc_demgr(hyp_elec_o), Clinton=calc_demgr(hyp_elec_c),
                 Sanders=calc_demgr(hyp_elec_s), .id="candidate") %>%
    left_join(grps_act)

comp %>%
    #mutate(error = qlogis(dem_pct) - qlogis(dem_pct_act)) %>%
    mutate(error = dem_pct - dem_pct_act) %>%
    group_by(candidate) %>%
    summarize(sqrt(mean(error^2)))

ggplot(comp, aes(dem_pct, dem_pct_act, color=candidate)) +
    geom_abline(slope=1) +
    geom_line() +
    scale_color_viridis_d()

comp %>%
    arrange(candidate) %>%
    group_by(candidate) %>%
    group_map(~ summary(lm(dem_pct_act ~ dem_pct, data=.x))$r.squared)

comp %>%
    #filter(candidate == "Clinton") %>%
    pivot_longer(dem_pct:dem_pct_act, names_to="type", values_to="pct") %>%
    mutate(type = if_else(type=="dem_pct", "Estimated", "Actual")) %>%
ggplot(aes(group, pct, fill=type)) +
    facet_wrap("candidate") +
    geom_col(position="dodge")# +
    scale_fill_viridis_d()

