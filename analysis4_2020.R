library(tidyverse)
library(usmap)
library(furrr)
source("IPF.R")
source("calc_plot.R")

options(future.fork.enable=T)
future::plan(multiprocess)

palette = c("#5fb0e6", "#4c5070", "#b050b0", "#73bc5e", "#eebc3b")
palette_nb = c("#8c54b0", "#4c6060", "#73bc5e", "#eebc3b")
palette_cl = c("#5fb0e6", "#8c54b0", "#d65454", "#4c6060", "#73bc5e", "#eebc3b")
red = "#d03230"
blue = "#2070c0"

theme_set(theme_minimal() + theme())


################################################################################
# Load old electorate and adjust to new estimates

elec = read_rds("data/electorate_2016.rds")
elec_adj = read_rds("data/chg_2016_2020.rds") %>%
    mutate_if(is_logical, ~ as.numeric(.))

elec = elec %>%
    mutate(white_coll = white * coll,
           white_nocoll = white * nocoll,
           white_men = white * men,
           white_women = white * women) %>%
    left_join(elec_adj) %>%
    mutate(w = w * (1 + pct_change + coll*0.06 + nocoll*(-0.06)),
           w_vote = w_vote * (1 + pct_change + coll*0.06 + nocoll*(-0.06)))

# print proportion in each category
elec %>%
    mutate(total = sum(w)) %>%
    group_by_at(vars(men:white_women)) %>%
    summarize(total = sum(w) / mean(total)) %>%
    ungroup %>%
    mutate(other = !white & !black & !hisp) %>%
    mutate_at(vars(-total), ~ . * total) %>%
    summarize_all(sum) %>%
    select(-total) %>%
    gather(group, total)
    
################################################################################
# Estimate necessary adjustment per group to reflect candidate strengths and
# weaknesses

grps_old = read_csv("data/demg_2016.csv") %>%
    transmute(group=group, pct_dem = 0.5+margin_dem/200) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf) %>%
    mutate(total = 0.511) %>% 
    mutate_at(vars(-total), ~ qlogis(.) - qlogis(total)) %>%
    select(-total) %>%
    pivot_longer(everything(), names_to="group", values_to="diff_old")

grps_now = read_csv("data/quinnipiac_hth_2020.csv") %>%
    #rename(age_65_inf = age_60_inf) %>%
    group_by(candidate, hth) %>%
    summarize_at(vars(total:hisp), ~ weighted.mean(., n)) %>%
    select(-gop:-ind) %>%
    filter(candidate != "[lost]") %>%
    pivot_longer(men:hisp, names_to="group", values_to="pct") %>%
    ungroup %>%
    mutate(candidate = if_else(candidate == "Trump", "gop", "dem")) %>%
    pivot_wider(names_from=candidate, values_from=c(pct, total)) %>%
    rename(candidate=hth) %>%
    mutate(dem = pct_dem/(pct_dem + pct_gop),
           total = total_dem/(total_dem + total_gop),
           diff_now = qlogis(dem) - qlogis(total))# %>%
    filter(group == "hisp") %>%
    mutate(diff = dem-total, avg_diff = mean(dem-total))
    select(candidate, group, diff_now)

chg = left_join(grps_now, grps_old, by="group") %>%
    mutate(adj = 0.75*(diff_now - diff_old)) %>%
    select(-diff_now, -diff_old) 

# plotting
pretty = tribble(~group, ~name,
                 "men", "Men",
                 "women", "Women",
                 "white", "White",
                 "black", "Black",
                 "hisp", "Hispanic",
                 "age_18_34", "Age 18-34",
                 "age_35_49", "Age 35-49",
                 "age_50_64", "Age 50-64",
                 "age_65_inf", "Age 65+",
                 "white_men", "White men",
                 "white_women", "White women",
                 "white_coll", "White, college-educated",
                 "white_nocoll", "White, no college") %>%
    mutate(name = as_factor(name))

ann_d = tibble(x=c(0,0,-1), y=0.00, candidate=c("Biden", "Buttigieg", "Harris"),
               label=c("better than average \u2192",
                       "\u2190 worse than average", ""))
grps_now %>%
    left_join(pretty, by="group") %>%
    mutate(diff = plogis(diff_now) - 0.5) %>% 
    group_by(group, name) %>%
    mutate(avg_diff = mean(diff),
           rel_diff = diff - avg_diff) %>%
ggplot(aes(fct_rev(name), rel_diff, fill=candidate, group=candidate)) + 
    facet_wrap("candidate") +
    coord_flip() +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), 
                       name="Support in demographic\nbetter than average \u2192\n\u2190 worse than average") +
    geom_col() +
    geom_hline(yintercept=0, lwd=0.5) +
    scale_fill_manual(values=palette) +
    scale_color_manual(values=palette) +
    #geom_text(aes(x, y, label=label), data=ann_d, size=2.5, 
    #          color="#666666", hjust="center") +
    labs(x=NULL) +
    guides(fill=F)

ggsave("img/fig_1.png", width=8, height=5, dpi=300)

ann_d = tibble(x=1:2, y=-0.15, candidate="Biden", 
               label=c("better than 2016 \u2192",
                       "\u2190 worse than 2016"))
left_join(grps_now, grps_old, by="group") %>%
    mutate(adj = plogis(diff_now) - plogis(diff_old)) %>%
    left_join(pretty, by="group")%>%
ggplot(aes(fct_rev(name), adj, fill=candidate)) + 
    facet_wrap("candidate") +
    coord_flip() +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), 
                       name="Support in demographic versus Clinton in 2016") +
    geom_col() +
    geom_hline(yintercept=0, lwd=0.5) +
    scale_fill_manual(values=palette) +
    geom_text(aes(x, y, label=label), data=ann_d, size=3, 
              color="#666666", hjust="left") +
    labs(x=NULL) +
    guides(fill=F)

ggsave("img/fig_2.png", width=8, height=5, dpi=300)

# testing only
read_csv("data/demg_2012.csv") %>%
    transmute(group=group, pct_dem = 0.5+margin_dem/200) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf) %>%
    mutate(total = 0.519) %>% 
    mutate_at(vars(-total), ~ . - total) %>%
    select(-total) %>%
    pivot_longer(everything(), names_to="group", values_to="diff_old")


################################################################################
# Apply calculated adjustments and look at scenarios across popular vote range

apply_adj = function(elec, chg, lname) {
    elec %>%
        rownames_to_column("id") %>%
        pivot_longer(men:white_women, names_to="group", values_to="val") %>%
        left_join(filter(chg, candidate==lname), by="group") %>%
        group_by(id) %>%
        mutate(adj = sum(adj*val, na.rm=T)) %>%
        ungroup %>%
        select(-candidate) %>%
        pivot_wider(names_from="group", values_from="val") %>%
        select(-id) %>%
        mutate(pred = pred + adj,
               w_vote = plogis(pred) * w) %>%
        select(-adj) 
}

# Scenario simulation
scenarios = crossing(overall = seq(0.47, 0.53, 0.001),
                     candidate = c("Biden", "Sanders", "Warren", 
                                   "Buttigieg", "Harris", "Clinton"))

ests = scenarios %>%
    group_by_all() %>%
    group_modify(function(.x, .y) {
        cat("|")
        if (.y$candidate == "Clinton")
            elec %>%
                adj_overall(.y$overall, n=6) %>%
                calc_states
        else 
            elec %>%
                apply_adj(chg, .y$candidate) %>%
                adj_overall(.y$overall, n=6) %>%
                calc_states
    })

write_rds(ests, "out/ests_09_09_baseline.rds", compress="gz")
ests = read_rds("out/ests_09_09_baseline.rds")


################################################################################
# Pictures!

theme_set(theme_minimal() + theme(
    #panel.background = element_rect(fill="#fafafa", linetype=0),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(color="#000000"),
    plot.title = element_text(face="bold"),
    plot.margin = margin(),
    legend.margin = margin(),
    plot.caption = element_text(hjust=0, color="#666666", margin=margin(t=12))
))

# Pop vote vs. EVs
ests %>%
    filter(candidate != "Clinton") %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
ggplot(aes(overall, dem_ev, color=candidate)) +
    geom_hline(yintercept=270, lty="dashed") +
    geom_line(lwd=1.0) +
    scale_color_manual(values=palette) +
    labs(x="Share of national popular vote", y="Electoral votes", color=NULL,
         title="Candidates win different states\nwith same overall support",
         subtitle="Estimated electoral votes versus national popular vote",
         caption="Source: Analysis of Quinnipiac polling data") +
    annotate("text", x=0.481, y=271, label="270 electoral votes to win", 
             vjust=0, hjust=0, size=3) +
    coord_cartesian(xlim=c(0.48, 0.525), ylim=c(200, 300)) +
    scale_x_continuous(labels=function(x) scales::percent(x, accuracy=1), expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0))

ggsave("img/tmc_fig1.png", width=5, height=4, dpi=300)
ggsave("img/fig_4.png", width=6, height=4, dpi=300)
ggsave("img/pop_ev_09_09.png", width=8, height=6, dpi=300)


ests %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
    ungroup %>%
    spread(candidate, dem_ev) %>%
    #with(., mean(Buttigieg - Sanders))
ggplot(aes(overall, Buttigieg-Biden)) + geom_line()
    
ests %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
    ungroup %>%
    group_by(candidate) %>%
    summarize(avg_ev = mean(dem_ev,)) %>%
    filter(candidate != "Clinton") %>%
ggplot(aes(reorder(candidate, avg_ev), avg_ev, fill=candidate)) + 
    geom_col() +
    geom_text(aes(label=round(avg_ev)), nudge_y=3, fontface="bold") +
    scale_fill_manual(values=palette) +
    scale_y_continuous(expand=c(0, 1)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(220, 270)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="Buttigieg leads the pack, on average",
         subtitle="Average electoral votes",
         caption=str_c("Averages taken across a range of the national popular ",
                       "vote from 47% to 53%.\n———\n",
                       "Source: Analysis of Quinnipiac poll data")) +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())

ggsave("img/tmc_fig5.png", width=5, height=3, dpi=300)

# Maps for each candidate
ex.d = filter(ests, overall == 0.49) %>% 
    select(candidate, state, dem_win)
p_b = map_win(filter(ex.d, candidate=="Biden"), "Biden")
p_s = map_win(filter(ex.d, candidate=="Sanders"), "Sanders")
p_w = map_win(filter(ex.d, candidate=="Warren"), "Warren")
p_u = map_win(filter(ex.d, candidate=="Buttigieg"), "Buttigieg")
p_h = map_win(filter(ex.d, candidate=="Harris"), "Harris")
p = gridExtra::grid.arrange(p_b, p_s, p_w, p_u, p_h, nrow=2, ncol=3, 
                            top="National Popular Vote: 49%")

ggsave("img/fig_3a.png", plot=p, width=9, height=6, dpi=300)

filter(ests, overall == 0.49) %>% 
    filter(state %in% c("PA", "MI", "WI", "OH", "IA", "NH", "VA", 
                        "FL", "NM", "NV"),
           candidate != "Clinton") %>%
ggplot(aes(reorder(state, dem_pct), dem_pct, fill=dem_win)) + 
    facet_wrap("candidate") +
    coord_flip(ylim=c(0.4, 0.6)) +
    geom_hline(yintercept=0.5) +
    geom_col() +
    scale_fill_manual(values=c(red, blue)) +
    guides(fill=F) +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1),
                       name="Votes won by Democratic candidate") +
    labs(x=NULL, title="National Popular Vote: 49% Democratic")

ggsave("img/fig_3b.png", width=9, height=5, dpi=300)


# rust belt
ests %>%
    filter(state %in% c("PA", "WI", "MI"), candidate != "Clinton") %>%
    arrange(candidate, state, overall) %>%
    group_by(candidate, overall) %>%
    summarize(won_all = min(dem_pct > 0.5)) %>%
    group_by(candidate) %>%
    summarize(win = overall[match(1, won_all)]) %>%
ggplot(aes(reorder(candidate, -win), win, fill=candidate)) +
    geom_col() +
    geom_text(aes(label=scales::percent(win, accuracy=0.1)), nudge_y=0.002, fontface="bold") +
    scale_fill_manual(values=palette) +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), expand=c(0, 0.0001)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(0.48, 0.51)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="Buttigieg first to win the Rust Belt",
         subtitle="Percentage of the national popular vote needed to win\nMichigan, Pennsylvania, and Wisconsin",
         caption="Source: Analysis of Quinnipiac polling data") +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())

ggsave("img/tmc_fig3.png", width=5, height=3, dpi=300)

# sun belt
ests %>%
    filter(state %in% c("NV", "NM"), candidate != "Clinton") %>%
    arrange(candidate, state, overall) %>%
    group_by(candidate, overall) %>%
    summarize(won_all = min(dem_pct > 0.5)) %>%
    group_by(candidate) %>%
    summarize(win = overall[match(1, won_all)]) %>%
ggplot(aes(reorder(candidate, -win), win, fill=candidate)) +
    geom_col() +
    geom_text(aes(label=scales::percent(win, accuracy=0.1)), nudge_y=0.003, fontface="bold") +
    scale_fill_manual(values=palette) +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), expand=c(0, 0.0001)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(0.49, 0.53)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="Warren first to win Southwest swing states",
         subtitle="Percentage of the national popular vote needed to win\nNevada and New Mexico",
         caption="Source: Analysis of Quinnipiac polling data") +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
    
ggsave("img/tmc_fig4.png", width=5, height=3, dpi=300)


# tipping point graphics
tipping = ests %>%
    arrange(candidate, state, overall) %>%
    group_by(candidate, state) %>%
    summarize(tipping_pt = overall[match(T, dem_win)]) %>%
    mutate(tipping_pt = case_when(
        is.na(tipping_pt) ~ 1,
        tipping_pt == min(scenarios$overall) ~ 0,
        T ~ tipping_pt)) %>%
    left_join(group_by(ests, overall, candidate) %>%
                  summarize(dem_ev = sum(dem_ev)),
              by=c("tipping_pt"="overall", "candidate")) %>%
    arrange(candidate, tipping_pt) %>%
    group_by(candidate, tipping_pt, dem_ev) %>%
    summarize(state = paste(state, collapse="\n")) %>%
    ungroup %>%
    drop_na %>%
    mutate(tipping_diff = tipping_pt - lag(tipping_pt, default=0))

ev_range = filter(tipping, dem_ev > 0, dem_ev < 538) %>% pull(dem_ev) %>% range

win_pcts = ests %>%
    group_by(overall, candidate) %>%
    summarize(dem_ev = sum(dem_ev)) %>%
    arrange(candidate, overall) %>%
    group_by(candidate) %>%
    summarize(win = overall[match(T, dem_ev>=270)])

win_pcts %>%
    filter(candidate != "Clinton") %>%
ggplot(aes(reorder(candidate, -win), win, fill=candidate)) +
    geom_col() +
    geom_text(aes(label=scales::percent(win, accuracy=0.1)), nudge_y=0.002, fontface="bold") +
    scale_fill_manual(values=palette) +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1), expand=c(0, 0.0001)) +
    scale_x_discrete(expand=c(0, 0)) +
    coord_flip(ylim=c(0.49, 0.52)) +
    guides(fill=F) +
    labs(x=NULL, y=NULL, 
         title="Warren first across the finish line",
         subtitle="National popular vote needed to win Electoral College",
         caption="Source: Analysis of Quinnipiac polling data") +
    theme(plot.margin=margin(r=10),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())

ggsave("img/tmc_fig2.png", width=5, height=3, dpi=300)

tipping %>%
ggplot(aes(fct_rev(candidate), tipping_diff, fill=lag(dem_ev))) +
    geom_col() +
    geom_errorbar(aes(candidate, ymin=win, ymax=win), data=win_pcts, 
                  inherit.aes=F, alpha=0.3) +
    geom_text(aes(y=tipping_pt, label=state), size=2.5, fontface="bold") +
    scale_fill_gradient2(midpoint=269, limits=ev_range, 
                         na.value=scales::muted("red")) +
    guides(fill=F) +
    scale_y_continuous(labels=scales::percent) +
    labs(x=NULL, y="Popular vote") +
    coord_flip(ylim=range(scenarios$overall))

ggsave("img/tipping_09_09.png", width=12, height=3, dpi=300)

# demographic estimates
ests_demgr = scenarios %>%
    group_by_all() %>%
    group_modify(function(.x, .y) {
        cat("|")
        elec %>%
            apply_adj(chg, .y$candidate) %>%
            adj_overall(.y$overall, n=6) %>%
            calc_demgr
    })

ggplot(ests_demgr, aes(overall, dem_pct, color=candidate)) +
    facet_wrap("group") +
    geom_line() +
    scale_color_viridis_d()


# old ests (comparison)
ests_old = read_rds("ev_ests_2000_2016_avg.Rdata")
ggplot(filter(ests_old, dem+gop==0.98), aes(dem_pct, dem_ev, color=candidate)) +
    geom_hline(yintercept=270, lty="dashed") +
    geom_line(lwd=0.6) +
    scale_color_viridis_d() +
    labs(x="Two-party popular vote", y="Electoral College", color="Candidate",
         title="") +
    scale_x_continuous(labels=scales::percent)



################################################################################
# Robustness

# Scenario simulation
scenarios = crossing(overall = seq(0.490, 0.526, 0.002),
                     candidate = c("Biden", "Sanders", "Warren", 
                                   "Buttigieg", "Harris"))
scenarios = tibble(overall = 0.485, candidate="Biden")

simul_once = function(x, sd_all=0.05, sd_ind=0.04) {
    n_grp = length(unique(chg$group))
    overall_adj = rep(rnorm(n_grp, 0, sd_all), nrow(chg)/n_grp)
    chg_2 = mutate(chg, adj = adj + overall_adj + rnorm(n(), 0, sd_ind))
    
    est = scenarios %>%
        group_by_all() %>%
        group_modify(function(.x, .y) {
            elec %>%
                apply_adj(chg_2, .y$candidate) %>%
                adj_overall(.y$overall, n=6) %>%
                calc_states
        })
    
    est %>%
        group_by(overall, candidate) %>%
        summarize(dem_ev = sum(dem_ev)) %>%
        group_by(candidate) %>%
        mutate(iter = x,
               avg_ev = mean(dem_ev),
               win = overall[match(T, dem_ev>=270)])
}

ests_sim = future_map_dfr(1:100, simul_once, .progress=T)

write_rds(ests_sim, "out/ests_10_09_sim_100_sd_0.05_0.04.rds", compress="gz")
ests_sim = read_rds("out/ests_10_09_sim_100_sd_0.05_0.04.rds")

ests_sim %>%
    select(iter, candidate, win) %>%
    distinct %>%
ggplot(aes(win, color=candidate)) + 
    geom_density(adjust=2, lwd=0.7) +
    geom_hline(yintercept=0, lwd=0.7) +
    scale_color_viridis_d() +
    scale_x_continuous(name="Popular vote needed to win Electoral College",
                       labels=scales::percent, limits=c(0.495, 0.525)) +
    labs(y=NULL, color=NULL, title="Warren First Across the Finish Line") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

ggplot(ests_sim, aes(avg_ev, color=candidate)) + 
    geom_density(adjust=2) +
    scale_color_viridis_d()

ests_sim %>%
    select(iter, candidate, avg_ev) %>%
    distinct %>%
    spread(candidate, avg_ev) %>%
    mutate_at(vars(Buttigieg:Warren), ~ . - Biden) %>%
    select(-Biden) %>%
    gather(candidate, diff_biden, Buttigieg:Warren) %>%
ggplot(aes(reorder(candidate, diff_biden), diff_biden, fill=candidate)) + 
    geom_hline(yintercept=0, lwd=1, color=palette[1]) +
    geom_boxplot() +
    #geom_jitter(aes(color=candidate), width=0.3) +
    annotate(geom="text", x=4.5, y=-0.1, label="Biden", size=2.5, hjust="right") +
    scale_fill_manual(values=palette_nb) +
    scale_color_manual(values=palette_nb) +
    guides(fill=F, color=F) +
    labs(y="Electoral votes versus Biden", x=NULL, 
         title="Buttigieg Leads the Pack, On Average") +
    coord_flip()

ggsave("img/fig_6a.png", width=6, height=4, dpi=300)

ests_sim %>%
    select(iter, candidate, win) %>%
    distinct %>%
    group_by(iter) %>%
    summarize(first=candidate[which.min(win)]) %>%
    right_join(ests_sim, by="iter") %>%
    mutate(first = first==candidate) %>%
    select(iter, candidate, first) %>%
    distinct %>%
    group_by(candidate) %>%
    summarize(pct_first = mean(first)) %>%
ggplot(aes(reorder(candidate, pct_first), pct_first, fill=candidate)) +
    geom_col() +
    scale_fill_manual(values=palette) +
    scale_y_continuous(labels=scales::percent) +
    guides(fill=F) +
    labs(y="Chance of winning Electoral College first", x=NULL, fill=NULL, 
         title="Warren First Across the Finish Line") +
    coord_flip()

ggsave("img/fig_5.png", width=6, height=4, dpi=300)

ests_sim %>%
    #filter(candidate != "Harris") %>%
    select(iter, candidate, overall, dem_ev) %>%
    group_by(candidate, overall) %>%
    summarize(med_ev = median(dem_ev),
              q25_ev = quantile(dem_ev, 0.25),
              q75_ev = quantile(dem_ev, 0.75),
              q05_ev = quantile(dem_ev, 0.05),
              q10_ev = quantile(dem_ev, 0.9),
              q90_ev = quantile(dem_ev, 0.1),
              q95_ev = quantile(dem_ev, 0.95),
              min_ev = min(dem_ev),
              max_ev = max(dem_ev)) %>%
ggplot(aes(overall, med_ev, group=candidate)) +
    facet_wrap("candidate") +
    geom_hline(yintercept=270, lty="dashed") +
    #geom_ribbon(aes(ymin=min_ev, ymax=max_ev, fill=candidate), alpha=0.05) +
    geom_ribbon(aes(ymin=q05_ev, ymax=q95_ev, fill=candidate), alpha=0.25) +
    geom_ribbon(aes(ymin=q25_ev, ymax=q75_ev, fill=candidate), alpha=0.25) +
    geom_line(aes(color=candidate), lwd=0.6) +
    geom_line(lwd=0.6, alpha=0.5) +
    guides(color=F, fill=F) +
    scale_color_manual(values=palette) +
    scale_fill_manual(values=palette) +
    labs(x="Two-party popular vote", y="Electoral College", color="Candidate",
         title="") +
    scale_x_continuous(labels=function(x) scales::percent(x, accuracy=1))
