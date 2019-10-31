library(tidyverse)
library(purrr)
library(usmap)

voters = read_rds("data/voters_samp.rds")

# returns the proportion of voters within a certain group in each demographic category
get_profile = function(group) {
    voters %>%
        select(-state, -population, -starts_with("white_")) %>%
        mutate(all = T, other=!white & !black & !hisp) %>%
        filter(!!!syms(group)) %>%
        mutate(total = sum(w)) %>%
        group_by_at(vars(-w, -total, -all, -other)) %>%
        summarize(total = sum(w) / mean(total),
                  one = 1) %>%
        select(one, everything(), total) %>%
        #filter(white | black | hisp) %>%
        ungroup %>%
        mutate_at(vars(-total), ~ . * total) %>%
        summarize_all(sum) %>%
        select(-one, -total)
} 

# total proportion of voters in each group
total_prop = voters %>%
    select(-state, -population, -starts_with("white_")) %>%
    mutate(total = sum(w)) %>%
    group_by_at(vars(-w, -total)) %>%
    summarize(total = sum(w) / mean(total),
              one = 1) %>%
    select(one, everything(), total) %>%
    ungroup %>%
    mutate(white_men = white & men,
           white_women = white & women,
           other = !white & !black & !hisp) %>%
    mutate_at(vars(-total), ~ . * total) %>%
    summarize_all(sum) %>%
    select(-one, -total) %>%
    gather(group, total)

# groups of interest
groups = c("men", "women", "white", "black", "hisp", "age_18_34", 
           "age_35_49", "age_50_64", "age_65_inf", "coll", "nocoll", 
           "white_men", "white_women", "other")

# for each group, the proportion of voters in each demographic within that group
UP = list("men", "women", "white", "black", "hisp", "age_18_34", 
           "age_35_49", "age_50_64", "age_65_inf", "coll", "nocoll", 
           c("white","men"), c("white","women"), "other") %>%
    map(get_profile) %>%
    bind_rows %>%
    mutate(group=groups) %>%
    left_join(total_prop, by="group") %>%
    select(group, total, everything())

# add polling data
d.h = read_csv("data/quinnipiac_hth_2016.csv") %>%
    rename(coll=white_coll, nocoll=white_nocoll)
UP = d.h %>% 
    #filter(candidate !="Trump", candidate != "[lost]") %>%
    mutate_at(vars(gop:hisp), ~ qlogis(.) - qlogis(total)) %>%
    select(candidate, hth, men:women, white:hisp, age_18_34:age_65_inf, coll:nocoll, 
           white_men:white_women) %>%
    mutate(other=0) %>%
    gather("group", "obs", -candidate, -hth) %>%
    #mutate(obs = qlogis(obs)) %>%
    right_join(UP, by="group")

d.h.2012 = read_csv("data/demg_2012.csv") %>%
    transmute(group=group, pct_dem = pct_dem/100) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf) %>%
    mutate(candidate="Obama", hth="Obama")

UP2 = d.h.2012 %>%
    select(candidate, hth, men, women, white, black, hisp, age_18_34:age_65_inf, 
           coll, nocoll, white_men, white_women) %>%
    mutate_at(vars(men:white_women), ~ qlogis(.) - qlogis(0.511)) %>% 
    mutate(other=0)  %>%
    gather("group", "obs", -candidate, -hth) %>%
    right_join(UP, by="group")
    

state_ev = read_csv("data/state_ev.csv") %>%
    transmute(state=state, ev=ev.2020) %>%
    left_join(read_csv("data/states.csv"), by=c("state"="name")) %>%
    select(state=abbr, ev) %>%
    spread(state, ev) %>%
    as_vector
state_wgt = voters %>% group_by(state) %>% summarize(w=sum(w))

est_totals = function(lname="Clinton", glob_diff=0.0) {
    UP %>%
        filter(candidate == lname) %>%
        filter(group %in% c("white", "black", "hisp")) %>%
        select(group, obs_dem=obs) %>%
        left_join(select(UP2, group, obs_obama=obs), by="group") %>%
        left_join(gather(by_state, group, total, -state, -dem, -year), by="group") %>%
        mutate(baseline = dem,
               pct_obama = plogis(baseline + obs_obama),
               pct_clinton = plogis(baseline + obs_dem),
               diff = pct_clinton - pct_obama + glob_diff,
               adj = diff * total) %>%
        group_by(state) %>%
        summarize(adj=sum(adj),
                  dem=plogis(mean(dem)) + adj,
                  winner=dem>0.49,
                  dem_ev = ifelse(winner, state.ev[state], 0))# %>%
        left_join(state_wgt, by="state") %>%
        drop_na(state) %>%
        mutate(dem_ev = sum(dem_ev),
               gop_ev = 538 - dem_ev,
               dem_pct_total = weighted.mean(dem, w)) %>%
        select(-w)
}
    

scenarios = crossing(glob_diff = seq(-0.02, 0.01, 0.001), 
                     candidate = c("Clinton", "Sanders"))

ests = scenarios %>%
    group_by(candidate, glob_diff) %>%
    group_modify(~ est_totals(.y$candidate, .y$glob_diff))

ests %>%
    select(candidate, glob_diff, dem_ev, dem_pct_total) %>%
    distinct %>%
ggplot(aes(dem_pct_total, dem_ev, color=candidate)) +
    geom_hline(yintercept=270) +
    geom_line(lwd=0.8) +
    geom_point() +
    theme_grey()
    
ests %>%
    filter(dem_pct_total > 0.473, dem_pct_total < 0.478) %>%
    filter(candidate == "Sanders") %>%
plot_usmap(data=., values="winner") +
    scale_fill_manual(values=c("red", "blue"))
    


# estimate effects for each candidate
cand_eff = UP %>%
    select(-group, -total) %>%
    group_nest(candidate, hth) %>%
    mutate(model = map(data, ~ lm(obs ~ . + white:men, data=.))) %>%
    select(-data)

obama_eff = lm(obs ~ . + white:men, data=select(UP2, -group, -total, -candidate, -hth))

# estimate state effects, after controlling for demographics
by_state = voters %>%
    select(-population, -starts_with("white_")) %>%
    group_by(state) %>%
    summarize_at(vars(-w), ~ sum(w * .) / sum(w))

hist_elec = read_csv("data/state_data_combined.csv") %>%
    #filter(year >= 2000, year <= 2012) %>%
    filter(year == 2012) %>%
    transmute(state = abbr, dem = qlogis(dem), year=year)

by_state = right_join(by_state, hist_elec, by="state")

#state_eff = lm(dem ~ white + black + hisp - state, data=by_state) %>%
state_eff = lm(dem ~ black + hisp - state, data=by_state) %>%
    broom::augment() %>%
    select(state, dem, .resid, .fitted)

stan_lm(dem ~ . - state - women - age_65_inf - nocoll - year, data=by_state, 
        prior=R2(0.3), chains=1, iter=1000)# %>% 
    summary

# plotting
{
ggplot(state_eff, aes(dem, .resid, label=state)) + geom_text(size=2) +
    geom_hline(yintercept=0, alpha=0.5)

ggplot(state_eff, aes(dem, .fitted, label=state)) + geom_text(size=2) +
    geom_abline(intercept=0, slope=1, alpha=0.5) 

plot_usmap(data=state_eff, values=".resid") + 
    scale_fill_gradient2(limits=c(-1.2, 1.2))

plot_usmap(data=state_eff, values=".fitted") + 
    scale_fill_gradient2(limits=c(-1.2, 1.2))
}


clinton_eff = cand_eff %>%
    filter(candidate == "Clinton") %>%
    pull(model) %>%
    extract2(1)

voters %>%
    left_join(select(state_eff, state, eff=.resid), by="state") %>%
    mutate_if(is_logical, ~ as.numeric(.)) %>%
    #mutate(linpred = predict(clinton_eff, newdata=.)) %>%
    mutate(linpred = predict(obama_eff, newdata=.)) %>%
    select(state, w, eff, linpred) %>%
    #filter(state == "MS")
    mutate(p = plogis(eff + linpred+0.0)) %>%
    group_by(state) %>%
    summarize(total = sum(w),
              linpred = sum(linpred * w) / total,
              eff = sum(eff * w) / total,
              dem = sum(p * w) / total,
              winner = dem > 0.49) %>%
    #summarize(dem = weighted.mean(dem, total))
plot_usmap(data=., values="linpred") + 
    #scale_fill_manual(values=c("red", "blue"))
    #scale_fill_gradient2(midpoint=0.5, limits=c(NA, 0.7))
    scale_fill_gradient2(midpoint=0.0)

voters %>%
    filter(state=="WA", men, white, coll, age_18_34) %>%
    mutate_if(is_logical, ~ as.numeric(.)) %>%
    predict(clinton_eff, newdata=.) %>%
    plogis
    
