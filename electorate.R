library(tidyverse)
library(usmap)
source("IPF.R")
source("calc_plot.R")

################################################################################
# data import
voters = read_rds("data/voters_samp_2016.rds")

################################################################################
# fraction of each state's voters that fall in each demographic category
total_2012_voters = 129235000
total_2016_voters = 136669276

by_state = voters %>%
    select(-population, -starts_with("white_")) %>%
    #group_by(state, men, women) %>%
    mutate(total = sum(w)) %>%
    group_by_at(vars(-w, -total)) %>%
    summarize(w = sum(w) * total_2016_voters/mean(total)) %>%
    ungroup %>%
    transmute(state = state, w = w,
              sex = if_else(men, "male", "female"),
              age_cat = case_when(
                  age_18_34 ~ "18_34",
                  age_35_49 ~ "35_49",
                  age_50_64 ~ "50_64",
                  age_65_inf ~ "65_inf"),
              race = case_when(
                  white ~ "white",
                  black ~ "black",
                  hisp ~ "hisp",
                  T ~ "other"),
              coll = coll)

################################################################################
# fraction of each state's demographics that voted dem in 2012
# use IPF

# prop of each group won by dem in previous election
grps_old = read_csv("data/demg_2016.csv") %>%
    transmute(group=group, pct_dem = 0.5+margin_dem/200) %>%
    spread(group, pct_dem) %>%
    rename(age_65_inf = age_60_inf)

# actual 2012 results by state
states_old = read_csv("data/state_data_combined.csv") %>%
    filter(year == 2016) %>%
    transmute(state=abbr, dem_act = dem/(dem+gop)) %>%
    arrange(state)

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
        ungroup %>%
        mutate_at(vars(-total), ~ . * total) %>%
        summarize_all(sum) %>%
        select(-one, -total)
} 

# groups of interest
groups = c("men", "women", "white", "black", "hisp", "age_18_34", 
           "age_35_49", "age_50_64", "age_65_inf", "coll", "nocoll", 
           "white_men", "white_women", "other")
grp_breakdown = list("men", "women", "white", "black", "hisp", "age_18_34", 
           "age_35_49", "age_50_64", "age_65_inf", "coll", "nocoll", 
           c("white","men"), c("white","women"), "other") %>%
    map(get_profile) %>%
    bind_rows %>%
    mutate(group=groups) %>%
    select(group, everything())

# model for dem's support by group in previous election, used as baseline for IPF
model = grps_old %>%
    select(men, women, white, black, hisp, age_18_34:age_65_inf, 
           coll, nocoll, white_men, white_women) %>%
    mutate_at(vars(men:white_women), ~ qlogis(.)) %>% # - qlogis(0.511)) %>% 
    mutate(other=0) %>%
    gather("group", "obs") %>%
    left_join(grp_breakdown, by="group") %>%
    select(-group) %>%
    lm(obs ~ . + white*men + white*coll, data=.) # interactions not used in primary analysis

# essentially by_state, but with a column for dem votes
vote_state = by_state %>%
    mutate(men = sex=="male",
           women = sex=="female",
           age_18_34 = age_cat == "18_34",
           age_35_49 = age_cat == "35_49",
           age_50_64 = age_cat == "50_64",
           age_65_inf = age_cat == "65_inf",
           white = race=="white",
           black = race=="black",
           hisp = race=="hisp",
           nocoll = !coll) %>%
    select(-sex, -age_cat, -race) %>%
    mutate_if(is_logical, ~ as.numeric(.)) %>%
    mutate(pred = predict(model, newdata=.),
           w_vote = plogis(pred) * w) %>%
    select(state, w, w_vote, pred, men:hisp, coll, nocoll)

vote_state_adj = ipf_state_demg(vote_state, states_old, grps_old)

write_rds(vote_state_adj, "data/electorate_2016.rds", compress="gz")
    
# plotting for fun
vote_state_adj %>%
    #adj_overall(dem_act = 0.5, n=3) %>%
    #summarize(dem = sum(w_vote)/sum(w)) 
    #mutate(pred = pred + (white*nocoll)*(-0.8) + (black)*(-1.2),
    #       w_vote = plogis(pred) * w) %>%
    #adj_overall(dem_act = 0.501, n=5) %>%
    #summarize(dem = sum(w_vote)/sum(w)) 
    group_by(state) %>%
    summarize(dem = sum(w_vote)/sum(w),
              winner = dem > 0.5) %>%
plot_usmap(data=., values="winner") + scale_fill_manual(values=c("red", "blue"))
