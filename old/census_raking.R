library(tidyverse)

raw = read_csv("~/Documents/Analyses/census/acs_p.csv")
states = read_csv("~/Documents/Analyses/census/states.csv")
electorate = read_csv("data/turnout_demg.csv") %>%
    transmute(group=group, pct=pct_electorate/100)

acs = raw %>%
    sample_n(20000) %>%
    mutate(AGEP = as.integer(AGEP)) %>%
    filter(AGEP >= 18) %>%
    filter(CIT != 5) %>%
    sample_n(10000) %>%
    left_join(states, by=c("ST"="id")) %>%
    transmute(
        state_name = name,
        state = abbr,
        weight = as.integer(PWGTP),
        age = case_when(
            AGEP < 35 ~ "18-34",
            AGEP < 49 ~ "35-49",
            AGEP < 65 ~ "50-64",
            T ~ "65+"
        ),
        educ = case_when(
            SCHL <= 20 ~ "nocoll",
            T ~ "coll"
        ),
        sex = if_else(SEX == 1, "male", "female"),
        hisp = HISP != "01",
        race = RAC1P %>% as.factor %>% fct_collapse(white = "1", black = "2",
            other = c("3", "4", "5", "6", "7", "8", "9")) %>%
            as.character %>%
            if_else(hisp, "hisp", .),
    ) %>% 
    select(-hisp)

acs2 = acs %>%
    transmute(state = state,
              w = weight,
              w_vote = w,
              men = sex=="male",
              women = sex=="female",
              age_18_34 = age=="18-34",
              age_35_49 = age=="35-49",
              age_50_64 = age=="50-64",
              age_60_inf = age=="65+",
              white = race=="white",
              black = race=="black",
              hisp = race=="hisp",
              white_men = white & men,
              white_women = white & women,
              coll = educ=="coll",
              nocoll = educ=="nocoll",
              white_coll = white & educ=="coll",
              white_nocoll = white & educ=="nocoll")

grps = nrow(electorate)
for (i in 1:(30*grps)) {
    idx = ((i-1) %% grps) + 1
    grp = electorate$group[idx]
    exp.pct = electorate$pct[idx]
    obs.pct = weighted.mean(acs2[[grp]], acs2$w_vote)
    acs2$w_vote[acs2[[grp]]] = acs2$w_vote[acs2[[grp]]] * exp.pct/obs.pct
}

for (i in 1:grps) {
    idx = ((i-1) %% grps) + 1
    grp = electorate$group[idx]
    exp.pct = electorate$pct[idx]
    obs.pct = weighted.mean(acs2[[grp]], acs2$w_vote)
    cat(str_glue("{str_pad(grp, 13, 'right')}:\tExp. {round(100*exp.pct)}%,\tObs. {round(100*obs.pct)}%\n\n"))
}

write_csv(acs2, "data/pop_sample.csv")

