library(tidyverse)

# raw data downloaded from 
# http://thedataweb.rm.census.gov/pub/cps/supps/nov12pub.dat.gz

raw = read_fwf("data/nov16pub.dat",  
             fwf_cols(weight=c(613, 622), state_fips=93:94, age=122:123, 
                      sex=129:130, educ=137:138, race=139:140, hisp=157:158, 
                      vote=951:952)) 

states = read_csv("data/states.csv") %>% 
    select(state_fips=id, state=abbr)

d = raw %>%
    left_join(states, by="state_fips") %>%
    select(weight, state, everything(), -state_fips) %>%
    transmute(w = weight,
              state = state,
              population = T,
              voted = vote == 1,
              men = sex==1,
              women = sex==2,
              age_18_34 = age >= 18 & age <= 34,
              age_35_49 = age >= 35 & age <= 49,
              age_50_64 = age >= 50 & age <= 64,
              age_65_inf = age >= 65,
              white = race == 1 & hisp == 2,
              black = race == 2 & hisp == 2,
              hisp = hisp == 1,
              white_men = white & men,
              white_women = white & women,
              coll = educ >= 43,
              nocoll = educ < 43,
              white_coll = white & coll,
              white_nocoll = white & nocoll)

d %>%
    group_by(state) %>%
    summarize_at(vars(-w), ~ sum(. * w)) %>%
    mutate_at(vars(-state, -population), ~ . / population) %>%
plot_usmap(data=., values="voted")

d %>%
    filter(voted) %>%
    select(-voted) %>%
write_rds("data/voters_samp_2016.rds", compress="gz")
