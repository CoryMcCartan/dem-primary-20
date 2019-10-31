
# not used
P = crossing(sex=c("men", "women"),
         age=c("18_34", "35_49", "50_64", "65_inf"),
         race=c("white", "black", "hisp"),
         educ=c("coll", "nocoll")) %>%
    mutate_all(~ as.factor(.)) %>%
    model.matrix(~ ., data=., contrasts=map(d, contrasts, contrasts=F)) %>%
    as_tibble %>%
    rename(one=`(Intercept)`) %>%
    rename_at(vars(starts_with("sex")), ~ str_sub(., 4)) %>%
    rename_at(vars(starts_with("race")), ~ str_sub(., 5)) %>%
    rename_at(vars(starts_with("educ")), ~ str_sub(., 5))


anes = read_rds("../public-opinion/data/anes_clean.rds") %>%
    filter(year==2012, age >= 18) %>%
    select(state, regn, wgt, age, sex, race, educ, voted, vote.pres) %>%
    filter(voted == "regT.voteT") %>%
    mutate(age_cat = case_when(
               age >= 18 & age <= 34 ~ "18_34",
               age >= 35 & age <= 49 ~ "35_49",
               age >= 50 & age <= 64 ~ "50_64" ,
               age >= 65 ~ "65_inf"),
           coll = educ == "Bachl" | educ == "Postg",
           vote = vote.pres == "dem") %>%
    drop_na(state, age, sex, race, educ) 

demg.m.2012 = glmer(vote ~ sex + regn + (1|age_cat) + coll + 
                        (1|state) + (sex+coll|race) + (1|regn:race), 
                    data=anes, weights=wgt, family=binomial())

# plotting
ranef(demg.m.2012)$state %>%
    rownames_to_column("state") %>%
plot_usmap(data=., values="(Intercept)") +
    scale_fill_gradient2()

regn_lookup = anes %>% select(state, regn) %>% distinct
by_state = left_join(by_state, regn_lookup, by="state")

state_est_2012 = by_state %>%
    mutate(pred = predict(demg.m.2012, newdata=., type="response"))
state_est_2012 %>%
    group_by(state) %>%
    summarize(dem = weighted.mean(pred, w),
              winner = dem >= 0.5) %>%
plot_usmap(data=., values="winner") +
    scale_fill_manual(values=c("red", "blue"))
    #scale_fill_gradient2(midpoint=0.5)

anes %>%
    group_by(state) %>%
    drop_na(vote) %>%
    summarize(dem = weighted.mean(vote, wgt),
              winner = dem >= 0.5) %>%
plot_usmap(data=., values="winner") +
    scale_fill_manual(values=c("red", "blue"))
