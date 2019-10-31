library(tidyverse)

tbl_2012 = read_csv("data/np2012_d1.csv")
tbl_2017 = read_csv("data/np2017_d1.csv")

est_changes = function(d, year1, year2) {
    d %>%
        rename_all(str_to_lower) %>%
        select(-total_pop) %>%
        filter(year == year1 | year == year2) %>%
        mutate(year = if_else(year==year1, 1, 2)) %>%
        pivot_longer(pop_0:pop_100, names_to="age", names_prefix="pop_", 
                     values_to="population") %>%
        mutate(age = as.integer(age),
               age_cat = case_when(
                   age < 18 ~ "0_18",#NA_character_,
                   age < 35 ~ "18_34",
                   age < 50 ~ "35_49",
                   age < 65 ~ "50_64",
                   T ~ "65_inf"
               ),
               race = case_when(
                    race == 0 ~ NA_character_,
                    race == 1 & origin == 1 ~ "white", 
                    race == 2 & origin == 1 ~ "black", 
                    origin == 2 & race <= 6 ~ "hisp", 
                    race >= 3 & race <= 6 ~ "other",
                    T ~ NA_character_
               ),
               sex = case_when(
                   sex == 0 ~ NA_character_,
                   sex == 1 ~ "male",
                   sex == 2 ~ "female"
               ),
               origin = na_if(origin, 0)) %>%
        drop_na %>%
        select(year, sex, age_cat, race, population) %>%
        group_by(year, sex, age_cat, race) %>%
        summarize(population = sum(population)) %>%
        pivot_wider(names_from=year, names_prefix="pop_", values_from=population) %>%
        mutate(pct_change = (pop_2 - pop_1) / pop_2) %>%
        transmute(men = sex=="male",
                  women = sex=="female",
                  age_18_34 = age_cat == "18_34",
                  age_35_49 = age_cat == "35_49",
                  age_50_64 = age_cat == "50_64",
                  age_65_inf = age_cat == "65_inf",
                  white = race=="white",
                  black = race=="black",
                  hisp = race=="hisp",
                  pct_change = pct_change)
}

write_rds(est_changes(tbl_2012, 2012, 2016), "data/chg_2012_2016.rds", compress="gz")
write_rds(est_changes(tbl_2017, 2016, 2020), "data/chg_2016_2020.rds", compress="gz")
