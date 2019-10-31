library(dplyr)

# perform an IPF step to match state margins
# state_mrg should have a `state` column and a `dem_act` column
adj_by_state = function(d, state_mrg, n=3) {
    d = group_by(d, state) %>%
        left_join(state_mrg, by="state")
    
    for (i in 1:n) {
        d =  mutate(d, 
                    dem_obs = sum(w_vote)/sum(w),
                    pred = pred + qlogis(dem_act) - qlogis(dem_obs),
                    w_vote = plogis(pred) * w)
    }
    
    select(d, -dem_act, -dem_obs) %>%
        ungroup
}

# perform an IPF step to match demographics
# demg_mrg should have a column for each demographic group and a single row with the margins
adj_by_demg = function(d, demg_mrg, n=2) {
    for (j in 1:n) {
        for (i in sample(5:15)) {
            demgr = names(d)[i]
            only_d = d[[demgr]] == 1
            
            dem_act = demg_mrg[[demgr]]
            dem_obs = with(d[only_d,], sum(w_vote)/sum(w))
            
            d[only_d,] = d[only_d,] %>%
                mutate(pred = pred + qlogis(dem_act) - qlogis(dem_obs),
                       w_vote = plogis(pred) * w)
        }
    }
    
    d
}

adj_overall = function(d, dem_act=0.5, n=5) {
    for (i in 1:n) {
        dem_obs = with(d, sum(w_vote)/sum(w)) 
        d = mutate(d,
                   pred = pred + qlogis(dem_act) - qlogis(dem_obs),
                   w_vote = plogis(pred) * w) 
    }
    
    d
}

ipf_state_demg = function(d, state_mrg, demg_mrg, tol_state=0.002, tol_demg=0.004) {
    for (i in 1:500) {
        d = d %>% 
            adj_by_state(state_mrg) %>% 
            adj_by_demg(demg_mrg)
        
        err_state = d %>%
            group_by(state) %>%
            summarize(dem_obs = sum(w_vote)/sum(w)) %>%
            left_join(state_mrg, by="state") %>%
            mutate(diff = abs(dem_act - dem_obs)) %>%
            pull %>% max
        
        err_demg = d %>%
            summarize_at(vars(men:nocoll), ~ sum(. * w_vote) / sum(. * w)) %>%
            gather(demg, dem_obs) %>%
            left_join(gather(demg_mrg, demg, dem_act), by="demg") %>%
            mutate(diff = abs(dem_act - dem_obs)) %>%
            pull %>% max
        
        if (err_state < tol_state & err_demg < tol_demg) break
    } 
    
    if (err_state > tol_state) print("Exceeding state tolerance.")
    if (err_demg > tol_demg) print("Exceeding demographic tolerance.")
    
    d
}
