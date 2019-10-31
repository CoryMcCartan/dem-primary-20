library(tidyverse)

# data load
state_ev = read_csv("data/state_ev.csv") %>%
    transmute(state=state, ev=ev.2020) %>%
    left_join(read_csv("data/states.csv"), by=c("state"="name")) %>%
    select(state=abbr, ev) %>%
    spread(state, ev) %>%
    unlist

# popular vote
calc_overall = function(elec) summarize(elec, sum(w_vote)/sum(w)) %>% pull
# electoral college vote
calc_ec = function(elec) {
    if (ncol(elec) > 6) elec = calc_states(elec) 
    sum(elec$dem_ev)
}

# state-by-state pct, winner, and EV
calc_states = function(elec) {
    elec %>%
        group_by(state) %>%
        summarize(dem_pct = sum(w_vote)/sum(w),
                  dem_win = dem_pct > 0.5,
                  dem_ev = ifelse(dem_win, state_ev[state], 0))
}

# dem pct by demographic
calc_demgr = function(elec) {
    elec %>%
        summarize_at(vars(men:white_women), ~ sum(. * w_vote) / sum(. * w)) %>%
        gather(group, dem_pct)
}

# red/blue map with winners
map_win = function(elec, title=NULL) {
    if (ncol(elec) > 6) elec = calc_states(elec) 
    plot_usmap(data=elec, values="dem_win", lines="#ffffff88") +
        scale_fill_manual(values=c("#cf222c", "#1a80c4")) + 
        guides(fill=F) +
        labs(title=title)
}

# red/blue gradient map with dem pct
map_pct = function(elec, title=NULL) {
    if (ncol(elec) > 6) elec = calc_states(elec) 
    plot_usmap(data=elec, values="dem_pct") + 
        scale_fill_gradient2(midpoint=0.5, low="#cc1100", high="#0011aa", 
                             limits=c(NA, 0.7), oob=scales::squish) +
        guides(fill=F) +
        labs(title=title)
}

