library(tidyverse)
library(boot)

d = read_csv("data/early_polls.csv")

perm.cor.test = function(x, y, n=1000, method="spearman") {
    ref_dist = map_dbl(1:n, ~ cor(x, sample(y), method=method))
    obs_stat = cor(x, y, method=method)
    mean(abs(ref_dist) >= abs(obs_stat))
}

# OVERALL
overall = d %>%
    select(year, candidate, poll_id, poll_margin, elec_margin) %>%
    distinct
overall_avg = overall %>%
    group_by(year, candidate) %>%
    summarize_at(vars(contains("margin")), ~ mean(.))

ggplot(overall, aes(poll_margin, elec_margin)) +
    geom_point() +
    geom_smooth(method=lm)
ggplot(overall_avg, aes(poll_margin, elec_margin)) +
    geom_point() +
    geom_smooth(method=lm)

lm(elec_margin ~ poll_margin + as_factor(year), data=overall) %>%
    summary
lm(elec_margin ~ poll_margin, data=overall_avg) %>%
    summary
with(overall, cor(poll_margin, elec_margin))
with(overall_avg, cor(poll_margin, elec_margin))
with(overall_avg, cor(poll_margin, elec_margin, method="spearman"))
with(d, cor(poll_margin, elec_margin))

with(overall_avg, perm.cor.test(poll_margin, elec_margin, n=2000, method="pearson"))
with(overall_avg, perm.cor.test(poll_margin, elec_margin, n=2000, method="spearman"))

lm(elec_margin ~ poll_margin, data=overall_avg) %>%
    summary

# DEMOGRAPHICS
demg = d %>%
    filter(is.na(notes)) %>%
    mutate(poll_supp = 0.5 + poll_margin/200,
           elec_supp = 0.5 + elec_margin/200,
           grp_poll = poll_supp + poll_diff/100,
           grp_elec = elec_supp + elec_diff/100,
           poll_diff_l = qlogis(grp_poll) - qlogis(poll_supp),
           elec_diff_l = qlogis(grp_elec) - qlogis(elec_supp)) %>%
    select(candidate, year, group, poll_diff, elec_diff, poll_diff_l, elec_diff_l) 

demg_avg = demg %>%
    group_by(candidate, year, group) %>%
    summarize_at(vars(contains("diff")), ~ mean(.))

ggplot(demg, aes(poll_diff/100, elec_diff/100)) +
    geom_point() +
    geom_smooth(method=lm)  +
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    labs(x="Difference measured in poll", y="Difference in election exit poll")
ggsave("img/early_polls.png", width=5, height=4, dpi=300)

ggplot(demg_avg, aes(poll_diff, elec_diff)) +
    geom_point() +
    geom_smooth(method=lm)
ggplot(demg_avg, aes(poll_diff_l, elec_diff_l)) +
    geom_point() +
    geom_smooth(method=lm)

lm(elec_diff ~ poll_diff + as_factor(year), data=demg_avg) %>%
    summary
lm(elec_diff ~ poll_diff + group, data=demg) %>%
    summary
lm(elec_diff ~ poll_diff, data=demg_avg) %>%
    summary
lm(elec_diff_l ~ poll_diff_l, data=demg_avg) %>%
    summary

with(demg, cor(poll_diff, elec_diff))
with(demg_avg, cor(poll_diff, elec_diff))

with(demg_avg, perm.cor.test(poll_diff, elec_diff, n=10000, method="pearson"))
with(demg_avg, perm.cor.test(poll_diff, elec_diff, n=10000, method="spearman"))

cor.boot = boot(demg_avg, function(d, i) {
    d = d[i,]
    cor(d$poll_diff, d$elec_diff, method="spearman")
}, R=5000)
cor.ci = boot.ci(cor.boot, type="perc")
round(cor.ci$percent[4:5], 3)


# RELATIVE DEMOGRAPHIC CHANGES SINCE LAST ELECTION
chg_avg = demg_avg %>%
    group_by(group) %>%
    mutate(last_elec_diff = lag(elec_diff_l, order_by=year),
           poll_chg = poll_diff_l - last_elec_diff,
           elec_chg = elec_diff_l - last_elec_diff) %>%
    drop_na

ggplot(chg_avg, aes(poll_chg, elec_chg)) +
    geom_point()

lm(elec_chg ~ poll_chg, data=chg_avg) %>%
    summary
lm(qlogis(elec_chg/100+0.5) ~ qlogis(poll_chg/100+0.5), data=chg_avg) %>%
    summary

rstanarm::stan_lm(elec_chg ~ poll_chg, data=chg_avg, chains=1, prior=NULL,
                  prior_intercept=rstanarm::normal(0, 100)) %>%
    summary(pars="poll_chg")

with(chg_avg, cor(poll_chg, elec_chg))

with(chg_avg, perm.cor.test(poll_chg, elec_chg, n=50000, method="pearson"))
with(chg_avg, perm.cor.test(poll_chg, elec_chg, n=50000, method="spearman"))

cor.boot = boot(chg_avg, function(d, i) {
    d = d[i,]
    cor(d$poll_chg, d$elec_chg, method="spearman")
}, R=5000)

cor.ci = boot.ci(cor.boot, type="perc")
round(cor.ci$percent[4:5], 3)
