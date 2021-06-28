## INDIANA DEATH DATA
library(wesanderson)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)
df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath_age <- aggregate(covid_deaths ~ startdate + age, data = df_swiss, FUN = sum)

c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_posterior = "orange"
c_prior = "aquamarine2"

df_coviddeath_age$date = ymd(df_coviddeath_age$startdate)
df_coviddeath_age$death_dt = df_coviddeath_age$covid_deaths

# df_swiss = df_swiss[df_swiss$date < "2020-07-15",] # single bump to start

## Death Distribution
death_distribution = pnorm(seq(0.5,40.5, by =1), mean = 25, sd = 5)-pnorm(c(0,seq(0.5,39.5, by =1)), mean = 25, sd = 5)
death_distribution = death_distribution/sum(death_distribution)

# Swiss population
N <- 6.732E6;

#initial conditions
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)

# Deaths
deaths = rep(0,0)
for(age_levels in levels(df_coviddeath_age$age)) {
  deaths <- rbind(deaths, df_coviddeath_age$death_dt[df_coviddeath_age$age == age_levels])
}

# times
n_days <- ncol(deaths)
max_death_day <- length(death_distribution)-1
t <- seq(1, n_days+max_death_day, by = 1)
t0 = 0
t <- t

date_switch <- "2020-03-23" # date of introduction of control measures (empirical)
tswitch <- df_swiss %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number
date_switch_two <- "2020-06-15" # date of ending of control measures
tswitch_two <- df_swiss %>% filter(date < date_switch_two) %>% nrow() + 1 # convert time to number
date_switch_three <- "2020-10-01" # date of ending of control measures
tswitch_three <- df_swiss %>% filter(date < date_switch_three) %>% nrow() + 1 # convert time to number


## PLOTTING DEATH DATA BY AGE CATEGORY
df_coviddeath_age_plot = df_coviddeath_age
levels(df_coviddeath_age_plot$age) = c(1,1,1,2,2,2,3,3,3)
levels(df_coviddeath_age_plot$age) = c("0-39", "40-69", "70+")

df_coviddeath_age_plot %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = death_dt, fill = age), stat = "identity") +
  labs(y="Number of COVID-19 Reported Deaths") + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+ 
  geom_vline(aes(xintercept = date(date_switch)), linetype="dotted") + 
  geom_vline(aes(xintercept = date(date_switch_two)), linetype="dotted") + 
  geom_vline(aes(xintercept = date(date_switch_three)), linetype="dotted") +
  scale_fill_manual(values=wes_palette(n = 3, "IsleofDogs1"))


death_rates = c(2.975607e-05, 1.440472e-04, 4.557553e-04, 1.441978e-03, 
                4.562317e-03, 1.443485e-02, 4.567086e-02, 1.546816e-01)

data_forcing <- list(n_days = n_days, t0 = t0, ts = t, N = N, deaths = deaths, 
                     tswitch = tswitch+max_death_day, tswitch_two = tswitch_two + max_death_day,
                     tswitch_three = tswitch_three + max_death_day, 
                     death_distribution = death_distribution,
                     max_death_day = max_death_day, p_death = death_rates,
                     num_ages = length(death_rates))
model_forcing <- stan_model("./8_sir_model.stan")

fit_forcing <- sampling(model_forcing, 
                        data_forcing, 
                        iter=2000,
                        control = list(max_treedepth = 13, adapt_delta=0.9),
                        seed=2,
                        chains = 1)

saveRDS(fit_forcing, "../data/fit_forcing_byage.RDS")

# fit_forcing = readRDS("../data/fit_forcing.RDS")

check_hmc_diagnostics(fit_forcing)

pairs(fit_forcing, pars = c("beta", "eta", "eta_two", "eta_three"))

pairs(fit_forcing, pars = c("nu", "nu_two", "nu_three"))

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary[(max_death_day+1):(max_death_day+length(deaths)-1),]),
                  t=1:(n_days-1))
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = t, y = X50.), color = c_posterior) +
  # geom_point(mapping = aes(y = cases)) +
  labs(x = "Day", y = "Incidence")

png(filename = "../figs/deaths_ppc.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_deaths", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary), 
                  t=1:(n_days-1), deaths = deaths[1:length(deaths)-1])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = t, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = deaths)) +
  labs(x = "Day", y = "Incidence")

dev.off()


fit_forcing %>% 
  spread_draws(Reff[n_days]) %>% 
  group_by(n_days) %>% 
  summarise(R0_mean = mean(Reff), R09 = quantile(Reff, 0.95), R01 = quantile(Reff, 0.05)) %>% 
  ggplot() +
  geom_ribbon(aes(x = n_days, ymin = R01, ymax = R09), fill = c_posterior, alpha=0.35)+
  geom_line(mapping = aes(n_days, R0_mean), color = c_posterior) +
  geom_vline(aes(xintercept = tswitch)) + 
  geom_vline(aes(xintercept = tswitch_two)) + 
  geom_vline(aes(xintercept = tswitch_three))

n = 4000
prior = tibble(
  beta = abs(rnorm(n,2,1)),
  gamma = abs(rnorm(n,.4,.5)),
  a = abs(rnorm(n,.4,.5)),
  phi_inv = rexp(n,5),
  p_reported = rbeta(n, 1, 2),
  eta = rnorm(n, 0, 0.10),
) %>%
  pivot_longer(everything()) %>%
  mutate(type="Prior")

pars = c("beta","gamma","phi_inv","a","exp_eta")
samp =
  extract(fit_forcing,pars) %>%
  as.data.frame() %>%
  pivot_longer(everything()) %>%
  mutate(type="Posterior") %>%
  bind_rows(prior) %>%
  mutate(name=factor(name,levels=pars),
         type=factor(type,levels=c("Prior","Posterior")))

samp = samp[!is.na(samp$name),]

ggplot(samp) +
  geom_density(aes(x=value,fill=type),alpha=.8) +
  facet_wrap(~name,scale="free",ncol=4) +
  scale_fill_manual(values=c(c_prior,c_posterior)) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  labs(x="Value",y="Probability density",fill=NULL) +
  theme(legend.position="bottom")