## Libraries and Colors
library(wesanderson)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)

c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_posterior = "orange"
c_prior = "aquamarine2"

## Death model
temp = read.csv("../data/covid_indiana_age.csv")

temp$date = ymd(temp$DATE)

aggregate(temp$COVID_COUNT ~ date, data = temp, sum)

df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath <- aggregate(covid_deaths ~ startdate, data = df_coviddeath, FUN = sum)
df_coviddeath$date = ymd(df_coviddeath$startdate)
df_coviddeath$death_dt = df_coviddeath$covid_deaths

# Swiss population
N <- 6.732E6;

# Deaths
deaths = df_coviddeath$death_dt[-length(df_coviddeath$death_dt)]
dates = df_coviddeath$date[-length(df_coviddeath$death_dt)]

# times
n_days <- length(deaths)

fit_forcing = readRDS("../data/fit_forcing_threejumps.RDS")
max_death_day <- 41 # Hard coded

check_hmc_diagnostics(fit_forcing)

pairs(fit_forcing, pars = c("beta", "eta", "eta_two", "eta_three"))

pairs(fit_forcing, pars = c("nu", "nu_two", "nu_three"))

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary[(max_death_day+1):(max_death_day+length(deaths)-1),]),
                  t=1:(n_days-1))
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

smr_pred$date = date(dates[1:(length(dates)-1)])

ggplot(smr_pred, mapping = aes(x = date)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = date, y = X50.), color = c_posterior) +
  # geom_point(mapping = aes(y = cases)) +
  labs(x = "Day", y = "Incidence")

png(filename = "../figs/deaths_ppc.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_deaths", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary), 
                  t=1:n_days, deaths = deaths)
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names
smr_pred$date = date(dates)
smr_pred = smr_pred[smr_pred$date < "2021-02-01",]

ggplot(smr_pred, mapping = aes(x = date)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = date, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = deaths)) +
  labs(x = "Day", y = "Deaths") +
  scale_x_date(date_labels = "%b %Y")

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