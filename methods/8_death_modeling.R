## SWISS DATA
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)
# df_swiss <- read.csv("../../disease_transmission_workflow/data/swiss_agg_data.csv")
df_swiss <- readRDS("../data/dailycoviddata_total.RDS")

c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_posterior = "orange"
c_prior = "aquamarine2"

df_swiss$date = ymd(df_swiss$startdate)
df_swiss$death_dt = df_swiss$covid_deaths

df_swiss = df_swiss[df_swiss$date < "2020-08-01",] # single bump to start

df_swiss %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = death_dt), fill = c_mid, color = c_dark, stat = "identity") +
  labs(y="Number of COVID-19 Reported Deaths") + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

## Death Distribution
death_distribution = pnorm(seq(0.5,40.5, by =1), mean = 25, sd = 5)-pnorm(c(0,seq(0.5,39.5, by =1)), mean = 25, sd = 5)
death_distribution = death_distribution/sum(death_distribution)

# Swiss population
N <- 6.732E6;

#initial conditions
i0 <- 1
s0 <- N - i0
i02 <- 0
r0 <- 0
y0 = c(S = s0, I = i0, I2 = i02, R = r0)

# Deaths
deaths <- df_swiss$death_dt

# times
n_days <- length(deaths)
max_death_day <- length(death_distribution)-1
t <- seq(1, n_days+max_death_day, by = 1)
t0 = 0
t <- t

date_switch <- "2020-03-29" # date of introduction of control measures
tswitch <- df_swiss %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number

data_forcing <- list(n_days = n_days, t0 = t0, ts = t, N = N, deaths = deaths, 
                     tswitch = tswitch+max_death_day, death_distribution = death_distribution,
                     max_death_day = max_death_day, p_death = 0.01)
model_forcing <- stan_model("./8_sir_model.stan")

fit_forcing <- sampling(model_forcing, 
                        data_forcing, 
                        iter=1000,
                        control = list(max_treedepth = 13, adapt_delta=0.9),
                        seed=0,
                        chains = 1)

check_hmc_diagnostics(fit_forcing)

pairs(fit_forcing, pars = c("beta", "gamma", "a", "eta", "nu", "xi", "phi"))

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary[(max_death_day+1):(max_death_day+length(cases)-1),]), 
                                t=1:(n_days-1), cases = cases[1:length(cases)-1])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = t, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = cases)) +
  labs(x = "Day", y = "Incidence")

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_deaths", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary), 
                  t=1:(n_days-1), deaths = deaths[1:length(deaths)-1])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = t, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = cases)) +
  labs(x = "Day", y = "Incidence")


fit_forcing %>% 
  spread_draws(Reff[n_days]) %>% 
  group_by(n_days) %>% 
  summarise(R0_mean = mean(Reff), R09 = quantile(Reff, 0.95), R01 = quantile(Reff, 0.05)) %>% 
  ggplot() +
  geom_ribbon(aes(x = n_days, ymin = R01, ymax = R09), fill = c_posterior, alpha=0.35)+
  geom_line(mapping = aes(n_days, R0_mean), color = c_posterior) +
  geom_vline(aes(xintercept = tswitch))

n = 4000
prior = tibble(
  beta = abs(rnorm(n,2,1)),
  gamma = abs(rnorm(n,.4,.5)),
  a = abs(rnorm(n,.4,.5)),
  phi_inv = rexp(n,5),
  p_reported = rbeta(n, 1, 2),
  eta = rbeta(n, 2.5, 4),
  nu = rexp(n,1./5),
  xi = .5 + rbeta(n,1, 1)
) %>%
  pivot_longer(everything()) %>%
  mutate(type="Prior")

pars = c("beta","gamma","phi_inv","a","eta","nu","xi")
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
