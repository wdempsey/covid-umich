## SWISS DATA
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
df_swiss <- read.csv("../../disease_transmission_workflow/data/swiss_agg_data.csv")
c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_posterior = "orange"
c_prior = "aquamarine2"

df_swiss %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = report_dt), fill = c_mid, color = c_dark, stat = "identity") +
  labs(y="Number of reported cases")

# Swiss population
N <- 8.57E6;

#initial conditions
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)

# Cases
cases <- df_swiss$report_dt

# times
n_days <- length(cases)
t <- seq(1, n_days, by = 1)
t0 = 0
t <- t

date_switch <- "2020-03-13" # date of introduction of control measures
tswitch <- df_swiss %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number

data_forcing <- list(n_days = n_days, t0 = t0, ts = t, N = N, cases = cases, tswitch = tswitch)

date_survey_left <- "2020-05-04"
date_survey_right <- "2020-05-07"
t_survey_start <- df_swiss %>% filter(date < date_survey_left) %>% nrow() + 1 # convert time to number
t_survey_end <- df_swiss %>% filter(date < date_survey_right) %>% nrow() + 1 # convert time to number
n_infected_survey <-  83
n_tested_survey <-  775
# add these data to the data given to stan
data_forcing_survey <- c(data_forcing, list(t_survey_start = t_survey_start, 
                                            t_survey_end = t_survey_end,
                                            n_infected_survey = n_infected_survey,
                                            n_tested_survey = n_tested_survey))

model_forcing_survey <- stan_model("./sir_model.stan")
#fit_forcing_survey = readRDS("saved_fits/fit_swiss/fit_forcing_survey.rds")
fit_forcing_survey_max <- sampling(model_forcing_survey, 
                                   data_forcing_survey,  
                                   control = list(max_treedepth = 13, adapt_delta=0.9), 
                                   iter=1000,
                                   seed = 0)

check_hmc_diagnostics(fit_forcing_survey_max)


stan_hist(fit_forcing_survey_max, pars = "p_reported", fill = c_posterior, color=c_dark)


smr_pred <- cbind(as.data.frame(summary(fit_forcing_survey_max, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary), t=1:(n_days-1), cases = cases[1:length(cases)-1])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = t, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = cases)) +
  labs(x = "Day", y = "Incidence")

fit_forcing_survey_max %>% 
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

pars = c("beta","gamma","phi_inv","a","p_reported","eta","nu","xi")
samp =
  extract(fit_forcing_survey_max,pars) %>%
  as.data.frame() %>%
  pivot_longer(everything()) %>%
  mutate(type="Posterior") %>%
  bind_rows(prior) %>%
  mutate(name=factor(name,levels=pars),
         type=factor(type,levels=c("Prior","Posterior")))

ggplot(samp) +
  geom_density(aes(x=value,fill=type),alpha=.8) +
  facet_wrap(~name,scale="free",ncol=4) +
  scale_fill_manual(values=c(c_prior,c_posterior)) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  labs(x="Value",y="Probability density",fill=NULL) +
  theme(legend.position="bottom")
