## INDIANA DEATH DATA
# library(wesanderson)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)

df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
levels(df_coviddeath$age)[1:3] = "0-39" # Collapsing due to limited data
df_coviddeath_age <- aggregate(covid_deaths ~ startdate + age, data = df_coviddeath, FUN = sum)
df_coviddeath_age$date = ymd(df_coviddeath_age$startdate)
df_coviddeath_age$death_dt = df_coviddeath_age$covid_deaths

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

## Switch Dates
date_switch <- "2020-03-23" # date of introduction of control measures (empirical)
tswitch <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number
date_switch_two <- "2020-06-15" # date of ending of control measures
tswitch_two <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch_two) %>% nrow() + 1 # convert time to number
date_switch_three <- "2020-10-01" # date of ending of control measures
tswitch_three <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch_three) %>% nrow() + 1 # convert time to number

death_rates = readRDS("../data/mean_ifr.RDS")

death_rates = death_rates/100

data_forcing <- list(n_days = n_days, t0 = t0, ts = t, N = N, deaths = deaths, 
                     tswitch = tswitch+max_death_day, tswitch_two = tswitch_two + max_death_day,
                     tswitch_three = tswitch_three + max_death_day, 
                     death_distribution = death_distribution,
                     max_death_day = max_death_day, p_death = death_rates,
                     num_ages = length(death_rates),
                     alpha = rep(1, length(death_rates)))
model_forcing <- stan_model("./8_sir_model.stan")

set.seed(17194871)
fit_forcing <- sampling(model_forcing, 
                        data_forcing, 
                        iter=2000,
                        control = list(max_treedepth = 13, adapt_delta=0.9),
                        seed=2,
                        chains = 1)

saveRDS(fit_forcing, "../data/fit_forcing_byage_090121.RDS")
