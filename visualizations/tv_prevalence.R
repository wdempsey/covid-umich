library(lubridate)
library(MMWRweek)
library(ggplot2)
library(wesanderson)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

prevalence = readRDS("../data/invweights_071521.RDS")
prevalence_alt = readRDS("../data/invweights_alt.RDS")

prevalence = data.frame(prevalence)
prevalence_alt = data.frame(prevalence_alt)
names(prevalence) = names(prevalence_alt)  = c("week", "year", "prev", "ipw_prev")

prevalence$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence$year,
                                     MMWRweek = prevalence$week,
                                     MMWRday = 1)

prevalence_alt$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence$year,
                                          MMWRweek = prevalence$week,
                                          MMWRday = 1)

prevalence_long = rbind(prevalence, prevalence, prevalence_alt)

prevalence_long$Method = c(rep("Unweighted", nrow(prevalence)),
                           rep("IPW1", nrow(prevalence)),
                           rep("IPW2", nrow(prevalence))
                           )

prevalence_long$estimate = prevalence_long$prev * (prevalence_long$Method == "Unweighted") +
  prevalence_long$ipw_prev * (prevalence_long$Method == "IPW1") + 
  prevalence_long$ipw_prev * (prevalence_long$Method == "IPW2")

prevalence_long = data.frame(date = prevalence_long$date, Method = prevalence_long$Method, estimate = prevalence_long$estimate)


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

# times
n_days <- length(deaths)
max_death_day <- 41 # Hard coded

fit_forcing = readRDS("../data/fit_forcing_byage.RDS")

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary[(max_death_day+1):(max_death_day+length(deaths)-1),]),
                  t=1:(n_days-1))
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

smr_pred$current_date = df_coviddeath$date[-c(469:470)]
smr_pred$air = smr_pred$mean/N ## TEMP

prevalence_temp = data.frame(date = smr_pred$current_date, Method = rep("Model-based", nrow(smr_pred)), 
                             estimate = smr_pred$air)

prevalence_temp = prevalence_temp[prevalence_temp$date <= max(prevalence_long$date),]

prevalence_temp$week = week(prevalence_temp$date)
prevalence_temp$year = year(prevalence_temp$date)

prevalence_temp = aggregate(estimate ~ week + year, data = prevalence_temp, FUN = sum)

prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp$year,
                                            MMWRweek = prevalence_temp$week,
                                            MMWRday = 1)

prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate)

prevalence_long = rbind(prevalence_long, prevalence_temp)

## STILL NEED TO ADD DOUBLY ROBUST

png(filename = "../figs/tv_air.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = prevalence_long, aes(x = date, y = estimate, col = Method)) +
  geom_line(size = 2) +
  labs(x = "Date",
       y = "Active Infection Rate Estimate") + 
  theme(text = element_text(size=25)) +
  scale_color_manual(values=wes_palette(n = 4, "IsleofDogs1"))

dev.off()
