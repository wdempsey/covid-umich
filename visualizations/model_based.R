## Libraries and Colors
library("RColorBrewer")
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
my_palette <- brewer.pal(name="Greys",n=9)[7]

## Death model
df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath <- aggregate(covid_deaths ~ startdate, data = df_coviddeath, FUN = sum)
df_coviddeath$date = ymd(df_coviddeath$startdate)
df_coviddeath$death_dt = df_coviddeath$covid_deaths

# Indiana population
N <- 6.732E6;

# Deaths
deaths = df_coviddeath$death_dt[-length(df_coviddeath$death_dt)]
dates = df_coviddeath$date[-length(df_coviddeath$death_dt)]

# times
n_days <- length(deaths)

fit_forcing = readRDS("../data/fit_forcing_byage_090121.RDS")

test = summary(fit_forcing, pars = "pred_deaths", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary
n_days = nrow(test)/6
timepoints = rep(1:n_days, 6)

test = data.frame(test)
test$timepoints = timepoints

agg_deaths = aggregate(mean ~ timepoints, data = test, FUN = sum)
agg_deaths$X5. = aggregate(X5. ~ timepoints, data = test, FUN = sum)$X5.
agg_deaths$X50. = aggregate(X50. ~ timepoints, data = test, FUN = sum)$X50.
agg_deaths$X95. = aggregate(X95. ~ timepoints, data = test, FUN = sum)$X95.

png(filename = "../figs/deaths_ppc.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)

smr_pred <- cbind(agg_deaths, 
                  t=1:n_days, deaths = deaths)
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names
smr_pred$date = date(dates)
smr_pred = smr_pred[smr_pred$date < "2021-02-01",]
c_posterior = my_palette # "orange"


ggplot(smr_pred, mapping = aes(x = date)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = date, y = X50.), color = c_posterior) +
  geom_point(mapping = aes(y = deaths)) +
  labs(x = "Day", y = "Deaths") +
  scale_x_date(date_labels = "%b %Y")

dev.off()

## Build a plot for case count too

summary_fit = summary(fit_forcing, pars = "pred_cases_per_agegroup", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary
n_days = nrow(summary_fit)/6
dates = c(min(dates)-1:(n_days - length(dates)), dates) ## EXTEND BACKWARDS FOR CASES
timepoints = rep(dates, 6)
age_group = rep(c(1:6), each = length(dates))
summary_fit = data.frame(summary_fit)
summary_fit$timepoints = timepoints
summary_fit$age_group = age_group
summary_fit$week = week(summary_fit$timepoints)
summary_fit$year = year(summary_fit$timepoints)

agg_cases = aggregate(mean ~ timepoints, data = summary_fit, FUN = sum)
agg_cases$X5. = aggregate(X5. ~ timepoints, data = summary_fit, FUN = sum)$X5.
agg_cases$X50. = aggregate(X50. ~ timepoints, data = summary_fit, FUN = sum)$X50.
agg_cases$X95. = aggregate(X95. ~ timepoints, data = summary_fit, FUN = sum)$X95.

png(filename = "../figs/cases_ppc.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)

smr_pred <- agg_cases
smr_pred$date = date(dates)
smr_pred = smr_pred[smr_pred$date < "2021-02-01",]
c_posterior = my_palette # "orange"


ggplot(smr_pred, mapping = aes(x = date)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = date, y = X50.), color = c_posterior) +
  labs(x = "Day", y = "Cases") +
  scale_x_date(date_labels = "%b %Y")

dev.off()

## Aggregate Undercount calculation
library(MMWRweek)
df_case_counts = read.csv("../data/covid_indiana_age.csv", header = T)
names(df_case_counts) = c("date", "Age", "covid_test", "covid_count", "covid_deaths")
df_case_counts$date = ymd(df_case_counts$date)
df_case_counts_total = aggregate(covid_count ~ date, df_case_counts, sum)


weeks_for_undercount = unique(week(smr_pred$date[smr_pred$date >= "2020-04-01"]))
years_for_undercount = c(rep(2020, 40), rep(2021, 5))
number_of_dates = length(weeks_for_undercount)
undercount = data.frame(week = weeks_for_undercount, year = years_for_undercount, 
                        undercountfactor = rep(0, number_of_dates),
                        undercountfactor.X5 = rep(0, number_of_dates),
                        undercountfactor.X95 = rep(0, number_of_dates))

for(i in 1:nrow(undercount)) {
  observed_count = sum(df_case_counts_total$covid_count[week(df_case_counts_total$date) == weeks_for_undercount[i] &
                                                      year(df_case_counts_total$date) == years_for_undercount[i]
                                                    ], na.rm = TRUE)
  mean_latent_count = sum(smr_pred$mean[week(smr_pred$date) == weeks_for_undercount[i] &
                                      year(smr_pred$date) == years_for_undercount[i]], na.rm = TRUE)
  X5_latent_count = sum(smr_pred$X5.[week(smr_pred$date) == weeks_for_undercount[i] &
                                          year(smr_pred$date) == years_for_undercount[i]], na.rm = TRUE)
  X95_latent_count = sum(smr_pred$X95.[week(smr_pred$date) == weeks_for_undercount[i] &
                                          year(smr_pred$date) == years_for_undercount[i]], na.rm = TRUE)
  
  undercount$undercountfactor[i] = mean_latent_count/observed_count
  undercount$undercountfactor.X5[i] = X5_latent_count/observed_count
  undercount$undercountfactor.X95[i] = X95_latent_count/observed_count
}


undercount$date = MMWRweek::MMWRweek2Date(MMWRyear = undercount$year,
                                              MMWRweek = undercount$week,
                                              MMWRday = 1)

png(filename = "../figs/undercounting.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)

ggplot(undercount, mapping = aes(x = date)) +
  #geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), fill = c_dark, ) +
  geom_ribbon(aes(ymin = undercountfactor.X5, ymax = undercountfactor.X95), fill = c_posterior, alpha=0.35) +
  #geom_ribbon(aes(ymin = X10., ymax = X90.), fill = c_light) +
  geom_line(mapping = aes(x = date, y = undercountfactor), color = c_posterior) +
  labs(x = "Day", y = "Cases") +
  geom_hline(yintercept=c(1), linetype="dotted") +
  scale_x_date(date_labels = "%b %Y")

dev.off()
