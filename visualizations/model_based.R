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

fit_forcing = readRDS("../data/fit_forcing_byage_072621.RDS")

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
