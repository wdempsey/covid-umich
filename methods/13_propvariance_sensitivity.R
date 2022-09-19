## Read in data
fb_data = readRDS("../data/fb_weeklycomplete.RDS")
names(fb_data)[3] = "fever"
fb_data$contact = as.logical(fb_data$contact)

calculate_varpis <- function(propensities, model, fb_data) {
  weeks = c(14:53,1:5)
  years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    # print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_params = propensities[which(weeks == current_week & years == current_year) ,3:ncol(propensities)]
    current_fb_data = subset(fb_data, week == current_week & year == current_year)
    current_fb_X = model.matrix(model, current_fb_data)
    pis = 1/(1+exp(-current_fb_X%*%t(current_params)))
    varpi = pis*(1-pis)
    weighted_varpi = sum(varpi*current_fb_data$weight)/sum(current_fb_data$weight)
    results[i,] = c(current_week, current_year, weighted_varpi)
  }
  return(results)
}

# FIT EACH PROPENSITY AND CALCULATE THE VARIANCE!

## Aggregate
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_082621.RDS")
agg_results = calculate_varpis(propensities, model, fb_data)

## No Age
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
propensities = readRDS("../data/smoothedpropensities_noage_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_noage = 1-agg_results[,3]/results[,3]

## No Race
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_norace_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_norace = 1-agg_results[,3]/results[,3]

## No Ethnicity
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_noethnicity_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_noethnicity = 1-agg_results[,3]/results[,3]

## No Gender
model = ~ -1+as.factor(fever) + as.factor(contact) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_nogender_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_nogender = 1-agg_results[,3]/results[,3]

## No Symptoms
model = ~ -1+ as.factor(gender) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_nosymptoms_091521.RDS")
results_nosymptoms = calculate_varpis(propensities, model, fb_data)
alpha_t_nosymptoms = 1-agg_results[,3]/results_nosymptoms[,3]


## GGPLOT OF ETHNICITY, RACE, SYMPTOMS
library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

my_palette <- brewer.pal(name="Greys",n=9)[seq(3,9,3)]

alpha_plot_df = data.frame(results_nosymptoms)
alpha_plot_df$X3 = 1-agg_results[,3]/results_nosymptoms[,3]
names(alpha_plot_df) = c("week", "year", "alpha_t")
alpha_plot_df$Method = "No Symptoms"

alpha_plot_df$date = MMWRweek::MMWRweek2Date(MMWRyear = alpha_plot_df$year,
                                          MMWRweek = alpha_plot_df$week,
                                          MMWRday = 1)

alpha_plot_df = rbind(alpha_plot_df,alpha_plot_df, alpha_plot_df)

alpha_plot_df[(nrow(results)+1):(2*nrow(results)),]$alpha_t = alpha_t_norace
alpha_plot_df[(nrow(results)+1):(2*nrow(results)),]$Method = rep("No Race", nrow(results))

alpha_plot_df[(2*nrow(results)+1):(3*nrow(results)),]$alpha_t = alpha_t_noethnicity
alpha_plot_df[(2*nrow(results)+1):(3*nrow(results)),]$Method = rep("No Ethnicity", nrow(results))
subset_alpha_plot_df = subset(alpha_plot_df, alpha_t >= 0)

png(filename = "../figs/alpha_sensitivity_fig.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = subset_alpha_plot_df, aes(x = date, y = alpha_t, group = Method)) +
  ylim(0,0.08)+ geom_point(size = 5, aes(color = Method, shape = Method)) +
  labs(x = "Date",
       y = expression(alpha)) + 
  # theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) + theme_minimal() + 
  geom_smooth(method = "loess", aes(color = Method)) + 
  theme(text = element_text(size=20))

dev.off()

y = subset(alpha_plot_df, Method == "No Symptoms" & alpha_t >= 0)$alpha_t
x = as.numeric(subset(alpha_plot_df, Method == "No Symptoms" & alpha_t >= 0)$date)
fit = loess(y ~ x)
# plot(x, y)
x2 = as.numeric(subset(alpha_plot_df, Method == "No Symptoms")$date)
y2 = predict(fit, x2)
# lines(x2, y2)

alpha_t_optionone = y2


y = subset(alpha_plot_df, Method == "No Race" & alpha_t >= 0)$alpha_t
x = as.numeric(subset(alpha_plot_df, Method == "No Race" & alpha_t >= 0)$date)
fit = loess(y ~ x)
# plot(x, y)
x2 = as.numeric(subset(alpha_plot_df, Method == "No Race")$date)
y2 = predict(fit, x2)
# lines(x2, y2)

alpha_t_optiontwo = y2

y = subset(alpha_plot_df, Method == "No Ethnicity" & alpha_t >= 0)$alpha_t
x = as.numeric(subset(alpha_plot_df, Method == "No Ethnicity" & alpha_t >= 0)$date)
fit = loess(y ~ x)
# plot(x, y)
x2 = as.numeric(subset(alpha_plot_df, Method == "No Ethnicity")$date)
y2 = predict(fit, x2)
# lines(x2, y2)

alpha_t_optionthree = y2

