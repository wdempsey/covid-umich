## RUN 13 & 14 FIRST TO GET THE SEQUENCES
## Read in data
fb_data = readRDS("../data/fb_weeklycomplete.RDS")
names(fb_data)[3] = "fever"
fb_data$contact = as.logical(fb_data$contact)

model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_082621.RDS")
weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))

## CALCULATE THE IPW TERM:

calculate_ipw <- function(propensities, model, fb_data) {
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_fb_data = subset(fb_data, week == current_week & year == current_year)
    current_fb_X = model.matrix(model, current_fb_data)
    current_params = propensities[which(weeks == current_week & years == current_year) ,3:ncol(propensities)]
    pis = 1/(1+exp(-current_fb_X%*%t(current_params)))
    invpis = 1/pis
    expected_weights = sum(invpis*current_fb_data$weight)/sum(current_fb_data$weight)
    results[i,] = c(current_week, current_year, expected_weights)
  }
  return(results)
}

ipw_results = calculate_ipw(propensities, model, fb_data)

## GIVEN ALPHA AND RSEQ, COMPUTE BIAS

calculate_delta <- function(propensities, model, indiana_data, alpha_seq, rsq_seq) {
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_indiana_data = subset(indiana_data, week == current_week & year == current_year)
    current_indiana_X = model.matrix(model, current_indiana_data)
    current_alpha = alpha_seq[i]
    current_rsq = rsq_seq[i]
    current_params = propensities[which(weeks == current_week & years == current_year) ,3:ncol(propensities)]
    pis = 1/(1+exp(-current_indiana_X%*%t(current_params)))
    current_rmse = agg_rmse[i,3]
    current_numerator = sum(
      (trigamma(pis*(1/current_alpha - 1) + 1) + trigamma((1-pis)*(1/current_alpha - 1)))*
        round(current_indiana_data$covid_tests))
    current_deltasq = solve(current_numerator/current_rmse, current_rsq)
    current_delta = sqrt(current_deltasq)
    results[i,] = c(current_week, current_year, current_delta)
  }
  return(results)
}

indiana_data = readRDS("../data/weeklycoviddata_withsympcontact.RDS")
library(lubridate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)
alpha_seq = alpha_t_optionone # rep(0.001,length(weeks))
rsq_seq = rsq_t_optionone # rep(0.1, length(weeks))
results = calculate_delta(propensities, model, indiana_data, alpha_seq, rsq_seq)
alpha_seq2 = alpha_t_optiontwo # rep(0.001,length(weeks))
rsq_seq2 = rsq_t_optiontwo # rep(0.1, length(weeks))
results_two = calculate_delta(propensities, model, indiana_data, alpha_seq2, rsq_seq2)
alpha_seq3 = alpha_t_optionthree # rep(0.001,length(weeks))
rsq_seq3 = rsq_t_optionthree # rep(0.1, length(weeks))
results_three = calculate_delta(propensities, model, indiana_data, alpha_seq3, rsq_seq3)

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

bias_plot_df = data.frame(results)
names(bias_plot_df) = c("week", "year", "bias")
bias_plot_df$Method = "No Symptoms"

bias_plot_df$date = MMWRweek::MMWRweek2Date(MMWRyear = bias_plot_df$year,
                                           MMWRweek = bias_plot_df$week,
                                           MMWRday = 1)

bias_plot_df = rbind(bias_plot_df,bias_plot_df, bias_plot_df)

bias_plot_df[(nrow(results)+1):(2*nrow(results)),]$bias = results_two[,3]
bias_plot_df[(nrow(results)+1):(2*nrow(results)),]$Method = rep("No Race", nrow(results))

bias_plot_df[(2*nrow(results)+1):(3*nrow(results)),]$bias = results_three[,3]
bias_plot_df[(2*nrow(results)+1):(3*nrow(results)),]$Method = rep("No Ethnicity", nrow(results))

png(filename = "../figs/bias_sensitivity_fig.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = bias_plot_df, aes(x = date, y = bias, group = Method)) +
  ylim(0,0.2)+ geom_point(aes(shape = Method, color = Method), size = 2) +
  labs(x = "Date",
       y = "Bias") + 
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) + theme_minimal() #+ geom_smooth(method = "loess")

dev.off()
