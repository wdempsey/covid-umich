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
alpha_seq = rep(0.001,length(weeks))
rsq_seq = rep(0.01, length(weeks))
results = calculate_delta(propensities, model, indiana_data, alpha_seq, rsq_seq)

lines(results[,3]*alpha_seq/(1-alpha_seq) * ipw_results[,3], ylim = c(0,0.2))
