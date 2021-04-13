indiana_data = readRDS("../data/weeklycoviddata_withfever.RDS")
propensities = readRDS("../data/smoothedpropensities.RDS")

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                            MMWRweek = propensities$week,
                                            MMWRday = 1)
## First term can be precomputed given design matrix
model = ~ -1+as.factor(fever) + as.factor(gender) + ethnicity + race
indiana_X = model.matrix(model, indiana_data)

library(lubridate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)

invweight = vector(length = nrow(indiana_data))
for(i in 1:nrow(indiana_data)) {
  modelmatrix_row = indiana_X[i,]
  week_row = as.numeric(week(indiana_data$startdate[i]))
  year_row = as.numeric(year(indiana_data$startdate[i]))
  prop_row = which(propensities$week == week_row & propensities$year == year_row)
  term = crossprod(t(propensities[prop_row,3:9]),modelmatrix_row)
  prob = 1/(1+exp(-term))
  invweight[i] = 1/prob
}

current_week = 45
current_year = 2020

weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 4) 

for(i in 1:length(weeks)) {
  current_week = weeks[i]
  current_year = years[i]
  current_counts = indiana_data$covid_counts[indiana_data$week == current_week & indiana_data$year == current_year]
  current_tests = indiana_data$covid_tests[indiana_data$week == current_week & indiana_data$year == current_year]
  weights = invweight[indiana_data$week == current_week & indiana_data$year == current_year]
  results[i,1] = current_week
  results[i,2] = current_year
  results[i,3] = sum(current_counts)/sum(current_tests)
  results[i,4] = sum(current_counts*weights)/sum(current_tests*weights) 
}

saveRDS(results, "../data/invweights.RDS")
