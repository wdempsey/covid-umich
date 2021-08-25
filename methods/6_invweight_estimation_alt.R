indiana_data = readRDS("../data/weeklycoviddata_withsymptoms_alt.RDS")
indiana_data$startdate = mdy(indiana_data$startdate)
propensities = readRDS("../data/smoothedpropensities_alt_08252021.RDS")

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                            MMWRweek = propensities$week,
                                            MMWRday = 1)
## First term can be precomputed given design matrix
model = ~ -1+as.factor(symptoms) + as.factor(gender) + ethnicity + race + as.factor(age)
indiana_X = model.matrix(model, indiana_data)

library(lubridate)
indiana_data$startdate = ymd(indiana_data$startdate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)

invweight = vector(length = nrow(indiana_data))
for(i in 1:nrow(indiana_data)) {
  modelmatrix_row = indiana_X[i,]
  week_row = as.numeric(week(indiana_data$startdate[i]))
  year_row = as.numeric(year(indiana_data$startdate[i]))
  if(week_row > 5 & year_row == 2021) {
    invweight[i] = NA
  } else {
    prop_row = which(propensities$week == week_row & propensities$year == year_row)
    term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
    invweight[i] = (1+exp(-term))
  }
}

indiana_data$invweight = invweight

current_week = 48
current_year = 2020
FP = 0.024
FN = 0.13

weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 4) 

for(i in 1:length(weeks)) {
  current_week = weeks[i]
  current_year = years[i]
  print(paste("At week", current_week, "in year", current_year))
  current_counts = indiana_data$covid_counts[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  current_tests = indiana_data$covid_tests[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  weights = indiana_data$invweight[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  results[i,1] = current_week
  results[i,2] = current_year
  results[i,3] = (sum(current_counts)/sum(current_tests)-FP)/(1-FP-FN)
  results[i,4] = (sum(current_counts*weights)/sum(current_tests*weights)-FP)/(1-FP-FN)
}

saveRDS(results, "../data/invweights_alt_08252021.RDS")

