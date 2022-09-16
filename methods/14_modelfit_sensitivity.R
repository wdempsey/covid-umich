indiana_data = readRDS("../data/weeklycoviddata_withsympcontact.RDS")
library(lubridate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)
indiana_data$covid_counts_round = round(indiana_data$covid_counts)
indiana_data$covid_tests_round = round(indiana_data$covid_tests)
indiana_data$covid_negtests_round = indiana_data$covid_tests_round - indiana_data$covid_counts_round

calculate_RMSE <- function(model, indiana_data) {
  weeks = c(14:53,1:5)
  years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_indiana_data = subset(indiana_data, week == current_week & year == current_year)
    current_indiana_X = model.matrix(model, current_indiana_data)
    success = current_indiana_data$covid_counts_round
    failure = current_indiana_data$covid_negtests_round
    Q_fit = glm( cbind(success,failure) ~ current_indiana_X-1, family = "binomial")$fitted.values
    RMSE = sum((Q_fit - 1)^2 * success + (Q_fit-0)^2 * failure)
    results[i,] = c(current_week, current_year, RMSE)
  }
  
  results
}

## Aggregate
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
agg_rmse = calculate_RMSE(model, indiana_data)

## No Age
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
rmse = calculate_RMSE(model, indiana_data)
r_sq_noage = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Race
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_norace = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Ethnicity
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_noethnicity = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Gender
model = ~ -1+as.factor(fever) + as.factor(contact) + ethnicity + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_nogender = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Symptoms
model = ~ -1 + as.factor(gender) + ethnicity + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_nosymptoms = (rmse[,3] - agg_rmse[,3])/rmse[,3]


