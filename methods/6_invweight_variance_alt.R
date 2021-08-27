## Distance Functions

indiana_distance <- function(current_week, current_year) {
  ind_dist <- function(row) {
    rowyear = year(row$startdate)
    rowweek = week(row$startdate)
    if (rowyear == current_year){
      dis = abs(rowweek - current_week)
    } else {
      if (rowyear == 2020) {
        dis = (53 - rowweek) + current_week
      } else {
        dis = (rowweek) + (53-current_week)
      }
    }
    return(dis)
  }
  return(ind_dist)
}

fb_distance <-  function(current_week, current_year) {
  fb_dist <- function(row) {
    rowyear = as.numeric(row[7])
    rowweek = as.numeric(row[6])
    if (rowyear == current_year){
      dis = abs(rowweek - current_week)
    } else {
      if (rowyear == 2020) {
        dis = (53 - rowweek) + current_week
      } else {
        dis = (rowweek) + (53-current_week)
      }
    }
    return(dis)
  }
  return(fb_dist)
}

kernelweight <- function(distance) {
  kw2 <- function(row) {
    dis = distance(row)
    h = sqrt(-1/(2*log(0.7))) # Set a deterministic bandwidth for now
    kernel = exp(-dis^2/(2*h^2))
    return(kernel)
  }
  return(kw2)
}


## INV WEIGHT COMPUTATION
indiana_data = readRDS("../data/weeklycoviddata_withsymptoms_alt.RDS")
propensities = readRDS("../data/smoothedpropensities_alt_08262021.RDS")
estimates = readRDS("../data/invweights_alt_08262021.RDS")
estimates = data.frame(estimates)
names(estimates) = c("week", "year", "unweighted", "ipw")

library(MMWRweek)
library(lubridate)

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                            MMWRweek = propensities$week,
                                            MMWRday = 1)
## First term can be precomputed given design matrix
model = ~ -1+as.factor(symptoms) + as.factor(gender) + ethnicity + race + as.factor(age)
indiana_X = model.matrix(model, indiana_data)

library(lubridate)
indiana_data$startdate = mdy(indiana_data$startdate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)

current_week = 42
current_year = 2020
ind_dist <- indiana_distance(current_week, current_year)
## COMPUTE INDIANA DATA TERMS

invweight = vector(length = nrow(indiana_data))
top_row = rep(0, ncol(indiana_X)+1)
for(i in 1:nrow(indiana_data)) {
  modelmatrix_row = indiana_X[i,]
  ind_dist(indiana_data[i,])
  week_row = as.numeric(week(indiana_data$startdate[i]))
  year_row = as.numeric(year(indiana_data$startdate[i]))
  total_tests = indiana_data$covid_tests[i]
  pos_tests = indiana_data$covid_counts[i]
  neg_tests = total_tests - pos_tests
  prev_estimate = estimates$ipw[estimates$week == week_row & estimates$year == current_year]
  if(week_row > 5 & year_row == 2021) {
    invweight[i] = NA
  } else {
    prop_row = which(propensities$week == week_row & propensities$year == year_row)
    term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
    invweight[i] = (1+exp(-term))
  }
  temp_pi = 1/invweight[i]
  temp_odds = (1-temp_pi)/temp_pi
  constant = modelmatrix_row*temp_odds
  top_row[1] = top_row[1] + -(1-FP-FN) * invweight[i]
  top_row[2:length(top_row)] = top_row[2:length(top_row)] + (pos_tests + (-FP-(1-FP-FN)*prev_estimate)*total_tests ) * constant
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
  
  A[1,1] = sum(weights)*(1-FP-FN)
  results[i,1] = current_week
  results[i,2] = current_year
  results[i,3] = (sum(current_counts)/sum(current_tests)-FP)/(1-FP-FN)
  results[i,4] = (sum(current_counts*weights)/sum(current_tests*weights)-FP)/(1-FP-FN)
}

saveRDS(results, "../data/invweights_alt_08262021.RDS")

