## Bring in libraries
library(lubridate)

## Define Functions

indiana_distance <- function(row) {
  rowyear = year(row[1])
  rowweek = week(row[1])
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

fb_distance <- function(row) {
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

kernelweight <- function(distance) {
  kw2 <- function(row) {
    dis = distance(row)
    h = sqrt(-1/(2*log(0.75))) # Set a deterministic bandwidth for now
    kernel = exp(-dis^2/(2*h^2))
    return(kernel)
    }
  return(kw2)
}

compute_probs <- function(X, theta) {
  return(1/(1+exp(-X%*%theta)))
}

irls <- function(fb_X, indiana_X, indiana_data, fb_data, max.iter = 20, min.tol = 1e-10) {

  weights = apply(indiana_data, 1, kernelweight(indiana_distance))
  weighted_tests = weights*indiana_data$covid_tests
  first_term = t(indiana_X)%*%weighted_tests
  
  fbweights = apply(fb_data, 1, kernelweight(fb_distance))
  fbweighted_tests = fbweights*fb_data$weight
  
  base_rate = sum(weighted_tests)/sum(fbweighted_tests)
  theta_0 = -log(1/base_rate-1)
  
  current_theta = c(theta_0,rep(0,ncol(fb_X)-1))
  # current_theta = rep(0,ncol(fb_X))
  for(iter in 1:max.iter){
    
    current_probs = compute_probs(fb_X, current_theta)
    firstder_complete_weight = as.vector(fbweighted_tests*current_probs)
    secondder_complete_weight = as.vector(fbweighted_tests*current_probs*(1-current_probs))
    
    second_term = t(fb_X)%*%firstder_complete_weight
    hessian = t(fb_X)%*%diag(secondder_complete_weight)%*%fb_X
    
    old_theta = current_theta
    current_theta = current_theta + solve(hessian, first_term - second_term)
    # print(current_theta)
    tol = old_theta - current_theta
    # print(max(abs(tol)))
    if(max(abs(tol)) < min.tol){break}
  }
  return(current_theta)
}



## Brining in complete FB and Indiana data

indiana_data = readRDS("../data/weeklycoviddata_withfever.RDS")
fb_data = readRDS("../data/fb_weeklycomplete.RDS")

## Build \pi(x; \theta)
## Logistic regression with no interactions

## First term can be precomputed given design matrix
model = ~ 1 + fever + ethnicity + race + as.factor(gender)
indiana_X = model.matrix(model, indiana_data)
fb_X = model.matrix(model, fb_data)

weeks = c(25:53,1:5)
years = c(rep(2020, length = length(c(25:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 2+ncol(fb_X))

for(i in 1:length(weeks)) {
  print(paste("On week", weeks[i], "in year", years[i]))
  current_week = weeks[i]
  current_year = years[i]
  current_estimated_theta = irls(fb_X, indiana_X, indiana_data, fb_data)  
  results[i,] = c(current_week, current_year, as.vector(current_estimated_theta))
}

results = data.frame(results)
names(results) = c("week", "year", "Intercept", "feverTRUE", "NotHoL", "raceAA", "raceOther", "raceWhite", "genderF")

saveRDS(results, "../data/smoothedpropensities.RDS")
