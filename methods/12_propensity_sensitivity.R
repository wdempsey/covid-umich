## Bring in libraries
library(lubridate)

## Define Functions
indiana_distance <- function(current_week, current_year) {
  ind_dist <- function(row) {
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

compute_probs <- function(X, theta) {
  return(1/(1+exp(-X%*%theta)))
}

irls <- function(current_week, current_year, fb_X, indiana_X, 
                 indiana_data, fb_data, max.iter = 100, min.tol = 1e-20) {

  ind_dist = indiana_distance(current_week, current_year)
  weights = apply(indiana_data, 1, kernelweight(ind_dist))
  weighted_tests = weights*indiana_data$covid_tests
  first_term = t(indiana_X)%*%weighted_tests
  
  fb_dist = fb_distance(current_week, current_year)
  fbweights = apply(fb_data, 1, kernelweight(fb_dist))
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

irls_fitmodel <- function(model) {
  indiana_X = model.matrix(model, indiana_data)
  fb_X = model.matrix(model, fb_data)
  
  weeks = c(14:53,1:5)
  years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
  results = matrix(nrow = length(weeks), ncol = 2+ncol(fb_X))
  
  for(i in 1:length(weeks)) {
    print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_estimated_theta = irls(current_week, current_year, fb_X, indiana_X, indiana_data, fb_data)  
    print(current_estimated_theta)
    results[i,] = c(current_week, current_year, as.vector(current_estimated_theta))
  }
  
  results = data.frame(results)
  return(results)
}


## Bringing in complete FB and Indiana data
setwd("~/../Dropbox/Fb-Data/sensitivityanalysis/")
indiana_data = readRDS("./weeklycoviddata_withsympcontact.RDS")
fb_data = readRDS("./fb_weeklycomplete.RDS")
names(fb_data)[3] = "fever"
fb_data$contact = as.logical(fb_data$contact)

## How do we match ages?
## FB <--> INDIANA
## 18-24 (1) <--> 0-19 and 20-29.
## 25--34 (2) <--> 30--39.
## 35--44 (3) <--> 40--49
## 45-54 years (4) <--> 50--59
## 55-64 years (5) <--> 60--69
## 65-74 years (6) <--> 70--79
## 75 years or older (7) <--> 80+

## Build \pi(x; \theta)
## Logistic regression with no interactions

## First term can be precomputed given design matrix
# complete_model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
# names(complete_results) = c("week", "year", "feverFALSE", "feverTRUE", "contactTRUE",
  # "genderF", "NotHoL", "raceAA", "raceOther", "raceWhite", 
  # "25to34", "35to44", "45to54", "55to64", "65to74", "75plus")
## Model: no age
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
results = irls_fitmodel(model)
names(results) = c("week", "year", "feverFALSE", "feverTRUE", "contactTRUE",
                   "genderF", "NotHoL", "raceAA", "raceOther", "raceWhite")
saveRDS(results, "./smoothedpropensities_noage_091521.RDS")
## Model: no race
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + as.factor(age)
results = irls_fitmodel(model)
names(results) = c("week", "year", "feverFALSE", "feverTRUE", "contactTRUE",
                   "genderF", "NotHoL", 
                   "25to34", "35to44", "45to54", "55to64", "65to74", "75plus")
saveRDS(results, "./smoothedpropensities_norace_091521.RDS")
## Model: no ethnicity
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + race + as.factor(age)
results = irls_fitmodel(model)
names(results) = c("week", "year", "feverFALSE", "feverTRUE", "contactTRUE",
                   "genderF", "raceAA", "raceOther", "raceWhite", 
                   "25to34", "35to44", "45to54", "55to64", "65to74", "75plus")
saveRDS(results, "./smoothedpropensities_noethnicity_091521.RDS")
## No gender
model = ~ -1+as.factor(fever) + as.factor(contact) + ethnicity + race + as.factor(age)
results = irls_fitmodel(model)
names(results) = c("week", "year", "feverFALSE", "feverTRUE", "contactTRUE",
                   "NotHoL", "raceAA", "raceOther", "raceWhite", 
                   "25to34", "35to44", "45to54", "55to64", "65to74", "75plus")
saveRDS(results, "./smoothedpropensities_nogender_091521.RDS")
## No fever/contact (i.e., symptoms)
model = ~ -1+as.factor(gender) + ethnicity + race + as.factor(age)
results = irls_fitmodel(model)
names(results) = c("week", "year", 
                   "genderF", "NotHoL", "raceAA", "raceOther", "raceWhite", 
                   "25to34", "35to44", "45to54", "55to64", "65to74", "75plus")
saveRDS(results, "./smoothedpropensities_nosymptoms_091521.RDS")
