## Bring in libraries
library(lubridate)

## Define Functions

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
    h = sqrt(-1/(2*log(0.75))) # Set a deterministic bandwidth for now
    kernel = exp(-dis^2/(2*h^2))
    return(kernel)
  }
  return(kw2)
}

compute_probs <- function(X, theta) {
  return(1/(1+exp(-X%*%theta)))
}

irls <- function(current_week, current_year, fb_X, fb_data, 
                 max.iter = 20, min.tol = 1e-10) {
  
  fb_dist = fb_distance(current_week, current_year)
  weights = apply(fb_data, 1, kernelweight(fb_dist))
  weighted_symptoms = weights*fb_data$weighthospital
  first_term = t(fb_X)%*%weighted_symptoms

  fbweighted = weights*fb_data$weight
  
  # base_rate = sum(weighted_fever)/sum(fbweighted)
  # theta_0 = -log(1/base_rate-1)
  
  # current_theta = c(theta_0,rep(0,ncol(fb_X)-1))
  current_theta = rep(0,ncol(fb_X))
  for(iter in 1:max.iter){
    
    current_probs = compute_probs(fb_X, current_theta)
    firstder_complete_weight = as.vector(fbweighted*current_probs)
    secondder_complete_weight = as.vector(fbweighted*current_probs*(1-current_probs))
    
    second_term = t(fb_X)%*%firstder_complete_weight
    hessian = t(fb_X)%*%diag(secondder_complete_weight)%*%fb_X
    
    old_theta = current_theta
    current_theta = current_theta + solve(hessian, first_term - second_term)
    # print(current_theta)
    tol = old_theta - current_theta
    if(max(abs(tol)) < min.tol){break}
  }
  print(max(abs(tol)))
  return(current_theta)
}


## Brining in complete FB and Indiana data
fb_data_pos_hospital = readRDS("../data/fb_weeklycomplete_pos_hospital_alt.RDS")
names(fb_data_pos_hospital)[3] = "symptoms"

## Build \pi(x; \theta)
## Logistic regression with no interactions

## First term can be precomputed given design matrix
model = ~ -1+as.factor(gender)+as.factor(age) + symptoms
fb_X = model.matrix(model, fb_data_pos_hospital)

weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 2+ncol(fb_X))

for(i in 1:length(weeks)) {
  print(paste("On week", weeks[i], "in year", years[i]))
  current_week = weeks[i]
  current_year = years[i]
  current_estimated_theta = irls(current_week, current_year, fb_X, fb_data_pos_hospital)  
  print(current_estimated_theta)
  results[i,] = c(current_week, current_year, as.vector(current_estimated_theta))
}

results = data.frame(results)
names(results) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                   "55to64", "65to74", "75plus", "symptom")

saveRDS(results, "../data/smoothedpropensities_pos_hospital_alt.RDS")
