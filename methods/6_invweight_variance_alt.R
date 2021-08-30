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
fb_data = readRDS("../data/fb_weeklycomplete.RDS")
names(fb_data)[3] = "symptoms"

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

set_of_weeks = c(14:52, 1:5)
set_of_years = c(rep(2020, length(14:52)), rep(2021, length(1:5)))
results = rep(0,0)

for(current_location in 1:length(set_of_weeks)) {
  current_week = set_of_weeks[current_location]
  current_year = set_of_years[current_location]
  print(paste("On week", current_week, "in year", current_year))
  FP = 0.024
  FN = 0.13
  ind_dist <- indiana_distance(current_week, current_year)
  ## COMPUTE INDIANA DATA TERMS
  top_row = rep(0, ncol(indiana_X)+1)
  var_1 = matrix(0, nrow = ncol(indiana_X)+1, ncol = ncol(indiana_X)+1)
  stability = 6.732e6 # Just a constant to make scale of sums not be problematic (equal to N)
  unique_weights = data.frame("week" = rep(0,0), "year" = rep(0,0), "weight"= rep(0,0))
  
  for(i in 1:nrow(indiana_data)) {
    modelmatrix_row = indiana_X[i,]
    row = indiana_data[i,]
    current_kernel_weight = kernelweight(ind_dist)(row)
    
    week_row = as.numeric(week(indiana_data$startdate[i]))
    year_row = as.numeric(year(indiana_data$startdate[i]))
    
    total_tests = indiana_data$covid_tests[i]
    pos_tests = indiana_data$covid_counts[i]
    neg_tests = total_tests - pos_tests
    
    if(week_row > 5 & year_row == 2021) {
      temp_weight = NA
    } else {
      if(!(is.element(week_row, unique_weights$week) & is.element(year_row, unique_weights$year))) {
        unique_weights[nrow(unique_weights)+1,] = c(week_row, year_row, current_kernel_weight)
      }
      prop_row = which(propensities$week == week_row & propensities$year == year_row)
      term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
      temp_weight = (1+exp(-term))
      temp_pi = as.numeric(1/temp_weight)
      temp_odds = (1-temp_pi)/temp_pi
      constant = modelmatrix_row*temp_odds
      prev_estimate = estimates$ipw[estimates$week == week_row & estimates$year == year_row]
      if(week_row == current_week & year_row == current_year) {
        top_row[1] = top_row[1] + -(1-FP-FN) * temp_weight * total_tests / stability
        top_row[2:length(top_row)] = top_row[2:length(top_row)] + (pos_tests + (-FP-(1-FP-FN)*prev_estimate)*total_tests ) * constant/stability
        
        var_1[1,1] = var_1[1,1] + temp_odds*temp_weight* ( (1-FP - (1-FP-FN) *prev_estimate)^2 * pos_tests + (-FP-(1-FP-FN) *prev_estimate)^2 * neg_tests) / stability^2
      }
      
      zeta = (1- FP - (1-FP-FN) * prev_estimate) * pos_tests + (0- FP - (1-FP-FN) * prev_estimate) * neg_tests
      var_1[1,2:ncol(var_1)] = var_1[1,2:ncol(var_1)] + current_kernel_weight * temp_odds * zeta * modelmatrix_row / stability^2
      var_1[2:ncol(var_1),1] = var_1[2:ncol(var_1),1] + current_kernel_weight * temp_odds * zeta * modelmatrix_row / stability^2
      var_1[2:ncol(var_1),2:ncol(var_1)] = var_1[2:ncol(var_1),2:ncol(var_1)] + current_kernel_weight * (1-temp_pi) * outer(modelmatrix_row,modelmatrix_row) / stability^2
    }
  }
  
  var_1[1,2:ncol(var_1)] = var_1[1,2:ncol(var_1)]/sum(unique_weights$weight)
  var_1[2:ncol(var_1),1] = var_1[2:ncol(var_1),1]/sum(unique_weights$weight)
  var_1[2:ncol(var_1),2:ncol(var_1)] = var_1[2:ncol(var_1),2:ncol(var_1)]/sum(unique_weights$weight)
  prev_estimate = estimates$ipw[estimates$week == current_week & estimates$year == current_year]
  
  c(prev_estimate - 1.97 * sqrt(var_1[1,1]/top_row[1]^2),
    prev_estimate,
    prev_estimate + 1.97 * sqrt(var_1[1,1]/top_row[1]^2)
  )
  
  ## SAVE SPACE AND NEW MODEL MATRIX
  # rm(indiana_X)
  fb_X = model.matrix(model, fb_data)
  fb_dist = fb_distance(current_week, current_year)
  var_2 = middle_block = matrix(0, nrow = ncol(fb_X), ncol = ncol(fb_X))
  
  fb_mean = matrix(0, nrow = nrow(unique_weights), ncol = ncol(fb_X))
  ## COMPUTE MEAN
  for(i in 1:nrow(fb_data)) {
    row = fb_data[i,]
    modelmatrix_row = fb_X[i,]
    week_row = fb_data$week[i]
    year_row = fb_data$year[i]
    prev_estimate = estimates$ipw[estimates$week == week_row & estimates$year == year_row]
    which_row = which(unique_weights$week == week_row & unique_weights$year == year_row)
    if(!(week_row > 5 & year_row == 2021)) {
      prop_row = which(propensities$week == week_row & propensities$year == year_row)
      term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
      temp_weight = (1+exp(-term))
      temp_pi = 1/temp_weight
      fb_mean[which_row,] = fb_mean[which_row,] + as.numeric(temp_pi * row$weight) * modelmatrix_row / stability
    }
  }
  
  middle_block = var_2 = matrix(0, ncol = ncol(fb_X), nrow = ncol(fb_X))
  for(i in 1:nrow(fb_data)) {
    row = fb_data[i,]
    current_kernel_weight = kernelweight(fb_dist)(row)/sum(unique_weights$weight)
    modelmatrix_row = fb_X[i,]
    week_row = fb_data$week[i]
    year_row = fb_data$year[i]
    prev_estimate = estimates$ipw[estimates$week == week_row & estimates$year == year_row]
    which_row = which(unique_weights$week == week_row & unique_weights$year == year_row)
    total_weight = sum(fb_data$weight[fb_data$week == week_row & fb_data$year == year_row])
    if(!(week_row > 5 & year_row == 2021)) {
      prop_row = which(propensities$week == week_row & propensities$year == year_row)
      term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
      temp_weight = (1+exp(-term))
      temp_pi = 1/temp_weight
      constant = as.numeric(row$weight * temp_pi*(1-temp_pi))
      middle_block = middle_block +  constant * outer(modelmatrix_row , modelmatrix_row) / stability
      
      constant2 = as.numeric(temp_pi) * modelmatrix_row / stability
      error = (constant2 - fb_mean[which_row,])
      var_2 = var_2 + current_kernel_weight^2 * row$weight^2 * outer(error, error) / total_weight^2
    }
    
  }
  
  complete_var = var_1
  complete_var[2:ncol(complete_var),2:ncol(complete_var)] = complete_var[2:ncol(complete_var),2:ncol(complete_var)] + var_2
  
  first_term = complete_var * 0
  first_term[1,] = top_row
  first_term[,1] = top_row
  first_term[2:ncol(first_term),2:ncol(first_term)] = middle_block
  
  variance = solve(first_term, complete_var)%*%solve(first_term)
  
  stderr_approx1 = sqrt(diag(variance)[1])
  
  stderr_approx2 = sqrt(var_1[1,1]/first_term[1]^2)
  
  prev_estimate = estimates$ipw[estimates$week == current_week & estimates$year == current_year]
  
  c(prev_estimate - 1.97 * stderr_approx1,
    prev_estimate,
    prev_estimate + 1.97 * stderr_approx1
  )
  
  c(prev_estimate - 1.97 * stderr_approx2,
    prev_estimate,
    prev_estimate + 1.97 * stderr_approx2
  )
  
  ## ADDING IN THE UNCERTAINTY IN FP AND FN
  n_FP = 59
  n_FN = 45
  
  full_first_term = full_var = matrix(0, nrow = nrow(complete_var) + 2, ncol = ncol(complete_var) + 2)
  full_var[1:nrow(complete_var), 1:ncol(complete_var)] = complete_var      
  full_var[nrow(complete_var)+1, ncol(complete_var)+1] = 1/n_FP * (FP*(1-FP))^(-1)
  full_var[nrow(complete_var)+2, ncol(complete_var)+2] = 1/n_FN * (FN*(1-FN))^(-1)
  
  
  full_first_term[1:nrow(first_term), 1:ncol(first_term)] = first_term
  full_first_term[nrow(first_term)+1, ncol(first_term)+1] = (FP*(1-FP))^(-1)
  full_first_term[nrow(first_term)+2, ncol(first_term)+2] = (FN*(1-FN))^(-1)
  
  complete_variance = solve(full_first_term, full_var)%*%solve(full_first_term)
  
  stderr_approx3 = sqrt(diag(complete_variance)[1])
  
  temp= c(current_week, current_year, prev_estimate, stderr_approx3, 
          prev_estimate - 1.97 * stderr_approx3,
          prev_estimate + 1.97 * stderr_approx3)
  
  print(temp[c(5,3,6)])
  
  results = rbind(results, 
                  c(current_week, current_year, prev_estimate, stderr_approx3, 
                    prev_estimate - 1.97 * stderr_approx3,
                    prev_estimate + 1.97 * stderr_approx3)
  )
}

results

saveRDS(results, "../data/ipw_cis.RDS")
