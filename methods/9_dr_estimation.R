## Libraries
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)
library(MMWRweek)

## INDIANA AND PROPENSITIES
indiana_data = readRDS("../data/weeklycoviddata_withsymptoms_alt.RDS")
indiana_data$startdate = mdy(indiana_data$startdate)
propensities = readRDS("../data/smoothedpropensities_alt_08262021.RDS")

### MODEL BASED ESTIMATES
# aggregate_air = readRDS("../data/aggregate_air_2022_24_03.RDS")
# strata_air = readRDS("../data/modelbased_air_bystrata_2022_24_03.RDS")
# air_flags = readRDS("../data/modelbased_flags_2022_24_03.RDS")

### MODEL BASED ESTIMATES: LOWER IFR
aggregate_air = readRDS("../data/aggregate_air_lowerifr_2022_24_03.RDS")
strata_air = readRDS("../data/modelbased_air_bystrata_lowerifr_2022_24_03.RDS")
air_flags = readRDS("../data/modelbased_flags_lowerifr_2022_24_03.RDS")

### MODEL BASED ESTIMATES: HIGHER IFR
# aggregate_air = readRDS("../data/aggregate_air_2022_24_03.RDS")
# strata_air = readRDS("../data/modelbased_air_bystrata_2022_24_03.RDS")
# air_flags = readRDS("../data/modelbased_flags_2022_24_03.RDS")


### FB Data
fb_data = readRDS("../data/fb_weeklycomplete.RDS")

##  Death DATA
df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath$week = week(df_coviddeath$startdate)
df_coviddeath$year = year(df_coviddeath$startdate)
df_coviddeath <- aggregate(covid_deaths ~ age + gender + ethnicity + race + week + year, data = df_coviddeath, FUN = sum)

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                            MMWRweek = propensities$week,
                                            MMWRday = 1)
## First term can be precomputed given design matrix
model = ~ -1+as.factor(symptoms) + as.factor(gender) + ethnicity + race + as.factor(age)
indiana_X = model.matrix(model, indiana_data)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)
indiana_data$age[indiana_data$age > 1] = (indiana_data$age[indiana_data$age > 1] - 1)

invweight = complete_air = vector(length = nrow(indiana_data))
for(i in 1:nrow(indiana_data)) {
  modelmatrix_row = indiana_X[i,]
  week_row = as.numeric(week(indiana_data$startdate[i]))
  year_row = as.numeric(year(indiana_data$startdate[i]))
  if(week_row > 5 & year_row == 2021) {
    invweight[i] = NA
  } else {
    isflagged = subset(air_flags, race == indiana_data$race[i] &
                         ethnicity == indiana_data$ethnicity[i] &
                         gender == indiana_data$gender[i] &
                         age == indiana_data$age[i])$flagged
    if(isflagged) {
      row_air = mean(subset(aggregate_air, week == week_row & year == year_row)$estimate)
    } else{
      strata_subset = subset(strata_air, race == indiana_data$race[i] &
                               ethnicity == indiana_data$ethnicity[i] &
                               gender == indiana_data$gender[i] &
                               age == indiana_data$age[i] & 
                               week == week_row &
                               year == year_row)
      if(nrow(strata_subset) == 0) {
        row_air = mean(subset(aggregate_air, week == week_row & year == year_row)$estimate)
      } else {
        row_air = mean(strata_subset$air)
      }
    }

    complete_air[i] = row_air    
    prop_row = which(propensities$week == week_row & propensities$year == year_row)
    term = as.matrix(propensities[prop_row,3:15])%*%modelmatrix_row
    invweight[i] = (1+exp(-term))
  
  }
}

indiana_data$air = complete_air
indiana_data$invweight = invweight

## FB weighted estimate
complete_air = vector(length = nrow(fb_data))
fb_data$gender = as.factor(fb_data$gender)
levels(fb_data$gender) = c("M", "F")
fb_data$age[fb_data$age>1] = fb_data$age[fb_data$age>1] -1

for(i in 1:nrow(fb_data)) {
  current_row = fb_data[i,]
  week_row = current_row$week
  year_row = current_row$year
  isflagged = subset(air_flags, race == current_row$race &
                       ethnicity == current_row$ethnicity &
                       gender == current_row$gender &
                       age == current_row$age)$flagged
  if(isflagged) {
    row_air = mean(subset(aggregate_air, week == week_row & year == year_row)$estimate)
  } else{
    strata_subset = subset(strata_air, race == current_row$race &
                             ethnicity == current_row$ethnicity &
                             gender == current_row$gender &
                             age == current_row$age & 
                             week == week_row &
                             year == year_row)
    if(nrow(strata_subset) == 0) {
      row_air = mean(subset(aggregate_air, week == week_row & year == year_row)$estimate)
    } else {
      row_air = mean(strata_subset$air)
    }
    complete_air[i] = row_air    
  }
}
fb_data$air = complete_air

current_week = 48
current_year = 2020

weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 5) 
FP= 0.024
FN = 0.13

for(i in 1:length(weeks)) {
  current_week = weeks[i]
  current_year = years[i]
  print(paste("At week", current_week, "in year", current_year))
  current_counts = indiana_data$covid_counts[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  current_tests = indiana_data$covid_tests[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  weights = indiana_data$invweight[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  air = indiana_data$air[which(indiana_data$week == current_week & indiana_data$year == current_year)]
  invweighted_air = sum(air*weights, na.rm = TRUE)/sum(weights, na.rm = TRUE)
  fb_air = sum(fb_data$air * fb_data$weight, na.rm = TRUE)/ sum(fb_data$weight, na.rm = TRUE)
  results[i,1] = current_week
  results[i,2] = current_year
  results[i,3] = (sum(current_counts)/sum(current_tests) - FP)/(1-FP-FN)
  results[i,4] = (sum(current_counts*weights)/sum(current_tests*weights) - FP)/(1-FP-FN)
  results[i,5] = fb_air + (results[i,4] - invweighted_air)
}

# saveRDS(results,"../data/drestimates_alt_2022_24_03.RDS")
saveRDS(results,"../data/drestimates_alt_lowerifr_2022_24_03.RDS")
# saveRDS(results,"../data/drestimates_alt_upperifr_2022_24_03.RDS")

