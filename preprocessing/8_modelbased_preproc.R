## Libraries
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)

## FUNCTIONS
distance_function <-  function(current_week, current_year) {
  fb_dist <- function(row) {
    rowyear = as.numeric(row[3])
    rowweek = as.numeric(row[2])
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
    h = sqrt(-1/(2*log(0.98))) # Set a deterministic bandwidth for now
    kernel = exp(-dis^2/(2*h^2))
    return(kernel)
  }
  return(kw2)
}

## Model-based estimation
## How do we match ages?
## FB <--> INDIANA
## 18-24 (1) <--> 0-19 and 20-29.
## 25--34 (2) <--> 30--39.
## 35--44 (3) <--> 40--49
## 45-54 years (4) <--> 50--59
## 55-64 years (5) <--> 60--69
## 65-74 years (6) <--> 70--79
## 75 years or older (7) <--> 80+

## FIRST THREE INDIANA WERE COLLAPSE DUE TO LIMITED
## DEATH DATA ON INDIVIDUALS <40 YEARS OF AGE.

df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath$date = ymd(df_coviddeath$startdate)
df_coviddeath_agg <- aggregate(covid_deaths ~ date, data = df_coviddeath, FUN = sum)
dates = ymd(df_coviddeath_agg$date)[-length(df_coviddeath_agg$date)]

# fit_forcing = readRDS("../data/fit_forcing_byage_090121.RDS")
# fit_forcing = readRDS("../data/fit_forcing_byage_lowerifr_032522.RDS")
fit_forcing = readRDS("../data/fit_forcing_byage_upperifr_032522.RDS")

# HOW LONG AS ACTIVE INFECTION
## USE a in model to see how long exposed/infected
# a = summary(fit_forcing, pars = "a", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary
# a_mean = a[1]
## Gamma much larger so 
# gamma = summary(fit_forcing,pars = "gamma", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary
a_mean = -log(0.35)/21

summary_fit = summary(fit_forcing, pars = "pred_cases_per_agegroup", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary
n_days = nrow(summary_fit)/6
dates = c(min(dates)-(n_days - length(dates)):1, dates) ## EXTEND BACKWARDS FOR CASES
timepoints = rep(dates, 6)
age_group = rep(c(1:6), each = length(dates))
summary_fit = data.frame(summary_fit)
summary_fit$timepoints = timepoints
summary_fit$age_group = age_group
summary_fit$week = week(summary_fit$timepoints)
summary_fit$year = year(summary_fit$timepoints)
summary_fit$ai_estimate = rep(NA, nrow(summary_fit))
summary_fit$recovered = rep(NA, nrow(summary_fit))

for (i in 1:nrow(summary_fit)){
  current_day = summary_fit$timepoints[i]
  current_agegrp = summary_fit$age_group[i]
  summary_fit_agesubset = subset(summary_fit, age_group == current_agegrp)
  previous_days = as.numeric(difftime(summary_fit_agesubset$timepoints, current_day, units = "days"))
  exp_rate = a_mean
  weight = exp(exp_rate*previous_days)
  active_infection_temp = sum(weight[previous_days <= 0]*summary_fit_agesubset$mean[summary_fit_agesubset$timepoints <= current_day])
  recovered_temp = sum((1-weight[previous_days <= 0])*summary_fit_agesubset$mean[summary_fit_agesubset$timepoints <= current_day])
  summary_fit$ai_estimate[i] = active_infection_temp
  summary_fit$recovered[i] = recovered_temp
}
# 
summary_aggregate = aggregate(ai_estimate ~ age_group + week + year, data = summary_fit, FUN = mean)
summary_aggregate_recovered = aggregate(recovered ~ age_group + week + year, data = summary_fit, FUN = mean)
summary_aggregate = cbind(summary_aggregate,summary_aggregate_recovered$recovered)
names(summary_aggregate)[5] = "recovered"


## WE NOW NEED DEATH FRACTIONS PER SUBGROUP
## We DO THIS AS A KERNEL ESTIMATOR 
levels(df_coviddeath$age) = c(1,1,1:6)
df_coviddeath$week = week(df_coviddeath$startdate)
df_coviddeath$year = year(df_coviddeath$startdate)

aggregate_death = aggregate(covid_deaths ~ age + week + year + race + ethnicity + gender, data = df_coviddeath, FUN = sum)

ethnicity_levels = levels(aggregate_death$ethnicity)
race_levels = levels(aggregate_death$race)
gender_levels = levels(aggregate_death$gender)

## EACH WEEK, YEAR, AGE COMBINATION
## COMPUTE A WEIGHT FOR EACH COMBO
## OF RACE, ETHNICITY, GENDER USING A 
## KERNEL SMOOTHER

current_week = 45
current_year = 2020
current_age = 4

weighteddeaths <- function(current_week, current_year, current_age) {
  
  allcombinations = expand.grid(ethnicity = ethnicity_levels, 
                                race = race_levels,
                                gender = gender_levels)
  allcombinations$weights = 0 ## BUILDING COVID WEIGHTS
  
  temp_subset = subset(aggregate_death, age == current_age)
  
  for (i in 1:nrow(allcombinations)) {
    current_race = allcombinations$race[i]
    current_ethnicity = allcombinations$ethnicity[i]
    current_gender = allcombinations$gender[i]
    
    temp_subset_reg = subset(temp_subset, race ==current_race & ethnicity == current_ethnicity &
                               gender == current_gender)
    
    dist_func = distance_function(current_week, current_year)
    weights = apply(X = temp_subset_reg, FUN = kernelweight(dist_func),MARGIN = 1)
    temp_coviddeaths = sum(temp_subset_reg$covid_deaths*weights)
    allcombinations$weights[i] = temp_coviddeaths
  }
  
  allcombinations$weights = allcombinations$weights/sum(allcombinations$weights)
  
  return(allcombinations)
  
}

age_range = 1:6
weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = rep(0,0)
for(i in 1:nrow(summary_aggregate)) {
  current_week = summary_aggregate$week[i]
  current_year = summary_aggregate$year[i]
  current_age = summary_aggregate$age_group[i]
  print(paste("At week", current_week, "in year", current_year))
  temp = weighteddeaths(current_week, current_year,current_age)     
  ai_temp = summary_aggregate$ai_estimate[i]
  recovered_temp = summary_aggregate$recovered[i]
  temp$ai_estimate = ai_temp * temp$weights
  temp$recovered = recovered_temp * temp$weights
  temp$week = current_week
  temp$year = current_year
  temp$age = current_age
  results = rbind(results,temp)
}

# Indiana population
N <- 6.732E6;
## Subpopulation totals
allcombinations = expand.grid(ethnicity = ethnicity_levels, 
                              race = race_levels,
                              gender = gender_levels,
                              age = 1:6)
## CENSUS INFORMATION
gender_census = data.frame(gender = c("M", "F"), fraction = c(0.493, 0.507))

## Census: 95.5% non-hispanic 4.5% histpanic
## Census: 88.7% white, 9.9% Black or African American, 1.2% Asian-American, Other Race = 0.2%
ethnicity_census = data.frame(ethnicity =  c("Hispanic or Latino","Not Hispanic or Latino"),
                              fraction = c(0.073, 0.927))
race_census = data.frame(race = c("Asian", "Black or African American", "Other Race", "White"),
                         fraction = c(0.026,0.099,0.027,0.848))
  
#0-30, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
## https://www.infoplease.com/us/census/indiana/demographic-statistics
age_fraction = c(7+7.3+7.3+7.5+7+13.7+15.8/2, 15.8/2+13.4/2, 13.4/2+4.8,
                 3.9+6.5/2, 6.5/2+4.4/2, 4.4/2+1.5)/100
age_census = data.frame(age = 1:6, fraction = age_fraction/sum(age_fraction))

## WE USE: https://www.census.gov/prod/cen2010/briefs/c2010br-04.pdf
## This tells us the breakdown of Hispanic -> race as reported in 2010 census
census = c(209128 , 1243471, 18503103, 26735713)
hist_match = data.frame(race = c("Asian","Black or African American", "Other Race","White"))
hist_match$fraction = census/sum(census)

allcombinations$weight = 0 ## Constructing fraction of pop per strata
for (i in 1:nrow(allcombinations)) {
  current_row = allcombinations[i,]
  if(current_row$ethnicity == "Hispanic or Latino"){
    age_weight = age_census$fraction[age_census$age == current_row$age]
    ethnicity_weight = ethnicity_census$fraction[ethnicity_census$ethnicity == current_row$ethnicity]
    gender_weight = gender_census$fraction[gender_census$gender == current_row$gender]
    race_weight = hist_match$fraction[hist_match$race == current_row$race]
    total_weight = age_weight * ethnicity_weight * gender_weight * race_weight
  } else {
    age_weight = age_census$fraction[age_census$age == current_row$age]
    ethnicity_weight = ethnicity_census$fraction[ethnicity_census$ethnicity == current_row$ethnicity]
    gender_weight = gender_census$fraction[gender_census$gender == current_row$gender]
    race_weight = race_census$fraction[race_census$race == current_row$race]
    total_weight = age_weight * ethnicity_weight * gender_weight * race_weight
  }
  allcombinations$weight[i] = total_weight
  
}

allcombinations$N = allcombinations$weight * N

### Per strata compute the AIR by removing 
flagged_user_aggregate_air = rep(FALSE, nrow(allcombinations))
air_results = rep(0,0)
for(i in 1:nrow(allcombinations)) {
  current_row = allcombinations[i,]
  temp = subset(results, ethnicity == current_row$ethnicity & race == current_row$race &
                  gender == current_row$gender & age == current_row$age)
  air = temp$ai_estimate/(current_row$N-temp$recovered)
  if(all(air == 0) | any(air > 1) | any(air < 0)) {
    print(current_row)
    flagged_user_aggregate_air[i] = TRUE
  } else{
    air_temp = data.frame(cbind(temp$week, temp$year, air))
    air_temp$ethnicity = current_row$ethnicity
    air_temp$race = current_row$race
    air_temp$gender = current_row$gender
    air_temp$age = current_row$age
    names(air_temp)[1:2] = c("week", "year")
    air_results = rbind(air_results, air_temp)
  }
}

# saveRDS(air_results, "../data/modelbased_air_bystrata_2022_28_03.RDS")
# saveRDS(air_results, "../data/modelbased_air_bystrata_lowerifr_2022_28_03.RDS")
saveRDS(air_results, "../data/modelbased_air_bystrata_upperifr_2022_28_03.RDS")

allcombinations$flagged = flagged_user_aggregate_air

# saveRDS(allcombinations, "../data/modelbased_flags_2022_28_03.RDS")
# saveRDS(allcombinations, "../data/modelbased_flags_lowerifr_2022_28_03.RDS")
saveRDS(allcombinations, "../data/modelbased_flags_upperifr_2022_28_03.RDS")


### AGGREGATE 

## Death model
df_coviddeath <- readRDS("../data/dailycoviddata.RDS")
df_coviddeath <- aggregate(covid_deaths ~ startdate, data = df_coviddeath, FUN = sum)
df_coviddeath$date = ymd(df_coviddeath$startdate)
df_coviddeath$death_dt = df_coviddeath$covid_deaths

# Indiana population
N <- 6.732E6;

# Deaths
deaths = df_coviddeath$death_dt[-length(df_coviddeath$death_dt)]

# times
n_days <- length(deaths)
max_death_day <- 41 # Hard coded

smr_pred <- cbind(as.data.frame(summary(fit_forcing, pars = "pred_cases", probs = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))$summary[(max_death_day+1):(max_death_day+length(deaths)-1),]),
                  t=1:(n_days-1))
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

smr_pred$current_date = df_coviddeath$date[-c(469:470)]

prevalence_temp = data.frame(date = smr_pred$current_date, Method = rep("Model-based", nrow(smr_pred)), 
                             mean = smr_pred$mean, ai_smoothed = rep(NA, nrow(smr_pred)),
                             recovered_smoothed = rep(NA, nrow(smr_pred)))

for (i in 1:nrow(prevalence_temp)){
  current_day = prevalence_temp$date[i]
  previous_days = as.numeric(difftime(prevalence_temp$date, current_day, units = "days"))
  exp_rate = a_mean
  weight = exp(exp_rate*previous_days)
  active_infection_temp = sum(weight[previous_days <= 0]*prevalence_temp$mean[prevalence_temp$date <= current_day])
  recovered_temp = sum((1-weight[previous_days <= 0])*prevalence_temp$mean[prevalence_temp$date <= current_day])
  prevalence_temp$ai_smoothed[i] = active_infection_temp
  prevalence_temp$recovered_smoothed[i] = recovered_temp
}

prevalence_temp = prevalence_temp[prevalence_temp$date <= "2021-01-31",]
prevalence_temp$week = week(prevalence_temp$date)
prevalence_temp$year = year(prevalence_temp$date)

prevalence_temp$estimate = prevalence_temp$ai_smoothed/(N-prevalence_temp$recovered_smoothed)

prevalence_temp = aggregate(estimate ~ week + year, data = prevalence_temp, FUN = mean)

prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp$year,
                                               MMWRweek = prevalence_temp$week,
                                               MMWRday = 1)

# prevalence_temp$estimate[45] = prevalence_temp$estimate[45]*7/2  ## THIS IS A FIX FOR WEEK LENGTH
# prevalence_temp = prevalence_temp[-50,] # REMOVE FINAL POINT DUE TO EXTRAPOLATION

# saveRDS(prevalence_temp, "../data/aggregate_air_2022_28_03.RDS")
# saveRDS(prevalence_temp, "../data/aggregate_air_lowerifr_2022_28_03.RDS")
saveRDS(prevalence_temp, "../data/aggregate_air_upperifr_2022_28_03.RDS")
