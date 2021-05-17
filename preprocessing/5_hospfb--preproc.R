## Building a combined dataset

library("lubridate")

all_data_pos_hospital = readRDS("../data/fb_alldata_weekly_pos_hospital_alt.RDS")
all_data_pos_symptom = readRDS("../data/fb_alldata_weekly_pos_symptom_alt.RDS")
all_data_covid = readRDS("../data/fb_alldata_weekly_covid_alt.RDS")
indiana_data = readRDS("../data/weeklycoviddata.RDS")

## How do we match ages?
## FB <--> INDIANA
## 18-24 (1) <--> 0-19 and 20-29.
## 25--34 (2) <--> 30--39.
## 35--44 (3) <--> 40--49
## 45-54 years (4) <--> 50--59
## 55-64 years (5) <--> 60--69
## 65-74 years (6) <--> 70--79
## 75 years or older (7) <--> 80+

## How do we match gender?
## FB <--> Indiana
## 1 <--> M
## 2 <--> F

## FB DATA: NEED TO COMPUTE \sum_{t^\prime=1}^T K_h(|t-t^\prime|) \sum_{j=1}^N I_{j,t^\prime} X_{j,t^\prime} within each strata for each week
## FB DATA: NEED TO COMPUTE \sum_{t^\prime=1}^T K_h(|t-t^\prime|) \sum_{j=1}^N W_{j,t^\prime} \tilde I_{j,t^\prime} X_{j,t^\prime} within each strata for each week 
## Recall that we can combine $X_{j,t^\prime}$ is constant across subgroups.
## So we can re-write $\sum_{h=1}^H I_{h, t^\prime} X_{h,t^\prime}$
## Strata: age, gender, race, ethnicity, fever

## Facebook data
## Step 1: Break weight out by ethnicity and race

## Census: 92.7% non-hispanic 7.3% hispanic
## https://www.census.gov/quickfacts/IN
## Census: 88.7% white, 9.9% Black or African American, 1.2% Asian-American, Other Race = 0.2%
ethnicity_levels = levels(indiana_data$ethnicity)
ethnicity_census = c(0.073, 0.927)
race_levels = levels(indiana_data$race)
race_census = c(0.026,0.099,0.027,0.848)

## Pull each week and year
## Build every combination
## WE USE: https://www.census.gov/prod/cen2010/briefs/c2010br-04.pdf
## This tells us the breakdown of Hispanic -> race as reported in 2010 census
census = c(209128 , 1243471, 18503103, 26735713)
hispanic_match = data.frame(race = c("Asian","Black or African American", "Other Race","White"))
hispanic_match$census = census/sum(census)


allcombinations = expand.grid(ethnicity = ethnicity_levels, 
                              race = race_levels)

complete_data_pos_hospital = complete_data_pos_symptom = complete_data_covid =  data.frame()

for(all_row in 1:nrow(all_data_pos_hospital)) {
  
  temp = data.frame(all_data_pos_hospital[all_row,], nrow = nrow(allcombinations), ncol = length(all_data_pos_hospital[all_row,]))
  temp[1:nrow(allcombinations),] = temp[1,]
  temp$ethnicity = allcombinations$ethnicity
  temp$race = allcombinations$race
  
  for (row in 1:nrow(temp)) {
    if (temp$ethnicity[row] != "Hispanic or Latino") {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]  
      temp$weighthospital[row] = temp$weighthospital[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]
    } else {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
      temp$weighthospital[row] = temp$weighthospital[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
    }
    
  }
  complete_data_pos_hospital = rbind(complete_data_pos_hospital, temp)
}

saveRDS(complete_data_pos_hospital,"../data/fb_weeklycomplete_pos_hospital_alt.RDS")

for(all_row in 1:nrow(all_data_pos_symptom)) {
  
  temp = data.frame(all_data_pos_symptom[all_row,], nrow = nrow(allcombinations), ncol = length(all_data_pos_symptom[all_row,]))
  temp[1:nrow(allcombinations),] = temp[1,]
  temp$ethnicity = allcombinations$ethnicity
  temp$race = allcombinations$race
  
  for (row in 1:nrow(temp)) {
    if (temp$ethnicity[row] != "Hispanic or Latino") {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]  
      temp$weighthospital[row] = temp$weighthospital[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]
    } else {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
      temp$weighthospital[row] = temp$weighthospital[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
    }
    
  }
  complete_data_pos_symptom = rbind(complete_data_pos_symptom, temp)
}


saveRDS(complete_data_pos_symptom,"../data/fb_weeklycomplete_pos_symptom_alt.RDS")


## Now build covid given no symptoms

for(all_row in 1:nrow(all_data_covid)) {
  temp = data.frame(all_data_covid[all_row,], nrow = nrow(allcombinations), ncol = length(all_data_covid[all_row,]))
  temp[1:nrow(allcombinations),] = temp[1,]
  temp$ethnicity = allcombinations$ethnicity
  temp$race = allcombinations$race
  
  for (row in 1:nrow(temp)) {
    if (temp$ethnicity[row] != "Hispanic or Latino") {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]  
      temp$weightcovid[row] = temp$weightcovid[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]
    } else {
      temp$weight[row] = temp$weight[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
      temp$weightcovid[row] = temp$weightcovid[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
    }
    
  }
  complete_data_covid = rbind(complete_data_covid, temp)
}

unique(complete_data_covid$week)


saveRDS(complete_data_covid,"../data/fb_weeklycomplete_covid_alt.RDS")
