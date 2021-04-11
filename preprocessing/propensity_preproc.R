## Building a combined dataset

library("lubridate")

fb_data = read.csv("../data/fb_indiana_data.csv")
indiana_data = readRDS("../data/weeklycoviddata.RDS")
all_data = readRDS("../data/fb_weekly.RDS")
propensities_got = readRDS("../data/smoothedfeverpropensities_got.RDS")
propensities_pos = readRDS("../data/smoothedfeverpropensities_pos.RDS")

names(propensities_pos) = names(propensities_got) = 
  c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
    "55to64", "65to74", "75plus")


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

## INDIANA DATA: NEED TO COMPUTE \sum_{t^\prime=1}^T K_h(|t-t^\prime|) \sum_{j=1}^N I_{j,t^\prime} X_{j,t^\prime} within each strata for each week
## FB DATA: NEED TO COMPUTE \sum_{t^\prime=1}^T K_h(|t-t^\prime|) \sum_{j=1}^N W_{j,t^\prime} \tilde I_{j,t^\prime} X_{j,t^\prime} within each strata for each week 
## Recall that we can combine $X_{j,t^\prime}$ is constant across subgroups.
## So we can re-write $\sum_{h=1}^H I_{h, t^\prime} X_{h,t^\prime}$
## Strata: age, gender, race, ethnicity, fever

## Indiana data
## Step 1: Add fever to each subgroup
## USE FB data to split by age and gender
indiana_data_with_fever = rbind(indiana_data,indiana_data) 
indiana_data_with_fever$fever = c(rep(TRUE, nrow(indiana_data)), rep(FALSE, nrow(indiana_data)))
indiana_to_fb_ages = c(1,1,2,3,4,5,6,7)
levels(indiana_data$age) = 1:8
indiana_to_fb_gender = c(1,2)
levels(indiana_data$gender) = c(2,1)

construct_design <- function(gender, age) {
  basex =rep(0,8)
  basex[gender] = 1
  if (age != 1) {
    basex[1+age] = 1
  }
  basex
}

expit <- function(x, beta){
  1/(1+exp(-x%*%t(beta)))
}

for(row in 1:nrow(indiana_data_with_fever)){
  temp = indiana_data_with_fever[row,]
  tempfb_age = indiana_to_fb_ages[temp$age]
  tempfb_gender = indiana_to_fb_gender[temp$gender]
  tempx = construct_design(tempfb_age, tempfb_gender)
  tempbeta_got = propensities_got[propensities_got$week == week(temp$startdate) & propensities_got$year == year(temp$startdate),3:10]
  tempbeta_pos = propensities_pos[propensities_pos$week == week(temp$startdate) & propensities_pos$year == year(temp$startdate),3:10]
  probfever_got = expit(tempx, tempbeta_got)
  probfever_pos = expit(tempx, tempbeta_pos)
  temp$covid_tests = temp$covid_tests * ( (temp$fever == TRUE) * probfever_got + (temp$fever == FALSE) * (1-probfever_got))
  temp$covid_counts = temp$covid_counts * ( (temp$fever == TRUE) * probfever_pos + (temp$fever == FALSE) * (1-probfever_pos))
  temp$covid_posrate = temp$covid_counts/temp$covid_tests
  indiana_data_with_fever[row,] = temp
}

saveRDS(indiana_data_with_fever, "../data/weeklycoviddata_withfever.RDS")


## Facebook data
## Step 1: Break weight out by ethnicity and race

## Census: 95.5% non-hispanic 4.5% histpanic
## Census: 88.7% white, 9.9% Black or African American, 1.2% Asian-American, Other Race = 0.2%
ethnicity_levels = levels(indiana_data$ethnicity)
ethnicity_census = c(0.045, 0.955)
race_levels = levels(indiana_data$race)
race_census = c(0.012,0.099,0.002,0.887)

## Pull each week and year
## Build every combination
## WE USE: https://www.census.gov/prod/cen2010/briefs/c2010br-04.pdf
## This tells us the breakdown of Hispanic -> race as reported in 2010 census
census = c(209128 , 1243471, 18503103, 26735713)
hispanic_match = data.frame(race = c("Asian","Black or African American", "Other Race","White"))
hispanic_match$census = census/sum(census)


allcombinations = expand.grid(ethnicity = ethnicity_levels, 
                              race = race_levels)

complete_data = data.frame()

for(all_row in 1:nrow(all_data)) {
  temp = data.frame(all_data[all_row,], nrow = nrow(allcombinations), ncol = length(all_data[all_row,]))
  temp[1:nrow(allcombinations),] = temp[1,]
  temp$ethnicity = allcombinations$ethnicity
  temp$race = allcombinations$race
  
  for (row in 1:nrow(temp)) {
    if (temp$ethnicity[row] == "Hispanic or Latino") {
      temp$weight[row] = temp$weight[row] * 1/ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * 1/race_census[race_levels == temp$race[row]]  
      # temp$weight_gottested[row] = temp$weight_gottested[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * race_census[race_levels == temp$race[row]]
    } else {
      temp$weight[row] = temp$weight[row] * 1/ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * 1/hispanic_match$census[hispanic_match$race == temp$race[row]]  
      # temp$weight_gottested[row] = temp$weight_gottested[row] * ethnicity_census[ethnicity_levels == temp$ethnicity[row]] * hispanic_match$census[hispanic_match$race == temp$race[row]]  
    }
    
  }
  complete_data = rbind(complete_data, temp)
}


saveRDS(complete_data,"../data/fb_weeklycomplete.RDS")
