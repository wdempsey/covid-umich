### Input: 
### Daily total tests, positive tests, deaths
### Aggregate by age, gender, ethnicity, and race separately

### Output: 
### Weekly total tests, positive tests, deaths
### Aggregate by age, gender, ethnicity, and race separately

### Total data before 
age_data = read.csv("../data/covid_indiana_age.csv")
gender_data = read.csv("../data/covid_indiana_gender.csv")
ethnicity_data = read.csv("../data/covid_indiana_ethnicity.csv")
race_data = read.csv("../data/covid_indiana_race.csv")
totals_data = read.csv("../data/covid_indiana_all_testing_data.csv")

names(age_data)[2] = "age"

unique_dates = unique(age_data$DATE)

library(lubridate)
## MAKE DATES
age_data$date = ymd(age_data$DATE)
gender_data$date = ymd(gender_data$DATE)
ethnicity_data$date = ymd(ethnicity_data$DATE)
race_data$date = ymd(race_data$DATE)
totals_data$date = mdy(totals_data$DATE)

age_data$week = week(age_data$date)
gender_data$week = week(gender_data$date)
ethnicity_data$week = week(ethnicity_data$date)
race_data$week = week(race_data$date)

age_data$year = year(age_data$date)
gender_data$year = year(gender_data$date)
ethnicity_data$year = year(ethnicity_data$date)
race_data$year = year(race_data$date)

totals_data$day = day(totals_data$date)
totals_data$week = week(totals_data$date)
totals_data$year = year(totals_data$date)
weekweight = aggregate(day ~ week, age_data, function(x){length(unique(x))})


## Pull each week and year
## Build every combination
## WE USE: https://www.census.gov/prod/cen2010/briefs/c2010br-04.pdf
## This tells us the breakdown of Hispanic -> race as reported in 2010 census
census = c(209128 , 1243471, 18503103, 26735713)
hist_match = data.frame(race = c("Asian","Black or African American", "Other Race","White"))
hist_match$census = census/sum(census)

## Not perfect but better than using independence which clearly does not hold.
## We assume independence for non-Hispanic.

week = 20
year = 2020

construct_combos <- function(week, year) {
  addweight = 7/weekweight$day[weekweight$week == week]

  subset_age = age_data[age_data$week == week & age_data$year == year,]
  subset_gender = gender_data[gender_data$week == week & gender_data$year == year,]
  subset_ethnicity = ethnicity_data[ethnicity_data$week == week & ethnicity_data$year == year,]
  subset_race = race_data[race_data$week == week & race_data$year == year,]
  subset_totals = totals_data[totals_data$week == week & totals_data$year == year,]
  
  ## FIX TESTING
  age_test = aggregate(COVID_TEST~age, data = subset_age, sum)
  gender_test = aggregate(COVID_TEST~GENDER, data = subset_gender, sum)
  ethnicity_test = aggregate(COVID_TEST~ETHNICITY, data = subset_ethnicity, sum)
  race_test = aggregate(COVID_TEST~RACE, data = subset_race, sum)
  total_tests = sum(subset_totals$COVID_TEST, na.rm = TRUE)*addweight
  
  ## COMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_test$test_frac = age_test$COVID_TEST / sum(age_test$COVID_TEST[!age_test$age == 'Unknown'])
  gender_test$test_frac = gender_test$COVID_TEST / sum(gender_test$COVID_TEST[!gender_test$GENDER == 'Unknown'])
  ethnicity_test$test_frac = ethnicity_test$COVID_TEST / sum(ethnicity_test$COVID_TEST[!ethnicity_test$ETHNICITY == 'Unknown'])
  race_test$test_frac = race_test$COVID_TEST / sum(race_test$COVID_TEST[!race_test$RACE == 'Unknown'])
  
  ## REMOVE UNKNOWN
  temp = (age_test$COVID_TEST[age_test$age == 'Unknown'] * age_test$test_frac[!age_test$age == 'Unknown'])
  age_test = age_test[!age_test$age == 'Unknown',]
  age_test$COVID_TEST = age_test$COVID_TEST + temp
  
  temp = (gender_test$COVID_TEST[gender_test$GENDER == 'Unknown'] * gender_test$test_frac[!gender_test$GENDER == 'Unknown'])
  gender_test = gender_test[!gender_test$GENDER == 'Unknown',]
  gender_test$COVID_TEST = gender_test$COVID_TEST + temp
  
  temp = (ethnicity_test$COVID_TEST[ethnicity_test$ETHNICITY == 'Unknown'] * ethnicity_test$test_frac[!ethnicity_test$ETHNICITY == 'Unknown'])
  ethnicity_test = ethnicity_test[!ethnicity_test$ETHNICITY == 'Unknown',]
  ethnicity_test$COVID_TEST = ethnicity_test$COVID_TEST + temp
  
  temp = (race_test$COVID_TEST[race_test$RACE == 'Unknown'] * race_test$test_frac[!race_test$RACE == 'Unknown'])
  race_test = race_test[!race_test$RACE == 'Unknown',]
  race_test$COVID_TEST = race_test$COVID_TEST + temp
  
  ## RECOMPUTE TESTING FRACTIONS
  age_test$test_frac = age_test$COVID_TEST / sum(age_test$COVID_TEST)
  gender_test$test_frac = gender_test$COVID_TEST / sum(gender_test$COVID_TEST)
  ethnicity_test$test_frac = ethnicity_test$COVID_TEST / sum(ethnicity_test$COVID_TEST)
  race_test$test_frac = race_test$COVID_TEST / sum(race_test$COVID_TEST)
  
  allcombinations = expand.grid(startdate = min(subset_age$date), age = age_test$age, 
                                gender = gender_test$GENDER, ethnicity = ethnicity_test$ETHNICITY, 
                                race = race_test$RACE)
  
  allcombinations$covid_tests = rep(NA, nrow(allcombinations))
  
  for(comb_iter in 1:nrow(allcombinations)) {
    temp_age = age_test$test_frac[age_test$age == allcombinations[comb_iter,]$age]
    temp_gender = gender_test$test_frac[gender_test$GENDER == allcombinations[comb_iter,]$gender]
    temp_ethnicity = ethnicity_test$test_frac[ethnicity_test$ETHNICITY == allcombinations[comb_iter,]$ethnicity]
    if(allcombinations[comb_iter,]$ethnicity == 'Hispanic or Latino') {
      temp_race = hist_match$census[hist_match$race == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race  
    } else{
      temp_race = race_test$test_frac[race_test$RACE == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    }
    allcombinations$covid_tests[comb_iter] = total_tests * comb_frac
  }
  
  ## FIX Counts
  age_count = aggregate(COVID_COUNT~age, data = subset_age, sum)
  gender_count = aggregate(COVID_COUNT~GENDER, data = subset_gender, sum)
  ethnicity_count = aggregate(COVID_COUNT~ETHNICITY, data = subset_ethnicity, sum)
  race_count = aggregate(COVID_COUNT~RACE, data = subset_race, sum)
  total_counts = sum(age_count$COVID_COUNT)*addweight
  
  ## COMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_count$count_frac = age_count$COVID_COUNT / sum(age_count$COVID_COUNT[!age_count$age == 'Unknown'])
  gender_count$count_frac = gender_count$COVID_COUNT / sum(gender_count$COVID_COUNT[!gender_test$GENDER == 'Unknown'])
  ethnicity_count$count_frac = ethnicity_count$COVID_COUNT / sum(ethnicity_count$COVID_COUNT[!ethnicity_count$ETHNICITY == 'Unknown'])
  race_count$count_frac = race_count$COVID_COUNT / sum(race_count$COVID_COUNT[!race_count$RACE == 'Unknown'])
  
  ## REMOVE UNKNOWN
  temp = (age_count$COVID_COUNT[age_count$age == 'Unknown'] * age_count$count_frac[!age_count$age == 'Unknown'])
  age_count = age_count[!age_count$age == 'Unknown',]
  age_count$COVID_COUNT = age_count$COVID_COUNT + temp
  
  temp = (gender_count$COVID_COUNT[gender_count$GENDER == 'Unknown'] * gender_count$count_frac[!gender_count$GENDER == 'Unknown'])
  gender_count = gender_count[!gender_count$GENDER == 'Unknown',]
  gender_count$COVID_COUNT = gender_count$COVID_COUNT + temp
  
  temp = (ethnicity_count$COVID_COUNT[ethnicity_count$ETHNICITY == 'Unknown'] * ethnicity_count$count_frac[!ethnicity_count$ETHNICITY == 'Unknown'])
  ethnicity_count = ethnicity_count[!ethnicity_count$ETHNICITY == 'Unknown',]
  ethnicity_count$COVID_COUNT = ethnicity_count$COVID_COUNT + temp
  
  temp = (race_count$COVID_COUNT[race_count$RACE == 'Unknown'] * race_count$count_frac[!race_count$RACE == 'Unknown'])
  race_count = race_count[!race_count$RACE == 'Unknown',]
  race_count$COVID_COUNT = race_count$COVID_COUNT + temp
  
  ## RECOMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_count$count_frac = age_count$COVID_COUNT / sum(age_count$COVID_COUNT)
  gender_count$count_frac = gender_count$COVID_COUNT / sum(gender_count$COVID_COUNT)
  ethnicity_count$count_frac = ethnicity_count$COVID_COUNT / sum(ethnicity_count$COVID_COUNT[!ethnicity_count$ETHNICITY == 'Unknown'])
  race_count$count_frac = race_count$COVID_COUNT / sum(race_count$COVID_COUNT[!race_count$RACE == 'Unknown'])
  
  allcombinations$covid_counts = rep(NA, nrow(allcombinations))
  
  for(comb_iter in 1:nrow(allcombinations)) {
    temp_age = age_count$count_frac[age_count$age == allcombinations[comb_iter,]$age]
    temp_gender = gender_count$count_frac[gender_count$GENDER == allcombinations[comb_iter,]$gender]
    temp_ethnicity = ethnicity_count$count_frac[ethnicity_count$ETHNICITY == allcombinations[comb_iter,]$ethnicity]
    if(allcombinations[comb_iter,]$ethnicity == 'Hispanic or Latino') {
      temp_race = hist_match$census[hist_match$race == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    } else {
      temp_race = race_count$count_frac[race_count$RACE == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    }
    allcombinations$covid_counts[comb_iter] = total_counts * comb_frac
  }
  
  allcombinations$covid_posrate = allcombinations$covid_counts/allcombinations$covid_tests
  
  ## FIX DEATHS
  age_death = aggregate(COVID_DEATHS~age, data = subset_age, sum)
  gender_death = aggregate(COVID_DEATHS~GENDER, data = subset_gender, sum)
  ethnicity_death = aggregate(COVID_DEATHS~ETHNICITY, data = subset_ethnicity, sum)
  race_death = aggregate(COVID_DEATHS~RACE, data = subset_race, sum)
  total_deaths = sum(age_death$COVID_DEATHS)*addweight
  
  ## COMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_death$death_frac = age_death$COVID_DEATHS / sum(age_death$COVID_DEATHS[!age_death$age == 'Unknown'])
  gender_death$death_frac = gender_death$COVID_DEATHS / sum(gender_death$COVID_DEATHS[!gender_death$GENDER == 'Unknown'])
  ethnicity_death$death_frac = ethnicity_death$COVID_DEATHS / sum(ethnicity_death$COVID_DEATHS[!ethnicity_death$ETHNICITY == 'Unknown'])
  race_death$death_frac = race_death$COVID_DEATHS / sum(race_death$COVID_DEATHS[!race_death$RACE == 'Unknown'])
  
  ## REMOVE UNKNOWN
  temp = (age_death$COVID_DEATHS[age_death$age == 'Unknown'] * age_death$death_frac[!age_death$age == 'Unknown'])
  age_death = age_death[!age_death$age == 'Unknown',]
  age_death$COVID_DEATHS = age_death$COVID_DEATHS + temp
  
  temp = (gender_death$COVID_DEATHS[gender_death$GENDER == 'Unknown'] * gender_death$death_frac[!gender_death$GENDER == 'Unknown'])
  gender_death = gender_death[!gender_death$GENDER == 'Unknown',]
  gender_death$COVID_DEATHS = gender_death$COVID_DEATHS + temp
  
  temp = (ethnicity_death$COVID_DEATHS[ethnicity_death$ETHNICITY == 'Unknown'] * ethnicity_death$death_frac[!ethnicity_death$ETHNICITY == 'Unknown'])
  ethnicity_death = ethnicity_death[!ethnicity_death$ETHNICITY == 'Unknown',]
  ethnicity_death$COVID_DEATHS = ethnicity_death$COVID_DEATHS + temp
  
  temp = (race_death$COVID_DEATHS[race_death$RACE == 'Unknown'] * race_death$death_frac[!race_death$RACE == 'Unknown'])
  race_death = race_death[!race_death$RACE == 'Unknown',]
  race_death$COVID_DEATHS = race_death$COVID_DEATHS + temp
  
  ## COMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_death$death_frac = age_death$COVID_DEATHS / sum(age_death$COVID_DEATHS)
  gender_death$death_frac = gender_death$COVID_DEATHS / sum(gender_death$COVID_DEATHS)
  ethnicity_death$death_frac = ethnicity_death$COVID_DEATHS / sum(ethnicity_death$COVID_DEATHS)
  race_death$death_frac = race_death$COVID_DEATHS / sum(race_death$COVID_DEATHS)
  
  allcombinations$covid_deaths = rep(NA, nrow(allcombinations))
  
  for(comb_iter in 1:nrow(allcombinations)) {
    temp_age = age_death$death_frac[age_death$age == allcombinations[comb_iter,]$age]
    temp_gender = gender_death$death_frac[gender_death$GENDER == allcombinations[comb_iter,]$gender]
    temp_ethnicity = ethnicity_death$death_frac[ethnicity_death$ETHNICITY == allcombinations[comb_iter,]$ethnicity]
    if(allcombinations[comb_iter,]$ethnicity == 'Hispanic or Latino') {
      temp_race = hist_match$census[hist_match$race == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    } else {
      temp_race = race_death$death_frac[race_death$RACE == allcombinations[comb_iter,]$race]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    }
    allcombinations$covid_deaths[comb_iter] = total_deaths * comb_frac
  }
  
  return(allcombinations)
}


weeksin2020 = unique(age_data$week[age_data$year == 2020])
weeksin2021 = unique(age_data$week[age_data$year == 2021])

weeklycoviddata = construct_combos(14,2020)

for(week in 15:max(weeksin2020)) {
  temp_weekdata = construct_combos(week,2020)
  weeklycoviddata = rbind(weeklycoviddata, temp_weekdata)
}

for(week in 1:max(weeksin2021)) {
  temp_weekdata = construct_combos(week,2021)
  weeklycoviddata = rbind(weeklycoviddata, temp_weekdata)
}

saveRDS(weeklycoviddata, file = "../data/weeklycoviddata.RDS")
