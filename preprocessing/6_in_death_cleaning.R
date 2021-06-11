### Input: 
### Daily total tests, positive tests, deaths
### Aggregate by age, gender, ethnicitiy, and race separately

### Output: 
### Weekly total tests, positive tests, deaths
### Aggregate by age, gender, ethnicitiy, and race separately

age_data = read.csv("../data/covid_indiana_age.csv")
gender_data = read.csv("../data/covid_indiana_gender.csv")
ethnicity_data = read.csv("../data/covid_indiana_ethnicity.csv")
race_data = read.csv("../data/covid_indiana_race.csv")

unique_dates = unique(age_data$DATE)

library(lubridate)
## MAKE DATES
age_data$date = ymd(age_data$DATE)
gender_data$date = ymd(gender_data$DATE)
ethnicity_data$date = ymd(ethnicity_data$DATE)
race_data$date = ymd(race_data$DATE)

age_data$week = week(age_data$date)
gender_data$week = week(gender_data$date)
ethnicity_data$week = week(ethnicity_data$date)
race_data$week = week(race_data$date)

age_data$year = year(age_data$date)
gender_data$year = year(gender_data$date)
ethnicity_data$year = year(ethnicity_data$date)
race_data$year = year(race_data$date)

age_data$day = day(age_data$date)
gender_data$day = day(gender_data$date)
ethnicity_data$day = day(ethnicity_data$date)
race_data$day = day(race_data$date)

age_levels = levels(age_data$AGE_GRP)[-9]
gender_levels = levels(gender_data$GENDER)[-3]
ethnicity_levels = levels(ethnicity_data$ETHNICITY)[-3]
race_levels = levels(race_data$RACE)[-4]

## Pull each week and year
## Build every combination
## WE USE: https://www.census.gov/prod/cen2010/briefs/c2010br-04.pdf
## This tells us the breakdown of Hispanic -> race as reported in 2010 census
census = c(209128 , 1243471, 18503103, 26735713)
hist_match = data.frame(race = c("Asian","Black or African American", "Other Race","White"))
hist_match$census = census/sum(census)

## Not perfect but better than using independence which clearly does not hold.
## We assume independence for non-Hispanic.

date = age_data$date[2000]

construct_combos <- function(date) {
  subset_age = age_data[age_data$date == date,]
  subset_gender = gender_data[gender_data$date == date,]
  subset_ethnicity = ethnicity_data[ethnicity_data$date == date,]
  subset_race = race_data[race_data$date == date,]
  
  ## FIX DEATHS
  age_death = aggregate(COVID_DEATHS~AGE_GRP, data = subset_age, sum)
  gender_death = aggregate(COVID_DEATHS~GENDER, data = subset_gender, sum)
  ethnicity_death = aggregate(COVID_DEATHS~ETHNICITY, data = subset_ethnicity, sum)
  race_death = aggregate(COVID_DEATHS~RACE, data = subset_race, sum)
  total_deaths = sum(age_death$COVID_DEATHS)
  
  ## COMPUTE TESTING FRACTIONS IGNORING UNKNOWN
  age_death$death_frac = age_death$COVID_DEATHS / sum(age_death$COVID_DEATHS[!age_death$AGE_GRP == 'Unknown'])
  gender_death$death_frac = gender_death$COVID_DEATHS / sum(gender_death$COVID_DEATHS[!gender_death$GENDER == 'Unknown'])
  ethnicity_death$death_frac = ethnicity_death$COVID_DEATHS / sum(ethnicity_death$COVID_DEATHS[!ethnicity_death$ETHNICITY == 'Unknown'])
  race_death$death_frac = race_death$COVID_DEATHS / sum(race_death$COVID_DEATHS[!race_death$RACE == 'Unknown'])
  
  age_death$death_frac[is.nan(age_death$death_frac)] = 0
  gender_death$death_frac[is.nan(gender_death$death_frac)] = 0
  ethnicity_death$death_frac[is.nan(ethnicity_death$death_frac)] = 0
  race_death$death_frac[is.nan(race_death$death_frac)] = 0
  
  ## REMOVE UNKNOWN
  temp = (age_death$COVID_DEATHS[age_death$AGE_GRP == 'Unknown'] * age_death$death_frac[!age_death$AGE_GRP == 'Unknown'])
  age_death = age_death[!age_death$AGE_GRP == 'Unknown',]
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
  
  age_death$death_frac[is.nan(age_death$death_frac)] = 0
  gender_death$death_frac[is.nan(gender_death$death_frac)] = 0
  ethnicity_death$death_frac[is.nan(ethnicity_death$death_frac)] = 0
  race_death$death_frac[is.nan(race_death$death_frac)] = 0
  
  allcombinations = expand.grid(startdate = min(subset_age$date), age = age_levels, 
                                gender = gender_levels, 
                                ethnicity = ethnicity_levels, 
                                race = race_levels)
  
  allcombinations$covid_deaths = rep(NA, nrow(allcombinations))
  
  for(comb_iter in 1:nrow(allcombinations)) {
    temp_age = age_death$death_frac[age_death$AGE_GRP == as.character(allcombinations[comb_iter,]$age)]
    temp_gender = gender_death$death_frac[gender_death$GENDER == as.character(allcombinations[comb_iter,]$gender)]
    temp_ethnicity = ethnicity_death$death_frac[ethnicity_death$ETHNICITY == as.character(allcombinations[comb_iter,]$ethnicity)]
    if(allcombinations[comb_iter,]$ethnicity == 'Hispanic or Latino') {
      temp_race = hist_match$census[hist_match$race == as.character(allcombinations[comb_iter,]$race)]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    } else {
      temp_race = race_death$death_frac[race_death$RACE == as.character(allcombinations[comb_iter,]$race)]
      comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race
    }
    allcombinations$covid_deaths[comb_iter] = total_deaths * comb_frac
  }
  extra_deaths = allcombinations$covid_deaths - floor(allcombinations$covid_deaths)
  rounded_deaths = unlist(lapply(extra_deaths, function(x){rbinom(n = 1, size = 1, prob = x)}))
  allcombinations$covid_deaths = floor(allcombinations$covid_deaths) + rounded_deaths
  
  return(allcombinations)
}

daysin2020 = unique(age_data$date[age_data$year == 2020])
daysin2021 = unique(age_data$date[age_data$year == 2021])

set.seed(187149)
dailycoviddata = construct_combos(daysin2020[1])

for(day in daysin2020[-1]) {
  temp_dailydata = construct_combos(day)
  dailycoviddata = rbind(dailycoviddata, temp_dailydata)
}

for(day in daysin2021) {
  temp_dailydata = construct_combos(day)
  dailycoviddata = rbind(dailycoviddata, temp_dailydata)
}

dailycoviddata_total = aggregate(covid_deaths ~ startdate, dailycoviddata, sum)

saveRDS(dailycoviddata, file = "../data/dailycoviddata.RDS")

saveRDS(dailycoviddata_total, file = "../data/dailycoviddata_total.RDS")
