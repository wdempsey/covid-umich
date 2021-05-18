### Input: 
### Daily total tests, positive tests, deaths
### Aggregate by age, gender, ethnicitiy, and race separately

### Output: 
### Weekly total tests, positive tests, deaths
### Aggregate by age, gender, ethnicitiy, and race separately

indiana_hospital_data = read.csv("../data/indiana_hospital_data_05032021.csv")
indiana_test_data = readRDS("../data/weeklycoviddata_withsympcontact.RDS")

library(lubridate)
## MAKE DATES
indiana_hospital_data$date = mdy(indiana_hospital_data$Date)
indiana_hospital_data$week = week(indiana_hospital_data$date)
indiana_hospital_data$year = year(indiana_hospital_data$date)
indiana_hospital_data$day = day(indiana_hospital_data$date)
weekweight = aggregate(day ~ week + year, indiana_hospital_data, function(x){length(unique(x))})


## Pull each week and year
## Sample totals were only available at the aggregate level (from several checks seems relatively stable)
## Counts by AGE
# age_groups = c(0-4 + 5-19 + 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+)
male_by_age = c(120+256+745,1406,2479,4441,6243,6317,4801)
female_by_age = c(118+462+2233,2346,2592,3928,5490,5855,5710)
gender_hosp_frac = sum(male_by_age)/sum(male_by_age, female_by_age)
male_hosp_frac = male_by_age/sum(male_by_age)
female_hosp_frac = female_by_age/sum(female_by_age)

## Counts by Race
race_levels= levels(indiana_test_data$race)
dashboard_race = c(639+613, 3532+4564, 0, 21839+22924) # From DASHBOARD
dashboard_race[3] = sum(male_by_age, female_by_age) - sum(dashboard_race)
race_hosp_frac = dashboard_race/sum(dashboard_race)


## No hispanic information so use the census data here
## Census: 95.5% non-hispanic 4.5% hispanic
ethnicity_levels = levels(indiana_test_data$ethnicity)
ethnicity_census = c(0.073, 0.927)

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

  subset_hospital = indiana_hospital_data[indiana_hospital_data$week == week & indiana_hospital_data$year == year,]
  weekly_hospitalization_count = sum(subset_hospital$Hospitalization.Count, subset_hospital$ED.Visit.Count)

  allcombinations = expand.grid(startdate = min(subset_hospital$Date), age = unique(indiana_test_data$age), 
                                gender = c("M", "F"), ethnicity = unique(indiana_test_data$ethnicity), 
                                race = hist_match$race)
  
  allcombinations$hospitalization_count = rep(NA, nrow(allcombinations))
  
  for(comb_iter in 1:nrow(allcombinations)) {
    if(allcombinations$gender[comb_iter] == "M") {
      temp_gender = gender_hosp_frac
      temp_age = male_hosp_frac[allcombinations$age[comb_iter]]
    } else{
      temp_gender = 1 - gender_hosp_frac
      temp_age = female_hosp_frac[allcombinations$age[comb_iter]]
    }
    
    temp_race = race_hosp_frac[race_levels == allcombinations$race[comb_iter]]
    temp_hisp_race = hist_match$census[hist_match$race == allcombinations$race[comb_iter]]
    denominator = temp_race* ethnicity_census[2] + temp_hisp_race * ethnicity_census[1]
    if(allcombinations$ethnicity[comb_iter] == "Hispanic or Latino") {
      temp_ethnicity = temp_hisp_race * ethnicity_census[1] / denominator
    } else {
      temp_ethnicity = temp_race* ethnicity_census[2] / denominator
    }
    comb_frac = temp_age * temp_gender * temp_ethnicity * temp_race  
    allcombinations$hospitalization_count[comb_iter] = weekly_hospitalization_count * comb_frac
  }
  
  return(allcombinations)
}


weeksin2020 = unique(indiana_hospital_data$week[indiana_hospital_data$year == 2020])
weeksin2021 = unique(indiana_hospital_data$week[indiana_hospital_data$year == 2021])

weeklycoviddata = construct_combos(14,2020)

for(week in 15:max(weeksin2020)) {
  print(week)
  temp_weekdata = construct_combos(week,2020)
  weeklycoviddata = rbind(weeklycoviddata, temp_weekdata)
}

for(week in 1:max(weeksin2021)) {
  temp_weekdata = construct_combos(week,2021)
  weeklycoviddata = rbind(weeklycoviddata, temp_weekdata)
}

saveRDS(weeklycoviddata, file = "../data/weeklycoviddata_hospital.RDS")
