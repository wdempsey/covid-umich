weeklycoviddata_hospital = readRDS(file = "../data/weeklycoviddata_hospital.RDS")
weeklycoviddata_testing = readRDS(file = "../data/weeklycoviddata.RDS")

propensity_pos_hospital = readRDS("../data/smoothedpropensities_pos_hospital_alt.RDS")
propensity_pos_symptom = readRDS("../data/smoothedpropensities_pos_symptom_alt.RDS")
propensity_covid = readRDS("../data/smoothedpropensities_covid_alt.RDS")

library(lubridate)
weeklycoviddata_hospital$week = week(mdy(weeklycoviddata_hospital$startdate))
weeklycoviddata_hospital$year = year(mdy(weeklycoviddata_hospital$startdate))
weeklycoviddata_testing$week = week(weeklycoviddata_testing$startdate)
weeklycoviddata_testing$year = year(weeklycoviddata_testing$startdate)

## Fixing levels of age
levels(weeklycoviddata_testing$age) = c(1,1:7)

construct_X <- function(temp) {
  ## Build the feature vector for the propensity model
  ## Pr ( Hospitalization | Age, Gender, Time, Symptom +)
  feature_vector = rep(0,9)
  feature_vector[9] = 1
  if(temp$age != 1) {
    feature_vector[1+temp$age] = 1
  }
  feature_vector[1] = as.numeric(temp$gender == "M")
  feature_vector[2] = as.numeric(temp$gender == "F")
  return(feature_vector)
}

## ADD ROWS

weeklycoviddata_hospital$neg_symptom_count = NA
weeklycoviddata_hospital$neg_nosymptom_count = NA
weeklycoviddata_hospital$pos_symptom_count = NA
weeklycoviddata_hospital$pos_nosymptom_count = NA

for(current_row in 1:nrow(weeklycoviddata_hospital)) {
  temp = weeklycoviddata_hospital[current_row, ]
  feature_vector = construct_X(temp)
  if(temp$week > 5 & temp$year == 2021) {
    weeklycoviddata_hospital$neg_symptom_count[current_row] = NA
    weeklycoviddata_hospital$neg_nosymptom_count[current_row] = NA
    weeklycoviddata_hospital$pos_symptom_count[current_row] = NA
    weeklycoviddata_hospital$pos_nosymptom_count[current_row] = NA
  } else {
    covidtesting_match = which(weeklycoviddata_testing$week == temp$week & weeklycoviddata_testing$year == temp$year &
                                 weeklycoviddata_testing$age == temp$age & weeklycoviddata_testing$gender == temp$gender &
                                 weeklycoviddata_testing$ethnicity == temp$ethnicity & weeklycoviddata_testing$race == temp$race)
    covid_tests = sum(weeklycoviddata_testing$covid_tests[covidtesting_match])
    covid_counts = sum(weeklycoviddata_testing$covid_counts[covidtesting_match])
    frac_hospitalized = min(temp$hospitalization_count / covid_counts,1)
    
    prop_temp_pos_symptom = propensity_pos_symptom[propensity_pos_symptom$week == temp$week & propensity_pos_symptom$year == temp$year,]
    pos_symptom_term = sum(prop_temp_pos_symptom[3:11]*feature_vector)
    prob_symptom_hosp = 1/(1+exp(-pos_symptom_term)) # Prob Symptom | Hospital & C+
    
    pos_symptom_term = sum(prop_temp_pos_symptom[3:11]*c(feature_vector[1:8],0))
    prob_symptom_nohosp = 1/(1+exp(-pos_symptom_term)) # Prob Symptom | NH & C+
    
    symptom_given_covidpos = frac_hospitalized * prob_symptom_hosp + (1-frac_hospitalized) * prob_symptom_nohosp
    symptom_given_covidneg = 0.1 # Holder
    
    pos_symptom_count = covid_counts * symptom_given_covidpos
    pos_nosymptom_count = max(covid_counts - pos_symptom_count,0)
    neg_counts = covid_tests - covid_counts
    neg_symptom_count = neg_counts * symptom_given_covidneg
    neg_nosymptom_count = max(neg_counts - neg_symptom_count,0)
    
    weeklycoviddata_hospital$neg_symptom_count[current_row] = neg_symptom_count
    weeklycoviddata_hospital$neg_nosymptom_count[current_row] = neg_nosymptom_count
    weeklycoviddata_hospital$pos_symptom_count[current_row] = pos_symptom_count
    weeklycoviddata_hospital$pos_nosymptom_count[current_row] = pos_nosymptom_count
    
  }
}

weeklycoviddata_withsymptoms = rbind(weeklycoviddata_hospital,weeklycoviddata_hospital)

weeklycoviddata_withsymptoms$symptoms = c(rep(TRUE, nrow(weeklycoviddata_hospital)),rep(FALSE, nrow(weeklycoviddata_hospital)))

weeklycoviddata_withsymptoms$covid_tests = (weeklycoviddata_withsymptoms$neg_symptom_count+weeklycoviddata_withsymptoms$pos_symptom_count) * (weeklycoviddata_withsymptoms$symptoms==TRUE) + 
  (weeklycoviddata_withsymptoms$neg_nosymptom_count+weeklycoviddata_withsymptoms$pos_nosymptom_count) * (weeklycoviddata_withsymptoms$symptoms==FALSE)
weeklycoviddata_withsymptoms$covid_counts = (weeklycoviddata_withsymptoms$pos_symptom_count) * (weeklycoviddata_withsymptoms$symptoms==TRUE) + 
  (weeklycoviddata_withsymptoms$pos_nosymptom_count) * (weeklycoviddata_withsymptoms$symptoms==FALSE)

weeklycoviddata_withsymptoms = weeklycoviddata_withsymptoms[,-c(6,9:12)]

saveRDS(weeklycoviddata_withsymptoms, "../data/weeklycoviddata_withsymptoms_alt.RDS")
