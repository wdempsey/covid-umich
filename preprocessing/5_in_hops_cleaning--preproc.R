weeklycoviddata_hospital = readRDS(file = "../data/weeklycoviddata_hospital.RDS")
weeklycoviddata_testing = readRDS(file = "../data/weeklycoviddata.RDS")

propensity_pos_hospital = readRDS("../data/smoothedpropensities_pos_hospital_alt.RDS")
propensity_neg_hospital = readRDS("../data/smoothedpropensities_neg_hospital_alt.RDS")
propensity_pos_symptom = readRDS("../data/smoothedpropensities_pos_symptom_alt.RDS")
propensity_neg_symptom = readRDS("../data/smoothedpropensities_neg_symptom_alt.RDS")


propensity_pos_hospital[1,]

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
    prop_temp_pos_hospital = propensity_pos_hospital[propensity_pos_hospital$week == temp$week & propensity_pos_hospital$year == temp$year,]
    prop_temp_pos_symptom = propensity_pos_symptom[propensity_pos_symptom$week == temp$week & propensity_pos_symptom$year == temp$year,]
    prop_temp_neg_symptom = propensity_neg_symptom[propensity_neg_symptom$week == temp$week & propensity_neg_symptom$year == temp$year,]
    pos_hospital_term = sum(prop_temp_pos_hospital[3:11]*feature_vector)
    prob_hosp_temp = 1/(1+exp(-pos_hospital_term))
    
    pos_symptom_term = sum(prop_temp_pos_symptom[3:10]*feature_vector[1:8])
    prob_pos_symp_term = 1/(1+exp(-pos_symptom_term))
    neg_symptom_term = sum(prop_temp_neg_symptom[3:10]*feature_vector[1:8])
    prob_neg_symp_term = 1/(1+exp(-neg_symptom_term))
    
    covidtesting_match = which(weeklycoviddata_testing$week == temp$week & weeklycoviddata_testing$year == temp$year &
                                 weeklycoviddata_testing$age == temp$age & weeklycoviddata_testing$gender == temp$gender & 
                                 weeklycoviddata_testing$ethnicity == temp$ethnicity & weeklycoviddata_testing$race == temp$race)
    covid_posrate_temp = sum(weeklycoviddata_testing$covid_counts[covidtesting_match])/sum(weeklycoviddata_testing$covid_tests[covidtesting_match])
    
    covid_odds_temp = covid_posrate_temp/(1-covid_posrate_temp)
    
    covid_neg_rate = prob_hosp_temp*prob_pos_symp_term/prob_neg_symp_term * covid_odds_temp
    if(length(covid_neg_rate) > 1) {print(current_row)}
    
    neg_counts = weeklycoviddata_testing$covid_tests[covidtesting_match]-weeklycoviddata_testing$covid_counts[covidtesting_match]
    pos_counts = weeklycoviddata_testing$covid_counts[covidtesting_match]
    pos_symptom_count = min(temp$hospitalization_count/prob_hosp_temp,pos_counts)
    pos_nosymptom_count = max(pos_counts - pos_symptom_count,0)
    neg_symptom_count = min(temp$hospitalization_count/covid_neg_rate,neg_counts)
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
