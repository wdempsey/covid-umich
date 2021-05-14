## Inputs:
## (1) Cleaned Facebook Data
## (2) Cleaned Weekly Indiana Data
## Outputs:
## (1) Collaping down to SYMPTOM AND HOSPITAL

library("lubridate")

fb_data = read.csv("../data/fb_test_data.csv")
fb_data$day = day(fb_data$date)
fb_data$week = week(fb_data$date) 
weekweight = aggregate(day ~ week, fb_data, function(x){length(unique(x))})

## Build weekly version of FB data first
weeksin2020 = 14:53
all_data_neg_hospital = all_data_pos_hospital = 
  all_data_covid = 
  data.frame(gender = rep(0,0), age = rep(0,0), 
             weight = rep(0,0), week = rep(0,0),
             year = rep(0,0))

tested_fbdata = subset(fb_data, gottested == 1)
rm(fb_data)

tested_fbdata$hospital = is.element(tested_fbdata$hospital, c("TRUE","1"))

for (week in weeksin2020) {
  print(paste("At week", week, "in year 2020"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  ## Building Hospital | Symptom
  temp$weighthospital = temp$weight * temp$hospital
  temp = subset(temp, !is.na(weighthospital))
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weighthospital = sum(temp$weighthospital[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weighthospital ~ gender+ age + fever, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weighthospital = agg_temp$weighthospital/sum(agg_temp$weighthospital) * total_weighthospital * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_neg_hospital = rbind(all_data_neg_hospital, agg_temp)
  
  ## Positive 
  total_weight = sum(temp$weight[temp$postest ==1], na.rm = TRUE)
  total_weighthospital = sum(temp$weighthospital[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weighthospital ~ gender+ age + fever, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weighthospital= agg_temp$weighthospital/sum(agg_temp$weighthospital) * total_weighthospital * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_pos_hospital = rbind(all_data_pos_hospital, agg_temp)
  
  ## Adding covid positive given symptoms
  temp$weightcovid =  temp$weight * temp$postest
  total_weight = sum(temp$weight[temp$symptoms == TRUE], na.rm = TRUE)
  total_weightcovid = sum(temp$weightcovid[temp$symptoms == TRUE], na.rm = TRUE)
  agg_temp = aggregate(weightcovid ~ gender+ age, subset(temp, symptoms == TRUE), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcovid = agg_temp$weightcovid/sum(agg_temp$weightcovid) * total_weightcovid * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_covid = rbind(all_data_covid, agg_temp)
  
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  print(paste("At week", week, "in year 2021"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2021,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weighthospital = temp$weight * temp$hospital
  temp = subset(temp, !is.na(weighthospital))
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weighthospital = sum(temp$weighthospital[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weighthospital ~ gender+ age + fever, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weighthospital = agg_temp$weighthospital/sum(agg_temp$weighthospital) * total_weighthospital * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_neg_hospital = rbind(all_data_neg_hospital, agg_temp)
  
  ## Positive 
  total_weight = sum(temp$weight[temp$postest ==1], na.rm = TRUE)
  total_weighthospital = sum(temp$weighthospital[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weighthospital ~ gender+ age + fever, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weighthospital= agg_temp$weighthospital/sum(agg_temp$weighthospital) * total_weighthospital * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_pos_hospital = rbind(all_data_pos_hospital, agg_temp)
  
  ## Adding covid positive given symptoms
  temp$weightcovid =  temp$weight * temp$postest
  total_weight = sum(temp$weight[temp$symptoms == TRUE], na.rm = TRUE)
  total_weightcovid = sum(temp$weightcovid[temp$symptoms == TRUE], na.rm = TRUE)
  agg_temp = aggregate(weightcovid ~ gender+ age, subset(temp, symptoms == TRUE), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcovid = agg_temp$weightcovid/sum(agg_temp$weightcovid) * total_weightcovid * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_covid = rbind(all_data_covid, agg_temp)
}


#aggregate(weight~week, all_data_pos, sum)

saveRDS(all_data_pos_hospital, file = "../data/fb_alldata_weekly_pos_hospital_alt.RDS")
saveRDS(all_data_covid, file = "../data/fb_alldata_weekly_covid_alt.RDS")


