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
tested_fbdata = subset(fb_data, gottested == 1)
rm(fb_data)

tested_fbdata$hospital = is.element(tested_fbdata$hospital, c("TRUE","1"))

## Build weekly version of FB data first
weeksin2020 = 14:53
all_data_neg_symptom = all_data_pos_symptom =
  data.frame(gender = rep(0,0), age = rep(0,0), 
             weight = rep(0,0), week = rep(0,0),
             year = rep(0,0))

for (week in weeksin2020) {
  print(paste("At week", week, "in year 2020"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  ## Adding Symptom given Covid Positive, Hospitalized
  temp$weightsymptom =  temp$weight * temp$symptoms
  total_weight = sum(temp$weight[temp$postest == 1], na.rm = TRUE)
  total_weightsymptom = sum(temp$weightsymptom[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weightsymptom ~ gender+ age + hospital, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + hospital, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptom = agg_temp$weightsymptom/sum(agg_temp$weightsymptom) * total_weightsymptom * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_pos_symptom = rbind(all_data_pos_symptom, agg_temp)
  
  ## Adding Symptom given Covid Negative (don't include hospitalization)
  total_weight = sum(temp$weight[temp$postest == 0], na.rm = TRUE)
  total_weightsymptom = sum(temp$weightsymptom[temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightsymptom ~ gender+ age, subset(temp, postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptom = agg_temp$weightsymptom/sum(agg_temp$weightsymptom) * total_weightsymptom * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_neg_symptom = rbind(all_data_neg_symptom, agg_temp)
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  print(paste("At week", week, "in year 2021"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2021,]
  temp = subset(temp, is.element(gender, c(1,2)))
  ## Adding Symptom given Covid Positive, Hospitalized
  temp$weightsymptom =  temp$weight * temp$symptoms
  total_weight = sum(temp$weight[temp$postest == 1], na.rm = TRUE)
  total_weightsymptom = sum(temp$weightsymptom[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weightsymptom ~ gender+ age + hospital, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + hospital, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptom = agg_temp$weightsymptom/sum(agg_temp$weightsymptom) * total_weightsymptom * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_pos_symptom = rbind(all_data_pos_symptom, agg_temp)
  
  ## Adding Symptom given Covid Negative (don't include hospitalization)
  total_weight = sum(temp$weight[temp$postest == 0], na.rm = TRUE)
  total_weightsymptom = sum(temp$weightsymptom[temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightsymptom ~ gender+ age, subset(temp, postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptom = agg_temp$weightsymptom/sum(agg_temp$weightsymptom) * total_weightsymptom * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_neg_symptom = rbind(all_data_neg_symptom, agg_temp)
}

#aggregate(weight~week, all_data_pos_symptom, sum)

saveRDS(all_data_pos_symptom, file = "../data/fb_alldata_weekly_pos_symptom_alt.RDS")
saveRDS(all_data_neg_symptom, file = "../data/fb_alldata_weekly_neg_symptom_alt.RDS")


