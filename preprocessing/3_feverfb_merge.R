## Inputs:
## (1) Cleaned Facebook Data
## (2) Cleaned Weekly Indiana Data
## Outputs:
## (1) Collaping down to FEVER only

library("lubridate")

fb_data = read.csv("../data/fb_test_data.csv")
fb_data$day = day(fb_data$date)
fb_data$week = week(fb_data$date) 
weekweight = aggregate(day ~ week, fb_data, function(x){length(unique(x))})

## Build weekly version of FB data first
weeksin2020 = 14:53
all_data_neg_contact = all_data_neg_symptom = 
  all_data_pos_contact = all_data_pos_symptom = data.frame(gender = rep(0,0), age = rep(0,0), 
                                                           weight = rep(0,0), week = rep(0,0),
                                                           year = rep(0,0))

tested_fbdata = subset(fb_data, gottested == 1)
rm(fb_data)

for (week in weeksin2020) {
  print(paste("At week", week, "in year 2020"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weightcontact = temp$weight * (temp$contact==1)
  temp = subset(temp, !is.na(weightcontact))
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weightcontact = sum(temp$weightcontact[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightcontact ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcontact = agg_temp$weightcontact/sum(agg_temp$weightcontact) * total_weightcontact * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_neg_contact = rbind(all_data_neg_contact, agg_temp)
  
  ## Adding fever given contact
  temp$weightsymptoms =  temp$weight * temp$fever
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weightsymptoms = sum(temp$weightsymptoms[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightsymptoms ~ gender+ age + contact, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + contact, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptoms = agg_temp$weightsymptoms/sum(agg_temp$weightsymptoms) * total_weightsymptoms * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_neg_symptom = rbind(all_data_neg_symptom, agg_temp)
  
  ## Positive 
  total_weight = sum(temp$weight[temp$postest ==1], na.rm = TRUE)
  total_weightcontact = sum(temp$weightcontact[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weightcontact ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcontact = agg_temp$weightcontact/sum(agg_temp$weightcontact) * total_weightcontact * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_pos_contact = rbind(all_data_pos_contact, agg_temp)
  
  ## Adding fever given contact
  temp$weightsymptoms =  temp$weight * temp$fever
  total_weight = sum(temp$weight[temp$postest == 1], na.rm = TRUE)
  total_weightsymptoms = sum(temp$weightsymptoms[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weightsymptoms ~ gender+ age + contact, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + contact, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptoms = agg_temp$weightsymptoms/sum(agg_temp$weightsymptoms) * total_weightsymptoms * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_pos_symptom = rbind(all_data_pos_symptom, agg_temp)
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  print(paste("At week", week, "in year 2021"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2021,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weightcontact = temp$weight * (temp$contact==1)
  temp = subset(temp, !is.na(weightcontact))
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weightcontact = sum(temp$weightcontact[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightcontact ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcontact = agg_temp$weightcontact/sum(agg_temp$weightcontact) * total_weightcontact * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_neg_contact = rbind(all_data_neg_contact, agg_temp)
  
  ## Adding fever given contact
  temp$weightsymptoms =  temp$weight * temp$fever
  total_weight = sum(temp$weight[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  total_weightsymptoms = sum(temp$weightsymptoms[temp$gottested ==1 & temp$postest == 0], na.rm = TRUE)
  agg_temp = aggregate(weightsymptoms ~ gender+ age + contact, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + contact, subset(temp, gottested == 1 & postest == 0), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptoms = agg_temp$weightsymptoms/sum(agg_temp$weightsymptoms) * total_weightsymptoms * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_neg_symptom = rbind(all_data_neg_symptom, agg_temp)
  
  ## Positive 
  total_weight = sum(temp$weight[temp$postest ==1], na.rm = TRUE)
  total_weightcontact = sum(temp$weightcontact[temp$postest ==1], na.rm = TRUE)
  agg_temp = aggregate(weightcontact ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightcontact = agg_temp$weightcontact/sum(agg_temp$weightcontact) * total_weightcontact * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_pos_contact = rbind(all_data_pos_contact, agg_temp)
  
  ## Adding fever given contact
  temp$weightsymptoms =  temp$weight * temp$fever
  total_weight = sum(temp$weight[temp$postest == 1], na.rm = TRUE)
  total_weightsymptoms = sum(temp$weightsymptoms[temp$postest == 1], na.rm = TRUE)
  agg_temp = aggregate(weightsymptoms ~ gender+ age + contact, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age + contact, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightsymptoms = agg_temp$weightsymptoms/sum(agg_temp$weightsymptoms) * total_weightsymptoms * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data_pos_symptom = rbind(all_data_pos_symptom, agg_temp)
}


#aggregate(weight~week, all_data_pos, sum)

saveRDS(all_data_neg_contact, file = "../data/fb_alldata_weekly_neg_contact.RDS")
saveRDS(all_data_neg_symptom, file = "../data/fb_alldata_weekly_neg_symptom.RDS")
saveRDS(all_data_pos_contact, file = "../data/fb_alldata_weekly_pos_contact.RDS")
saveRDS(all_data_pos_symptom, file = "../data/fb_alldata_weekly_pos_symptom.RDS")


