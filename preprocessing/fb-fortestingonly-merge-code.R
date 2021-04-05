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
all_data_got = all_data_pos = data.frame(gender = rep(0,0), age = rep(0,0), fever = rep(0,0), 
                      weight_gottested = rep(0,0), weight = rep(0,0), week = rep(0,0),
                      year = rep(0,0))

tested_fbdata = subset(fb_data, gottested == 1)
rm(fb_data)

for (week in weeksin2020) {
  print(paste("At week", week, "in year 2020"))
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = tested_fbdata[week(tested_fbdata$date) == week & year(tested_fbdata$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weightfever = temp$weight * temp$fever
  temp = subset(temp, !is.na(weightfever))
  total_weight = sum(temp$weight[temp$gottested ==1])
  total_weightfever = sum(temp$weightfever[temp$gottested ==1])
  agg_temp = aggregate(weightfever ~ gender+ age, subset(temp, gottested == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, gottested == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightfever = agg_temp$weightfever/sum(agg_temp$weightfever) * total_weightfever * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  all_data_got = rbind(all_data_got, agg_temp)
  
  total_weight = sum(temp$weight[temp$postest ==1])
  total_weightfever = sum(temp$weightfever[temp$postest ==1])
  agg_temp = aggregate(weightfever ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp2 = aggregate(weight ~ gender+ age, subset(temp, postest == 1), sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$weightfever = agg_temp$weightfever/sum(agg_temp$weightfever) * total_weightfever * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data_pos = rbind(all_data_pos, agg_temp)
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = fb_data[week(fb_data$date) == week & year(fb_data$date) == 2021,]
  total_weight = sum(temp$weight)
  total_weightgottested = sum(temp$weight_gottested)
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weight_gottested = temp$weight * temp$gottested
  temp$weight_postested = temp$weight * temp$postest
  temp = subset(temp, !is.na(weight_gottested))
  agg_temp = aggregate(weight_gottested ~ gender+ age + fever, temp, sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, temp, sum)
  agg_temp3 = aggregate(weight_postested ~ gender+ age + fever, temp, sum)
  total_weightpostested = sum(temp$weight_postested)
  agg_temp$posweight = NA
  for(row in 1:nrow(agg_temp)) {
    which_row = which(agg_temp3$gender == agg_temp[row,1] & agg_temp3$age == agg_temp[row,2] & agg_temp3$fever == agg_temp[row,3])
    if(length(which_row) > 0) {
      agg_temp$posweight[row] = agg_temp3$weight_postested[row]
    }
  }
  agg_temp$weight = agg_temp2$weight
  agg_temp$weight_gottested = agg_temp$weight_gottested/sum(agg_temp$weight_gottested) * total_weightgottested * addweight
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$posweight = agg_temp$posweight/sum(agg_temp$posweight) * total_weightpostested * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data = rbind(all_data, agg_temp)
}

aggregate(weight~week, all_data, sum)


saveRDS(all_data, file = "../data/fb_weekly.RDS")


