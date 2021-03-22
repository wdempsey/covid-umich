library("lubridate")

fb_data = read.csv("../data/fb_indiana_data.csv")
indiana_data = readRDS("../data/weeklycoviddata.RDS")

## Build weekly version of FB data first

weeksin2020 = 14:53
all_data = data.frame(gender = rep(0,0), age = rep(0,0), fever = rep(0,0), 
                      weight_gottested = rep(0,0), weight = rep(0,0), week = rep(0,0),
                      year = rep(0,0))

for (week in weeksin2020) {
  temp = fb_data[week(fb_data$date) == week & year(fb_data$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weight_gottested = temp$weight * temp$gottested
  temp$weight_postested = temp$weight * temp$postest
  temp = subset(temp, !is.na(weight_gottested))
  agg_temp = aggregate(weight_gottested ~ gender+ age + fever, temp, sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, temp, sum)
  # agg_temp3 = aggregate(weight_postested ~ gender+ age + fever, temp, sum)
  # agg_temp$posweight = 0
  # for(row in 1:nrow(agg_temp)) {
  #   which_row = which(agg_temp3$gender == agg_temp[row,1] & agg_temp3$age == agg_temp[row,2] & agg_temp3$fever == agg_temp[row,3])
  #   if(length(which_row) == 0) {
  #     agg_temp$posweight == agg_temp3$weight_postested[row]
  #   }
  # }  
  agg_temp$weight = agg_temp2$weight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data = rbind(all_data, agg_temp)
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  temp = fb_data[week(fb_data$date) == week & year(fb_data$date) == 2021,]
  temp = subset(temp, is.element(gender, c(1,2)))
  temp$weight_gottested = temp$weight * temp$gottested
  temp = subset(temp, !is.na(weight_gottested))
  agg_temp = aggregate(weight_gottested ~ gender+ age + fever, temp, sum)
  agg_temp2 = aggregate(weight ~ gender+ age + fever, temp, sum)
  agg_temp$weight = agg_temp2$weight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data = rbind(all_data, agg_temp)
}

saveRDS(all_data, file = "data/fb_weekly.RDS")


