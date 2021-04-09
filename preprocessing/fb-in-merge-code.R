## Inputs:
## (1) Cleaned Facebook Data
## (2) Cleaned Weekly Indiana Data
## Outputs:
## (1) Collaping down to FEVER only

library("lubridate")

fb_data = read.csv("../data/fb_indiana_data.csv")
indiana_data = readRDS("../data/weeklycoviddata.RDS")

fb_data$day = day(fb_data$date)
fb_data$week = week(fb_data$date) 
weekweight = aggregate(day ~ week, fb_data, function(x){length(unique(x))})

## Build weekly version of FB data first

weeksin2020 = 14:53
all_data = data.frame(gender = rep(0,0), age = rep(0,0), fever = rep(0,0), 
                      weight_gottested = rep(0,0), weight = rep(0,0), week = rep(0,0),
                      year = rep(0,0))

for (week in weeksin2020) {
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = fb_data[week(fb_data$date) == week & year(fb_data$date) == 2020,]
  temp = subset(temp, is.element(gender, c(1,2)))
  total_weight = sum(temp$weight)
  agg_temp = aggregate(weight ~ gender+ age + fever, temp, sum)
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2020, nrow(agg_temp))
  all_data = rbind(all_data, agg_temp)
}

weeksin2021 = 1:5

for (week in weeksin2021) {
  addweight = 7/weekweight$day[weekweight$week == week]
  temp = fb_data[week(fb_data$date) == week & year(fb_data$date) == 2021,]
  temp = subset(temp, is.element(gender, c(1,2)))
  total_weight = sum(temp$weight)
  agg_temp = aggregate(weight ~ gender+ age + fever, temp, sum)
  agg_temp$weight = agg_temp$weight/sum(agg_temp$weight) * total_weight * addweight
  agg_temp$week = rep(week, nrow(agg_temp))
  agg_temp$year = rep(2021, nrow(agg_temp))
  all_data = rbind(all_data, agg_temp)
}

aggregate(weight~week, all_data, sum)


saveRDS(all_data, file = "../data/fb_weekly.RDS")


