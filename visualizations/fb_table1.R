work_dir = "/home/wdem/Dropbox/Fb-Data/"
home_dir = "../data/"
fb_data = read.csv(paste(work_dir, "fb_indiana_data.csv", sep = ""))
fb_data = read.csv(paste(home_dir, "fb_indiana_data.csv", sep = ""))

## Gender Summary
## 1 = MALE, 2 = Female, 3 = Non-binary, 4 = Self-Describe, 5 = Prefer not to answer
agg_gender = aggregate(weight ~ gender, fb_data, sum)
weights_gender = agg_gender$weight[is.element(agg_gender$gender,c(1,2))]
weights_gender/sum(weights_gender)

## Age Summary
#  18-24 years (1), 25-34 years (2), 35-44 years (3), 45-54 years (4), 
#  55-64 years (5), 65-74 years (6), 75 years or older (7)
agg_age = aggregate(weight ~ age, fb_data, sum)
agg_age_strata = c(sum(agg_age$weight[1:2]) + agg_age$weight[3]/2, 
  agg_age$weight[3]/2 + agg_age$weight[4] +  agg_age$weight[5]/2,
  agg_age$weight[5]/2 + sum(agg_age$weight[6:7]))
agg_age_strata/sum(agg_age_strata)

## Fever (Only April 25th to 29th)
library(lubridate)
fb_data$date  = ymd(fb_data$date )
window = fb_data$date >= ymd("2020-04-25") & fb_data$date <= ymd("2020-04-29") 
agg_fever = aggregate(weight ~ fever, subset(fb_data, window), sum)
agg_fever$weight/sum(agg_fever$weight)

agg_cough = aggregate(weight ~ cough, subset(fb_data, window), sum)
agg_cough$weight/sum(agg_cough$weight)

agg_shortness = aggregate(weight ~ shortness, subset(fb_data, window), sum)
agg_shortness$weight/sum(agg_shortness$weight)

agg_household = aggregate(weight ~ contact, subset(fb_data, window), sum)
agg_household$weight/sum(agg_household$weight)
