fb_data = read.csv("../data/fb_indiana_data.csv")


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

## Fever (Only  )
fb_data$date 
agg_fever = aggregate(weight ~ fever, subset(, sum)
agg_age_strata = c(sum(agg_age$weight[1:2]) + agg_age$weight[3]/2, 
                   agg_age$weight[3]/2 + agg_age$weight[4] +  agg_age$weight[5]/2,
                   agg_age$weight[5]/2 + sum(agg_age$weight[6:7]))
agg_age_strata/sum(agg_age_strata)
