library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities_got = readRDS("../data/smoothedfeverpropensities_got.RDS")
propensities_got$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_got$year,
                                                MMWRweek = propensities_got$week,
                                                MMWRday = 1)

## Example propensity: Fever + Not H + White + Male
x = as.matrix(c(1,0), ncol = 1)
propensities_got$probs = as.vector(1/(1+exp(-as.matrix(propensities_got[,3:4])%*%x)))

ggplot(data = propensities_got, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Female who got tested in the past 24 hours")

propensities_pos = readRDS("../data/smoothedfeverpropensities_pos.RDS")
propensities_pos$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_pos$year,
                                                MMWRweek = propensities_pos$week,
                                                MMWRday = 1)

## Example propensity: Fever + Not H + White + Male
x = as.matrix(c(1,0), ncol = 1)
propensities_pos$probs = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:4])%*%x)))

ggplot(data = propensities_pos, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Female who tested positive in the past 24 hours")

