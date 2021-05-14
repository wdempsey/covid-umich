library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities = readRDS("../data/smoothedpropensities.RDS")

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                     MMWRweek = propensities$week,
                                     MMWRday = 1)

## Example propensity: Fever + Contact + Not H + White + Male + 35to44
x = as.matrix(c(0,1,1,0,1,0,0,1,0,1,0,0,0,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:16])%*%x)))

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "35-44 year old non-Hispanic, White Male with Fever and COVID-19 Contact")

## Example propensity: Not Fever + Not H + African American + Female 35to44
x = as.matrix(c(1,0,0,1,1,0,0,1,0,1,0,0,0,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:16])%*%x)))

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "35-44 year old non-Hispanic, White Female without fever and COVID-19 Contact")
