library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities = readRDS("../data/smoothedpropensities.RDS")

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                     MMWRweek = propensities$week,
                                     MMWRday = 1)

## Example propensity: Fever + Not H + White + Male
x = as.matrix(c(1,1,0,0,0,0,1), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:9])%*%x)))

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "Precipitation Data",
       subtitle = "Boulder, Colorado 2013")

## Example propensity: Fever + Not H + Asian + Female
x = as.matrix(c(1,1,0,0,0,0,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:9])%*%x)))

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "Precipitation Data",
       subtitle = "Boulder, Colorado 2013")
