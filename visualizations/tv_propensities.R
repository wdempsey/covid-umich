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

png(filename = "../figs/tvprop_fig1.png",
    width = 960, height = 480, units = "px", pointsize = 12)

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "35-44 year old non-Hispanic, White Male with Fever and COVID-19 Contact")

dev.off()

## Example propensity: Not Fever + Not Contact + Not H + White + Male + 65to74
x = as.matrix(c(1,0,0,1,1,0,0,0,1,0,0,1,0,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:16])%*%x)))


png(filename = "../figs/tvprop_fig2.png",
    width = 960, height = 480, units = "px", pointsize = 12)

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point() +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "65-74 year old non-Hispanic, White Female without fever nor COVID-19 Contact")

dev.off()