library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities_got = readRDS("../data/smoothedfeverpropensities_got.RDS")
propensities_got$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_got$year,
                                                MMWRweek = propensities_got$week,
                                                MMWRday = 1)
names(propensities_got) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "date")

## Example propensity: Fever + Not H + White + Male
xfemale = as.matrix(c(0,1,0,0,0,0,1,0), ncol = 1)
xmale =   as.matrix(c(1,0,0,0,0,0,1,0), ncol = 1)
propensities_got$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_got[,3:10])%*%xfemale)))
propensities_got$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_got[,3:10])%*%xmale)))

propensities_got_long = rbind(propensities_got, propensities_got)

propensities_got_long$gender = c(rep("F", nrow(propensities_got)), rep("M", nrow(propensities_got)))

propensities_got_long$probs = propensities_got_long$probs_female * (propensities_got_long$gender == "F") +propensities_got_long$probs_male * (propensities_got_long$gender == "M")

ggplot(data = propensities_got_long, aes(x = date, y = probs, col = gender)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals <= 25 tested in the past 24 hours")

propensities_pos = readRDS("../data/smoothedfeverpropensities_pos.RDS")
propensities_pos$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_pos$year,
                                                MMWRweek = propensities_pos$week,
                                                MMWRday = 1)

names(propensities_pos) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "date")

## Example propensity: Fever + Not H + White + Male
xfemale = as.matrix(c(0,1,1,0,0,0,0,0), ncol = 1)
xmale =   as.matrix(c(1,0,1,0,0,0,0,0), ncol = 1)
propensities_pos$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:10])%*%xfemale)))
propensities_pos$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:10])%*%xmale)))

propensities_pos_long = rbind(propensities_pos, propensities_pos)

propensities_pos_long$gender = c(rep("F", nrow(propensities_pos)), rep("M", nrow(propensities_pos)))

propensities_pos_long$probs = propensities_pos_long$probs_female * (propensities_pos_long$gender == "F") +propensities_pos_long$probs_male * (propensities_pos_long$gender == "M")

ggplot(data = propensities_pos_long, aes(x = date, y = probs, col = gender)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals <= 25 tested positive in the past 24 hours")
