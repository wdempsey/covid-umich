library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities_neg = readRDS("../data/smoothedpropensities_neg_symptom.RDS")
propensities_neg$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_neg$year,
                                                MMWRweek = propensities_neg$week,
                                                MMWRday = 1)
names(propensities_neg) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "nocontact", "date")

## Example propensity: Fever + Not H + White + Male
xfemalecontact =   as.matrix(c(0,1,0,1,0,0,0,0,0), ncol = 1)
xmalenocontact =   as.matrix(c(1,0,0,1,0,0,0,0,1), ncol = 1)
propensities_neg$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:11])%*%xfemalecontact)))
propensities_neg$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:11])%*%xmalenocontact)))

propensities_neg_long = rbind(propensities_neg, propensities_neg)

propensities_neg_long$gender = c(rep("F", nrow(propensities_neg)), rep("M", nrow(propensities_neg)))

propensities_neg_long$probs = propensities_neg_long$probs_female * (propensities_neg_long$gender == "F") +propensities_neg_long$probs_male * (propensities_neg_long$gender == "M")

ggplot(data = propensities_neg_long, aes(x = date, y = probs, col = gender)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals <= 25 tested negative in the past 24 hours")

propensities_pos = readRDS("../data/smoothedpropensities_pos_symptom.RDS")
propensities_pos$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_pos$year,
                                                MMWRweek = propensities_pos$week,
                                                MMWRday = 1)

names(propensities_pos) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "nocontact", "date")

## Example propensity: Fever + Not H + White + Male
xfemalecontact =   as.matrix(c(0,1,0,1,0,0,0,0,0), ncol = 1)
xmalenocontact =   as.matrix(c(1,0,0,1,0,0,0,0,1), ncol = 1)
propensities_pos$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xfemalecontact)))
propensities_pos$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xmalenocontact)))

propensities_pos_long = rbind(propensities_pos, propensities_pos)

propensities_pos_long$gender = c(rep("F", nrow(propensities_pos)), rep("M", nrow(propensities_pos)))

propensities_pos_long$probs = propensities_pos_long$probs_female * (propensities_pos_long$gender == "F") +propensities_pos_long$probs_male * (propensities_pos_long$gender == "M")

ggplot(data = propensities_pos_long, aes(x = date, y = probs, col = gender)) +
  geom_point() +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals <= 25 tested positive in the past 24 hours")

