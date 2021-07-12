library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities_neg = readRDS("../data/smoothedpropensities_neg_symptom_alt.RDS")
propensities_neg$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_neg$year,
                                                MMWRweek = propensities_neg$week,
                                                MMWRday = 1)
names(propensities_neg) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "date")

## Example propensity: Fever + Not H + White + Male
xmale =   as.matrix(c(1,0,0,1,0,0,0,0), ncol = 1)
xfemale =   as.matrix(c(0,1,0,1,0,0,0,0), ncol = 1)
propensities_neg$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:10])%*%xmale)))
propensities_neg$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:10])%*%xfemale)))

propensities_neg_long = rbind(propensities_neg, propensities_neg)

propensities_neg_long$Gender = c(rep("Male", nrow(propensities_neg)), rep("Female", nrow(propensities_neg)))

propensities_neg_long$probs = propensities_neg_long$probs_male * (propensities_neg_long$Gender == "Male") + propensities_neg_long$probs_female * (propensities_neg_long$Gender == "Female")

png(filename = "../figs/tvprop_symptom_alt_fig1.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_neg_long, aes(x = date, y = probs, col = Gender)) +
  geom_point(size = 5) +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals 35-44 tested negative in the past 24 hours")+ 
  theme(text = element_text(size=25))

dev.off()

propensities_pos = readRDS("../data/smoothedpropensities_pos_symptom_alt.RDS")
propensities_pos$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_pos$year,
                                                MMWRweek = propensities_pos$week,
                                                MMWRday = 1)

names(propensities_pos) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "hospital", "date")

## Example propensity: Hospitalized versus not hospitalized
xmale =   as.matrix(c(1,0,0,1,0,0,0,0,1), ncol = 1)
xfemale =   as.matrix(c(0,1,0,1,0,0,0,0,0), ncol = 1)
propensities_pos$probs_male = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xmale)))
propensities_pos$probs_female = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xfemale)))

propensities_pos_long = rbind(propensities_pos, propensities_pos)

propensities_pos_long$Hospitalized = c(rep("Yes", nrow(propensities_pos)), rep("No", nrow(propensities_pos)))

propensities_pos_long$probs = propensities_pos_long$probs_male * (propensities_pos_long$Hospitalized == "Yes") + propensities_pos_long$probs_female * (propensities_pos_long$Hospitalized == "No")

png(filename = "../figs/tvprop_symptom_alt_fig2.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_pos_long, aes(x = date, y = probs, col = Hospitalized)) +
  geom_point(size = 5) +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Individuals 35-44 tested positive in the past 24 hours") + 
  theme(text = element_text(size=25))

dev.off()
