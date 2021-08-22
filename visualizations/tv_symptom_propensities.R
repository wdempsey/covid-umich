library("RColorBrewer")
library(lubridate)
library(MMWRweek)
library(ggplot2)

propensities_neg = readRDS("../data/smoothedpropensities_neg_symptom.RDS")
propensities_neg$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_neg$year,
                                                MMWRweek = propensities_neg$week,
                                                MMWRday = 1)
names(propensities_neg) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "contact", "date")

my_palette <- brewer.pal(name="Greys",n=9)[c(4,6)]

## Example propensity: Fever + Not H + White + Male
xmalenocontact =   as.matrix(c(1,0,0,1,0,0,0,0,1), ncol = 1)
xmalecontact =   as.matrix(c(1,0,0,1,0,0,0,0,0), ncol = 1)
propensities_neg$probs_nocontact = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:11])%*%xmalenocontact)))
propensities_neg$probs_contact = as.vector(1/(1+exp(-as.matrix(propensities_neg[,3:11])%*%xmalecontact)))

propensities_neg_long = rbind(propensities_neg, propensities_neg)

propensities_neg_long$Contact = c(rep("Contact", nrow(propensities_neg)), rep("No Contact", nrow(propensities_neg)))

propensities_neg_long$probs = propensities_neg_long$probs_contact * (propensities_neg_long$Contact == "Contact") + propensities_neg_long$probs_nocontact * (propensities_neg_long$Contact == "No Contact")

png(filename = "../figs/tvprop_symptom_fig1.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_neg_long, aes(x = date, y = probs, col = Contact)) +
  geom_point(size=5) +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Male Individuals 35-44 tested negative in the past 24 hours") + 
  scale_color_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()

propensities_pos = readRDS("../data/smoothedpropensities_pos_symptom.RDS")
propensities_pos$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities_pos$year,
                                                MMWRweek = propensities_pos$week,
                                                MMWRday = 1)

names(propensities_pos) = c("week", "year", "gender1", "gender2", "25to34", "35to44", "45to54",
                            "55to64", "65to74", "75plus", "nocontact", "date")

## Example propensity: Fever + Not H + White + Male
xfemalenocontact = as.matrix(c(0,1,0,1,0,0,0,0,1), ncol = 1)
xfemalecontact     = as.matrix(c(0,1,0,1,0,0,0,0,0), ncol = 1)
propensities_pos$probs_nocontact = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xfemalenocontact)))
propensities_pos$probs_contact = as.vector(1/(1+exp(-as.matrix(propensities_pos[,3:11])%*%xfemalecontact)))

propensities_pos_long = rbind(propensities_pos, propensities_pos)

propensities_pos_long$Contact = c(rep("Contact", nrow(propensities_neg)), rep("No Contact", nrow(propensities_neg)))

propensities_pos_long$probs = propensities_pos_long$probs_contact * (propensities_pos_long$Contact == "Contact") + propensities_pos_long$probs_nocontact * (propensities_pos_long$Contact == "No Contact")

png(filename = "../figs/tvprop_symptom_fig2.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_pos_long, aes(x = date, y = probs, col = Contact)) +
  geom_point(size=5) +
  labs(x = "Date",
       y = "Likelihood of Fever",
       title = "Female individuals 35-44 tested positive in the past 24 hours") +
  scale_color_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()
