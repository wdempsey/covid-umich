library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

my_palette <- brewer.pal(name="Greys",n=9)[seq(1,9,2)]
prevalence_long = data.frame(date = rep(0,0), Method = rep(0,0), estimate = rep(0,0))
ratio_long = data.frame(date = rep(0,0), Method = rep(0,0), estimate = rep(0,0))

## ADD IN MODEL-BASED
prevalence_temp = readRDS("../data/aggregate_air_2022_24_03.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate)
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST
prevalence_temp = readRDS("../data/drestimates_alt_2022_24_03.RDS")
prevalence_temp = data.frame(prevalence_temp)
prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp[,2],
                                          MMWRweek = prevalence_temp[,1],
                                          MMWRday = 1)
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Doubly Robust", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5])
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN MODEL-BASED: LOWER
prevalence_temp = readRDS("../data/aggregate_air_lowerifr_2022_24_03.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based, Low", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate)
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST: LOWER
prevalence_temp = readRDS("../data/drestimates_alt_lowerifr_2022_24_03.RDS")
prevalence_temp = data.frame(prevalence_temp)
prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp[,2],
                                               MMWRweek = prevalence_temp[,1],
                                               MMWRday = 1)
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Doubly Robust, Low", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5])
prevalence_long = rbind(prevalence_long, prevalence_temp)

## FINAL FIGURES
# png(filename = "../figs/tv_air_sensitivity.png",
# width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = prevalence_long, aes(x = date, y = estimate, col = Method)) +
  geom_line(size = 2) +
  labs(x = "Date",
       y = "Active Infection Rate Estimate") + 
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()


## Confidence Interval Visualization for Appendix.
my_palette <- brewer.pal(name="Greys",n=9)[7]

prevalence_alt_cis = readRDS("../data/ipw_cis.RDS")
prevalence_alt_cis = data.frame(prevalence_alt_cis)
names(prevalence_alt_cis)  = c("week", "year", "estimate", "stderr", "lowci", "upperci")
prevalence_alt_cis$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_alt_cis$year,
                                          MMWRweek = prevalence_alt_cis$week,
                                          MMWRday = 1)

pd <- position_dodge(0.1) # move them .05 to the left and right

# png(filename = "../figs/tv_air_cis.png",
    # width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = prevalence_alt_cis, aes(x = date, y = estimate)) +
  geom_point(size = 2, color = my_palette) +
  geom_errorbar(aes(ymin=lowci, ymax=upperci), width=.2, position = pd) +  labs(x = "Date",
       y = "Active Infection Rate Estimate") + 
  theme(text = element_text(size=25))

# dev.off()
