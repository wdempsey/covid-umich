library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

my_palette <- brewer.pal(name="Greys",n=9)[seq(1,9,2)]

prevalence = readRDS("../data/invweights_08262021.RDS")
prevalence_alt = readRDS("../data/invweights_alt_08262021.RDS")

prevalence = data.frame(prevalence)
prevalence_alt = data.frame(prevalence_alt)
names(prevalence) = names(prevalence_alt)  = c("week", "year", "prev", "ipw_prev")

prevalence$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence$year,
                                     MMWRweek = prevalence$week,
                                     MMWRday = 1)

prevalence_alt$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence$year,
                                          MMWRweek = prevalence$week,
                                          MMWRday = 1)

dates = prevalence_alt$date

prevalence_long = rbind(prevalence, prevalence, prevalence_alt)

prevalence_long$Method = c(rep("Unweighted", nrow(prevalence)),
                           rep("IPW1", nrow(prevalence)),
                           rep("IPW2", nrow(prevalence))
                           )

prevalence_long$estimate = prevalence_long$prev * (prevalence_long$Method == "Unweighted") +
  prevalence_long$ipw_prev * (prevalence_long$Method == "IPW1") + 
  prevalence_long$ipw_prev * (prevalence_long$Method == "IPW2")

prevalence_long = data.frame(date = prevalence_long$date, Method = prevalence_long$Method, estimate = prevalence_long$estimate)

ratio_long = data.frame(date = rep(0,0), Method = rep(0,0), estimate = rep(0,0))

for(method in c("Unweighted", "IPW1", "IPW2")) {
  prevalence_temp = subset(prevalence_long, Method == method)
  ratio = prevalence_temp$estimate[2:length(prevalence_temp$estimate)]/prevalence_temp$estimate[1:(length(prevalence_temp$estimate)-1)]
  date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
  x = as.numeric(prevalence_temp$date - min(prevalence_temp$date))
  smoothed_estimate = (predict(loess(prevalence_temp$estimate ~ x)))
  ratio = smoothed_estimate[2:length(smoothed_estimate)]/smoothed_estimate[1:(length(smoothed_estimate)-1)]
  ratio_temp = data.frame(date = date, Method = rep(method, length(date)), 
                               estimate = ratio)
  ratio_long = rbind(ratio_long, ratio_temp)
}


## ADD IN MODEL-BASED
prevalence_temp = readRDS("../data/aggregate_air.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate)
ratio = prevalence_temp$estimate[2:length(prevalence_temp$estimate)]/prevalence_temp$estimate[1:(length(prevalence_temp$estimate)-1)]
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
x = as.numeric(prevalence_temp$date - min(prevalence_temp$date))
smoothed_estimate = (predict(loess(prevalence_temp$estimate ~ x)))
ratio = smoothed_estimate[2:length(smoothed_estimate)]/smoothed_estimate[1:(length(smoothed_estimate)-1)]
ratio_temp = data.frame(date = date, Method = rep("Model-based", length(date)), 
                        estimate = ratio)
ratio_long = rbind(ratio_long, ratio_temp)
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST
prevalence_temp = readRDS("../data/drestimates_alt_082621.RDS")
prevalence_temp = data.frame(date = dates, Method = rep("Doubly Robust", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5])
ratio = prevalence_temp$estimate[2:length(prevalence_temp$estimate)]/prevalence_temp$estimate[1:(length(prevalence_temp$estimate)-1)]
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
x = as.numeric(prevalence_temp$date - min(prevalence_temp$date))
smoothed_estimate = (predict(loess(prevalence_temp$estimate ~ x)))
ratio = smoothed_estimate[2:length(smoothed_estimate)]/smoothed_estimate[1:(length(smoothed_estimate)-1)]
ratio_temp = data.frame(date = date, Method = rep("Doubly Robust", length(date)), 
                        estimate = ratio)
ratio_long = rbind(ratio_long, ratio_temp)
prevalence_long = rbind(prevalence_long, prevalence_temp)

## FINAL FIGURES
png(filename = "../figs/tv_air.png",
    width = 960, height = 480, units = "px", pointsize = 25)

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

png(filename = "../figs/tv_air_cis.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = prevalence_alt_cis, aes(x = date, y = estimate)) +
  geom_point(size = 2, color = my_palette) +
  geom_errorbar(aes(ymin=lowci, ymax=upperci), width=.2, position = pd) +  labs(x = "Date",
       y = "Active Infection Rate Estimate") + 
  theme(text = element_text(size=25))

dev.off()
