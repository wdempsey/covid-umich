library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

my_palette <- brewer.pal(name="Greys",n=9)[seq(3,9,3)]
prevalence_long = data.frame(date = rep(0,0), Method = rep(0,0), 
                             estimate = rep(0,0), Type = rep(0,0))
ratio_long = data.frame(date = rep(0,0), Method = rep(0,0), 
                        estimate = rep(0,0), Type = rep(0,0))

## ADD IN MODEL-BASED
prevalence_temp = readRDS("../data/aggregate_air_2022_28_03.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate)
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
prevalence_temp$Type = "Avg IFR"
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST
prevalence_temp = readRDS("../data/drestimates_alt_2022_28_03.RDS")
prevalence_temp = data.frame(prevalence_temp)
prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp[,2],
                                          MMWRweek = prevalence_temp[,1],
                                          MMWRday = 1)
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Doubly Robust", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5])
prevalence_temp$Type = "Avg IFR"
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN MODEL-BASED: LOWER
prevalence_temp = readRDS("../data/aggregate_air_lowerifr_2022_28_03.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate,
                             Type = "Low IFR")
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST: LOWER
prevalence_temp = readRDS("../data/drestimates_alt_lowerifr_2022_28_03.RDS")
prevalence_temp = data.frame(prevalence_temp)
prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp[,2],
                                               MMWRweek = prevalence_temp[,1],
                                               MMWRday = 1)
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Doubly Robust", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5],
                             Type = "Low IFR")
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN MODEL-BASED: UPPER
prevalence_temp = readRDS("../data/aggregate_air_upperifr_2022_28_03.RDS")
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Model-based", nrow(prevalence_temp)), 
                             estimate = prevalence_temp$estimate,
                             Type = "High IFR")
date = prevalence_temp$date[2:length(prevalence_temp$estimate)]
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADD IN DOUBLY ROBUST: UPPER
prevalence_temp = readRDS("../data/drestimates_alt_upperifr_2022_28_03.RDS")
prevalence_temp = data.frame(prevalence_temp)
prevalence_temp$date = MMWRweek::MMWRweek2Date(MMWRyear = prevalence_temp[,2],
                                               MMWRweek = prevalence_temp[,1],
                                               MMWRday = 1)
prevalence_temp = data.frame(date = prevalence_temp$date, Method = rep("Doubly Robust", nrow(prevalence_temp)), 
                             estimate = prevalence_temp[,5],
                             Type = "High IFR")
prevalence_long = rbind(prevalence_long, prevalence_temp)

## ADDING FLOOR 
indiana_data = readRDS("../data/weeklycoviddata_withsympcontact.RDS")
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)
agg_counts = aggregate(covid_counts ~ week + year, indiana_data, sum)
N <- 6.732E6; # Indiana Population
agg_counts$air_floor = agg_counts$covid_counts/N

agg_counts$date = MMWRweek::MMWRweek2Date(MMWRyear = agg_counts$year,
                                          MMWRweek = agg_counts$week,
                                          MMWRday = 1)
agg_counts_df = data.frame("date" = agg_counts$date, 
                           "Method" = rep("Floor", nrow(agg_counts)), 
                           "estimate" = agg_counts$air_floor, 
                           "Type" = rep("Floor", nrow(agg_counts)))
prevalence_long = rbind(prevalence_long, agg_counts_df)


## FINAL FIGURES
png(filename = "../figs/tv_air_sensitivity.png",
width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = prevalence_long, aes(x = date, y = estimate, col = Method,
                                   linetype = Type)) +
  geom_line(size = 2) +
  labs(x = "Date",
       y = "Active Infection Rate Estimate") +
  theme_classic() + 
  theme(text = element_text(size=25)) +
  scale_color_manual(values=rep(my_palette,2)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey")
  )

dev.off()

