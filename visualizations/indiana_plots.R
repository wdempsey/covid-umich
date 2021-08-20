## INDIANA DEATH DATA
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)

## Measures
# date_switch <- "2020-03-23" # date of introduction of control measures (empirical)
# tswitch <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number
# date_switch_two <- "2020-06-15" # date of ending of control measures
# tswitch_two <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch_two) %>% nrow() + 1 # convert time to number
# date_switch_three <- "2020-10-01" # date of ending of control measures
# tswitch_three <- subset(df_coviddeath_age, age == "80+") %>% filter(date < date_switch_three) %>% nrow() + 1 # convert time to number

df_case_counts = read.csv("../data/covid_indiana_age.csv", header = T)
names(df_case_counts) = c("date", "Age", "covid_test", "covid_count", "covid_deaths")
df_case_counts$date = ymd(df_case_counts$date)
df_case_counts = subset(df_case_counts, Age != "Unknown")
df_case_counts$Age = as.factor(df_case_counts$Age)

levels(df_case_counts$Age) = c(1,1,1,2,2,2,3,3)
levels(df_case_counts$Age) = c("0-39", "40-69", "70+")

my_palette <- brewer.pal(name="Greys",n=9)[c(5,7,9)]

png(filename = "../figs/indianacasecounts_byage.png",
    width = 960, height = 480, units = "px", pointsize = 25)

df_case_counts %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = covid_count, fill = Age), stat = "identity") +
  labs(y="COVID-19 Reported Cases", x = "Date") + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  # geom_vline(aes(xintercept = date(date_switch)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_two)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_three)), size = 1.5, linetype="dotted") +
  scale_fill_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()

png(filename = "../figs/indianacovidtests_byage.png",
    width = 960, height = 480, units = "px", pointsize = 25)

df_case_counts %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = covid_test, fill = Age), stat = "identity") +
  labs(y="COVID-19 Reported Tests", x = "Date") + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  # geom_vline(aes(xintercept = date(date_switch)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_two)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_three)), size = 1.5, linetype="dotted") +
  scale_fill_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()

png(filename = "../figs/indianadeaths_byage.png",
    width = 960, height = 480, units = "px", pointsize = 12)

df_case_counts %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = covid_deaths, fill = Age), stat = "identity") +
  labs(y="COVID-19 Reported Tests", x = "Date") + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  # geom_vline(aes(xintercept = date(date_switch)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_two)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_three)), size = 1.5, linetype="dotted") +
  scale_fill_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()
