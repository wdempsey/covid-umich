## INDIANA DEATH DATA
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(ggplot2)
library(rstan)
library(lubridate)

df_case_counts = read.csv("../data/covid_indiana_age.csv", header = T)
names(df_case_counts) = c("date", "Age", "covid_test", "covid_count", "covid_deaths")
df_case_counts$date = ymd(df_case_counts$date)
df_case_counts = subset(df_case_counts, Age != "Unknown")
df_case_counts$Age = as.factor(df_case_counts$Age)

levels(df_case_counts$Age) = c(1,1,1,2,2,2,3,3)
levels(df_case_counts$Age) = c("0-39", "40-69", "70+")

df_actual_totals <- read.csv("../data/covid_indiana_all_testing_data.csv", header = T)
df_actual_totals$date = mdy(df_actual_totals$DATE)

unique_dates = unique(df_case_counts$date)

for(dates in unique_dates) {
  temp_tests = df_case_counts$covid_test[df_case_counts$date == dates]
  actual_total = df_actual_totals$COVID_TEST[df_actual_totals$date == dates]
  if(is.na(actual_total)) {
    updated_tests = rep(NA, length(temp_tests)) 
  } else{
    updated_tests = temp_tests/sum(temp_tests)*actual_total
  }
  df_case_counts$covid_test[df_case_counts$date == dates] = updated_tests
}

my_palette <- brewer.pal(name="Greys",n=9)[c(5,7,9)]

png(filename = "../figs/indianacasecounts_byage.png",
    width = 960, height = 480, units = "px", pointsize = 25)

df_case_counts %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = covid_count, fill = Age), stat = "identity") +
  labs(y="COVID-19 Reported Cases", x = "Date") + 
  scale_x_date(date_breaks = "3 month" , date_labels = "%b-%y") +
  # geom_vline(aes(xintercept = date(date_switch)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_two)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_three)), size = 1.5, linetype="dotted") +
  scale_fill_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()

subset_df_case_counts = subset(df_case_counts, !is.na(covid_test)) # Remove NA dates 

png(filename = "../figs/indianacovidtests_byage.png",
    width = 960, height = 480, units = "px", pointsize = 25)

subset_df_case_counts %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = date, y = covid_test, fill = Age), stat = "identity") +
  labs(y="COVID-19 Reported Tests", x = "Date") + 
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y") +
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
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y") +
  # geom_vline(aes(xintercept = date(date_switch)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_two)), size = 1.5, linetype="dotted") + 
  # geom_vline(aes(xintercept = date(date_switch_three)), size = 1.5, linetype="dotted") +
  scale_fill_manual(values = my_palette) +
  theme(text = element_text(size=25))

dev.off()
