## IPW estimator using random and nonrandom samples
overall_test = 0.117
NR_rate = R_rate = list()
NR_test = R_test = list()
NR_rate[['sex']] = c(0.582, 0.418)
NR_rate[['age']] = c(0.394, 0.411, 0.195)
NR_rate[['race']] = c(0.231, 0.769)
NR_rate[['fever']] = c(0.170, 0.830)

NR_test[['sex']] = c(0.217, 0.242)
NR_test[['sex']] = NR_test[['sex']] * overall_test/sum(NR_rate[['sex']]*NR_test[['sex']])
NR_test[['age']] = c(0.297, 0.249, 0.067)
NR_test[['age']] = NR_test[['age']] * overall_test/sum(NR_rate[['age']]*NR_test[['age']])
NR_test[['race']] = c(0.195, 0.250)
NR_test[['race']] = NR_test[['race']] * overall_test/sum(NR_rate[['race']]*NR_test[['race']])
NR_test[['fever']] = c(0.664, 0.156)
NR_test[['fever']] = NR_test[['fever']] * overall_test/sum(NR_rate[['fever']]*NR_test[['fever']])

R_rate[['sex']] = c(0.507, 0.493) # CENSUS
R_rate[['age']] = c(0.527, 0.252, 0.220) # CENSUS
R_rate[['race']] = c(0.869, 0.131) # CENSUS
R_rate[['fever']] = c(0.018, 0.982) # RANDOM SAMPLE

R_test[['sex']] = c(0.014, 0.021) # RANDOM SAMPLE
R_test[['age']] = c(0.017, 0.021, 0.009) # RANDOM SAMPLE
R_test[['race']] = c(0.015, 0.034) # RANDOM SAMPLE
R_test[['fever']] = c(0.045, 0.013) # RANDOM SAMPLE

weights = NR_array_rate = R_array_rate = array(0,dim = c(2,3,2,2))

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        numer = R_rate[['sex']][i] * R_rate[['age']][j] * R_rate[['race']][k] * R_rate[['fever']][l] 
        denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l] 
        weights[i,j,k,l] = numer/denom
        R_array_rate[i,j,k,l] = numer
        NR_array_rate[i,j,k,l] = denom
      }
    }
  }
}

weights = weights/sum(weights)

## GET THE ARRAY OF TEST RATES
NR_array_test = array(0,dim=c(2,3,2,2))
NR_array_test[,,,] = overall_test
colnames = c('sex', 'age', 'race', 'fever')

for(i in 1:2) {
  hatmu = sum(NR_array_test[i,,,] * NR_array_rate[i,,,]/sum(NR_array_rate[i,,,]))
  truemu = NR_test[['sex']][i]
  NR_array_test[i,,,] = NR_array_test[i,,,] * truemu/hatmu
}
for(j in 1:3) {
  hatmu = sum(NR_array_test[,j,,] * NR_array_rate[,j,,]/sum(NR_array_rate[,j,,]))
  truemu = NR_test[['age']][j]
  NR_array_test[,j,,] = NR_array_test[,j,,] * truemu/hatmu
}
for(k in 1:2) {
  hatmu = sum(NR_array_test[,,k,] * NR_array_rate[,,k,]/sum(NR_array_rate[,,k,]))
  truemu = NR_test[['race']][k]
  NR_array_test[,,k,] = NR_array_test[,,k,] * truemu/hatmu
}
for(l in 1:2) {
  hatmu = sum(NR_array_test[,,,l] * NR_array_rate[,,,l]/sum(NR_array_rate[,,,l]))
  truemu = NR_test[['fever']][l]
  NR_array_test[,,,l] = NR_array_test[,,,l] * truemu/hatmu
}

sum(NR_array_test * weights)


## CASE COUNT DATA PLOT
library(lubridate)
library(ggplot2)

case_counts = read.csv("data/daily_covid_report.csv", header = T)
case_counts$date = ymd_hms(case_counts$DATE)

age = unique(case_counts$AGEGRP)[1]

subcase = case_counts[case_counts$AGEGRP == age,]

temp = aggregate(COVID_COUNT ~ date, subcase, FUN = sum)

temp$cumsum = cumsum(temp$COVID_COUNT)

temp$Age = age

complete_date = temp

for(age in unique(case_counts$AGEGRP)[2:8]) {

  subcase = case_counts[case_counts$AGEGRP == age,]

  temp = aggregate(COVID_COUNT ~ date, subcase, FUN = sum)

  temp$cumsum = cumsum(temp$COVID_COUNT)
  
  temp$Age = age
  
  complete_date = rbind(complete_date, temp)

}

names(complete_date)[2:4] = c("count","cumulative_count","Age")

png(filename = "./figs/indianacasecounts.png",
    width = 960, height = 480, units = "px", pointsize = 12)

ggplot(data = complete_date, aes(x = date, y = cumulative_count, col = Age)) +
  geom_line(size=1.25) +
  scale_colour_grey() + theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Date",
       y = "Total Cases",
       title = "Cumulative Case-count",
       subtitle = "Indiana 2020")

dev.off()

## Example for paper (see distribution on April, July, and October 1st)

print(complete_date$date[14])
temp_cum = subset(complete_date, complete_date$date == complete_date$date[14])
april = temp_cum$cumulative_count/sum(temp_cum$cumulative_count)

print(complete_date$date[802])
temp_cum = subset(complete_date, complete_date$date == complete_date$date[802])
july = temp_cum$cumulative_count/sum(temp_cum$cumulative_count)

print(complete_date$date[197])
temp_cum = subset(complete_date, complete_date$date == complete_date$date[197])
october = temp_cum$cumulative_count/sum(temp_cum$cumulative_count)

rbind(april, july, october)

## Death data

deaths = read.csv("data/covid_report_death_date_agegrp.csv", header = T)
deaths$date = mdy_hm(deaths$ï..date)
age = unique(deaths$agegrp)[1]
subcase = deaths[deaths$agegrp == age,]
temp = aggregate(covid_deaths ~ date, subcase, FUN = sum)
temp$cumsum = cumsum(temp$covid_deaths)
temp$Age = age
complete_date = temp

for(age in unique(deaths$agegrp)[2:8]) {
  
  subcase = deaths[deaths$agegrp == age,]
  
  temp = aggregate(covid_deaths ~ date, subcase, FUN = sum)
  
  temp$cumsum = cumsum(temp$covid_deaths)
  
  temp$Age = age
  
  complete_date = rbind(complete_date, temp)
  
}

names(complete_date)[2:4] = c("count","cumulative_count","Age")

png(filename = "./figs/indianadeaths.png",
    width = 960, height = 480, units = "px", pointsize = 12)

ggplot(data = complete_date, aes(x = date, y = cumulative_count, col = Age)) +
  geom_line(size=1.25) +
  scale_colour_grey() + theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Date",
       y = "Total Deaths",
       title = "Cumulative Death-count",
       subtitle = "Indiana 2020")

dev.off()