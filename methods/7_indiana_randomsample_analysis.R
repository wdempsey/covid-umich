## IPW estimator using random and nonrandom samples
overall_test = 0.117
NR_rate = R_rate = list()
NR_test = R_test = list()
NR_rate[['sex']] = c(0.582, 0.418)
NR_rate[['age']] = c(0.394, 0.411, 0.195)
NR_rate[['race']] = c(0.231, 0.769)
NR_rate[['fever']] = c(0.170, 0.830)
NR_rate[['household']] = c(0.108, 0.892)
NR_rate[['prior']] = c(0.061, 0.939)

NR_test[['sex']] = c(0.217, 0.242)
NR_test[['sex']] = NR_test[['sex']] * overall_test/sum(NR_rate[['sex']]*NR_test[['sex']])
NR_test[['age']] = c(0.297, 0.249, 0.067)
NR_test[['age']] = NR_test[['age']] * overall_test/sum(NR_rate[['age']]*NR_test[['age']])
NR_test[['race']] = c(0.195, 0.250)
NR_test[['race']] = NR_test[['race']] * overall_test/sum(NR_rate[['race']]*NR_test[['race']])
NR_test[['fever']] = c(0.664, 0.156)
NR_test[['fever']] = NR_test[['fever']] * overall_test/sum(NR_rate[['fever']]*NR_test[['fever']])
NR_test[['household']] = c(0.461, 0.216)
NR_test[['household']] = NR_test[['household']] * overall_test/sum(NR_rate[['household']]*NR_test[['household']])
NR_test[['prior']] = c(0.392, 0.216)
NR_test[['prior']] = NR_test[['prior']] * overall_test/sum(NR_rate[['prior']]*NR_test[['prior']])

R_rate[['sex']] = c(0.507, 0.493) # CENSUS
R_rate[['age']] = c(0.527, 0.252, 0.220) # CENSUS
R_rate[['race']] = c(0.869, 0.131) # CENSUS
R_rate[['fever']] = c(0.018, 0.982) # RANDOM SAMPLE
R_rate[['household']] = c(0.014, 0.986) # RANDOM SAMPLE
R_rate[['prior']] = c(0.014, 0.986) # RANDOM SAMPLE

R_test[['sex']] = c(0.014, 0.021) # RANDOM SAMPLE
R_test[['age']] = c(0.017, 0.021, 0.009) # RANDOM SAMPLE
R_test[['race']] = c(0.015, 0.034) # RANDOM SAMPLE
R_test[['fever']] = c(0.045, 0.013) # RANDOM SAMPLE
R_test[['household']] = c(0.294, 0.013) # RANDOM SAMPLE
R_test[['prior']] = c(0.244, 0.013) # RANDOM SAMPLE

weights = NR_array_rate = R_array_rate = array(0,dim = c(2,3,2,2,2,2))

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        for (m in 1:2) {
          for (n in 1:2) {
            numer = R_rate[['sex']][i] * R_rate[['age']][j] * R_rate[['race']][k] * R_rate[['fever']][l] * R_rate[['household']][m] * R_rate[['prior']][n]  
            denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l] * NR_rate[['household']][m] * NR_rate[['prior']][n] 
            weights[i,j,k,l,m,n] = numer/denom
            R_array_rate[i,j,k,l,m,n] = numer
            NR_array_rate[i,j,k,l,m,n] = denom
          }
        }
      }
    }
  }
}

weights = weights/sum(weights)

## GET THE ARRAY OF TEST RATES
NR_array_test = array(0,dim=c(2,3,2,2,2,2))
NR_array_test[,,,,,] = overall_test
colnames = c('sex', 'age', 'race', 'fever')

for(i in 1:2) {
  hatmu = sum(NR_array_test[i,,,,,] * NR_array_rate[i,,,,,]/sum(NR_array_rate[i,,,,,]))
  truemu = NR_test[['sex']][i]
  NR_array_test[i,,,,,] = NR_array_test[i,,,,,] * truemu/hatmu
}
for(j in 1:3) {
  hatmu = sum(NR_array_test[,j,,,,] * NR_array_rate[,j,,,,]/sum(NR_array_rate[,j,,,,]))
  truemu = NR_test[['age']][j]
  NR_array_test[,j,,,,] = NR_array_test[,j,,,,] * truemu/hatmu
}
for(k in 1:2) {
  hatmu = sum(NR_array_test[,,k,,,] * NR_array_rate[,,k,,,]/sum(NR_array_rate[,,k,,,]))
  truemu = NR_test[['race']][k]
  NR_array_test[,,k,,,] = NR_array_test[,,k,,,] * truemu/hatmu
}
for(l in 1:2) {
  hatmu = sum(NR_array_test[,,,l,,] * NR_array_rate[,,,l,,]/sum(NR_array_rate[,,,l,,]))
  truemu = NR_test[['fever']][l]
  NR_array_test[,,,l,,] = NR_array_test[,,,l,,] * truemu/hatmu
}
for(m in 1:2) {
  hatmu = sum(NR_array_test[,,,,m,] * NR_array_rate[,,,,m,]/sum(NR_array_rate[,,,,m,]))
  truemu = NR_test[['household']][m]
  NR_array_test[,,,,m,] = NR_array_test[,,,,m,] * truemu/hatmu
}
for(n in 1:2) {
  hatmu = sum(NR_array_test[,,,,,n] * NR_array_rate[,,,,,n]/sum(NR_array_rate[,,,,,n]))
  truemu = NR_test[['prior']][n]
  NR_array_test[,,,,,n] = NR_array_test[,,,,,n] * truemu/hatmu
}

haty_nomem = sum(NR_array_test * weights)

FP = 0.05
FN = 0.30

haty = (haty_nomem - FP)/(1-FP-FN)

### Alternative using FB numbers

fb_agg_weights = readRDS("data/fb_agg_weights.RDS")
fb_agg_weights$altfever = (fb_agg_weights$fever == FALSE)*2 + (fb_agg_weights$fever == TRUE)*1

fb_weights = fb_R_array_rate = fb_NR_array_rate = array(0,dim = c(2,3,2,2))

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        numer = fb_agg_weights$frac[fb_agg_weights$gender == i & fb_agg_weights$altage == j & fb_agg_weights$altfever == l] * R_rate[['race']][k]
        denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l]
        fb_weights[i,j,k,l] = numer/denom
        fb_R_array_rate[i,j,k,l] = numer
        fb_NR_array_rate[i,j,k,l] = denom
      }
    }
  }
}

fb_weights = fb_weights/sum(fb_weights)

## GET THE ARRAY OF TEST RATES FOR FB EXAMPLE
fb_NR_array_test = array(0,dim=c(2,3,2,2))
fb_NR_array_test[,,,] = overall_test
colnames = c('sex', 'age', 'race', 'fever')

for(i in 1:2) {
  hatmu = sum(fb_NR_array_test[i,,,] * fb_NR_array_rate[i,,,]/sum(fb_NR_array_rate[i,,,]))
  truemu = NR_test[['sex']][i]
  fb_NR_array_test[i,,,] = fb_NR_array_test[i,,,] * truemu/hatmu
}
for(j in 1:3) {
  hatmu = sum(fb_NR_array_test[,j,,] * fb_NR_array_rate[,j,,]/sum(fb_NR_array_rate[,j,,]))
  truemu = NR_test[['age']][j]
  fb_NR_array_test[,j,,] = fb_NR_array_test[,j,,] * truemu/hatmu
}
for(k in 1:2) {
  hatmu = sum(fb_NR_array_test[,,k,] * fb_NR_array_rate[,,k,]/sum(fb_NR_array_rate[,,k,]))
  truemu = NR_test[['race']][k]
  fb_NR_array_test[,,k,] = fb_NR_array_test[,,k,] * truemu/hatmu
}
for(l in 1:2) {
  hatmu = sum(fb_NR_array_test[,,,l] * fb_NR_array_rate[,,,l]/sum(fb_NR_array_rate[,,,l]))
  truemu = NR_test[['fever']][l]
  fb_NR_array_test[,,,l] = fb_NR_array_test[,,,l] * truemu/hatmu
}


fb_haty_nomem = sum(fb_NR_array_test * fb_weights)
 
FP = 0.05
FN = 0.30

fb_haty = (fb_haty_nomem - FP)/(1-FP-FN)


print(paste("Hat y with no MEM =", round(haty_nomem,3)))
print(paste("Hat y with MEM =", round(haty,3)))


print(paste("FB-based hat y with no MEM =", round(fb_haty_nomem,3)))
print(paste("FB-based hat y with MEM =", round(fb_haty,3)))


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