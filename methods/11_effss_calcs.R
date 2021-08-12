## Bring in data and libraries
age_data = read.csv("../Documents/GitHub/covid-umich/data/covid_indiana_age.csv")

library(lubridate)
library(xtable)

## Clean Data and Define inputs

age_data$DATE = date(age_data$DATE)
temp = age_data[age_data$DATE >= "2020-04-25" & age_data$DATE <= "2020-04-29",]
prevalence = 0.117
N = 6.732e6

f = sum(temp$COVID_TEST)/(N-sum(age_data$COVID_TEST[age_data$DATE < "2020-04-25"]))
FP = 0.024
FN = 0.13
M = 1.3

effss_calc <- function(prevalence, f, M, FN, FP) {
  ## Effective SS calculation
  f0 = f/((M-1) * prevalence + 1 )
  f1 = M*f0
  Delta = f1 - f0
  ratio = sqrt(prevalence*(1-prevalence) / (f*(1-f)))
  rho = Delta * ratio
  odds = prevalence/(1-prevalence)
  ratio2 = (FP*(1-prevalence) + FN * prevalence)/ (f0 *(1-prevalence) + f1 *prevalence)
  DM = (1- Delta * ratio2)/(1-FP-FN)
  effss = 1/(rho^2 * DM^2) * (f/(1-f))
  return(effss) 
}

effss1 = effss_calc(prevalence, f, M, FN, FP)

## Results in Section 3.1:

print(paste("Number of tests between 25th and 29th of April 2020:", sum(temp$COVID_TEST)))
print(paste("Indiana population:", N))
print(paste("Sampling rate:", round(f,4)))
print(paste("False positive rate:", FP))
print(paste("False negative rate:", FN))
print(paste("Relative sampling rate:", M))
print(paste("Effective sample size:", round(effss,0)))


## Even if 
f2 = 0.01 # Increased sampling rate
M2 = 1.1 # Decrease relative sampling rate
effss2 = effss_calc(prevalence, f2, M2, FN, FP) 

print(paste("Sampling rate:", round(f2,4)))
print(paste("Relative sampling rate:", M2))
print(paste("Effective sample size:", round(effss_2,0)))

FN2 = 0.20 # Increase in the FN rate
effss_newFN = effss_calc(prevalence, f2, M2, FN2, FP) 

print(paste("Effective sample size:", round(effss_newFN,0)))
print(paste("% Reduction in effective sample size:", round((effss2-effss_newFN)/effss2,3)))


## APPENDIX B.8 
seq_prevalence = seq(0.05,0.13,0.02)
seq_M = seq(1.05,1.55,0.1)
results = matrix(nrow = length(seq_prevalence), ncol = length(seq_M))
FN = 0
FP = 0
f = 0.003

for(i in 1:nrow(results)) {
  for (j in 1:ncol(results)) {
    results[i,j] = effss_calc(seq_prevalence[j], f, seq_M[i], FN, FP) 
  }
}

results


