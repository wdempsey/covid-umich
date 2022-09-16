## Read in data
fb_data = readRDS("../data/fb_weeklycomplete.RDS")
names(fb_data)[3] = "fever"
fb_data$contact = as.logical(fb_data$contact)

calculate_varpis <- function(propensities, model, fb_data) {
  weeks = c(14:53,1:5)
  years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    # print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_params = propensities[which(weeks == current_week & years == current_year) ,3:ncol(propensities)]
    current_fb_data = subset(fb_data, week == current_week & year == current_year)
    current_fb_X = model.matrix(model, current_fb_data)
    pis = 1/(1+exp(-current_fb_X%*%t(current_params)))
    varpi = pis*(1-pis)
    weighted_varpi = sum(varpi*current_fb_data$weight)/sum(current_fb_data$weight)
    results[i,] = c(current_week, current_year, weighted_varpi)
  }
  return(results)
}

# FIT EACH PROPENSITY AND CALCULATE THE VARIANCE!

## Aggregate
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_082621.RDS")
agg_results = calculate_varpis(propensities, model, fb_data)

## No Age
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
propensities = readRDS("../data/smoothedpropensities_noage_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_noage = 1-agg_results[,3]/results[,3]

## No Race
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_norace_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_norace = 1-agg_results[,3]/results[,3]

## No Ethnicity
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_noethnicity_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_noethnicity = 1-agg_results[,3]/results[,3]

## No Gender
model = ~ -1+as.factor(fever) + as.factor(contact) + ethnicity + race + as.factor(age)
propensities = readRDS("../data/smoothedpropensities_nogender_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_nogender = 1-agg_results[,3]/results[,3]

## No Symptoms
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
propensities = readRDS("../data/smoothedpropensities_nosymptoms_091521.RDS")
results = calculate_varpis(propensities, model, fb_data)
alpha_t_nosymptoms = 1-agg_results[,3]/results[,3]

