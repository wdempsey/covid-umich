indiana_data = readRDS("../data/weeklycoviddata_withsympcontact.RDS")
library(lubridate)
indiana_data$week = week(indiana_data$startdate)
indiana_data$year = year(indiana_data$startdate)
indiana_data$covid_counts_round = round(indiana_data$covid_counts)
indiana_data$covid_tests_round = round(indiana_data$covid_tests)
indiana_data$covid_negtests_round = indiana_data$covid_tests_round - indiana_data$covid_counts_round

calculate_RMSE <- function(model, indiana_data) {
  weeks = c(14:53,1:5)
  years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
  results = matrix(nrow = length(weeks), ncol = 3)
  
  for(i in 1:length(weeks)) {
    print(paste("On week", weeks[i], "in year", years[i]))
    current_week = weeks[i]
    current_year = years[i]
    current_indiana_data = subset(indiana_data, week == current_week & year == current_year)
    current_indiana_X = model.matrix(model, current_indiana_data)
    success = current_indiana_data$covid_counts_round
    failure = current_indiana_data$covid_negtests_round
    Q_fit = glm( cbind(success,failure) ~ current_indiana_X-1, family = "binomial")$fitted.values
    RMSE = sum((Q_fit - 1)^2 * success + (Q_fit-0)^2 * failure)
    results[i,] = c(current_week, current_year, RMSE)
  }
  
  results
}

## Aggregate
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race + as.factor(age)
agg_rmse = calculate_RMSE(model, indiana_data)

## No Age
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + race
rmse = calculate_RMSE(model, indiana_data)
r_sq_noage = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Race
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + ethnicity + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_norace = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Ethnicity
model = ~ -1+as.factor(fever) + as.factor(contact) + as.factor(gender) + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_noethnicity = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Gender
model = ~ -1+as.factor(fever) + as.factor(contact) + ethnicity + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_nogender = (rmse[,3] - agg_rmse[,3])/rmse[,3]

## No Symptoms
model = ~ -1 + as.factor(gender) + ethnicity + race + as.factor(age)
rmse = calculate_RMSE(model, indiana_data)
r_sq_nosymptoms = (rmse[,3] - agg_rmse[,3])/rmse[,3]


## GGPLOT OF ETHNICITY, RACE, SYMPTOMS
library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(rstan)

my_palette <- brewer.pal(name="Greys",n=9)[seq(3,9,3)]

rsq_plot_df = data.frame(rmse)
rsq_plot_df$X3 = (rmse[,3] - agg_rmse[,3])/rmse[,3]
names(rsq_plot_df) = c("week", "year", "rsq_t")
rsq_plot_df$Method = "No Symptoms"

rsq_plot_df$date = MMWRweek::MMWRweek2Date(MMWRyear = rsq_plot_df$year,
                                             MMWRweek = rsq_plot_df$week,
                                             MMWRday = 1)

rsq_plot_df = rbind(rsq_plot_df,rsq_plot_df, rsq_plot_df)

rsq_plot_df[(nrow(rmse)+1):(2*nrow(rmse)),]$rsq_t = r_sq_norace
rsq_plot_df[(nrow(rmse)+1):(2*nrow(rmse)),]$Method = rep("No Race", nrow(rmse))

rsq_plot_df[(2*nrow(rmse)+1):(3*nrow(rmse)),]$rsq_t = r_sq_noethnicity
rsq_plot_df[(2*nrow(rmse)+1):(3*nrow(rmse)),]$Method = rep("No Ethnicity", nrow(rmse))
subset_rsq_plot_df = subset(rsq_plot_df, rsq_t >= 0)

png(filename = "../figs/rsq_sensitivity_fig.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = subset_rsq_plot_df, aes(x = date, y = rsq_t, group = Method)) +
  ylim(0,0.125)+ geom_point(size = 5, aes(color = Method, shape = Method)) +
  labs(x = "Date",
       y = expression(R^2)) + 
  # theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) + theme_minimal() + 
  geom_smooth(method = "loess", aes(color = Method)) + 
  theme(text = element_text(size=20))

dev.off()

y = subset(subset_rsq_plot_df, Method == "No Symptoms" & rsq_t >= 0)$rsq_t
x = as.numeric(subset(subset_rsq_plot_df, Method == "No Symptoms" & rsq_t >= 0)$date)
fit = loess(y ~ x)
# plot(x, y)
x2 = as.numeric(subset(subset_rsq_plot_df, Method == "No Symptoms")$date)
y2 = predict(fit, x2)
# lines(x2, y2)

rsq_t_optionone = y2

y = subset(subset_rsq_plot_df, Method == "No Race" & rsq_t >= 0)$rsq_t
x = as.numeric(subset(subset_rsq_plot_df, Method == "No Race" & rsq_t >= 0)$date)
fit = loess(y ~ x)
plot(x, y)
x2 = as.numeric(subset(subset_rsq_plot_df, Method == "No Race")$date)
y2 = predict(fit, x2)
lines(x2, y2)

rsq_t_optiontwo = y2


## LOG PREDICTION DUE TO BOUNDARY ISSUE!
y = subset(subset_rsq_plot_df, Method == "No Ethnicity" & rsq_t >= 0)$rsq_t
x = as.numeric(subset(subset_rsq_plot_df, Method == "No Ethnicity" & rsq_t >= 0)$date)
fit = loess(log(y) ~ x)
plot(x, y)
x2 = as.numeric(subset(subset_rsq_plot_df, Method == "No Ethnicity")$date)
y2 = predict(fit, x2)
lines(x2, exp(y2))

rsq_t_optionthree = exp(y2)



