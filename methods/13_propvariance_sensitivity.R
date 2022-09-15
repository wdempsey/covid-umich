## Here we compute the expected variance
## for each week by using the FB data distribution over X
## E[\pi (X_i,t) (1)]
weeks = c(14:53,1:5)
years = c(rep(2020, length = length(c(14:53))),rep(2021, length = length(1:5)))
results = matrix(nrow = length(weeks), ncol = 2+ncol(fb_X))

current_params = results[41,3:ncol(results)]
fb_X = model.matrix(model, fb_data)
pis = 1/(1+exp(-fb_X%*%t(current_params)))
varpi = pis*(1-pis)
weighted_varpi = sum(varpi*fb_data$weight)/sum(fb_data$weight)


for(i in 1:length(weeks)) {
  print(paste("On week", weeks[i], "in year", years[i]))
  current_week = weeks[i]
  current_year = years[i]
  current_estimated_variance = irls(current_week, current_year, fb_X, fb_data, model)  
  results[i,] = c(current_week, current_year, as.vector(current_estimated_theta))
}
