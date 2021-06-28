## Age vs IFR
## Using this to construct IFR per age blocl

age = c(9, 33, 55,65,75,85)
ifr = c(0.002, 0.05, 0.4,1.4,4.6,15)
plot(x,log(y))
abline(lm(log(y) ~ x))

## Suggests a linear fit on log-scale 
fit <- lm(log(ifr) ~ age)


## Age Ranges
lower_age = c(0,20,30,40,50,60,70, 80)
upper_age = c(19,29,39,49,59,69,79,90)
mean_ifr = vector(length = length(lower_age))

for (i in 1:length(lower_age)) {
  log_ifr = predict(fit, newdata = data.frame(age = seq(lower_age[i], upper_age[i], length.out = 100)) )
  pred_ifr = exp(log_ifr)
  mean_ifr[i] = mean(pred_ifr)
}

print(mean_ifr)
