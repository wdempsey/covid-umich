## Age vs IFR
## Using this to construct IFR per age block

age = c(9, 33, 55,65,75,85)
ifr = c(0.002, 0.05, 0.4,1.4,4.6,15)
par(mar = c(4,4,0.1,1)+0.1)
png(filename = "../figs/logifr.png",
    width = 960, height = 480, units = "px", pointsize = 25)
plot(age,log(ifr), bty = "n", xlab = "Age", ylab = "log(IFR)", xlim = c(0,80), ylim = c(-8,2))
seq_age = seq(2,80,1)
seq_logifr = predict(lm(log(ifr) ~ age), newdata=data.frame(age = seq_age))
lines(x = seq_age, y = seq_logifr)
dev.off()

## Suggests a linear fit on log-scale 
fit <- lm(log(ifr) ~ age)


## Age Ranges
lower_age = c(0,40,50,60,70, 80)
upper_age = c(39,49,59,69,79,90)
mean_ifr = vector(length = length(lower_age))
offset = 0.18 # To match the IFR estimate from https://www.pnas.org/content/pnas/118/31/e2103272118.full.pdf

for (i in 1:length(lower_age)) {
  log_ifr = predict(fit, newdata = data.frame(age = seq(lower_age[i], upper_age[i], length.out = 100)) ) - offset
  pred_ifr = exp(log_ifr)
  mean_ifr[i] = mean(pred_ifr)
}

print(mean_ifr)

saveRDS(mean_ifr, "../data/mean_ifr.RDS")

## https://www.infoplease.com/us/census/indiana/demographic-statistics
## Pop breakdown
pop = c(7+7.3+7.3+7.5+7+13.7+15.8/2, 
        15.8/2+13.4/2, 13.4/2 + 4.8, 
        3.9+6.5/2, 6.5/2 + 4.4/2, 4.4/2+1.5 ) 
pop = pop/sum(pop)

marginal_ifr = sum(mean_ifr * pop)  # should match marginal IFR of Raftery.




### OVERESTIMATE by 10%
for (i in 1:length(lower_age)) {
  log_ifr = predict(fit, newdata = data.frame(age = seq(lower_age[i], upper_age[i], length.out = 100)) ) - offset + log(1.1)
  pred_ifr = exp(log_ifr)
  mean_ifr[i] = mean(pred_ifr)
}

sum(mean_ifr * pop)
sum(mean_ifr * pop) / marginal_ifr

saveRDS(mean_ifr, "../data/mean_ifr_upper.RDS")


### UNDERESTIMATE by 10%
for (i in 1:length(lower_age)) {
  log_ifr = predict(fit, newdata = data.frame(age = seq(lower_age[i], upper_age[i], length.out = 100)) ) - offset + log(0.9)
  pred_ifr = exp(log_ifr)
  mean_ifr[i] = mean(pred_ifr)
}

sum(mean_ifr * pop)  
sum(mean_ifr * pop) / marginal_ifr
saveRDS(mean_ifr, "../data/mean_ifr_lower.RDS")



