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

for (i in 1:length(lower_age)) {
  log_ifr = predict(fit, newdata = data.frame(age = seq(lower_age[i], upper_age[i], length.out = 100)) )
  pred_ifr = exp(log_ifr)
  mean_ifr[i] = mean(pred_ifr)
}

print(mean_ifr)
