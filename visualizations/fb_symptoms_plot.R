## Plots of Symptoms over Time as Report in FB data
## Using Nonlinear smoother

library(lubridate)
library(ggplot2)
library(np)

indiana_data = read.csv("../data/fb_indiana_data.csv")

indiana_data$symptoms = (indiana_data$fever & (indiana_data$cough | indiana_data$shortness | indiana_data$diffbreath))

indiana_data$weightedfever = (indiana_data$fever) * indiana_data$weight
png(filename = "../figs/fbcovid19symptoms.png",
    width = 960, height = 480, units = "px", pointsize = 12)
par(mfrow = c(1,2), mar = c(4,4,0.5,1)+0.1)

denom = aggregate(weight ~ date + gender, data = indiana_data, sum)

numer = aggregate(weightedfever ~ date + gender, data = indiana_data, sum)

tempdf = data.frame('x' = difftime(date(numer$date), "2020-01-01"), 'y' = numer$weightedfever/denom$weight, 'gender' = denom$gender)
tempdf$x = as.vector(tempdf$x)

model.np <- npreg(y~x, regtype = 'll', bwmethod = 'cv.aic', gradients = TRUE, data = subset(tempdf, gender == 2))

plot(date(numer$date[numer$gender == 2]), fitted(model.np), type = "l", 
     bty= 'n', axes = FALSE, ylab = "Reported Fever", xlab = "Date",
     ylim = c(0.0,0.030), lwd = 3)
axis.Date(1, at=seq(min(date(numer$date[numer$gender == 2])), max(date(numer$date[numer$gender == 2])), by="months"), format="%b")
axis(side = 2)

lines(date(numer$date[numer$gender == 2]), fitted(model.np)+1.96*se(model.np), lty = 2)
lines(date(numer$date[numer$gender == 2]), fitted(model.np)-1.96*se(model.np), lty = 2)

model.np <- npreg(y~x, regtype = 'll', bwmethod = 'cv.aic', gradients = TRUE, data = subset(tempdf, gender == 1))

lines(date(numer$date[numer$gender == 1]), fitted(model.np), col = "red", lwd = 3)

lines(date(numer$date[numer$gender == 1]), fitted(model.np)+1.96*se(model.np), lty = 2, col = "red")
lines(date(numer$date[numer$gender == 1]), fitted(model.np)-1.96*se(model.np), lty = 2, col = "red")

legend(date("2020-04-20"), 0.025, c("Male", "Female"), lty = c(1,1), col = c("black", "red"), cex = 0.75, bty = 'n')

## Age plot

indiana_data$blockage = is.element(indiana_data$age, c(1,2,3)) + 2* is.element(indiana_data$age, c(4,5)) + 3 * is.element(indiana_data$age, c(6,7))

denomage = aggregate(weight ~ date + blockage, data = subset(indiana_data, blockage != 0), sum)

numerage = aggregate(weightedfever ~ date + blockage, data = subset(indiana_data, blockage != 0), sum)

tempdf = data.frame('x' = difftime(date(numerage$date), "2020-01-01"), 'y' = numerage$weightedfever/denomage$weight, 'age' = denomage$blo)
tempdf$x = as.vector(tempdf$x)

model.np <- npreg(y~x, regtype = 'll', bwmethod = 'cv.aic', gradients = TRUE, data = subset(tempdf, age == 1))

plot(date(numerage$date[numerage$blockage == 1]), fitted(model.np), type = "l", 
     bty= 'n', axes = FALSE, ylab = "Reported Fever", xlab = "Date",
     ylim = c(0.0,0.030), lwd = 3)
axis.Date(1, at=seq(min(date(numerage$date[numerage$blockage == 1])), max(date(numerage$date[numerage$blockage == 1])), by="months"), format="%b")
axis(side = 2)

lines(date(numerage$date[numerage$blockage == 1]), fitted(model.np)+se(model.np)*1.96, lty = 2)
lines(date(numerage$date[numerage$blockage == 1]), fitted(model.np)-se(model.np)*1.96, lty = 2)

model.np <- npreg(y~x, regtype = 'll', bwmethod = 'cv.aic', gradients = TRUE, data = subset(tempdf, age == 2))
lines(date(numerage$date[numerage$blockage == 2]), fitted(model.np), col = "red", lwd = 3)
lines(date(numerage$date[numerage$blockage == 2]), fitted(model.np)+1.96*se(model.np), col = "red", lty = 2)
lines(date(numerage$date[numerage$blockage == 2]), fitted(model.np)-1.96*se(model.np), col = "red", lty = 2)

model.np <- npreg(y~x, regtype = 'll', bwmethod = 'cv.aic', gradients = TRUE, data = subset(tempdf, age == 3))
lines(date(numerage$date[numerage$blockage == 3]), fitted(model.np), col = "blue", lwd = 3)
lines(date(numerage$date[numerage$blockage == 3]), fitted(model.np)+se(model.np)*1.96, col = "blue", lty = 2)
lines(date(numerage$date[numerage$blockage == 3]), fitted(model.np)-se(model.np)*1.96, col = "blue", lty = 2)

legend(date("2020-04-20"), 0.025, c("18-44", "45-64", "65+"), lty = c(1,1), col = c("black", "red", "blue"), cex = 0.75, bty = 'n')
dev.off()