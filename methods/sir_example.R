## Load deSolve package
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
## beta: infection parameter; gamma: recovery parameter
# parameters <- c(beta = 1.4247, gamma = 0.14286)
parameters1 <- c(beta = 1.2, gamma = 0.15)
parameters2 <- c(beta = 0.9, gamma = 0.15)
## Time frame
times      <- seq(0, 50, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out1 <- ode(y = init, times = times, func = sir, parms = parameters1)
out2 <- ode(y = init, times = times, func = sir, parms = parameters2)
## change to data frame
out1 <- as.data.frame(out1)
out2 <- as.data.frame(out2)
## Delete time variable
out1$time <- NULL
out2$time <- NULL
## Show data
png(filename = "C:/Users/Balthazar/Documents/GitHub/covid-umich/methods/figs/sir.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,2,1,1)+0.1)
plot(times, out1$I, type = "l", axes = FALSE, bty = "n")
lines(times, out2$I, col = "red")
axis(side = 1, labels = FALSE)
axis(side = 2, labels = FALSE)
mtext("Fraction Infected", side = 2, line = 1, cex = 0.75)
mtext("Time", side = 1, line = 1, cex = 0.75)
dev.off()

## Setup for bias plots
f = 0.02
M = 4
days = 1:365
FP = 0.01; FN = 0.1
bary1_new = -diff(out1$S)
bary1_all = out1$I
bary2_new = -diff(out2$S)
bary2_all = out2$I

bias_fn <- function(M, bary, f, FP = 0.01, FN = 0.15) {
  const = (bary*M + (1-bary))
  f0 = f/const
  f1 = M*f0
  Delta = f1 - f0
  D_M1 = 1 - Delta * bary/(1-bary) * (FP*(1-bary) + FN * bary)/ (f0 * (1-bary) + f1 * bary)
  D_M2 = 1 - Delta * bary/(1-bary) * (FP*(1-bary) + FN * bary)/ (f0 * (1-bary) + f1 * bary)
  D_M = D_M1 * D_M2
  rho = Delta*sqrt(bary*(1-bary))/sqrt(f*(1-f))
  error = rho*sqrt((1-f)/f) * sqrt(1-bary)/sqrt(bary)*D_M
  lag_error = error[1:(length(error)-1)]
  cur_error = error[2:(length(error))]
  
  lin_term = cur_error - lag_error
  quad_term = lag_error^2 - cur_error*lag_error
  
  cur_bary = bary[2:length(error)]
  lag_bary = bary[1:(length(error)-1)]
  
  bias = (lin_term+quad_term) * cur_bary/lag_bary
  
  Rt = 1+log(cur_bary/lag_bary)/7
  logbias = log(1+lin_term+quad_term)/7
  
  return(list("r"=cur_bary/lag_bary, "bias" = bias,
              "Rt"=Rt, "logbias"=logbias))
}

bias1_all = bias_fn(M, bary1_all, f)
bias1_new = bias_fn(M, bary1_new, f)
bias2_all = bias_fn(M, bary2_all, f)
bias2_new = bias_fn(M, bary2_new, f)

## Ratio Comparison: Use All for Prevalence

ymax = max(bias1_all$r+bias1_all$bias, bias1_all$r, bias2_all$r+bias2_all$bias, bias2_all$r)
ymin = min(bias1_all$r+bias1_all$bias, bias1_all$r, bias2_all$r+bias2_all$bias, bias2_all$r)
plot(bias1_all$r, type = "l", ylim = c(ymin, ymax))
lines(bias1_all$r+bias1_all$bias, lty = 2)
lines(bias2_all$r, col = "red")
lines(bias2_all$r+bias2_all$bias, col = "red", lty = 2)

## Ratio Comparison: Use New for Fraction of New Cases
ymax = max(bias1_new$Rt+bias1_new$logbias, bias1_new$Rt, bias2_new$Rt+bias2_new$logbias, bias2_new$Rt, na.rm = TRUE)
ymin = min(bias1_new$Rt+bias1_new$logbias, bias1_new$Rt, bias2_new$Rt+bias2_new$logbias, bias2_new$Rt, na.rm = TRUE)
png(filename = "C:/Users/Balthazar/Documents/GitHub/covid-umich/methods/figs/sir_rt_comparison.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,3,0,1)+0.1)
plot(bias1_new$Rt, type = "l", ylim = c(0.7, 1.3), axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(bias1_new$Rt + bias1_new$logbias, lty = 2)
abline(h = 1.0, col = "grey", lty = 2)
lines(bias2_new$Rt, lty = 1, col = "red")
lines(bias2_new$Rt+bias2_new$logbias, lty = 2, col = "red")
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 0.75, at = seq(0.8,1.2,0.1))
mtext(expression(R[t]), side =2, line = 2, cex = 0.75)
mtext("Time", side = 1, line = 1, cex = 0.75)
dev.off()

## Same curve, just different M value
M1 = 3; M2 = 4; M3 = 5
bias1 = bias_fn(M1, bary2_all, f, FP, FN)
bias2 = bias_fn(M2, bary2_all, f, FP, FN)
bias3 = bias_fn(M3, bary2_all, f, FP, FN)

## Ratio for different M: Use All for Prevalence
ymax = max(bias3$r+bias3$bias)
ymin = min(bias3$r+bias3$bias)
png(filename = "C:/Users/Balthazar/Documents/GitHub/covid-umich/methods/figs/sir_ratio.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,3,1,1)+0.1)
plot(bias1$r, type = "l", ylim = c(ymin, ymax), axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(bias1$r+bias1$bias, lty = 2)
lines(bias2$r+bias2$bias, col = "blue", lty = 2)
lines(bias3$r+bias3$bias, col = "red", lty = 2)
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 0.75)
mtext(expression(Y[t]/Y[t-1]), side =2, line = 2, cex = 0.75)
mtext("Time", side = 1, line = 1, cex = 0.75)
legend(35, 3.0, legend = c("M=1", "M=2", "M=3", "M=4"), lty = c(1,2,2,2), col = c("black", "black", "blue", "red"), cex = 0.75, bty = "n")
dev.off()

## Recompute bias for new only
bias1 = bias_fn(M1, bary2_new, f, FP, FN)
bias2 = bias_fn(M2, bary2_new, f, FP, FN)
bias3 = bias_fn(M3, bary2_new, f, FP, FN)


ymax = max(bias3$Rt+bias3$logbias)
ymin = min(bias3$Rt+bias3$logbias)
png(filename = "C:/Users/Balthazar/Documents/GitHub/covid-umich/methods/figs/sir_rt.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(2,3,1,1)+0.1)
plot(bias1$Rt, type = "l", ylim = c(ymin, ymax), axes = FALSE, bty = "n")
lines(bias1$Rt + bias1$logbias, lty = 2)
abline(h = 1.0, col = "grey", lty = 2)
lines(bias2$Rt+bias2$logbias, lty = 2, col = "blue")
lines(bias3$Rt+bias3$logbias, lty = 2, col = "red")
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 0.75)
mtext(expression(R[t]), side =2, line = 2, cex = 0.75)
mtext("Time", side = 1, line = 1, cex = 0.75)
legend(35, 1.175, legend = c("M=1", "M=2", "M=3", "M=4"), lty = c(1,2,2,2), col = c("black", "black", "blue", "red"), cex = 0.75, bty = "n")
dev.off()
