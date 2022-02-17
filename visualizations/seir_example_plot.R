## Potential bias due to self-selection and measurement error 
## from a deterministic SIR mode

## Load deSolve package
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dE <-  beta * S * I - sigma * E
    dI <-  sigma * E - gamma * I
    dR <-              gamma * I
    
    return(list(c(dS, dE, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Exposed 0.000001, Infected 0, Recovered 0
init       <- c(S = 1-1e-6, E = 1e-6, I = 0.0, R = 0.0)
## beta: infection parameter; gamma: recovery parameter
# parameters <- c(beta = 1.4247, gamma = 0.14286)
parameters1 <- c(beta = 1.2, sigma = 0.3, gamma = 0.15)
parameters2 <- c(beta = 0.9, sigma = 0.3, gamma = 0.15)
## Time frame
gap = 1
times      <- seq(0, 100, by = gap)

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
png(filename = "../figs/seir.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(3,3,1,1)+0.1)
plot(times, out1$I, type = "l", axes = FALSE, bty = "n")
lines(times, out2$I, col = "grey50")
axis(side = 1, labels = FALSE)
axis(side = 2, labels = FALSE)
mtext("Fraction Infected", side = 2, line = 1, cex = 1.5)
mtext("Time", side = 1, line = 1, cex = 1.5)
dev.off()

## Setup for bias plots
f = 0.02
M = 4 
days = 1:365
FP = 0.024; FN = 0.30
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
  D_M2 = 1/(1-FP-FN)
  D_M = D_M1 * D_M2
  rho = Delta*sqrt(bary*(1-bary))/sqrt(f*(1-f))
  error = rho*sqrt((1-f)/f) * sqrt(1-bary)/sqrt(bary)*D_M
  lag_error = error[1:(length(error)-1)]
  cur_error = error[2:(length(error))]
  
  lin_term = cur_error - lag_error
  quad_term = lag_error^2 - cur_error*lag_error
  
  cur_bary = bary[2:length(error)]
  lag_bary = bary[1:(length(error)-1)]
  
  cori = vector(length = length(bary)-1)
  for(i in 2:length(bary)) {
    temp_cur_bary = bary[i]
    prior_bary = bary[1:(i-1)]
    prior_times = 1:(i-1)
    weights = dgamma((i-1):1, shape = 4)
    weights = weights/sum(weights)
    denominator = sum(prior_bary*weights)
    cori[i-1] = temp_cur_bary/denominator
  }
  plot(cori)
  bias = (lin_term+quad_term) * cur_bary/lag_bary
  
  Rt = 1+log(cur_bary/lag_bary)* (1/7 + 1/3) / gap
  logbias = log(1+lin_term+quad_term) * (1/7 + 1/3) / gap
  
  return(list("r"=cur_bary/lag_bary, "bias" = bias,
              "Rt"=Rt, "logbias"=logbias))
}

bias1_all = bias_fn(M, bary1_all, f)
bias1_new = bias_fn(M, bary1_new, f)
bias2_all = bias_fn(M, bary2_all, f)
bias2_new = bias_fn(M, bary2_new, f)

## Ratio Comparison: Use New for Fraction of New Cases
ymax = max(bias1_new$Rt+bias1_new$logbias, bias1_new$Rt, bias2_new$Rt+bias2_new$logbias, bias2_new$Rt, na.rm = TRUE)
ymin = min(bias1_new$Rt+bias1_new$logbias, bias1_new$Rt, bias2_new$Rt+bias2_new$logbias, bias2_new$Rt, na.rm = TRUE)
good_obs = -c(1:2) # REMOVE A FEW OBS EARLY ON DUE TO STABILITY OF CALC ISSUE
png(filename = "../figs/seir_rt_comparison.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(3,4,1,1)+0.1)
plot(bias1_new$Rt[good_obs], type = "l", ylim = c(0.5, 1.3), axes = FALSE, bty = "n", xlab = "", ylab = "")
lines((bias1_new$Rt + bias1_new$logbias)[good_obs], lty = 2)
abline(h = 1.0, col = "grey", lty = 2)
lines(bias2_new$Rt[good_obs], lty = 1, col = "grey50")
lines((bias2_new$Rt+bias2_new$logbias)[good_obs], lty = 2, col = "grey50")
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 0.75, at = seq(0.6,1.3,0.1))
mtext(expression(R[t]), side = 2, line = 2, cex = 1.5)
mtext("Time", side = 1, line = 1, cex = 1.5)
dev.off()

## Same curve, just different M value
M1 = 2; M2 = 3; M3 = 4
bias1 = bias_fn(M1, bary2_all, f, FP, FN)
bias2 = bias_fn(M2, bary2_all, f, FP, FN)
bias3 = bias_fn(M3, bary2_all, f, FP, FN)

## Ratio for different M: Use All for Prevalence
ymax = max((bias3$r+bias3$bias)[-1])
ymin = min((bias3$r+bias3$bias)[-1])
good_obs = -c(1:3)
png(filename = "../figs/seir_ratio.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(3,4,1,1)+0.1)
plot((bias1$r)[good_obs], type = "l", ylim = c(ymin, ymax), axes = FALSE, bty = "n", xlab = "", ylab = "")
lines((bias1$r+bias1$bias)[good_obs], lty = 2)
lines((bias2$r+bias2$bias)[good_obs], col = "grey50", lty = 2, lwd = 2)
lines((bias3$r+bias3$bias)[good_obs], col = "grey50", lty = 3, lwd = 2)
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 1)
mtext(expression(Y[t]/Y[t-1]), side =2, line = 2, cex = 1.5)
mtext("Time", side = 1, line = 1, cex = 1.5)
legend(35, 4.0, legend = c("M=1", "M=2", "M=3", "M=4"), lty = c(1,2,2,3), col = c("black", "black", "grey50", "grey50"), cex = 1.5, bty = "n", lwd = c(1,1,2,2))
dev.off()

## Recompute bias for new only
bias1 = bias_fn(M1, bary2_new, f, FP, FN)
bias2 = bias_fn(M2, bary2_new, f, FP, FN)
bias3 = bias_fn(M3, bary2_new, f, FP, FN)


good_obs = -c(1:3)
ymax = max((bias3$Rt+bias3$logbias)[good_obs])
ymin = min((bias3$Rt+bias3$logbias)[good_obs])
png(filename = "../figs/seir_rt.png",
    width = 6.5, height = 4, units = "in", res = 1200, pointsize = 12)
par(mar = c(3,4,1,1)+0.1)
plot(bias1$Rt[good_obs], type = "l", ylim = c(ymin, ymax), axes = FALSE, bty = "n", ylab = "")
lines((bias1$Rt + bias1$logbias)[good_obs], lty = 2)
abline(h = 1.0, col = "grey", lty = 2)
lines((bias2$Rt+bias2$logbias)[good_obs], lty = 2, col = "grey50", lwd = 2)
lines((bias3$Rt+bias3$logbias)[good_obs], lty = 3, col = "grey50", lwd = 2)
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 1)
mtext(expression(R[t]), side =2, line = 2, cex = 1.5)
mtext("Time", side = 1, line = 1, cex = 1.5)
legend(55, 1.3, legend = c("M=1", "M=2", "M=3", "M=4"), lty = c(1,2,2,3), col = c("black", "black", "grey50", "grey50"), cex = 1.5, bty = "n", lwd = c(1,1,2,2))
dev.off()
