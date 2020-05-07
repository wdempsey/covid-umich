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
png(filename = "C:/Users/wdem/Documents/GitHub/covid-umich/methods/figs/sir.png",
    width = 480, height = 480, units = "px", pointsize = 12)
par(mar = c(1.5,2,1,1)+0.1)
plot(times, out1$I, type = "l", axes = FALSE, bty = "n")
lines(times, out2$I, col = "red")
axis(side = 1, labels = FALSE)
axis(side = 2, labels = FALSE)
mtext("Fraction Infected", side = 2, line = 1, cex = 0.75)
dev.off()

## Setup for bias plots
f = 0.02
M = 4.75
days = 1:365
bary1 = -diff(out1$S)
bary2 = -diff(out2$S)

bias_fn <- function(M, bary, f, FP, FN) {
  const = (bary*M + (1-bary))
  f0 = f/const
  f1 = M*f0
  Delta = f1 - f0
  rho = Delta*sqrt(bary*(1-bary))/sqrt(f*(1-f))
  error = rho*sqrt((1-f)/f) * sqrt(1-bary)/sqrt(bary)
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

bias1 = bias_fn(M, bary1, f)
bias2 = bias_fn(M, bary2, f)

ymax = max(bias1$r+bias1$bias, bias1$r, bias2$r+bias2$bias, bias2$r)
ymin = min(bias1$r+bias1$bias, bias1$r, bias2$r+bias2$bias, bias2$r)
plot(bias1$r, type = "l", ylim = c(ymin, ymax))
lines(bias1$r+bias1$bias, lty = 2)
lines(bias2$r, col = "red")
lines(bias2$r+bias2$bias, col = "red", lty = 2)


ymax = max(bias1$Rt+bias1$logbias, bias1$Rt, bias2$Rt+bias2$logbias, bias2$Rt, na.rm = TRUE)
ymin = min(bias1$Rt+bias1$logbias, bias1$Rt, bias2$Rt+bias2$logbias, bias2$Rt, na.rm = TRUE)
png(filename = "C:/Users/wdem/Documents/GitHub/covid-umich/methods/figs/sir_rt.png",
    width = 480, height = 480, units = "px", pointsize = 12)
par(mar = c(1.5,3,1,1)+0.1)
plot(bias1$Rt, type = "l", ylim = c(ymin, ymax), axes = FALSE, bty = "n")
lines(bias1$Rt + bias1$logbias, lty = 2)
abline(h = 1.0, col = "grey", lty = 2)
lines(bias2$Rt, lty = 1, col = "red")
lines(bias2$Rt+bias2$logbias, lty = 2, col = "red")
axis(side =1, labels = FALSE)
axis(side =2, cex.axis = 0.75)
mtext(expression(R[t]), side =2, line = 2, cex = 0.75)
dev.off()