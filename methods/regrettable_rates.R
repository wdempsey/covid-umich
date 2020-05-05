
bary1 = 0.05
bary2 = 0.02
rho = 0.01; f = 0.001
bary2/bary1

bary1 - bary2 - (1-bary1)*(1-bary2) * rho * sqrt((1-f)/f)


f = 0.1
bary = seq(0.02, 0.14, by = 0.01)
M = 1.5
f0 = f / (M*bary +(1-bary))
f1 = M*f0
Delta = f1-f0

rho = Delta * sqrt( bary*(1-bary)/ (f *(1-f)))

f/(1-f) * 1/(rho^2)

M = 2
FN = 0.3
FP = 0.01
rho = 0.05

temp = rho*sqrt(bary*(1-bary)) * (1 - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary)) + sqrt(f/(1-f))*(FP-(FP+FN)*bary)

temp2 = rho*sqrt(bary*(1-bary)) * (1 + FP + FN - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary))

(temp*sqrt((1-f)/f) /   (rho*sqrt((1-f)/f)*sqrt(bary*(1-bary))))^2

(temp2*sqrt((1-f)/f) /   (rho*sqrt((1-f)/f)*sqrt(bary*(1-bary))))^2


## Change f

f = 0.05
bary = seq(0.02, 0.14, by = 0.01)
M = 1.5
FN = 0.05; FP = 0.01; rho = 0.05

term1 = f/(1-f) * 1/(1 + FP + FN - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary))^2 * 1/rho^2

f = 0.1
bary = seq(0.02, 0.14, by = 0.01)
M = 1.5
FN = 0.2; FP = 0.01

term2 = f/(1-f) * 1/(1 + FP + FN - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary))^2 * 1/rho^2

f = 0.1
bary = seq(0.02, 0.14, by = 0.01)
M = 1.5
FN = 0.15; FP = 0.01

term3 = f/(1-f) * 1/(1 + FP + FN - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary))^2 * 1/rho^2

f = 0.1
bary = seq(0.02, 0.14, by = 0.01)
M = 1.5
FN = 0.3; FP = 0.05

term4 = f/(1-f) * 1/(1 + FP + FN - (M-1) * (FP * (1-bary) + FN* bary) / ( (1-bary) + M* bary) * bary / (1-bary))^2 * 1/rho^2



0.15/(1-0.15) / (0.05/(1-0.05))

term2/term1
term3/term1
term4/term1

## BIAS UNDER RATES

days = 1:365; true_max = 0.05
temp = dnorm(x = days, mean = max(days)/2, sd = max(days)/12)
true_prevalence = temp/max(temp)*true_max 
FN = 0.1; FP = 0.01
plot(days, true_prevalence)

compute_bias <- function(M, true_prevalence, FN, FP) {
  f0 = f / (M*true_prevalence +(1-true_prevalence))
  f1 = M*f0
  Delta = f1-f0
  rho = Delta * sqrt(true_prevalence*(1-true_prevalence)/(f*(1-f)))
  # D_M = 1 + FP + FN - (M-1) * (FP * (1-true_prevalence) + FN* true_prevalence) / ( (1-true_prevalence) + M* true_prevalence)
  error = D_M * rho * sqrt((1-f)/f) * sqrt((1-true_prevalence)/true_prevalence)
  lag_error = error[1:(length(error)-1)]
  cur_error = error[2:length(error)] 
  lag_bary = true_prevalence[1:(length(error)-1)]
  cur_bary = true_prevalence[2:(length(error))]
  bias = (cur_error - lag_error*(cur_error+1) )* cur_bary/lag_bary
  return(bias)
}

bias1 = compute_bias(1.5, true_prevalence, FN, FP)
bias2 = compute_bias(2, true_prevalence, FN, FP)
bias3 = compute_bias(4, true_prevalence, FN, FP)

gamma = 7

cur_bary/lag_bary

plot(cur_bary/lag_bary, type = "l", ylim = c(0.6, 1.3))
lines(days[2:length(days)], bias1+cur_bary/lag_bary, col = "red")
lines(days[2:length(days)], bias2+cur_bary/lag_bary, col = "red", lty = 2)
lines(days[2:length(days)], bias3+cur_bary/lag_bary, col = "red", lty = 3)


