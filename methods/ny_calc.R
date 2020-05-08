FP = 0.00; FN = 0.000
bary0 = 0.139
bary = bary0*(1+FN) - FP*(1-bary0)
f= 0.001
true_error = (0.325-bary)/sqrt(bary*(1-bary)) * sqrt(f/(1-f))

error <- function(FP, FN, bary, f, true_error) {
  delta_error <- function(Delta) {
    odds = bary / (1-bary)
    num = FP*(1-bary) + FN*bary
    f0 = f - Delta * bary
    f1 = f0 + Delta
    den = f0 *(1-bary) +  f1 * bary
    answer = Delta * sqrt(bary*(1-bary))/sqrt(f*(1-f)) * (1+FP+FN - Delta * odds * num/den )
    return(abs(answer-true_error))
  }
  return(delta_error)
}

temp = error(FP,FN,bary,f, true_error)
init = f/bary/2

output = optim(init, temp, method = "Brent", lower = 0, upper = f/bary)

f0 = f-output$par * bary
f1 = f0 + output$par
f1/f0
