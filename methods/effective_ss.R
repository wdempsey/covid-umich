bary = 0.091
f_current = 0.026
f_proposal = 1/7
M = 1.5
FP = 0.005; FN = 0.172

eff_ss <- function(bary, f, M, FP=0.0, FN=0.0) {
  f0 = f/(bary*(M-1)+1)
  f1 = M*f0
  Delta = f1 - f0  
  rho = Delta*sqrt(bary*(1-bary))/sqrt(f*(1-f))
  num = FP*(1-bary) + FN*bary
  den = f0*(1-bary) + f1*bary
  D_m = 1+FP+FN - Delta * bary/(1-bary) * num/den
  neff = f/(1-f) * 1/(rho^2 * D_m^2)
  return(neff)
}  

f = 0.1
seq_bary = seq(0.016, 0.096, 0.02)
seq_M = seq(1.2, 2, 0.2)
result = matrix(nrow = length(seq_bary), ncol = length(seq_M))

for(i in 1:length(seq_bary)) {
  for(j in 1:length(seq_M)) {
    result[i,j] = eff_ss(seq_bary[i], f_current, seq_M[j], 0, 0)    
  }
}

library("xtable")
colnames(result) = seq_M
rownames(result) = seq_bary
xtable(round(result,0))

result2 = matrix(nrow = length(seq_bary), ncol = length(seq_M))

for(i in 1:length(seq_bary)) {
  for(j in 1:length(seq_M)) {
    result2[i,j] = eff_ss(seq_bary[i], f_current, seq_M[j], FP, FN)    
  }
}

colnames(result2) = seq_M
rownames(result2) = seq_bary
xtable(round(result2,0))


rel_mse <- function(bary, f, M, FP=0.0, FN=0.0) {
  f0 = f/(bary*(M-1)+1)
  f1 = M*f0
  Delta = f1 - f0  
  rho = Delta*sqrt(bary*(1-bary))/sqrt(f*(1-f))
  num = FP*(1-bary) + FN*bary
  den = f0*(1-bary) + f1*bary
  D_m = 1 - Delta * bary/(1-bary) * num/den
  term1 = D_m * rho * sqrt(bary*(1-bary)) * sqrt(1-f)/sqrt(f)
  term2 = FP - (FP+FN) * bary
  mse_mem = (term1 + term2)^2
  mse = (rho * sqrt(bary*(1-bary)) * sqrt(1-f)/sqrt(f))^2
  return(mse_mem/mse)
}  


basef = 0.05; M = 1.5; FN_base = 0.05; FP_base = 0.005
f0 = f/(bary*(M-1)+1)
f1 = M*f0
Delta = f1 - f0  
baserho = Delta*sqrt(bary*(1-bary))/sqrt(basef*(1-basef))
num = FP_base*(1-bary) + FN_base*bary
den = f0*(1-bary) + f1*bary
base_D_m = 1+FP_base+FN_base - Delta * bary/(1-bary) * num/den


base_mse = baserho^2 * (1-f)/f * bary*(1-bary)

newf = 0.10; FN_new = 0.30; FP_new = 0.10
newrho = Delta*sqrt(bary*(1-bary))/sqrt(newf*(1-newf))
num = FP_new*(1-bary) + FN_new*bary
new_D_m = 1+FP_new+FN_new - Delta * bary/(1-bary) * num/den

new_mse = (newrho * sqrt((1-newf)/newf) * sqrt(bary*(1-bary)))^2

1-new_mse/base_mse

1-new_mse * new_D_m^2 / (base_mse * base_D_m^2)


(newf/(1-newf) * 1/(newrho^2 * new_D_m^2))/(basef/(1-basef) * 1/(baserho^2 * base_D_m^2))

(newf/(1-newf) * 1/(newrho^2))/(basef/(1-basef) * 1/(baserho^2 ))

