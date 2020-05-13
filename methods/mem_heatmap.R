memunadj <- function(rho, f, odds_ratio, M, FP, FN) {
  prevalence = odds_ratio/(1+odds_ratio)
  D_M = 1 - (M-1) * odds_ratio * (FP*(1-prevalence) + FN *(prevalence))/((1-prevalence) + M * prevalence)
  term1 = rho*D_M*sqrt(prevalence*(1-prevalence)) * sqrt((1-f)/f)
  term2 = FP-(FP+FN)*prevalence
  return (term1 + term2)
}

memadj <- function(odds_ratio, M, FP, FN) {
  prevalence = odds_ratio/(1+odds_ratio)
  return ( 1 + FP + FN - (M-1) * odds_ratio * (FP*(1-prevalence) + FN *(prevalence))/((1-prevalence) + M * prevalence))
}

seq_oddsratio = exp(seq(log(0.01), log(50), length.out = 100))
seq_M = exp(seq(log(0.5),log(20), length.out = 100))
result = matrix(nrow = length(seq_oddsratio), ncol = length(seq_M))
result_unadj = matrix(nrow = length(seq_oddsratio), ncol = length(seq_M))
FP = 0.005
FN = 0.172
rho = 0.01
f = 0.001


for(i in 1:length(seq_oddsratio)) {
  for (j in 1:length(seq_M)) {
    odds = seq_oddsratio[i]
    M = seq_M[j]
    result[i,j] = memadj(odds, M, FP, FN)
    result_unadj[i,j] = memunadj(rho, f, odds, M, FP, FN)
  }
}

result = data.frame(result)
row.names(result) = round(seq_oddsratio,3)
colnames(result) = round(seq_M,2)

result_unadj = data.frame(result_unadj)
row.names(result_unadj) = round(seq_oddsratio,3)
colnames(result_unadj) = round(seq_M,2)

library("gplots")
heatmap.2(as.matrix(result), col = bluered(100), dendrogram='none',
          trace = "none", density.info = "none", Rowv=FALSE,
          Colv=FALSE, key.xlab = "Relative Sampling Frequency", key.ylab = "Odds Ratio")

heatmap.2(as.matrix(result_unadj), col = bluered(100), dendrogram='none',
          trace = "none", density.info = "none", Rowv=FALSE,
          Colv=FALSE, key.xlab = "Relative Sampling Frequency", key.ylab = "Odds Ratio")
