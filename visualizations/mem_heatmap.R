# Construct a heatmap of the imperfect testing 
# adjustment as a function of the odds ratio and relative fraction
# for a fixed false positive and false negative rate

memadj <- function(odds_ratio, M, FP, FN) {
  ## Compute the Measurement error term D_M
  ## INPUT: Odds ration, relative fraction (M), False Positive rate (FP), and false negative rate (FN)
  ## OUPUT: Imperfect testing adjustment (D_M)
  prevalence = odds_ratio/(1+odds_ratio)
  firstterm = 1 - (M-1) * odds_ratio * (FP*(1-prevalence) + FN *(prevalence))/((1-prevalence) + M * prevalence)
  secondterm = 1/(1-FP-FN)
  return ( firstterm*secondterm )
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
  }
}

result = data.frame(result)
row.names(result) = round(seq_oddsratio,3)
colnames(result) = round(seq_M,2)

library("gplots")
png(filename = "../figs/mem_heatmap.png",
    width = 960, height = 960, units = "px", pointsize = 16)
heatmap.2(as.matrix(result), col = bluered(100), dendrogram='none',
          trace = "none", density.info = "none", Rowv=FALSE,
          Colv=FALSE, key.xlab = "Relative Sampling Frequency", key.ylab = "Odds Ratio")
dev.off()
