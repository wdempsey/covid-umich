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
FP = 0.024
FN = 0.13
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
row.names(result) = round(seq_oddsratio,4)
colnames(result) = round(seq_M,4)

png(filename = "../figs/mem_heatmap.png",
    width = 960, height = 640, units = "px", pointsize = 16)
par(mar = c(4,4,1,1)+0.1)
contour( z = as.matrix(result),
         y = log(seq_oddsratio),
         x = seq_M, nlevels = 30,
         xlab = "Relative Sampling Frequency (M)",
         ylab = "Log Odds Ratio", bty = "n",
         labcex = 0.9)
axis(side =1, cex = 1.5); axis(side = 2, cex = 1.5)
dev.off()
