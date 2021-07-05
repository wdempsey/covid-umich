## IPW estimator using random and nonrandom samples
overall_test = 0.117
NR_rate = R_rate = list()
NR_test = R_test = list()
NR_rate[['sex']] = c(0.582, 0.418)
NR_rate[['age']] = c(0.394, 0.411, 0.195)
NR_rate[['race']] = c(0.231, 0.769)
NR_rate[['fever']] = c(0.170, 0.830)
NR_rate[['household']] = c(0.108, 0.892)
NR_rate[['prior']] = c(0.061, 0.939)

NR_test[['sex']] = c(0.217, 0.242)
NR_test[['sex']] = NR_test[['sex']] * overall_test/sum(NR_rate[['sex']]*NR_test[['sex']])
NR_test[['age']] = c(0.297, 0.249, 0.067)
NR_test[['age']] = NR_test[['age']] * overall_test/sum(NR_rate[['age']]*NR_test[['age']])
NR_test[['race']] = c(0.195, 0.250)
NR_test[['race']] = NR_test[['race']] * overall_test/sum(NR_rate[['race']]*NR_test[['race']])
NR_test[['fever']] = c(0.664, 0.156)
NR_test[['fever']] = NR_test[['fever']] * overall_test/sum(NR_rate[['fever']]*NR_test[['fever']])
NR_test[['household']] = c(0.461, 0.216)
NR_test[['household']] = NR_test[['household']] * overall_test/sum(NR_rate[['household']]*NR_test[['household']])
NR_test[['prior']] = c(0.392, 0.216)
NR_test[['prior']] = NR_test[['prior']] * overall_test/sum(NR_rate[['prior']]*NR_test[['prior']])

R_rate[['sex']] = c(0.507, 0.493) # CENSUS
R_rate[['age']] = c(0.527, 0.252, 0.221) # CENSUS
R_rate[['race']] = c(0.869, 0.131) # CENSUS
R_rate[['fever']] = c(0.018, 0.982) # RANDOM SAMPLE
R_rate[['household']] = c(0.014, 0.986) # RANDOM SAMPLE
R_rate[['prior']] = c(0.014, 0.986) # RANDOM SAMPLE

R_test[['sex']] = c(0.014, 0.021) # RANDOM SAMPLE
R_test[['age']] = c(0.017, 0.021, 0.009) # RANDOM SAMPLE
R_test[['race']] = c(0.015, 0.034) # RANDOM SAMPLE
R_test[['fever']] = c(0.045, 0.013) # RANDOM SAMPLE
R_test[['household']] = c(0.294, 0.013) # RANDOM SAMPLE
R_test[['prior']] = c(0.244, 0.013) # RANDOM SAMPLE

weights = NR_array_rate = R_array_rate = array(0,dim = c(2,3,2,2,2,2))

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        for (m in 1:2) {
          for (n in 1:2) {
            numer = R_rate[['sex']][i] * R_rate[['age']][j] * R_rate[['race']][k] * R_rate[['fever']][l] * R_rate[['household']][m] * R_rate[['prior']][n]  
            denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l] * NR_rate[['household']][m] * NR_rate[['prior']][n] 
            weights[i,j,k,l,m,n] = numer/denom
            R_array_rate[i,j,k,l,m,n] = numer
            NR_array_rate[i,j,k,l,m,n] = denom
          }
        }
      }
    }
  }
}

weights = weights/sum(weights)

## GET THE ARRAY OF TEST RATES
NR_array_test = array(0,dim=c(2,3,2,2,2,2))
NR_array_test[,,,,,] = overall_test
colnames = c('sex', 'age', 'race', 'fever')

for(i in 1:2) {
  hatmu = sum(NR_array_test[i,,,,,] * NR_array_rate[i,,,,,]/sum(NR_array_rate[i,,,,,]))
  truemu = NR_test[['sex']][i]
  NR_array_test[i,,,,,] = NR_array_test[i,,,,,] * truemu/hatmu
}
for(j in 1:3) {
  hatmu = sum(NR_array_test[,j,,,,] * NR_array_rate[,j,,,,]/sum(NR_array_rate[,j,,,,]))
  truemu = NR_test[['age']][j]
  NR_array_test[,j,,,,] = NR_array_test[,j,,,,] * truemu/hatmu
}
for(k in 1:2) {
  hatmu = sum(NR_array_test[,,k,,,] * NR_array_rate[,,k,,,]/sum(NR_array_rate[,,k,,,]))
  truemu = NR_test[['race']][k]
  NR_array_test[,,k,,,] = NR_array_test[,,k,,,] * truemu/hatmu
}
for(l in 1:2) {
  hatmu = sum(NR_array_test[,,,l,,] * NR_array_rate[,,,l,,]/sum(NR_array_rate[,,,l,,]))
  truemu = NR_test[['fever']][l]
  NR_array_test[,,,l,,] = NR_array_test[,,,l,,] * truemu/hatmu
}
for(m in 1:2) {
  hatmu = sum(NR_array_test[,,,,m,] * NR_array_rate[,,,,m,]/sum(NR_array_rate[,,,,m,]))
  truemu = NR_test[['household']][m]
  NR_array_test[,,,,m,] = NR_array_test[,,,,m,] * truemu/hatmu
}
for(n in 1:2) {
  hatmu = sum(NR_array_test[,,,,,n] * NR_array_rate[,,,,,n]/sum(NR_array_rate[,,,,,n]))
  truemu = NR_test[['prior']][n]
  NR_array_test[,,,,,n] = NR_array_test[,,,,,n] * truemu/hatmu
}

haty_nomem = sum(NR_array_test * weights)

FP = 0.03 # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7307014/
FN = 0.13 # https://www.medrxiv.org/content/10.1101/2020.04.16.20066787v2.full.pdf

haty = (haty_nomem - FP)/(1-FP-FN)

### Alternative using FB numbers

fb_agg_weights = readRDS("../data/fb_agg_weights.RDS")
fb_agg_weights$altfever = (fb_agg_weights$fever == FALSE)*2 + (fb_agg_weights$fever == TRUE)*1

fb_weights = fb_R_array_rate = fb_NR_array_rate = array(0,dim = c(2,3,2,2))

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        numer = fb_agg_weights$frac[fb_agg_weights$gender == i & fb_agg_weights$altage == j & fb_agg_weights$altfever == l] * R_rate[['race']][k]
        denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l]
        fb_weights[i,j,k,l] = numer/denom
        fb_R_array_rate[i,j,k,l] = numer
        fb_NR_array_rate[i,j,k,l] = denom
      }
    }
  }
}

fb_weights = fb_weights/sum(fb_weights)

## GET THE ARRAY OF TEST RATES FOR FB EXAMPLE
fb_NR_array_test = array(0,dim=c(2,3,2,2))
fb_NR_array_test[,,,] = overall_test
colnames = c('sex', 'age', 'race', 'fever')

for(i in 1:2) {
  hatmu = sum(fb_NR_array_test[i,,,] * fb_NR_array_rate[i,,,]/sum(fb_NR_array_rate[i,,,]))
  truemu = NR_test[['sex']][i]
  fb_NR_array_test[i,,,] = fb_NR_array_test[i,,,] * truemu/hatmu
}
for(j in 1:3) {
  hatmu = sum(fb_NR_array_test[,j,,] * fb_NR_array_rate[,j,,]/sum(fb_NR_array_rate[,j,,]))
  truemu = NR_test[['age']][j]
  fb_NR_array_test[,j,,] = fb_NR_array_test[,j,,] * truemu/hatmu
}
for(k in 1:2) {
  hatmu = sum(fb_NR_array_test[,,k,] * fb_NR_array_rate[,,k,]/sum(fb_NR_array_rate[,,k,]))
  truemu = NR_test[['race']][k]
  fb_NR_array_test[,,k,] = fb_NR_array_test[,,k,] * truemu/hatmu
}
for(l in 1:2) {
  hatmu = sum(fb_NR_array_test[,,,l] * fb_NR_array_rate[,,,l]/sum(fb_NR_array_rate[,,,l]))
  truemu = NR_test[['fever']][l]
  fb_NR_array_test[,,,l] = fb_NR_array_test[,,,l] * truemu/hatmu
}


fb_haty_nomem = sum(fb_NR_array_test * fb_weights)
 
FP = 0.03 # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7307014/
FN = 0.13 # https://www.medrxiv.org/content/10.1101/2020.04.16.20066787v2.full.pdf

fb_haty = (fb_haty_nomem - FP)/(1-FP-FN)


print(paste("Hat y with no MEM =", round(haty_nomem,3)))
print(paste("Hat y with MEM =", round(haty,3)))


print(paste("FB-based hat y with no MEM =", round(fb_haty_nomem,3)))
print(paste("FB-based hat y with MEM =", round(fb_haty,3)))

## Statistical Error Decomposition
N <- 6.732E6; # Indiana Population
# https://www.cdc.gov/mmwr/volumes/69/wr/mm6929e1.htm
n <- 898 # NR from Indiana paper
priorn <- 95879
n <- 19649 # Tests in total

# April 25th - 29th
f <- n/(N-priorn)
true_y = 0.0181
init_Delta = 0.001

stat_decomp <- function(haty, true_y, falsepos, falseneg, f) {
  ## Defining error and variances
  haty_mem = (haty - falsepos)/(1-falsepos-falseneg)
  error <- haty - true_y
  var_y = true_y * (1-true_y)
  sigma_y = sqrt(var_y)
  
  interior_fn <- function(Delta) {
    rho = Delta * sqrt( var_y / (f*(1-f)))
    numerator = falsepos * (1-true_y) + falseneg * true_y
    f_0 = f - true_y * Delta
    f_1 = Delta + f_0
    denominator = f_0* (1-true_y) + f_1 * true_y
    odds = true_y/(1-true_y)
    D_M = (1-Delta * odds * numerator/denominator) * 1/(1-falsepos-falseneg)
    
    output = rho * D_M - sqrt(f/(1-f)) * error / sigma_y
    return(output^2)  
  }
  return(interior_fn)
}


error = (overall_test-FP)/(1-FP-FN) - true_y 
var_y = true_y * (1-true_y)
sigma_y = sqrt(var_y)
sqrt(f/(1-f)) * error / sigma_y

estimated_Delta = optim(par = init_Delta, fn = stat_decomp(overall_test, true_y, 0.03, 0.13, f))
best_Delta <- estimated_Delta$par
f_0 = f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0

print(paste("Unweighted prevalence with MEM: Error = ", round(error,4), "Delta =", round(best_Delta,10), "M =", round(best_M,3)))

# Smallest M for range of FP and FN
estimated_Delta = optim(par = init_Delta, fn = stat_decomp(overall_test, true_y, 0.05, 0.19, f))
best_Delta <- estimated_Delta$par
f_0 = f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0
best_M

# Largest M for range of FP and FN
estimated_Delta = optim(par = init_Delta, fn = stat_decomp(overall_test, true_y, 0.022, 0.09, f))
best_Delta <- estimated_Delta$par
f_0 = f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0
best_M


## Weighted error decomposition
expected_weight = expected_weightsq = 0.0

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        for (m in 1:2) {
          for (n in 1:2) {
            numer = R_rate[['sex']][i] * R_rate[['age']][j] * R_rate[['race']][k] * R_rate[['fever']][l] * R_rate[['household']][m] * R_rate[['prior']][n]  
            denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l] * NR_rate[['household']][m] * NR_rate[['prior']][n] 
            current_weight = numer/denom
            rate = denom
            expected_weight = expected_weight + current_weight*rate
            expected_weightsq = expected_weightsq + current_weight^2*rate
          }
        }
      }
    }
  }
}

var_weight = expected_weightsq - expected_weight^2
CV_Wsq = var_weight / expected_weight^2

fbexpected_weight = fbexpected_weightsq = 0.0

for (i in 1:2) {
  for (j in 1:3) {
    for (k in 1:2) {
      for (l in 1:2) {
        numer = fb_agg_weights$frac[fb_agg_weights$gender == i & fb_agg_weights$altage == j & fb_agg_weights$altfever == l] * R_rate[['race']][k]
        denom = NR_rate[['sex']][i] * NR_rate[['age']][j] * NR_rate[['race']][k] * NR_rate[['fever']][l]
        current_weight =  numer/denom
        rate = denom
        fbexpected_weight = fbexpected_weight + current_weight * rate
        fbexpected_weightsq = fbexpected_weightsq + current_weight^2 * rate
      }
    }
  }
}

fbvar_weight = fbexpected_weightsq - fbexpected_weight^2
fbCV_Wsq = fbvar_weight / fbexpected_weight^2

weighted_stat_decomp <- function(haty, true_y, falsepos, falseneg, f, expected_weight, var_weight) {
  ## Defining error and variances
  haty_mem = (haty - falsepos)/(1-falsepos-falseneg)
  error <- haty - true_y
  var_y = true_y * (1-true_y)
  sigma_y = sqrt(var_y)
  CV_Wsq = var_weight/expected_weight^2
  tilde_f = expected_weight * f
  
  interior_fn <- function(Delta) {
    rho = Delta * sqrt( var_y / (f*(1-f)*expected_weight^2+var_weight * f ))
    numerator = falsepos * (1-true_y) + falseneg * true_y
    f_0 = tilde_f - true_y * Delta
    f_1 = Delta + f_0
    denominator = f_0* (1-true_y) + f_1 * true_y
    odds = true_y/(1-true_y)
    D_M = (1-Delta * odds * numerator/denominator) * 1/(1-falsepos-falseneg)
    
    output = rho * D_M - sqrt(f/(1-f+CV_Wsq)) * error / sigma_y
    return(output^2)  
  }
  return(interior_fn)
}

error = (haty_nomem-FP)/(1-FP-FN) - true_y 
var_y = true_y * (1-true_y)
sigma_y = sqrt(var_y)
sqrt(f/(1-f+CV_Wsq)) * error / sigma_y

estimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(haty_nomem, true_y, 0.03, 0.13, f,expected_weight, var_weight))
best_Delta <- estimated_Delta$par
f_0 = expected_weight* f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0

print(paste("Weighted prevalence with MEM: Error = ", round(error,4), "Delta =", round(best_Delta,10), "M =", round(best_M,3)))

fberror = (fb_haty_nomem-FP)/(1-FP-FN) - true_y 
var_y = true_y * (1-true_y)
sigma_y = sqrt(var_y)
sqrt(f/(1-f+fbCV_Wsq)) * error / sigma_y

fbestimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(fb_haty_nomem, true_y, 0.03, 0.13, f, fbexpected_weight, fbvar_weight))
fbbest_Delta <- fbestimated_Delta$par
f_0 = fbexpected_weight * f - true_y * fbbest_Delta
f_1 = fbbest_Delta + f_0
fbbest_M = f_1/f_0

print(paste("FB Weighted prevalence with MEM: Error = ", round(fberror,4), "Delta =", round(fbbest_Delta,10), "M =", round(fbbest_M,3)))

# Smallest M for range of FP and FN
estimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(haty_nomem, true_y, 0.05, 0.19, f,expected_weight, var_weight))
best_Delta <- estimated_Delta$par
f_0 = expected_weight* f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0
best_M

fbestimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(fb_haty_nomem, true_y, 0.05, 0.19, f, fbexpected_weight, fbvar_weight))
fbbest_Delta <- fbestimated_Delta$par
f_0 = fbexpected_weight * f - true_y * fbbest_Delta
f_1 = fbbest_Delta + f_0
fbbest_M = f_1/f_0
fbbest_M

# Largest M for range of FP and FN
estimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(haty_nomem, true_y, 0.03, 0.09, f,expected_weight, var_weight))
best_Delta <- estimated_Delta$par
f_0 = expected_weight* f - true_y * best_Delta
f_1 = best_Delta + f_0
best_M = f_1/f_0
best_M

fbestimated_Delta = optim(par = init_Delta, fn = weighted_stat_decomp(fb_haty_nomem, true_y, 0.03, 0.09, f, fbexpected_weight, fbvar_weight))
fbbest_Delta <- fbestimated_Delta$par
f_0 = fbexpected_weight * f - true_y * fbbest_Delta
f_1 = fbbest_Delta + f_0
fbbest_M = f_1/f_0
fbbest_M

