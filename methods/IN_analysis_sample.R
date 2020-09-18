OVERALL = 11.7
NR_rate = data.frame("sex" = matrix(c(21.7, 24.2), nrow = 2))
NR_rate$age = c(29.7,24.9,6.7)

NR_tests = data.frame("sex" = matrix(c(58.2, 41.8), nrow = 2))
NR_tests$age = c(39.4,41.1,19.5)

adjustment = data.frame("sex" = OVERALL/sum(NR_rate$sex*(NR_tests$sex/100)))


NR_rate$sex*adjustment$sex
