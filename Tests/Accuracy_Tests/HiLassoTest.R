setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 8
CORES = 16
COLNAMES = c("HiLasso", "HiLassoDS", "HiLassoC", "HiLassoC+", "HiLassoC++", "HiLassoTRI", "HiLassoPURE", "HiLassoVPURE")

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim1_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

library('glmnet')
source("../../RandomLasso/R/helper.regression.R")
for (ii in 1:ITERATIONS) {

    s$coef[[1]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 1))

    print("HiLassoDS:")
    s$coef[[2]][,ii] <- HiLassoDS(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 2))

    print("HiLassoC:")
    s$coef[[3]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 3))

    print("HiLassoC:")
    s$coef[[4]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE, cutoff = 0.001, cutoff2 = 0.0001)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 4))

    print("HiLassoC:")
    s$coef[[5]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE, cutoff = 0.005, cutoff2 = 0.0001)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 5))

    print("HiLassoTRI:")
    s$coef[[6]][,ii] <- HiLassoTRI(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 6))

    print("HiLassoPURE:")
    s$coef[[7]][,ii] <- HiLassoPURE(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 7))

    print("HiLassoVPURE:")
    s$coef[[8]][,ii] <- HiLassoVPURE(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 8))
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "HiLassoTest", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "", angle = 45)
