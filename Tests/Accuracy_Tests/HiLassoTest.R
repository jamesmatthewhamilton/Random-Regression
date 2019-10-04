setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 3
CORES = 16
COLNAMES = c("HiLasso", "HiLassoDS", "HiLassoC")

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim3_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

library('glmnet')
source("../../RandomLasso/R/helper.regression.R")
for (ii in 1:ITERATIONS) {
    s$coef[[1]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 1))
    s$coef[[2]][,ii] <- HiLassoDS(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 2))
    s$coef[[TESTS]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 3))
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "HiLassoTest", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "", angle = 45)
