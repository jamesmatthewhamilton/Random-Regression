setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 1
CORES = 16
COLNAMES = "Delete Me"

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim1_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
    for(jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     cutoff = c(0,0,0),
                                     cores = CORES,
                                     verbose = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "DeleteMePlease", TESTS, ITERATIONS, COLNAMES)