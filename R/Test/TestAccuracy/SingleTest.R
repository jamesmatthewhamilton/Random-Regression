setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 2
CORES = 16

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)

source("../../RandomLasso/R/helper.regression.R")
for (ii in 1:ITERATIONS) {
    for(jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     cores = CORES,
                                     verbose = TRUE,
                                     test = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest("../log/SingleTest", s$coef, TESTS, ITERATIONS, s$ground.truth, s$y.val, s$x.val)