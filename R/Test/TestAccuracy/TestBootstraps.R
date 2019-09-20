setwd(dirname(parent.frame(2)$ofile))
detach("package:RandomLasso", unload = TRUE)
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 50

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     bootstraps = (20 * ii),
                                     verbose = TRUE,
                                     test = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest("../log/Bootstraps", s$coef, TESTS, ITERATIONS, s$ground.truth, s$y.val, s$x.val)