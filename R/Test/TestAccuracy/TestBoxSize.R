setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 5
TESTS = 4

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                    alpha = c(0.5, 1),
                                    box.width = (101 - (jj * 20)),
                                    verbose = TRUE,
                                    test = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest("../log/BoxSize", s$coef, TESTS, ITERATIONS, s$ground.truth, s$y.val, s$x.val)