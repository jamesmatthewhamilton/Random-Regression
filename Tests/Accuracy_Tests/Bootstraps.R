setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 5
TESTS = 150
CORES = 16
COLNAMES = seq(20, 3000, 20)

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim4_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     bootstraps = (20 * jj),
                                     cores = CORES,
                                     verbose = TRUE,
                                     test = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "Bootstraps", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "")