setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 1
CORES = TRUE
COLNAMES = "SingleTest"

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim1_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
        s$coef[[1]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     cores = CORES,
                                     verbose = FALSE)
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", paste0(COLNAMES, "_"), TESTS, ITERATIONS, COLNAMES)