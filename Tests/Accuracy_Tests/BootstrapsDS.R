setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 76
CORES = TRUE
COLNAMES = seq(200, 1700, 20)
if (!(length(COLNAMES) == length(1:TESTS))) { print("[!] Warning: Column names size incorect.")}

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim4_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLassoDS(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     bootstraps = 180 + (20 * jj),
                                     cores = CORES,
                                     divide.bootstrap = FALSE,
                                     verbose = FALSE,
                                     test = FALSE)
        setTxtProgressBar(pb, ((ii - 1) * TESTS + jj))
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "Increasing Bootstraps(DS)", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "Bootstraps_DS", angle = 45)