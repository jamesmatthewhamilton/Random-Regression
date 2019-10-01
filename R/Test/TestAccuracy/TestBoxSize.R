setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 98
CORES = 12
COLNAMES = seq(200, 6, -2) / 100

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                    alpha = c(0.5, 1),
                                    box.width = (202 - (jj * 2)),
                                    cores = CORES,
                                    verbose = TRUE,
                                    test = FALSE)
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest("../log/BoxSize", s, TESTS, ITERATIONS, COLNAMES, s$ground.truth, s$y.val, s$x.val)

source("../func/VisualizeResults.R")
VisualizeResults("../log/BoxSize", r, s, "Ratio[Features Selected by Total Samples]")

