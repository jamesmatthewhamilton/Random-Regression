setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 5
TESTS = 47
CORES = 16
COLNAMES = seq(245, 15, -5) / 200

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim3_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                    alpha = c(0.5, 1),
                                    box.width = (250 - (jj * 5)),
                                    cores = CORES,
                                    verbose = FALSE,
                                    test = FALSE)
        setTxtProgressBar(pb, ((ii - 1) * TESTS + jj))
    }
}


source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "BoxRatio", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "Ratio[Features Selected by Total Samples]", angle = 45)
