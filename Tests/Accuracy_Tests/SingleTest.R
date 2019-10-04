setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 5
TESTS = 1
CORES = FALSE
COLNAMES = NA

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)
#pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

for (ii in 1:ITERATIONS) {
    for(jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                     alpha = c(0.5, 1),
                                     cores = CORES,
                                     verbose = TRUE,
                                     test = FALSE)
        #setTxtProgressBar(pb, ((ii - 1) * TESTS + jj))
    }
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "SingleTest", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "")