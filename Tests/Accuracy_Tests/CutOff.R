setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 100
CORES = TRUE
COLNAMES = rep(1, 100)

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim4_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (TESTS * ITERATIONS), style = 3)

for (ii in 1:ITERATIONS) {
    for (jj in 1:TESTS) {
        s$coef[[jj]][,ii] <- HiLassoC(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1),
                                      cutoff = ifelse(jj != TESTS,
				      c(1-((jj-1)*0.0095), 0, 0),
				      c(0, 0, 0)),
                                      cores = CORES, verbose = FALSE)
        setTxtProgressBar(pb, ((ii - 1) * TESTS + jj))
        if(ii == 1) {COLNAMES[jj] = 1-((jj-1)*0.0095)}
    }
}
COLNAMES[TESTS] = 0

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "Cutoff[1-0.05]_at_Part1", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "Cutoff[1-0.05] at Part I", angle = 45)
