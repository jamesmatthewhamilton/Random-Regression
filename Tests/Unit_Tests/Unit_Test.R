setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
r = list()

source("../func/SimulateTestData.R")

for (ff in 1:2) {
    cat(paste0("Loading Simulated Dataset ", ff, "."))
    s <- SimulateTestData(paste0("../res/sim", ff, "_sig3_our.RData"), 1, ITERATIONS)

    for (ii in 1:ITERATIONS) {
        s$coef[[1]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                    alpha = c(0.5, 1),
                                    cores = TRUE,
                                    verbose = TRUE)
    }

    source("../func/RunAccuracyTests.R")
    r[[ff]] <- RunAccuracyTest(s, "", "", 1, ITERATIONS, "", FALSE)
}

check <-     read.csv("../res/Unit_Test.csv")
for (ff in 1:2) {
    cat("Starting Unit Testing...\n")
}
