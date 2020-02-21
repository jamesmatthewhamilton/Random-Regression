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

check <- read.csv("../res/Unit_Test.csv")
cat("Starting Unit Testing...\n")

for (ff in 1:2) {
    if (r[[ff]]$rmse[length(r[[ff]]$rmse)] < (check[1,ff] * 1.07)) {
        cat("[RMSE] pass\n")
    } else {
        cat(paste0("[RMSE] Failed (HIGH)", ff, ": ",
            "\n\tExpected: ", check[1,ff],
            " (ave)\t", check[1,ff] * 1.07, " (max)",
            "\n\tRecieved: ", r[[ff]]$rmse[length(r[[ff]]$rmse)], "\n"))
    }
    if (r[[ff]]$f1[length(r[[ff]]$f1)] < (check[2,ff] * 1.1)) {
        cat("[F1] pass\n")
    } else {
        cat(paste0("[F1] Failed [HIGH]", ff, ": ",
                   "\n\tExpected: ", check[2,ff], " (ave)\t",
                   check[2,ff] * 1.1, " (max)",
                   "\n\tRecieved: ", r[[ff]]$f1[length(r[[ff]]$f1)], "\n"))
    }
}
