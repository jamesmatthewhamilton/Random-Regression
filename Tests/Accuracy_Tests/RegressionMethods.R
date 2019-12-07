setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 14
CORES = TRUE
COLNAMES = c("Ridge Regression", "0.25", "Elatic-Net", "0.75", "Lasso", "Adaptive",
             "RandomLasso[0.5,0.5]", "RandomLasso[1,1]", "RandomLasso[0.5,A(1)]",
             "HiLasso[0,0.5]", "HiLasso[0,1]", "HiLasso[0.5,0.5]", "HiLasso[1,1]", "HiLasso[0.5,1]")

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim1_sig3_our.RData", TESTS, ITERATIONS)
pb <- txtProgressBar(min = 0, max = (2 * ITERATIONS), style = 3)

library('glmnet')
source("../../RandomLasso/R/helper.regression.R")
for (ii in 1:ITERATIONS) {
    #parrt1 = RandomLasso3(s$x[[ii]],s$y[[ii]],Importance_weight = 0,NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = FALSE, Method = 'Enet')
    #coef[[X]][,ii] = RandomLasso3(s$x[[ii]],s$y[[ii]],Importance_weight = abs(parrt1[,1]),NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = TRUE, Method = 'Enet')[,1]
    s$coef[[1]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0, nfold = 10) # Ridge Regression
    s$coef[[2]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0.25, nfold = 10) # Elastic-net
    s$coef[[3]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0.5, nfold = 10) # Elastic-net
    s$coef[[4]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0.75, nfold = 10) # Elastic-net
    s$coef[[5]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 1, nfold = 10) # Lasso
    s$coef[[6]][,ii] <- AdaptiveLasso2(s$x[[ii]], s$y[[ii]], alpha = 1, nfold = 10)
    s$coef[[7]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 0.5), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[8]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(1, 1), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[9]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 0.5), cores = CORES, verbose = FALSE, test = FALSE, adaptive = TRUE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 1)) # ~50% Complete.
    s$coef[[10]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 0.5), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[11]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 1), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[12]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 0.5), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[13]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(1, 1), cores = CORES, verbose = FALSE, test = FALSE)
    s$coef[[TESTS]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), cores = CORES, verbose = FALSE, test = FALSE)
    setTxtProgressBar(pb, ((ii - 1) * TESTS + 2))
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest(s, "../log", "RegressionMethods", TESTS, ITERATIONS, COLNAMES)

source("../func/VisualizeResults.R")
VisualizeResults(r, s, "", angle = 45)
