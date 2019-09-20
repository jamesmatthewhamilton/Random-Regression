setwd(dirname(parent.frame(2)$ofile))
detach("package:RandomLasso", unload = TRUE)
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

ITERATIONS = 10
TESTS = 14

source("../func/SimulateTestData.R")
s <- SimulateTestData("../res/sim2_sig3_our.RData", TESTS, ITERATIONS)

source("../../RandomLasso/R/helper.regression.R")
for (ii in 1:ITERATIONS) {
    #parrt1 = RandomLasso3(s$x[[ii]],s$y[[ii]],Importance_weight = 0,NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = FALSE, Method = 'Enet')
    #coef[[X]][,ii] = RandomLasso3(s$x[[ii]],s$y[[ii]],Importance_weight = abs(parrt1[,1]),NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = TRUE, Method = 'Enet')[,1]
    coef[[1]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0, nfold = 10) # Ridge Regression
    coef[[2]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 0.5, nfold = 10) # Elastic-net
    coef[[3]][,ii] <- Lasso(s$x[[ii]], s$y[[ii]], alpha = 1, nfold = 10) # Lasso
    coef[[4]][,ii] <- AdaptiveLasso(s$x[[ii]], s$y[[ii]], alpha = 1, importance.measure = coef[[1]][,ii], nfold = 10)
    coef[[5]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    coef[[6]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 1), verbose = TRUE, test = FALSE)
    coef[[7]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    coef[[8]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(1, 1), verbose = TRUE, test = FALSE)
    coef[[9]][,ii] <- RandomLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
    coef[[10]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    coef[[11]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0, 1), verbose = TRUE, test = FALSE)
    coef[[12]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    coef[[13]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(1, 1), verbose = TRUE, test = FALSE)
    coef[[TESTS]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]], alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
}

source("../func/RunAccuracyTests.R")
r <- RunAccuracyTest("../log/RegressionMethods", s$coef, TESTS, ITERATIONS, s$ground.truth, s$y.val, s$x.val)