options(scipen = 999)
setwd("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Test/")

rm(RandomLasso)
detach("package:RandomLasso", unload = TRUE)
install.packages("~/Dropbox/MatthewHamilton/Hi-Lasso/R/RandomLasso/",
                 repos = NULL, type = "source")
library(RandomLasso)
library("Metrics")

start <- Sys.time()
results.R <- RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
Sys.time() - start

start <- Sys.time()
results.H <- HiLasso(x, y, alpha = c(0.5,1), verbose = TRUE, test = FALSE)
Sys.time() - start

results.P = c(1.5324, 1.3097, -0.1802, 0.6110, 0.4852, 0, 1.1057, 0.8117, 0.3094, -0.0302)

beta.hat <- as.matrix(as.vector(results.H))
Truth <-  as.matrix(beta0)

RME.ALL <- t(Truth - beta.hat) %*% cov(Truth - beta.hat)
RME.NONZER <- t(Truth[1:10] - beta.hat) %*% cov(Truth[1:10] - beta.hat)

rmse(Truth, beta.hat)
rmse(Truth[1:10], beta.hat[1:10])
rmse(y.test, beta.hat[1:10])
rmse(y.val, beta.hat[1:10])

FindConfusion <- function(Truth, beta.hat) {
    if (beta.hat != 0) {
        if (Truth != 0) return("TP")
        else return("FP")
    }
    else {
        if (Truth != 0) return("FN")
        else return("TN")
    }
}

test <- Confusion(Truth, beta.hat)
TPR <- test[1]
PPV <- test[2]
F1 <- 2 * (PPV * TPR) / (PPV + TPR)
F1

confusion.values <- mapply(FindConfusion, Truth, beta.hat)
TP <- length(confusion.values[confusion.values == "TP"])
FP <- length(confusion.values[confusion.values == "FP"])
FN <- length(confusion.values[confusion.values == "FN"])
TN <- length(confusion.values[confusion.values == "TN"])

TPR <- TP / (TP + FN)
PPV <- TP / (TP + FP)

F1 <- 2 * (PPV * TPR) / (PPV + TPR)
F1

start <- Sys.time()
RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
Sys.time() - start

## Defunct:
# PART1 <- RandomLasso2(x, y, NumOfFeatures = 100, repeat_Boostrapping = 50, Importance_weight = 1, Method = 'LASSO')
# PART2 <- RandomLasso2(x, y, NumOfFeatures = 100, repeat_Boostrapping = 50, Importance_weight = abs(PART1[,1]), Method = 'LASSO')
