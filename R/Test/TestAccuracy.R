options(scipen = 999) # Ctrl-Shift-Enter
setwd(dirname(parent.frame(2)$ofile))
getwd()
library('Metrics')

ITERATIONS = 9
TESTS = 14

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

RMSE2 = function(truth, predicted){
    sqrt(mean((truth - predicted)^2))
}

# load("res/sim1_sig3_our.RData")
load("res/sim2_sig3_our.RData")
# load("res/sim3_sig3_our.RData")
# load("res/sim4_sig3_our.RData")
rm(RandomLasso)

coef <- vector("list", TESTS)
for (ii in 1:TESTS) {
  coef[[ii]] <- matrix(0, nrow = n_feature, ncol = ITERATIONS)
}

coef.avg <- matrix(data = 0, nrow = n_feature, ncol = TESTS)
rmse <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)
rmse2 <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)
f1 <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)
f2 <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)
dor <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)
dor2 <- matrix(data = 0, nrow = ITERATIONS, ncol = TESTS)

detach("package:RandomLasso", unload = TRUE)
install.packages("../RandomLasso/", repos = NULL,
                 type = "source")
library(RandomLasso)
ls("package:RandomLasso")

for (ii in 1:ITERATIONS) {

    y <- sim_data[[ii]][2]
    y <- unlist(y)
    y <- matrix(y, ncol = 1)

    x <- sim_data[[ii]][3]
    x <- unlist(x[1])
    x <- matrix(x, ncol = n_feature, byrow = FALSE)

    y.test <- sim_data[[ii]][4]
    y.test <- unlist(y.test)
    y.test <- matrix(y.test, ncol = 1)

    x.test <- sim_data[[ii]][5]
    x.test <- unlist(x.test[1])
    x.test <- matrix(x.test, ncol = n_feature, byrow = FALSE)

    y.val  <- sim_data[[ii]][6]
    y.val <- unlist(y.val)
    y.val <- matrix(y.val, ncol = 1)

    x.val  <- sim_data[[ii]][7]
    x.val <- unlist(x.val[1])
    x.val <- matrix(x.val, ncol = n_feature, byrow = FALSE)

    cov_x <- sim_data[[ii]][8]
    cov_x <- unlist(cov_x[1])
    cov_x <- matrix(cov_x, ncol = n_feature, byrow = FALSE)
    
    n_feature <- ncol(x)
    colnames(x)      <- paste('P', seq(1:n_feature), sep = '')
    colnames(x.val)  <- paste('P', seq(1:n_feature), sep = '')
    colnames(x.test) <- paste('P', seq(1:n_feature), sep = '')

    ground.truth <- as.matrix(beta0)

    coef[[1]][,ii] <- Lasso(x, y, alpha = 0, nfold = 10) # Ridge Regression
    coef[[2]][,ii] <- Lasso(x, y, alpha = 0.5, nfold = 10) # Elastic-net
    coef[[3]][,ii] <- Lasso(x, y, alpha = 1, nfold = 10) # Lasso
    coef[[4]][,ii] <- AdaptiveLasso(x, y, alpha = 1, importance.measure = coef[[1]][,ii], nfold = 10)
    coef[[5]][,ii] <- RandomLasso(x, y, alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    coef[[6]][,ii] <- RandomLasso(x, y, alpha = c(0, 1), verbose = TRUE, test = FALSE)
    coef[[7]][,ii] <- RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    coef[[8]][,ii] <- RandomLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
    coef[[9]][,ii] <- RandomLasso(x, y, alpha = c(1, 1), verbose = TRUE, test = FALSE)
    #X#parrt1 = RandomLasso3(x,y,Importance_weight = 0,NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = FALSE, Method = 'Enet')
    #X#coef[[X]][,ii] = RandomLasso3(x,y,Importance_weight = abs(parrt1[,1]),NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = TRUE, Method = 'Enet')[,1]
    coef[[10]][,ii] <- HiLasso(x, y, alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    coef[[11]][,ii] <- HiLasso(x, y, alpha = c(0, 1), verbose = TRUE, test = FALSE)
    coef[[12]][,ii] <- HiLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    coef[[13]][,ii] <- HiLasso(x, y, alpha = c(1, 1), verbose = TRUE, test = FALSE)
    coef[[TESTS]][,ii] <- HiLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
}

saveRDS(coef, paste("log/Coefficients[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".rds", sep = ""))

for (ii in 1:ITERATIONS) { # This should be vectorized. -jmh
    for (jj in 1:TESTS) {
        beta.hat <- as.matrix(as.vector(coef[[jj]][,ii]))

        coef.avg[,jj] = coef[[jj]][,ii] + beta.hat
        rmse[ii,jj] <- RMSE(threshold, ground.truth, beta.hat, cov_x, y.val, x.val)[1,2]
        rmse2[ii,jj] <- RMSE2(ground.truth, beta.hat)

        confusion.values <- mapply(FindConfusion, ground.truth, beta.hat)
        TP <- length(confusion.values[confusion.values == "TP"])
        FP <- length(confusion.values[confusion.values == "FP"])
        FN <- length(confusion.values[confusion.values == "FN"])
        TN <- length(confusion.values[confusion.values == "TN"])

        TPR <- TP / (TP + FN)
        PPV <- TP / (TP + FP)
        NPV <- TN / (TN + FN)

        f1[ii,jj] <- 2 * (PPV * TPR) / (PPV + TPR)
        f2[ii,jj] <- sqrt((TP / (TP + FP)) * (TP / (TP + FN)))
        dor[ii,jj] <- (TP / FP) / (FN / TN)
        dor2[ii,jj] <- (PPV * NPV) / ((1 - PPV) * (1 - NPV))
    }
}

coef.avg = coef.avg / TESTS

write.csv(x = coef.avg, paste("log/Averaged_Coefficients[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = rmse, paste("log/RMSE[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = rmse2, paste("log/RMSE_Raw[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = f1, paste("log/F1[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = f2, paste("log/F2[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = dor, paste("log/DOR[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = dor2, paste("log/DOR2[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))

apply(rmse, 2, mean)
apply(rmse2, 2, mean)
apply(f1, 2, mean)
apply(f2, 2, mean)
apply(dor, 2, mean)
apply(dor2, 2, mean)
