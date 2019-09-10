options(scipen = 999)
setwd("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Test/")

library('Metrics')

ITERATIONS = 9
TESTS = 1

RMSE2 = function(truth, predicted){
    sqrt(mean((truth - predicted)^2))
}

# load("res/sim1_sig3_our.RData")
 load("res/sim2_sig3_our.RData")
# load("res/sim3_sig3_our.RData")
# load("res/sim4_sig3_our.RData")

coef <- list()
coef.all <- matrix(data = 0, nrow = n_feature, ncol = TESTS)

rmse <- matrix(data = 0, nrow = 9, ncol = TESTS)
rmse2 <- matrix(data = 0, nrow = 9, ncol = TESTS)

rm(RandomLasso)
detach("package:RandomLasso", unload = TRUE)
install.packages("~/Dropbox/MatthewHamilton/Hi-Lasso/R/RandomLasso/", repos = NULL,
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

    #coef[[1]] <- Lasso(x, y, alpha = 0, nfold = 10) # Ridge Regression
    #coef[[2]] <- Lasso(x, y, alpha = 0.5, nfold = 10) # Elastic-net
    #coef[[3]] <- Lasso(x, y, alpha = 1, nfold = 10) # Lasso
    #coef[[4]] <- AdaptiveLasso(x, y, importance.measure = coef[[1]], nfold = 10)
    #coef[[5]] <- RandomLasso(x, y, alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    #coef[[6]] <- RandomLasso(x, y, alpha = c(0, 1), verbose = TRUE, test = FALSE)
    #coef[[7]] <- RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    #coef[[8]] <- RandomLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
    #X#coef[[X]] <- RapidRandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    #X#coef[[X]] <- RapidRandomLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
    #X#parrt1 = RandomLasso3(x,y,Importance_weight = 0,NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = FALSE, Method = 'Enet')
    #X#coef[[X]] = RandomLasso3(x,y,Importance_weight = abs(parrt1[,1]),NumOfFeatures = 100, repeat_Boostrapping = 100, step2 = TRUE, Method = 'Enet')[,1]
    #coef[[9]] <- HiLasso(x, y, alpha = c(0, 0.5), verbose = TRUE, test = FALSE)
    #coef[[10]] <- HiLasso(x, y, alpha = c(0, 1), verbose = TRUE, test = FALSE)
    #coef[[11]] <- HiLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    #coef[[TESTS]] <- HiLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
    coef[[TESTS]] <- HiLasso(x, y, alpha = c(1, 1), verbose = TRUE, test = FALSE)

    for (jj in 1:TESTS) {
        beta.hat <- as.matrix(as.vector(coef[[jj]]))

        coef.all[,jj] = coef.all[,jj] + beta.hat
        rmse[ii,jj] <- RMSE(threshold, ground.truth, beta.hat, cov_x, y.val, x.val)[36,2]
        rmse2[ii,jj] <- RMSE2(ground.truth, beta.hat)
    }
}

coef.all = coef.all / TESTS

write.csv(x = coef.all, paste("log/Coefficients[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = rmse, paste("log/RMSE[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
write.csv(x = rmse.sel, paste("log/RMSE_Raw[", ncol(x), "x", nrow(x), "]", format(Sys.time(), "%Fx%H-%M-%S"), ".csv", sep = ""))
apply(rmse, 2, mean)
apply(rmse.sel, 2, mean)