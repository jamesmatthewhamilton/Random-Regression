options(scipen = 999)
setwd("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Package/Test/res/")

library("Metrics")

ITERATIONS = 9
TESTS = 5

coef <- list(c(0,0))
rmse.all <- matrix(data = 0, nrow = 9, ncol = TESTS)
rmse.sel <- matrix(data = 0, nrow = 9, ncol = TESTS)

# load("sim1_sig3_our.RData")
load("sim2_sig3_our.RData")
# load("sim3_sig3_our.RData")
# load("sim4_sig3_our.RData")

rm(RandomLasso)
detach("package:RandomLasso", unload = TRUE)
install.packages("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Package/RandomLasso/", repos = NULL,
                 type = "source")
library(RandomLasso)
ls("package:RandomLasso")

for (i in 1:ITERATIONS) {
    
    y <- sim_data[[i]][2]
    y <- unlist(y)
    y <- matrix(y, ncol = 1)
    
    x <- sim_data[[i]][3]
    x <- unlist(x[1])
    x <- matrix(x, ncol = n_feature, byrow = FALSE)
    
    y.test <- sim_data[[i]][4]
    y.test <- unlist(y.test)
    y.test <- matrix(y.test, ncol = 1)
    
    x.test <- sim_data[[i]][5]
    x.test <- unlist(x.test[1])
    x.test <- matrix(x.test, ncol = n_feature, byrow = FALSE)
    
    y.val  <- sim_data[[i]][6]
    y.val <- unlist(y.val)
    y.val <- matrix(y.val, ncol = 1)
    
    x.val  <- sim_data[[i]][7]
    x.val <- unlist(x.val[1])
    x.val <- matrix(x.val, ncol = n_feature, byrow = FALSE)
    
    cov_x <- sim_data[[i]][8]
    cov_x <- unlist(cov_x[1])
    cov_x <- matrix(cov_x, ncol = n_feature, byrow = FALSE)
    
    n_feature <- ncol(x)
    colnames(x)      <- paste('P', seq(1:n_feature), sep = '')
    colnames(x.val)  <- paste('P', seq(1:n_feature), sep = '')
    colnames(x.test) <- paste('P', seq(1:n_feature), sep = '')
    
    coef[[1]] <- Lasso(x, y, alpha = 1, nfold = 5)
    coef[[2]] <- Lasso(x, y, alpha = 0.5, nfold = 5)
    coef[[3]] <- Lasso(x, y, alpha = 0, nfold = 5)
    coef[[4]] <- RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
    coef[[TESTS]] <- HiLasso(x, y, alpha = 0.5, verbose = TRUE, test = FALSE)
    
    for (jj in 1:TESTS) {
        beta.hat <- as.matrix(as.vector(coef[[jj]]))
        ground.truth <- as.matrix(beta0) # Redundant
    
        rmse.all[i,jj] <- rmse(ground.truth, beta.hat)
        rmse.sel[i,jj] <- rmse(ground.truth[1:10], beta.hat[1:10])
    }
}

write.csv(file = "TestAccuracy1000.csv", rmse.all)
write.csv(file = "TestAccuracy1000B.csv", rmse.sel)