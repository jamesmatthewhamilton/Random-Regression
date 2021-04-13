# Competing Paper: Spike-and-Slab Meets LASSO A Review of the Spike-and-Slab LASSO

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("GEOquery")
library("GEOquery")
library(glmnet)
library(RandomLasso)
#library("readr")  # problems().

gse <- getGEO(filename = "data/rat/GSE5680_series_matrix.txt.gz", GSEMatrix = TRUE)
X = t(exprs(object = gse))

response_index <- which(colnames(X)=="1389163_at")  #Trim32
X = X[, -response_index]
y = X[, response_index]

X <- apply(X, 2, scale)
y.mean <- mean(y)
y <- y - y.mean

#write.csv(x = x, file = "GSE2553.expression.matrix.csv", quote = F, row.names = F) #export expression matrix in file (.csv format).

#row.names(X)[order(apply(X, 1, var), decreasing = TRUE)][1:10]
#sort(apply(X, 1, var), decreasing = TRUE)[1:10000]

set.seed(100)

reps <- 10
mspe_train <- rep(NA, reps)
mspe_test <- rep(NA, reps)
rmspe_train <- rep(NA, reps)
rmspe_test <- rep(NA, reps)

for (ii in 1:reps) {

    index <-  sample(1:nrow(X), 90)
    train_X <- X[index,] # Create the training data
    test_X <- X[-index,] # Create the test data
    train_y <- y[index] # Create the training data
    test_y <- y[-index] # Create the test data

    #ridge_reg <- glmnet(train[,-response_index], train[,response_index], alpha = 0)

    cv <- cv.glmnet(train_X,
                    train_y,
                    nfold = 5, alpha = 1)

    coefficients <- glmnet(train_X,
                           train_y,
                           optimal_lambda = cv$lambda.min,
                           alpha = 1,
                           standardize = TRUE,
                           intercept = TRUE)

    beta_hat <- coef(coefficients, exact=TRUE)[-1]

    predictions_train <- predict(coefficients, s = optimal_lambda, newx = train_X)
    mspe_train[ii] <- sum((predictions_train - train_y)^2) / nrow(train_X)
    predictions_test <- predict(coefficients, s = optimal_lambda, newx = test_X)
    mspe_test[ii] <- sum((predictions_test - test_y)^2) / nrow(test_X)
    print("Lasso:")
    print(mspe_train)
    print(mspe_test)

    rcoefficients <- HiLasso(train_X, train_y, cores = 6)

    rpredictions_train <- as.matrix(apply(t(apply(train_X, 1, function(l) {l * rcoefficients})), 1, sum))
    rmspe_train[ii] <- sum((rpredictions_train - train_y)^2) / nrow(train_X)
    rpredictions_test <- as.matrix(apply(t(apply(test_X, 1, function(l) {l * rcoefficients})), 1, sum))
    rmspe_test[ii] <- sum((rpredictions_test - test_y)^2) / nrow(test_X)
    print("Random Lasso:")
    print(rmspe_train)
    print(rmspe_test)
}
