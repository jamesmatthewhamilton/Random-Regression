# Competing Paper: Spike-and-Slab Meets LASSO A Review of the Spike-and-Slab LASSO

setwd(dirname(parent.frame(2)$ofile)) # Set working to this file.
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)
library(glmnet)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("GEOquery")
library("GEOquery")

#library("readr")  # problems().

gse <- getGEO(filename = "../../data/rat/GSE5680_series_matrix.txt.gz", GSEMatrix = TRUE)
X = t(exprs(object = gse))

response_index <- which(colnames(X)=="1389163_at")  #Trim32
X = X[, -response_index]
y = X[, response_index]

n_features <- ncol(X)
n_samples <- nrow(X)

X <- apply(X, 2, scale)
y.mean <- mean(y)
y <- y - y.mean

#write.csv(x = x, file = "GSE2553.expression.matrix.csv", quote = F, row.names = F) #export expression matrix in file (.csv format).

#row.names(X)[order(apply(X, 1, var), decreasing = TRUE)][1:10]
#sort(apply(X, 1, var), decreasing = TRUE)[1:10000]

set.seed(100)

title = "Spike_and_Slab_Paper_Replication"
reps <- 2
n_training_samples <- 90

lasso_coef <- matrix(0, nrow = reps, ncol = n_features)
colnames(lasso_coef) <- colnames(X)
#lasso_pred_train <- matrix(0, nrow = reps, ncol = n_training_samples)
#lasso_pred_test <- matrix(0, nrow = reps, ncol = n_samples - n_training_samples)

hlasso_coef <- matrix(0, nrow = reps, ncol = n_features)
colnames(hlasso_coef) <- colnames(X)
#hlasso_pred_train <- matrix(0, nrow = reps, ncol = n_training_samples)
#hlasso_pred_test <- matrix(0, nrow = reps, ncol = n_samples - n_training_samples)

benchmark <- data.frame("LassoTrainingMSPM" = numeric(reps),
                      "LassoTestingMSPM" = numeric(reps),
                      "LassoFeaturesSelected" = numeric(reps),
                      "HiLassoTrainingMSPM" = numeric(reps),
                      "HiLassoTestingMSPM" = numeric(reps),
                      "HiLassoFeaturesSelected" = numeric(reps))

for (ii in 1:reps) {

    # Generate Samples
    index <-  sample(1:nrow(X), n_training_samples)
    x_train <- X[index,] # Create the training data
    x_test <- X[-index,] # Create the test data
    y_train <- y[index] # Create the training data
    y_test <- y[-index] # Create the test data

    # Benchmarking Lasso
    lasso_cv <- cv.glmnet(x_train, y_train,
                          nfold = 5, alpha = 1,
                          standardize = TRUE,
                          intercept = TRUE)

    lasso_coef[ii,] <- coef(lasso_cv, s=lasso_cv$lambda.min, exact=TRUE)[-1]
    benchmark$LassoFeaturesSelected[ii] <- sum(lasso_coef[ii,] != 0)

    lasso_pred_train <- predict(lasso_cv, s = lasso_cv$lambda.min, newx = x_train)
    benchmark$LassoTrainingMSPM[ii] <- sum((lasso_pred_train - y_train)^2) / nrow(x_train)
    lasso_pred_test <- predict(lasso_cv, s = lasso_cv$lambda.min, newx = x_test)
    benchmark$LassoTestingMSPM[ii] <- sum((lasso_pred_test - y_test)^2) / nrow(x_test)

    # Benchmarking Hi-Lasso
    hlasso_coef[ii,] <- HiLasso(x_train, y_train, cores = TRUE)
    benchmark$HiLassoFeaturesSelected[ii] <- sum(hlasso_coef[ii,] != 0)

    hlasso_pred_train <- as.matrix(apply(t(apply(x_train, 1, function(l) {l * hlasso_coef[ii,]})), 1, sum))
    benchmark$HiLassoTrainingMSPM[ii] <- sum((hlasso_pred_train - y_train)^2) / nrow(x_train)
    hlasso_pred_test <- as.matrix(apply(t(apply(x_test, 1, function(l) {l * hlasso_coef[ii,]})), 1, sum))
    benchmark$HiLassoTestingMSPM[ii] <- sum((hlasso_pred_test - y_test)^2) / nrow(x_test)

    print(benchmark)
}

filename <- paste0(title, "[", n_samples, "x", n_features, "]", format(Sys.time(), "%F_%H:%M"))

write.table(lasso_coef, file=paste0(filename, "_", "lasso_coef", "_", ".csv"))
write.table(hlasso_coef, file=paste0(filename, "_", "hlasso_coef", "_", ".csv"))
write.table(benchmark, file=paste0(filename, "_", "benchmark", "_", ".csv"))

