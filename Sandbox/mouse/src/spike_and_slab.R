# Competing Paper: Spike-and-Slab Meets LASSO A Review of the Spike-and-Slab LASSO

setwd(dirname(parent.frame(2)$ofile)) # Set working to this file.
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)
library(glmnet)
#library(hal9001)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("GEOquery")
library("GEOquery")

get_benchmark_results <- function(coef, x_train, y_train, x_test, y_test) {

    features_selected <- sum(coef != 0)

    prediction <- as.matrix(apply(t(apply(x_train, 1, function(l) {l * coef})), 1, sum))
    train_rme <- sum((prediction - y_train)^2) / nrow(x_test)

    prediction <- as.matrix(apply(t(apply(x_test, 1, function(l) {l * coef})), 1, sum))
    test_rme <- sum((prediction - y_test)^2) / nrow(x_test)

    return(list(features_selected=features_selected,
                train_rme=train_rme,
                test_rme=test_rme))
}

get_pretty_benchmark_algo_names <- function(benchmark_algorithms) {

    pretty_names <- lapply(benchmark_algorithms, function(x) class(x)[1])

    for (ii in seq(1, length(pretty_names))) {
        if (pretty_names[ii] == "Glmnet") {
            pretty_names[ii] = paste0(pretty_names[ii], " (alpha=",
                                      benchmark_algorithms[[ii]]@alpha, ")")
        }
    }
    return(pretty_names)
}
# Test classes.
setClass(
    "Glmnet",
    slots = list(alpha = "numeric",
                 nfold = "numeric"),
    prototype = list(alpha = 0, nfold = 5)
)


setClass(
    "HiLasso",
    slots = list(
        alpha = "numeric",
        nfold = "numeric",
        cores = "numeric"
    ),
    prototype = list(
        alpha = c(0.5, 1),
        nfold = 5,
        cores = 1
    )
)





# Global Params
title = "Spike_and_Slab"
benchmark_algorithms <- list(new("Glmnet", alpha = 0),
                             new("Glmnet", alpha = 1),
                             new("HiLasso")) # TODO make object.
reps <- 50
training_samples_frac <- 3/4
seed = 100
set.seed(seed)


# ------ START DATA IMPORT ------
gse <- getGEO(filename = "../../data/mouse/GSE5680_series_matrix.txt.gz", GSEMatrix = TRUE)
data <- t(exprs(object = gse))

response_index <- which(colnames(data)=="1389163_at")  #Trim32
X <- data[, -response_index]
y <- data[, response_index]
# ------ END DATA IMPORT ------


# ------ START PREPROCESSING ------

X <- X[,which(colnames(X) %in%
              names(sort(apply(X, 2, var),
                         decreasing = TRUE)[1:15000]))]

X <- apply(X, 2, scale)
y.mean <- mean(y)
y <- y - y.mean

n_features <- ncol(X)
n_samples <- nrow(X)
n_training_samples <- (n_samples * training_samples_frac)
n_benchmark_algorithms <- length(benchmark_algorithms)

# ------ END PREPROCESSING ------


# ------ START RESULTS INIT ------
benchmark_metrics <- list("Features_Selected", "Training_RME", "Testing_RME")
n_benchmark_metrics <- length(benchmark_metrics)

benchmark_results <- lapply(seq_len(reps), function(X) {
    matrix <- matrix(1, nrow = n_benchmark_metrics, ncol = n_benchmark_algorithms);
    rownames(matrix) <- benchmark_metrics;
    colnames(matrix) <- get_pretty_benchmark_algo_names(benchmark_algorithms);
    matrix;
    })

coef_results <- lapply(seq_len(reps), function(x) {
    matrix <- matrix(1, nrow = n_features, ncol = n_benchmark_algorithms);
    rownames(matrix) <- colnames(X);
        colnames(matrix) <- get_pretty_benchmark_algo_names(benchmark_algorithms);
        matrix;
    })
# ------ START RESULTS INIT ------


# ------ START RESULTS BENCHMARK ------

for (ii in seq(1, reps)) {
    for (jj in seq(1, n_benchmark_algorithms)) {

        index <-  sample(1:nrow(X), n_training_samples)
        x_train <- X[index,]  # Create the training data
        x_test <- X[-index,]  # Create the test data
        y_train <- y[index]  # Create the training data
        y_test <- y[-index]  # Create the test data

        if (class(benchmark_algorithms[[jj]])[1] == "Glmnet") {
            lasso_cv <- cv.glmnet(x_train, y_train,
                                  nfold = benchmark_algorithms[[jj]]@nfold,
                                  alpha = benchmark_algorithms[[jj]]@alpha,
                                  standardize = TRUE,
                                  intercept = TRUE)

            coef_results[[ii]][, jj] <-
                coef(lasso_cv, s = lasso_cv$lambda.min, exact = TRUE)[-1]
            benchmark_results[[ii]][, jj] <-
                unlist(get_benchmark_results(coef_results[[ii]][, jj],
                                             x_train, y_train,
                                             x_test, y_test))
        }
        else if (class(benchmark_algorithms[[jj]])[1] == "HiLasso") {
            coef_results[[ii]][, jj] <-
                HiLasso(x_train, y_train,
                        alpha = benchmark_algorithms[[jj]]@alpha,
                        nfold = benchmark_algorithms[[jj]]@nfold,
                        cores = benchmark_algorithms[[jj]]@cores)
            benchmark_results[[ii]][, jj] <-
                unlist(get_benchmark_results(coef_results[[ii]][, jj],
                                             x_train, y_train,
                                             x_test, y_test))
        }
        else if  (class(benchmark_algorithms[[jj]])[1] == "HAL") {
            coef_results[[ii]][, jj] <- fit_hal(X = x_train, Y = y_train)
            benchmark_results[[ii]][, jj] <-
                unlist(get_benchmark_results(coef_results[[ii]][, jj],
                                             x_train, y_train,
                                             x_test, y_test))
        }
        if (class(benchmark_algorithms[[jj]])[1] == "OLS") {
            ols_object <- lm(y_train ~ . -1,
                             data = data.frame(y_train, x_train))

            coef_results[[ii]][, jj] <- ols_object$coefficients[-1]
            benchmark_results[[ii]][, jj] <-
                unlist(get_benchmark_results(coef_results[[ii]][, jj],
                                             x_train, y_train,
                                             x_test, y_test))
        }
    }
    save(benchmark_algorithms, coef_results, benchmark_results, file="benchmark_backup.RData")
    print(benchmark_results)
}

filename <- paste0(title, "[", n_samples, "x", n_features, "]", format(Sys.time(), "%F_%H-%M"))

save(benchmark_algorithms, coef_results, benchmark_results, file=paste0(filename, ".RData"))
write.csv(coef_results[[1]], file=paste0(filename, "_", "coefficients", ".csv"))
write.csv(Reduce("+", benchmark_results) / reps, file=paste0(filename, "_", "benchmark", ".csv"))
#apply(simplify2array(benchmark_results), 1:2, median)
# write.csv(apply(simplify2array(coef_results), 1:2, mean), file=paste0(filename, "_", "mean_coefficients", ".csv"))
