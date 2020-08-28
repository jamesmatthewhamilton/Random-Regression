#' Performs variable selection and regularization using Hi Lasso.
#'
#' @param x Matrix of independent data.
#' @param y Matrix or vector of dependent data.
#' @param bootstraps Number of times features are randomly sampled.
#' @param alpha Regression method, e.g. ridge=0, elastic=0.5, lasso=1.
#' @param lambda_1se Largest value of lambda such that error is within 1
#'    standard error of the minimum.
#' @param box_width Number of features sampled when randomly sampling. By
#'    default box_width will be equal to the number of samples.
#' @param nfold Number of folds tested to find the optimal hyperparameter.
#' @param cores Number of cores used when running in parallel.
#' @param verbose Supresses all printing and time estimation.
#' @param verbose_output Returns additional information.
#' @keywords
#' @author James Matthew Hamilton
#' @export
#' @examples
#' RandomLasso(x, y)
#' RandomLasso(x, y, verbose=FALSE, bootstraps=300)
#'

HiLasso <- function(x, y, bootstraps, alpha=c(0.5, 1), box_width,
                    lambda_1se=c(FALSE, FALSE), nfold=5, cores=FALSE,
                    verbose=TRUE, verbose_output=FALSE) {
    message("Starting Random Lasso...")
    if (verbose) start_time <- Sys.time()
    x <- as.matrix(x)
    y <- as.matrix(y)
    n_features <- ncol(x)
    n_samples <- nrow(x)
    pb = NULL

    if(!all(is.na(x) == FALSE)) cat("Error: NA values detected in x.\n")
    if(!all(is.na(y) == FALSE)) cat("Error: NA values detected in y.\n")

    if (missing(box_width)) box_width <- n_samples
    if (cores == TRUE) {
        cores <- detectCores()
        if (verbose) cat(paste("[Detected", cores, "Cores]"))
    }
    if (missing(bootstraps)) {
        bootstraps <- ceiling(n_features / box_width) * 40
    }

    if (verbose) {
        cat("\nPart 1 of 2:\n")
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                randomBootstrap, x, y, pb, as.numeric(Sys.time()), bootstraps, box_width, alpha[1], nfold, lambda_1se[1], NULL, method="Regression", verbose)
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                  randomBootstrap, x, y, pb, as.numeric(Sys.time()), bootstraps, box_width, alpha[1], nfold, lambda_1se[1], NULL, method="Regression", verbose,
                                  mc.cores=cores)
    }
    importance_measure <- Reduce('+', lapply(list_beta_hat, abs)) + 5e-324

    .part2 <- function(ii, x, y, start_time) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }

        random_features <- sample(n_features, box_width,
                                  replace=FALSE, prob=importance_measure)
        random_samples <- sample(n_samples, replace=TRUE)

        random_x <- x[random_samples, random_features]
        random_y <- y[random_samples, ]
        random_importance <- importance_measure[random_features]

        random_y_mean <- mean(random_y)
        random_y_scaled <- random_y - random_y_mean

        random_x_mean <- apply(random_x, 2, mean)
        random_x_scaled <- scale(random_x, random_x_mean, FALSE)
        std_dev <- sqrt(apply(random_x_scaled ^ 2, 2, sum)) + 5e-324
        random_x_scaled <- scale(random_x_scaled, FALSE, std_dev)

        beta_hat <- replicate(n_features, 0)
        beta_hat[random_features] <- AdaptiveLasso(random_x_scaled,
                                                   random_y_scaled,
                                                   alpha[2],
                                                   random_importance,
                                                   nfold,
                                                   lambda_1se[2]) / std_dev
        return(beta_hat)
    }

    if (verbose) {
        cat("\nPart 2 of 2:\n")
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                randomBootstrap, x, y, pb, as.numeric(Sys.time()), bootstraps, box_width, alpha[2], nfold, lambda_1se[2], importance_measure, method="Adaptive", verbose)
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                  randomBootstrap, x, y, pb, as.numeric(Sys.time()), bootstraps, box_width, alpha[2], nfold, lambda_1se[2], importance_measure, method="Adaptive", verbose,
                                  mc.cores=cores)
    }

    if (verbose) {
        cat("\n[Done]\n")
        print(Sys.time() - start_time)
        close(pb)
    }

    reduced_beta_hat <- Reduce('+', list_beta_hat) / bootstraps
    reduced_beta_hat <- matrix(reduced_beta_hat,
                               nrow=n_features,
                               ncol=1)
    rownames(reduced_beta_hat) <- colnames(x)
    colnames(reduced_beta_hat) <- "Coefficients"
    if (verbose_output) {
        return(list(beta_hat = reduced_beta_hat,
                    importance_measure = importance_measure))
    }
    return(reduced_beta_hat)
}
