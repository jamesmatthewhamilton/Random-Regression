#' Performs variable selection and regularization using Hi Lasso.
#'
#' @param x Matrix of independent data.
#' @param y Matrix or vector of dependent data.
#' @param bootstraps Number of times features are randomly sampled.
#' @param alpha Regression method, e.g. ridge=0, elastic=0.5, lasso=1.
#' @param lambda_1se Largest value of lambda such that error is within 1
#'    standard error of the minimum.
#' @param box_width Number of features sampled when randomly sampling. By
#'     default box_width will be equal to the number of samples.
#' @param nfold Number of folds tested to find the optimal hyperparameter.
#' @param core Number of cores used when running in parallel.
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

    if (verbose) start_time <- Sys.time()
    x <- as.matrix(x)
    y <- as.matrix(y)
    number_of_features <- ncol(x)
    number_of_samples <- nrow(x)

    if (missing(box_width)) box_width <- number_of_samples
    if (cores == TRUE) {
        cores <- detectCores()
        if (verbose) cat(paste("[Detected", cores, "Cores]"))
    }
    if (missing(bootstraps)) {
        bootstraps <- ceiling(number_of_features / box_width) * 40
    }

    if (verbose) {
        cat("\nPart 1 of 2:\n")
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
    }

    .part1 <- function(ii, x, y, start_time) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }

        random_features <- sample(number_of_features, box_width, replace=FALSE)
        random_samples <- sample(number_of_samples, replace=TRUE)

        random_x <- x[random_samples, random_features]
        random_y <- y[random_samples, ]

        random_y_mean <- mean(random_y)
        random_y_scaled <- random_y - random_y_mean

        random_x_mean <- apply(random_x, 2, mean)
        random_x_scaled <- scale(random_x, random_x_mean, FALSE)
        std_dev <- sqrt(apply(random_x_scaled ^ 2, 2, sum))
        random_x_scaled <- scale(random_x_scaled, FALSE, std_dev)

        beta_hat <- replicate(number_of_features, 0)
        beta_hat[random_features] <- Lasso(random_x_scaled, random_y_scaled,
                                           alpha[1], nfold,
                                           lambda_1se[1]) / std_dev
        return(beta_hat)
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                .part1, x, y, as.numeric(Sys.time()))
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                  .part1, x, y, as.numeric(Sys.time()),
                                  mc.cores=cores)
    }

    importance_measure <- Reduce('+', lapply(list_beta_hat, abs)) + 10E-9

    .part2 <- function(ii, x, y, start_time) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }

        random_features <- sample(number_of_features, box_width,
                                  replace=FALSE, prob=importance_measure)
        random_samples <- sample(number_of_samples, replace=TRUE)

        random_x <- x[random_samples, random_features]
        random_y <- y[random_samples, ]
        random_importance <- importance_measure[random_features]

        random_y_mean <- mean(random_y)
        random_y_scaled <- random_y - random_y_mean

        random_x_mean <- apply(random_x, 2, mean)
        random_x_scaled <- scale(random_x, random_x_mean, FALSE)
        std_dev <- sqrt(apply(random_x_scaled ^ 2, 2, sum))
        random_x_scaled <- scale(random_x_scaled, FALSE, std_dev)

        beta_hat <- replicate(number_of_features, 0)
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
                                .part2, x, y, as.numeric(Sys.time()))
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                  .part2, x, y, as.numeric(Sys.time()),
                                  mc.cores=cores)
    }

    if (verbose) {
        cat("\n[Done]\n")
        print(Sys.time() - start_time)
        close(pb)
    }

    reduced_beta_hat <- Reduce('+', list_beta_hat) / bootstraps
    reduced_beta_hat <- matrix(reduced_beta_hat,
                               nrow=number_of_features,
                               ncol=1)
    rownames(reduced_beta_hat) <- colnames(x)
    colnames(reduced_beta_hat) <- "Coefficients"
    if (verbose_output) {
        return(list(beta_hat = reduced_beta_hat,
                    importance_measure = importance_measure))
    }
    return(reduced_beta_hat)
}