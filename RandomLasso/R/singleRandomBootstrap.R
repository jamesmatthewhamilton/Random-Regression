singleRandomBootstrap <- function(ii, X, y,
                                  pb,
                                  start_time,
                                  bootstraps,
                                  sample_size,
                                  alpha=1,
                                  nfold=5,
                                  lambda_1se=FALSE,
                                  importance_measure=NULL,
                                  method="Regression",
                                  verbose=FALSE)
{
    ## Progress bar pre-check.
    if (is.null(pb) != is.null(start_time)) {
        warning("Both pb and start_time must be test to use the progress bar.",
                " Forcing pb=NULL and start_time=NULL.")
        pb = NULL
        start_time = NULL
    }

    ## Progress bar utility.
    if (!is.null(pb) && !is.null(start_time)) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }
    }

    n_features <- ncol(X)
    n_samples <- nrow(X)

    if (missing(sample_size)) sample_size <- n_samples
    if (missing(bootstraps)) {
        bootstraps <- ceiling(n_features / sample_size) * 40
    }

    random_sample <- sampleRegressionData(X, y,
                                          sample_size,
                                          importance_measure=importance_measure)

    random_y_mean <- mean(random_sample$y)
    random_y_scaled <- random_sample$y - random_y_mean

    random_X_mean <- apply(random_sample$X, 2, mean)
    random_X_scaled <- scale(random_sample$X, random_X_mean, FALSE)
    std_dev <- sqrt(apply(random_X_scaled ^ 2, 2, sum)) + 5e-324
    random_X_scaled <- scale(random_X_scaled, FALSE, std_dev)

    beta_hat <- replicate(n_features, 0)

    if (method == "Regression") {
        beta_hat[random_sample$feature_idx] <- Lasso(random_X_scaled,
                                                     random_y_scaled,
                                                     alpha,
                                                     nfold,
                                                     lambda_1se)
        beta_hat[random_sample$feature_idx] <-
            beta_hat[random_sample$feature_idx] / std_dev
    }
    else if (method == "Adaptive") {
        random_importance <- importance_measure[random_sample$feature_idx]

        beta_hat[random_sample$feature_idx] <- AdaptiveLasso(random_X_scaled,
                                                             random_y_scaled,
                                                             alpha,
                                                             random_importance,
                                                             nfold,
                                                             lambda_1se)
        beta_hat[random_sample$feature_idx] <-
            beta_hat[random_sample$feature_idx] / std_dev
    }
    return(beta_hat)
}
