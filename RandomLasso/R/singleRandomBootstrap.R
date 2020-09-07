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
                                  seed,
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
                                          importance_measure=importance_measure,
                                          seed=seed)

    scaled_random_sample <- hiLassoStandardizationMethod(random_sample$X,
                                                         random_sample$y)

    beta_hat <- replicate(n_features, 0)

    if (method == "Regression") {
        beta_hat[random_sample$feature_idx] <- Lasso(scaled_random_sample$X,
                                                     scaled_random_sample$y,
                                                     alpha,
                                                     nfold,
                                                     lambda_1se)
    }
    else if (method == "Adaptive") {
        random_importance <- importance_measure[random_sample$feature_idx]

        beta_hat[random_sample$feature_idx] <- AdaptiveLasso(scaled_random_sample$X,
                                                             scaled_random_sample$y,
                                                             alpha,
                                                             random_importance,
                                                             nfold,
                                                             lambda_1se)
    }

    beta_hat[random_sample$feature_idx] <-
        beta_hat[random_sample$feature_idx] / scaled_random_sample$y_std_dev

    return(beta_hat)
}
