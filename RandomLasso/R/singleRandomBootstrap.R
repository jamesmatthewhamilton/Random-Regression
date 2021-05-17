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
        warning("Both pb and start_time must be set to use the progress bar.",
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

    scaled_random_sample <- hiLassoStandardizationMethod(random_sample@x,
                                                         random_sample@y)

    beta_hat <- replicate(n_features, 0)

    if (method == "Regression") {

        regression_object <- Lasso(scaled_random_sample@x,
                                   scaled_random_sample@y,
                                   alpha,
                                   nfold,
                                   lambda_1se)

        beta_hat[random_sample@features_sampled] <- regression_object@coef
    }
    else if (method == "Adaptive") {
        random_importance <- importance_measure[random_sample@features_sampled]

        regression_object <- AdaptiveLasso(scaled_random_sample@x,
                                           scaled_random_sample@y,
                                           alpha,
                                           random_importance,
                                           nfold,
                                           lambda_1se)

        beta_hat[random_sample@features_sampled] <- regression_object@coef
    }

    beta_hat[random_sample@features_sampled] <-
        beta_hat[random_sample@features_sampled] / scaled_random_sample@y_sd

    return(beta_hat)
}
