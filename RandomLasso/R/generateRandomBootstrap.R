generateRandomBootstrap <- function(ii, X, y, pb, start_time, bootstraps, sample_size, alpha, nfold, lambda_1se, importance_measure=NULL, method="Regression", verbose) {
    if (verbose) {
        .continue.progress.bar(pb, start_time, ii, bootstraps)
    }
    n_features <- ncol(X)
    n_samples <- nrow(X)

    random_sample <- sampleRegressionData(X, y, sample_size, importance_measure=importance_measure)

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
                                           lambda_1se) / std_dev
    }
    else if (method == "Adaptive") {
        random_importance <- importance_measure[random_sample$feature_idx]

        beta_hat[random_sample$feature_idx] <- AdaptiveLasso(random_X_scaled,
                                                   random_y_scaled,
                                                   alpha,
                                                   random_importance,
                                                   nfold,
                                                   lambda_1se) / std_dev
    }
    return(beta_hat)
}
