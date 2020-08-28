randomBootstrap <- function(ii, X, y, pb, start_time, bootstraps, box_width, alpha, nfold, lambda_1se, importance_measure=NULL, method="Regression", verbose) {
    if (verbose) {
        .continue.progress.bar(pb, start_time, ii, bootstraps)
    }
    n_features <- ncol(X)
    n_samples <- nrow(X)

    random_features <- sample(n_features, box_width, replace=FALSE, prob=importance_measure)
    random_samples <- sample(n_samples, replace=TRUE)

    random_X <- X[random_samples, random_features]
    random_y <- y[random_samples, ]

    random_y_mean <- mean(random_y)
    random_y_scaled <- random_y - random_y_mean

    random_X_mean <- apply(random_X, 2, mean)
    random_X_scaled <- scale(random_X, random_X_mean, FALSE)
    std_dev <- sqrt(apply(random_X_scaled ^ 2, 2, sum)) + 5e-324
    random_X_scaled <- scale(random_X_scaled, FALSE, std_dev)

    beta_hat <- replicate(n_features, 0)

    if (method == "Regression") {
        beta_hat[random_features] <- Lasso(random_X_scaled,
                                           random_y_scaled,
                                           alpha,
                                           nfold,
                                           lambda_1se) / std_dev
    }
    else if (method == "Adaptive") {
        random_importance <- importance_measure[random_features]

        beta_hat[random_features] <- AdaptiveLasso(random_X_scaled,
                                                   random_y_scaled,
                                                   alpha,
                                                   random_importance,
                                                   nfold,
                                                   lambda_1se) / std_dev
    }
    return(beta_hat)
}
