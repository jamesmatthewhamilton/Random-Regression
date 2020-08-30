sampleRegressionData <- function(X, y, sample_size, importance_measure=NULL) {

    n_features <- ncol(X)
    n_samples <- nrow(X)

    if (sample_size > n_features) {
        stop("Sample size can not be greater than the number of features in X.")
    }

    random_feature_idx <- sample(n_features, sample_size,
                              replace=FALSE, prob=importance_measure)
    random_sample_idx <- sample(n_samples, replace=TRUE)

    random_X <- X[random_sample_idx, random_feature_idx]
    random_y <- y[random_sample_idx, ]

    sampleRegressionData <- list(X=random_X, y=random_y, feature_idx=random_feature_idx, sample_idx=random_sample_idx)
    class(sampleRegressionData) <- "sampleRegressionData"

    return(sampleRegressionData)
}
