hiLassoStandardizationMethod <- function(X, y) {

    y_mean <- mean(y)
    y_scaled <- y - y_mean

    X_mean <- apply(X, 2, mean)
    X_scaled <- scale(X, X_mean, FALSE)
    y_sd <- sqrt(apply(X_scaled ^ 2, 2, sum)) + 5e-324
    X_scaled <- scale(X_scaled, FALSE, y_sd)

    return(
        new("Standardization",
            method="hiLassoStandardizationMethod",
            x=X_scaled,
            y=y_scaled,
            y_sd=y_sd
        )
    )
}
