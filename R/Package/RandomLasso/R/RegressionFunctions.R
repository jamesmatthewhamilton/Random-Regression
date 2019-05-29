Lasso <- function(x, y, alpha) {
    # Alpha 1: Lasso
    # Alpha 0: Ridge Regression
    lambda.min <- cv.glmnet(x, y, type.measure = "mse",
                            nfold = 5, alpha = alpha)$lambda.min
    coefficients <- glmnet(x, y, lambda = lambda.min, alpha = alpha,
                           standardize = FALSE, intercept = FALSE)
    # Coeffiecents of random sample.
    hat.beta <- coef(coefficients)[-1]
    return(hat.beta)
}

AdaptiveLasso <- function(x, y, importance.measure) {
    # Alpha 1: Lasso
    # Alpha 0: Ridge Regression
    lambda.min <- cv.glmnet(x, y, type.measure = "mse",
                            nfold = 5, alpha = 1)$lambda.min
    coefficients <- glmnet(x, y, lambda = lambda.min, alpha = 1,
                           standardize = FALSE,intercept = FALSE,
                           penalty.factor = 1 / (importance.measure + 0.0000001))
    # Coeffiecents of random sample.
    hat.beta <- coef(coefficients)[-1]
    return(hat.beta)
}

RapidLasso <- function(x, y, alpha, lambda) {
    # Alpha 1: Lasso
    # Alpha 0: Ridge Regression
    coefficients <- glmnet(x, y, lambda = lambda, alpha = alpha,
                           standardize = FALSE, intercept = FALSE)
    # Coeffiecents of random sample.
    hat.beta <- coef(lasso.results)[-1]
    return(hat.beta)
}
