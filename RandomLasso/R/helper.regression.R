Lasso <- function(x, y, alpha, nfold, lambda.1se = FALSE) {

    cv <- cv.glmnet(x, y, type.measure = "mse",
                    nfold = nfold, alpha = alpha)
    lambda = ifelse(lambda.1se, cv$lambda.1se, cv$lambda.min)

    coefficients <- glmnet(x, y, lambda = lambda, alpha = alpha,
                           standardize = FALSE, intercept = FALSE)

    hat.beta <- coef(coefficients, exact=TRUE)[-1]
    return(hat.beta)
}

ElasticNet <- function(x, y, nfold) { # Doctor Kim's Version

    test <- lapply(seq(0, 1, 0.1), function(ii, x,  y) {
        cv.glmnet(x, y, type.measure = "mse", nfold = nfold, alpha = ii)
    }, x, y)

    lambda <- lapply(test, function(test) test$lambda.min)
    cvm <- lapply(test, function(test) test$cvm[which.min(test$cvm)])

    coefficients <- glmnet(x, y, standardize = FALSE, intercept = FALSE,
                           lambda = as.numeric(lambda[which.min(cvm)]),
                           alpha = as.numeric(seq(0, 1, 0.1)[which.min(cvm)]))
    hat.beta <- coef(coefficients, exact=TRUE)[-1]
    return(hat.beta)
}

AdaptiveLasso <- function(x, y, alpha, importance.measure, nfold, lambda.1se = FALSE) {

    cv <- cv.glmnet(x, y, type.measure = "mse",
                    nfold = nfold, alpha = alpha)
    lambda = ifelse(lambda.1se, cv$lambda.1se, cv$lambda.min)

    coefficients <- glmnet(x, y, lambda = lambda, alpha = alpha,
                           standardize = FALSE, intercept = FALSE,
                           penalty.factor = (1 / abs(importance.measure)))
    hat.beta <- coef(coefficients)[-1]
    return(hat.beta)
}

AdaptiveLasso2 <- function(x, y, alpha, nfold) {
    
    cv <- cv.glmnet(x, y, type.measure = "mse",
                    nfold = nfold, alpha = 1)
    best_ridge_coef <- as.numeric(coef(cv, s = cv$lambda.min))[-1]
    coefficients <- glmnet(x, y, lambda = cv$lambda.min, alpha = alpha,
                           standardize = FALSE, intercept = FALSE,
                           penalty.factor = 1 / abs(best_ridge_coef))
    hat.beta <- coef(coefficients)[-1]
    return(hat.beta)
}

RapidLasso <- function(x, y, alpha, lambda) {
    coefficients <- glmnet(x, y, lambda = lambda, alpha = alpha,
                           standardize = FALSE, intercept = FALSE)

    hat.beta <- coef(coefficients)[-1]
    return(hat.beta)
}
