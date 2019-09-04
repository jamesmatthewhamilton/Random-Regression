#' Performs variable selection and regularization using Random Lasso.
#'
#' @param x Matrix of intependent data.
#' @param y Matrix of dependent data.
#' @param bootstraps Number of random bootstraps taken.
#' @param alpha Regression method, e.g. least square, ridge, net elastic, lasso.
#' @param nfold Number of folds run to find optimal hyperparameter.
#' @param verbose Supresses all printing and time estimating functions.
#' @param test Tests the functions performance. Returns additional parameters.
#' @keywords
#' @author James Matthew Hamilton
#' @export
#' @examples
#' RandomLasso(x, y)
#' RandomLasso(x, y, verbose = FALSE, bootstraps = 300)
#'

HiLasso <- function(x, y, bootstraps, alpha = 1,
                    nfold = 5, verbose = TRUE, test = FALSE) {

    if (test) {start = as.numeric(Sys.time())}
    x <- as.matrix(x)
    y <- as.matrix(y)
    features <- ncol(x)
    samples <- nrow(x)

    if (missing(bootstraps)) {
        bootstraps <- round(features / samples) * 80
    }
    
    if (verbose) {
        cat("\nPart 1 of 2:\n")
        pb <- txtProgressBar(min = 0, max = bootstraps, style = 3)
    }
    
    .part1 <- function(ii, x, y, start_time) {
        if (verbose) {
            .helper.time.remaining(pb, start_time, ii, bootstraps)
        }
        
        random.features <- sample(features, samples, replace = FALSE)
        random.samples <- sample(samples, replace = TRUE)
        
        random.x <- x[random.samples, random.features]
        random.y <- y[random.samples, ]

        random.y.mean <- mean(random.y)
        random.y.scaled <- random.y - random.y.mean

        random.x.mean <- apply(random.x, 2, mean)
        random.x.scaled <- scale(random.x, random.x.mean, FALSE)
        standard.deviation <- sqrt(apply(random.x.scaled ^ 2, 2, sum))
        random.x.scaled <- scale(random.x.scaled, FALSE, standard.deviation)

        beta.hat <- replicate(features, 0)
        beta.hat[random.features] <- Lasso(random.x.scaled,
                                           random.y.scaled, alpha = 0.5,
                                           nfold) / standard.deviation
        return(beta.hat)
    }
    
    list.beta.hat <- lapply(seq_len(bootstraps), .part1, x, y, as.numeric(Sys.time()))
    importance.measure <- Reduce('+', lapply(list.beta.hat, abs))

    .part2 <- function(ii, x, y, start_time) {
        if (verbose) {
            .helper.time.remaining(pb, start_time, ii, bootstraps)
        }
        
        random.features <- sample(features, samples, replace = FALSE,
                                  prob = importance.measure)
        random.samples <- sample(samples, replace = TRUE)
        
        random.x <- x[random.samples, random.features]
        random.y <- y[random.samples, ]
        random.importance <- importance.measure[random.features]
        
        random.y.mean <- mean(random.y)
        random.y.scaled <- random.y - random.y.mean
        
        random.x.mean <- apply(random.x, 2, mean)
        random.x.scaled <- scale(random.x, random.x.mean, FALSE)
        standard.deviation <- sqrt(apply(random.x.scaled ^ 2, 2, sum))
        random.x.scaled <- scale(random.x.scaled, FALSE, standard.deviation)

        beta.hat <- replicate(features, 0)
        beta.hat[random.features] <- AdaptiveLasso(random.x.scaled,
                                                   random.y.scaled,
                                                   random.importance,
                                                   nfold) / standard.deviation
        return(beta.hat)
    }
    
    if (verbose) {
        cat("\nPart 2 of 2:\n")
        pb <- txtProgressBar(min = 0, max = bootstraps, style = 3)
    }
    
    list.beta.hat <- lapply(seq_len(bootstraps), .part2, x, y, as.numeric(Sys.time()))
    
    if (verbose) {
        cat("\n[Done]\n")
        close(pb)
    }
    
    reduced.beta.hat <- Reduce('+', list.beta.hat) / bootstraps
    reduced.beta.hat <- matrix(reduced.beta.hat, nrow = features, ncol = 1)
    rownames(reduced.beta.hat) <- colnames(x)
    colnames(reduced.beta.hat) <- "Coefficients"
    if (test) {
        return(list(reduced.beta.hat, features, samples, bootstraps, as.numeric(Sys.time()) - start))
    }
    return(reduced.beta.hat)
}
