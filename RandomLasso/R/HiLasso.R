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

HiLasso <- function(x, y, bootstraps, alpha = c(0.5, 1), box.width,
                    nfold = 5, cores = FALSE, verbose = TRUE, test = FALSE) {

    if (test) {start = as.numeric(Sys.time())}
    x <- as.matrix(x)
    y <- as.matrix(y)
    number.of.features <- ncol(x)
    number.of.samples <- nrow(x)

    if (missing(box.width)) {
        box.width <- number.of.samples
    }

    if (missing(bootstraps)) {
        bootstraps <- ceiling(number.of.features / box.width) * 40
    }

    if (cores == TRUE) {
        cores <- detectCores()
    }

    if (verbose) {
        cat("\nPart 1 of 2:\n")
        pb <- txtProgressBar(min = 0, max = bootstraps, style = 3)
    }

    .part1 <- function(ii, x, y, start_time) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }

        random.features <- sample(number.of.features, box.width, replace = FALSE)
        random.samples <- sample(number.of.samples, replace = TRUE)

        random.x <- x[random.samples, random.features]
        random.y <- y[random.samples, ]

        random.y.mean <- mean(random.y)
        random.y.scaled <- random.y - random.y.mean

        random.x.mean <- apply(random.x, 2, mean)
        random.x.scaled <- scale(random.x, random.x.mean, FALSE)
        standard.deviation <- sqrt(apply(random.x.scaled ^ 2, 2, sum))
        random.x.scaled <- scale(random.x.scaled, FALSE, standard.deviation)

        beta.hat <- replicate(number.of.features, 0)
        beta.hat[random.features] <- Lasso(random.x.scaled,
                                           random.y.scaled,
                                           alpha[1],
                                           nfold) / standard.deviation
        return(beta.hat)
    }

    if (cores < 2) {
        list.beta.hat <- lapply(seq_len(bootstraps), .part1, x, y, as.numeric(Sys.time()))
    } else {
        list.beta.hat <- mclapply(seq_len(bootstraps), .part1, x, y, as.numeric(Sys.time()), mc.cores = cores)
    }

    importance.measure <- Reduce('+', lapply(list.beta.hat, abs)) + 10E-10 # Should release from memory.

    .sample <- function(ii, x, y) {
        random.features <- sample(number.of.features, box.width, replace = FALSE,
                                  prob = importance.measure)
        return(random.features)
    }

    .hits <- function(random.features, x, y) {
        random.hits <- replicate(number.of.features, 0)
        random.hits[random.features] <- 1
        return(random.hits)
    }

    .part2 <- function(random.features, x, y, start_time) {
        if (verbose) {
            .continue.progress.bar(pb, start_time, ii, bootstraps)
        }

        random.samples <- sample(number.of.samples, replace = TRUE)

        random.x <- x[random.samples, random.features]
        random.y <- y[random.samples, ]
        random.importance <- importance.measure[random.features]

        random.y.mean <- mean(random.y)
        random.y.scaled <- random.y - random.y.mean

        random.x.mean <- apply(random.x, 2, mean)
        random.x.scaled <- scale(random.x, random.x.mean, FALSE)
        standard.deviation <- sqrt(apply(random.x.scaled ^ 2, 2, sum))
        random.x.scaled <- scale(random.x.scaled, FALSE, standard.deviation)

        beta.hat <- replicate(number.of.features, 0)
        beta.hat[random.features] <- AdaptiveLasso(random.x.scaled,
                                                   random.y.scaled,
                                                   alpha[2],
                                                   random.importance,
                                                   nfold) / standard.deviation
        return(beta.hat)
    }

    if (verbose) {
        cat("\nPart 2 of 2:\n")
        pb <- txtProgressBar(min = 0, max = bootstraps, style = 3)
    }

    if (cores < 2) {
        random.features <- lapply(seq_len(bootstraps), .sample, x, y)
        hits <- lapply(random.features, .hits, x, y)
        list.beta.hat <- lapply(random.features, .part2, x, y, as.numeric(Sys.time()))
    } else {
        random.features <- mclapply(seq_len(bootstraps), .sample, x, y, mc.cores = cores)
        hits <- mclapply(random.features, .hits, x, y, mc.cores = cores)
        list.beta.hat <- mclapply(random.features, .part2, x, y, as.numeric(Sys.time()), mc.cores = cores)
    }

    if (verbose) {
        cat("\n[Done]\n")
        close(pb)
    }

    hit.count <- Reduce('+', hits)
    reduced.beta.hat <- Reduce('+', list.beta.hat) / hit.count
    reduced.beta.hat <- matrix(reduced.beta.hat, nrow = number.of.features, ncol = 1)
    reduced.beta.hat[is.na(reduced.beta.hat)] <- 0
    rownames(reduced.beta.hat) <- colnames(x)
    colnames(reduced.beta.hat) <- "Coefficients"
    if (test) {
        return(list(reduced.beta.hat, number.of.features, number.of.samples, bootstraps, box.width, as.numeric(Sys.time()) - start))
    }
    return(reduced.beta.hat)
}
