#' Performs variable selection and regularization using Hi Lasso.
#'
#' @param x Matrix of independent data.
#' @param y Matrix or vector of dependent data.
#' @param bootstraps Number of times features are randomly sampled.
#' @param alpha Regression method, e.g. ridge=0, elastic=0.5, lasso=1.
#' @param lambda_1se Largest value of lambda such that error is within 1
#'    standard error of the minimum.
#' @param sample_size Number of features sampled when randomly sampling. By
#'    default sample_size will be equal to the number of samples.
#' @param nfold Number of folds tested to find the optimal hyperparameter.
#' @param cores Number of cores used when running in parallel.
#' @param verbose Supresses all printing and time estimation.
#' @param verbose_output Returns additional information.
#' @keywords
#' @author James Matthew Hamilton
#' @export
#' @examples
#' RandomLasso(x, y)
#' RandomLasso(x, y, verbose=FALSE, bootstraps=300)
#'

HiLasso <- function(x, y,
                    bootstraps,
                    alpha=c(0.5, 1),
                    sample_size,
                    lambda_1se=c(FALSE, FALSE),
                    nfold=5,
                    cores=FALSE,
                    verbose=TRUE,
                    verbose_output=FALSE) {

    if (verbose) func_start_time <- Sys.time()
    if (verbose) message("Starting Hi-Lasso Algorithm...")

    ## ---------------- Setting Variables | START ----------------
    x <- as.matrix(x)  # TODO remove
    y <- as.matrix(y)
    n_features <- ncol(x)
    n_samples <- nrow(x)

    if (missing(sample_size)) sample_size <- n_samples
    if (cores == TRUE) {
        cores <- detectCores()
        if (verbose) message("Using auto-detected ", cores, " cores.")
    }
    if (missing(bootstraps)) {
        bootstraps <- ceiling(n_features / sample_size) * 40
        if (verbose) message("Setting bootstraps to ", bootstraps, ".")
    }
    ## ---------------- Setting Variables | END ----------------

    ## ---------------- Checking Inputs | START ----------------
    if(!all(is.na(x) == FALSE)) stop("Error: NA values detected in x.\n")
    if(!all(is.na(y) == FALSE)) stop("Error: NA values detected in y.\n")
    ## ---------------- Checking Inputs | END ----------------

    pb = NULL
    start_time = NULL

    if (verbose) {
        cat("\nPart 1 of 2:\n")
        if(!cores) {
            pb <- txtProgressBar(min=0, max=bootstraps, style=3)
            start_time = as.numeric(Sys.time())
        }
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                singleRandomBootstrap,
                                x,
                                y,
                                pb,
                                start_time,
                                bootstraps,
                                sample_size,
                                alpha[1],
                                nfold,
                                lambda_1se[1],
                                NULL,
                                method="Regression",
                                verbose)
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                singleRandomBootstrap,
                                x,
                                y,
                                pb,
                                start_time,
                                bootstraps,
                                sample_size,
                                alpha[1],
                                nfold,
                                lambda_1se[1],
                                NULL,
                                method="Regression",
                                verbose,
                                mc.cores=cores)
    }
    importance_measure <- Reduce('+', lapply(list_beta_hat, abs)) + 5e-324

    if (verbose) {
        cat("\nPart 2 of 2:\n")
        if (!cores) {
            pb <- txtProgressBar(min=0, max=bootstraps, style=3)
            start_time = as.numeric(Sys.time())
        }
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                singleRandomBootstrap,
                                x,
                                y,
                                pb,
                                start_time,
                                bootstraps,
                                sample_size,
                                alpha[2],
                                nfold,
                                lambda_1se[2],
                                importance_measure,
                                method="Adaptive",
                                verbose)
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                singleRandomBootstrap,
                                x,
                                y,
                                pb,
                                start_time,
                                bootstraps,
                                sample_size,
                                alpha[2],
                                nfold,
                                lambda_1se[2],
                                importance_measure,
                                method="Adaptive",
                                verbose,
                                mc.cores=cores)
    }

    if (verbose) {
        cat("\n[Done]\n")
        print(Sys.time() - func_start_time)
        if (!cores) close(pb)
    }

    reduced_beta_hat <- Reduce('+', list_beta_hat) / bootstraps
    reduced_beta_hat <- matrix(reduced_beta_hat,
                               nrow=n_features,
                               ncol=1)
    rownames(reduced_beta_hat) <- colnames(x)
    colnames(reduced_beta_hat) <- "Coefficients"
    if (verbose_output) {
        return(list(beta_hat = reduced_beta_hat,
                    importance_measure = importance_measure))
    }
    return(reduced_beta_hat)
}
