multipleRandomBootstraps <- function(X, y,
                                     start_time,
                                     bootstraps,
                                     sample_size,
                                     alpha=1,
                                     nfold=5,
                                     lambda_1se=FALSE,
                                     importance_measure=NULL,
                                     method="Regression",
                                     seed,
                                     cores=FALSE,
                                     verbose=FALSE)
{

    if (!missing(seed)) {
        if (verbose) message("Setting seed=", seed, ".")
        set.seed(seed)
        list_of_seeds <- sample.int(10000000, bootstraps)
    }
    if (!cores) {
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
        start_time <- as.numeric(Sys.time())
    } else {
        pb <-  NULL
        start_time <- NULL
    }

    args <- list(FUN=singleRandomBootstrap,
                 ii=seq_len(bootstraps),
                 seed=list_of_seeds,
                 MoreArgs = list(
                     X=X,
                     y=y,
                     pb=pb,
                     start_time=start_time,
                     bootstraps=bootstraps,
                     sample_size=sample_size,
                     alpha=alpha,
                     nfold=nfold,
                     lambda_1se=lambda_1se,
                     importance_measure=importance_measure,
                     method=method,
                     verbose=verbose))

    if (!cores) {
        list_beta_hat <- do.call("mapply", args)
    } else {
        args <- do.call(c, list(args, list(mc.cores=cores)))
        list_beta_hat <- do.call("mcmapply", args)
    }

    return(list_beta_hat)
}
