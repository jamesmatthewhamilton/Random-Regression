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
                                     cores=TRUE,
                                     verbose=FALSE)
{

    if (!cores && verbose) {
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
        start_time <- as.numeric(Sys.time())
    } else {
        pb <-  NULL
        start_time <- NULL
    }

    args <- list(FUN=singleRandomBootstrap,
                 ii=seq_len(bootstraps),
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

    if (cores) {
        bp_args <- list()
        if (!missing(cores)) {
            bp_args <- do.call(c, list(bp_args, list(workers=cores)))
        }
        if (!missing(seed)) {
            bp_args <- do.call(c, list(bp_args, list(RNGseed=seed)))
        }
        param <- MulticoreParam(progressbar=verbose, tasks=bootstraps)
        args <- do.call(c, list(args, list(BPPARAM=param)))

        list_beta_hat <- do.call("bpmapply", args)
    } else {
        list_beta_hat <- do.call("mapply", args)
    }

    return(list_beta_hat)
}
