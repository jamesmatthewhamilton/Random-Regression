multipleRandomBootstraps <- function(X, y,
                                     start_time,
                                     bootstraps,
                                     sample_size,
                                     alpha=1,
                                     nfold=5,
                                     lambda_1se=FALSE,
                                     importance_measure=NULL,
                                     method="Regression",
                                     cores=FALSE,
                                     verbose=FALSE)
{

    if (!cores) {
        pb <- txtProgressBar(min=0, max=bootstraps, style=3)
        start_time <- as.numeric(Sys.time())
    } else {
        pb <-  NULL
        start_time <- NULL
    }

    if (cores < 2) {
        list_beta_hat <- lapply(seq_len(bootstraps),
                                singleRandomBootstrap,
                                X,
                                y,
                                pb,
                                start_time,
                                bootstraps,
                                sample_size,
                                alpha,
                                nfold,
                                lambda_1se,
                                importance_measure,
                                method=method,
                                verbose)
    } else {
        list_beta_hat <- mclapply(seq_len(bootstraps),
                                  singleRandomBootstrap,
                                  X,
                                  y,
                                  pb,
                                  start_time,
                                  bootstraps,
                                  sample_size,
                                  alpha,
                                  nfold,
                                  lambda_1se,
                                  importance_measure,
                                  method=method,
                                  verbose,
                                  mc.cores=cores)
    }
    return(list_beta_hat)
}
