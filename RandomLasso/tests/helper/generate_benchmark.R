source("tests/helper/f1.R")
source("tests/helper/rmse.R")

tcount <-  10
sims <- c("sim1", "sim2", "sim3", "sim4")

rmse <- list()
f1 <- list()
time <- list()

benchmark <- matrix(0.0, nrow = 4, ncol = 6)
colnames(benchmark) <- c("mean time", "mean rmse", "mean f1",
                         "sd time", "sd rmse", "sd f1")
rownames(benchmark) <- sims

# Run regression function on the same data over-and-over.
for (ss in 1:length(sims)) {
    cat(paste0("-------- Benchmarking ", sims[ss], " ---------\n"))
    load(paste0("data/simulated/", sims[ss], "_1.RData"))
    for (ii in 1:tcount) {
        cat(paste0("Running Hi-Lasso [", ii, " of ", tcount, "]...\n"))
        start_time <- Sys.time()
        coef <- HiLasso(x=x, y=y, cores=TRUE, verbose=FALSE)
        time[[ii]] <- as.numeric(Sys.time() - start_time, unit="secs")
        rmse[[ii]] <- findRMSE(beta_hat=coef, x_val=x_val, y_val=y_val)
        f1[[ii]] <- findF1(beta_hat=coef, ground_truth=beta0)
    }
    mean_time <- Reduce("+", time) / tcount
    mean_rmse <- Reduce("+", rmse) / tcount
    mean_f1 <- Reduce("+", f1) / tcount

    mean_sq_time <- Reduce("+", lapply(time, "^", 2)) / length(time)
    sd_time <- sqrt(mean_sq_time - mean_time^2)

    mean_sq_rmse <- Reduce("+", lapply(rmse, "^", 2)) / length(rmse)
    sd_rmse <- sqrt(mean_sq_rmse - mean_rmse^2)

    mean_sq_f1 <- Reduce("+", lapply(f1, "^", 2)) / length(f1)
    sd_f1 <- sqrt(mean_sq_f1 - mean_f1^2)

    benchmark[ss,] <- c(mean_time, mean_rmse, mean_f1,
                        sd_time, sd_rmse, sd_f1)
}

write.table(benchmark, file = "data/benchmark.csv")
save(benchmark, file = "data/benchmark.RData")
