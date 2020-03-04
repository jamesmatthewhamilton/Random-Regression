source("tests/helper/f1.R")
source("tests/helper/rmse.R")

tcount <-  10
sims <- c("sim1", "sim2", "sim3", "sim4")

rmse <- list()
f1 <- list()
time <- list()

benchmark <- matrix(0.0, nrow = 4, ncol = 3)
colnames(benchmark) <- c("time", "rmse", "f1")
rownames(benchmark) <- sims

# Run regression function on the same data over-and-over.
for (ss in 1:length(sims)) {
    cat(paste0("-------- Benchmarking ", sims[ss], " ---------\n"))
    load(paste0("data/sim/", sims[ss], "_1.RData"))
    for (ii in 1:tcount) {
        cat(paste0("Running Hi-Lasso [", ii, " of ", tcount, "]...\n"))
        start_time <- Sys.time()
        coef <- HiLasso(x=x, y=y, cores=TRUE, verbose=FALSE)
        time[[ii]] <- Sys.time() - start_time
        rmse[[ii]] <- findRMSE(beta_hat=coef, x_val=x_val, y_val=y_val)
        f1[[ii]] <- findF1(beta_hat=coef, ground_truth=beta0)
    }
    mean_time <- Reduce("+", time) / tcount
    mean_rmse <- Reduce("+", rmse) / tcount
    mean_f1 <- Reduce("+", f1) / tcount
    benchmark[ss,] <- c(mean_time, mean_rmse, mean_f1)
}

write.table(benchmark, file = "data/benchmark.csv")
save(benchmark, file = "data/benchmark.RData")



