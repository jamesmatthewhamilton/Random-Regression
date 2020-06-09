context("Hi-Lasso Accuracy Test\n")
source("../helper/f1.R")
source("../helper/rmse.R")

sims <- c("sim1", "sim2", "sim3", "sim4")
count_of_tests <- length(sims)

accuracy_file <- "../../data/benchmark/benchmark.RData"
test_that("Binary file for accuracy testing exists.", {
  expect_true(file.exists(accuracy_file))
})
load(accuracy_file)

test_that("Dataframe row-4 column-2 should equal 4.42590435433364", {
  expect_equal(as.character(benchmark[4,2]), "4.42590435433364")
})

results <-sapply(sims, function(sims) {
    load(paste0("../../data/simulated/", sims, "_1.RData"))
    cat(paste0("Running Hi-Lasso on Simulated Dataset ", sims, "\n"))
    coef <- HiLasso(x=x, y=y, alpha = c(0.5, 1), cores=TRUE, verbose=FALSE)
    rmse <- findRMSE(beta_hat=coef, x_val=x_val, y_val=y_val)
    f1 <- findF1(beta_hat=coef, ground_truth=beta0)
    return(c(rmse, f1))
})

rmse <- results[1, ]
f1 <- results[2, ]

rmse_benchmark <- benchmark[1:count_of_tests, 2]
rmse_benchmark_sd <- benchmark[1:count_of_tests, 5]

f1_benchmark <- benchmark[1:count_of_tests, 3]
f1_benchmark_sd <- benchmark[1:count_of_tests, 6]

validateRMSE <- function(sims, rmse, rmse_benchmark, rmse_benchmark_sd) {
    cat(paste0("RMSE [", sims, "]: ",
               "\n\tExpected: ", rmse_benchmark,
               "\n\tRecieved: ", rmse, "\n"))
    test_that(paste0("RMSE is 3 sd's from expected. [", sims, "]"), {
        expect_true(rmse < (rmse_benchmark + (3 * rmse_benchmark_sd)))
    })
}
mapply(validateRMSE, sims, rmse, rmse_benchmark, rmse_benchmark_sd)

validateF1 <- function(sims, f1, f1_benchmark, f1_benchmark_sd) {
  cat(paste0("F1 [", sims, "]: ",
             "\n\tExpected: ", f1_benchmark,
             "\n\tRecieved: ", f1, "\n"))
  test_that(paste0("F1 is 3 sd's from expected. [", sims, "]"), {
    expect_true(f1 > (f1_benchmark - (3 * f1_benchmark_sd)))
  })
}
mapply(validateF1, sims, f1, f1_benchmark, f1_benchmark_sd)
