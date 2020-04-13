context("Hi-Lasso Accuracy Test\n")
source("../helper/f1.R")
source("../helper/rmse.R")

tcout <- 3
sims <- c("sim1", "sim2", "sim3", "sim4")

# Insuring that file with expected RMSE/F1 exists.
accuracy_file <- "../../data/benchmark/benchmark.RData"
test_that("Binary file for accuracy testing exists.", {
  expect_true(file.exists(accuracy_file))
})

# Checking the integrity of the file with expected RMSE/F1.
load(accuracy_file)
test_that("CSV row-4 column-2 should equal 4.42590435433364", {
  expect_equal(as.character(benchmark[4,2]), "4.42590435433364")
})

for (ss in 1:length(sims)) {
    load(paste0("../../data/simulated/", sims[ss], "_1.RData"))
    cat(paste0("Running Hi-Lasso on Simulated Dataset ", sims[ss], " set 1...\n"))

    #coef <- mapply(HiLasso, x=x, y=y, MoreArgs=(list(alpha = c(0.5, 1))),
    #               cores=TRUE, verbose=FALSE)
    #coef <- split(coef, rep(1:ncol(coef), each=nrow(coef)))

    coef <- HiLasso(x=x, y=y, alpha = c(0.5, 1), cores=TRUE, verbose=FALSE)

    #rmse <- mapply(findRMSE, beta_hat=coef, x_val=x_val, y_val=y_val)
    #mean_rmse <- mean(rmse)
    #f1 <- mapply(findF1, beta_hat=coef, MoreArgs=(list(ground_truth=beta0)))
    #mean_f1 <- mean(f1)

    mean_rmse <- findRMSE(beta_hat=coef, x_val=x_val, y_val=y_val)
    mean_f1 <- findF1(beta_hat=coef, ground_truth=beta0)

    # Insuring the RMSE does not deviate too far from past averages.
    cat(paste0("RMSE [", sims[ss], "]: ",
               "\n\tExpected: ", benchmark[ss, 2],
               "\n\tRecieved: ", mean_rmse, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        test_that(paste0("RMSE less than ", tt, "% of expected. [", sims[ss], "]"), {
            expect_true(mean_rmse < (benchmark[ss, 2] * tt))
        })
    }

    # Insuring the F1 does not deviate too far from past averages.
    cat(paste0("F1 [", sims[ss], "]: ",
               "\n\tExpected: ", benchmark[ss, 3],
               "\n\tRecieved: ", mean_f1, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        test_that(paste0("F1 less than ", tt,"% of expected. [Sim", sims[ss], "]"), {
            expect_true(mean_f1 < (benchmark[ss, 3] * tt))
        })
    }
}
