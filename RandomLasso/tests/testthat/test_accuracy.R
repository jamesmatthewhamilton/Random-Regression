context("Hi-Lasso Accuracy Test\n")
source("../helper/f1.R")
source("../helper/rmse.R")

# Insuring that file with expected RMSE/F1 exists.
accuracy_file <- "../../data/expected_accuracy.RData"
test_that("Binary file for accuracy testing exists.", {
  expect_true(file.exists(accuracy_file))
})

# Checking the integrity of the file with expected RMSE/F1.
load(accuracy_file)
test_that("CSV row-2 column-4 should equal 0.742846772795382.", {
  expect_equal(expected_accuracy[4,2], 0.742846772795382)
})

for (ff in 1:2) {
    load(paste0("../../data/sim", ff, ".RData"))
    cat(paste0("Running Hi-Lasso on Simulated Dataset ", ff, "...\n"))

    coef <- mapply(HiLasso, x=x, y=y, MoreArgs=(list(alpha = c(0.5, 1))),
                   cores=TRUE, verbose=FALSE)
    coef <- split(coef, rep(1:ncol(coef), each=nrow(coef)))

    rmse <- mapply(findRMSE, beta_hat=coef, x_val=x_val, y_val=y_val)
    mean_rmse <- mean(rmse)
    f1 <- mapply(findF1, beta_hat=coef, MoreArgs=(list(ground_truth=beta0)))
    mean_f1 <- mean(f1)

    # Insuring the RMSE does not deviate too far from past averages.
    cat(paste0("RMSE [Sim", ff, "]: ",
               "\n\tExpected: ", expected_accuracy[ff, 1],
               "\n\tRecieved: ", mean_rmse, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        test_that(paste0("RMSE less than ", tt, "% of expected. [Sim", ff, "]"), {
            expect_true(mean_rmse < (expected_accuracy[ff, 1] * tt))
        })
    }

    # Insuring the F1 does not deviate too far from past averages.
    cat(paste0("F1 [sim", ff, "]: ",
               "\n\tExpected: ", expected_accuracy[ff, 2],
               "\n\tRecieved: ", mean_f1, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        test_that(paste0("F1 less than ", tt,"% of expected. [Sim", ff, "]"), {
            expect_true(mean_f1 < (expected_accuracy[ff, 2] * tt))
        })
    }
}
