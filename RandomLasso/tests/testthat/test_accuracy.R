context("Hi-Lasso Accuracy Test\n")

ITERATIONS = 10
RESULTS = list()

source("../../../Tests/func/SimulateTestData.R")

for (ff in 1:2) {
    load(paste0("../../data/sim", ff, ".RData"))
    coef <- list(matrix(0, nrow = ncol(x[[1]]), ncol = ITERATIONS))

    cat(paste0("Running Hi-Lasso on Simulated Dataset ", ff, "...\n"))
    for (ii in 1:ITERATIONS) {
        temp <- HiLasso(x[[ii]], y[[ii]], alpha = c(0.5, 1),
                        cores = TRUE, verbose = FALSE)
        coef[[1]][ ,ii] <- temp
    }

    s <- list(coef=coef, x=x, y=y, x.val=x_val, y.val=y_val,
              cov.x=x_cov, ground.truth=beta0)
    source("../../../Tests/func/RunAccuracyTests.R")
    RESULTS[[ff]] <- RunAccuracyTest(s, "", "", 1, ITERATIONS, "", FALSE)
}

accuracy_file <- "../../data/expected_accuracy.RData"
test_that("Binary file for accuracy testing exists.", {
    expect_true(file.exists(accuracy_file))
})

load(accuracy_file)

test_that("CSV row-2 column-4 should equal 0.742846772795382.", {
    expect_equal(expected_accuracy[4,2], 0.742846772795382)
})

cat("Starting RMSE/F1 Unit Testing...\n")
for (ff in 1:2) {

    # Insuring the RMSE does not deviate too far from past averages.
    rmse_result = RESULTS[[ff]]$rmse[length(RESULTS[[ff]]$rmse)]
    cat(paste0("RMSE [Sim", ff, "]: ",
               "\n\tExpected: ", expected_accuracy[ff, 1],
               "\n\tRecieved: ", rmse_result, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        T1 = rmse_result < (expected_accuracy[ff, 1] * tt)
        test_that(paste0("RMSE less than ", tt, "% of expected. [Sim", ff, "]"), {
            expect_true(T1)
        })
    }

    # Insuring the F1 does not deviate too far from past averages.
    f1_result = RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)]
    cat(paste0("F1 [sim", ff, "]: ",
               "\n\tExpected: ", expected_accuracy[ff, 2],
               "\n\tRecieved: ", f1_result, "\n"))

    for (tt in seq(1.15, 1.03, by = -0.01)) {
        T1 = f1_result < (expected_accuracy[ff, 2] * tt)
        test_that(paste0("F1 less than ", tt,"% of expected. [Sim", ff, "]"), {
            expect_true(T1)
        })
    }
}
