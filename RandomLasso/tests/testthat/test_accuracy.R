context("Hi-Lasso Accuracy Test\n")

ITERATIONS = 10
RESULTS = list()

source("../../../Tests/func/SimulateTestData.R")

for (ff in 1:2) {
    cat(paste0("Loading Simulated Dataset ", ff, "...\n"))
    load(paste0("../../data/sim", ff, ".RData"))
    coef <- list(matrix(0, nrow = ncol(x[[1]]), ncol = ITERATIONS))

    for (ii in 1:ITERATIONS) {
        temp <- HiLasso(x[[ii]], y[[ii]],
                        alpha = c(0.5, 1),
                        cores = TRUE,
                        verbose = FALSE)
        coef[[1]][ ,ii] <- temp
    }

    s <- list(coef=coef, x=x, y=y, x.val=x_val, y.val=y_val,
              cov.x=x_cov, ground.truth=beta0)
    source("../../../Tests/func/RunAccuracyTests.R")
    RESULTS[[ff]] <- RunAccuracyTest(s, "", "", 1, ITERATIONS, "", FALSE)
}

expected_accuracy_location <- "../../data/expected_accuracy.RData"
test_that("Binary for accuracy testing exists.", {
    expect_true(file.exists(expected_accuracy_location))
})

load(expected_accuracy_location)
test_that("CSV row-2 column-4 should equal 0.742846772795382.", {
    expect_equal(expected_accuracy[4,2], 0.742846772795382)
})

cat("Starting Accuracy Unit Testing...\n")

for (ff in 1:2) {
    test_that(paste0("RMSE within 7% of expectation. [Simulation", ff, "]"), {
        T1 = RESULTS[[ff]]$rmse[length(RESULTS[[ff]]$rmse)] < (expected_accuracy[ff, 1] * 1.07)
        expect_true(T1)
        if (T1) {
            cat("[RMSE] pass\n")
        } else {
            cat(paste0("[RMSE] Failed (HIGH)", ff, ": ",
                       "\n\tExpected: ", expected_accuracy[ff, 1],
                       " (ave)\t", expected_accuracy[ff, 1] * 1.07, " (max)",
                       "\n\tRecieved: ",
                       RESULTS[[ff]]$rmse[length(RESULTS[[ff]]$rmse)], "\n"))
        }
    })

    test_that(paste0("F1 within 11% of expectation. [Simulation", ff, "]"), {
        T2 = RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)] < (expected_accuracy[ff, 2] * 1.1)
        expect_true(T2)
        if (RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)] < (expected_accuracy[ff, 2] * 1.1)) {
            cat("[F1] pass\n")
        } else {
            cat(paste0("[F1] Failed [HIGH]", ff, ": ",
                       "\n\tExpected: ", expected_accuracy[ff, 2], " (ave)\t",
                       expected_accuracy[ff, 2] * 1.1, " (max)",
                       "\n\tRecieved: ",
                       RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)], "\n"))
        }
    })
}
