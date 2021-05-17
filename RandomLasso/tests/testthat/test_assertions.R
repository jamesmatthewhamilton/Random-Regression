context("Basic Function I/O Tests\n")
cat(paste0("[Unit Test] Testing basic function I/O.\n"))

test_that("Cannot sample features greater than features/columns.", {
    expect_error(sampleRegressionData(
        matrix(0.0, nrow = 10, ncol = 100),
        matrix(0.0, nrow = 10, ncol = 1), 101,
        importance_measure = NULL
    ))

    expect_error(sampleRegressionData(
        matrix(0.0, nrow = 10, ncol = 100),
        matrix(0.0, nrow = 10, ncol = 1), 99,
        importance_measure = NULL
    ), regexp = NA)
})
