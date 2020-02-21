context("Hi-Lasso Accuracy Test")

ITERATIONS = 10
RESULTS = list()

source("../../../Tests/func/SimulateTestData.R")

for (ff in 1:2) {
    cat(paste0("Loading Simulated Dataset ", ff, "."))
    s <- SimulateTestData(paste0("../../../Tests/res/sim", ff, "_sig3_our.RData"), 1, ITERATIONS)
    
    for (ii in 1:ITERATIONS) {
        s$coef[[1]][,ii] <- HiLasso(s$x[[ii]], s$y[[ii]],
                                    alpha = c(0.5, 1),
                                    cores = TRUE,
                                    verbose = FALSE)
    }
    
    source("../../../Tests/func/RunAccuracyTests.R")
    RESULTS[[ff]] <- RunAccuracyTest(s, "", "", 1, ITERATIONS, "", FALSE)
}

check_csv <- "../../../Tests/res/Unit_Test.csv"
test_that("CSV for accuracy testing exists.", {
    expect_true(file.exists(check_csv))
})

check <- read.csv(check_csv)
test_that("CSV row-2 column-4 should equal 0.742846772795382.", {
    expect_equal(check[2,4], 0.742846772795382)
})

cat("Starting Accuracy Unit Testing...\n")

for (ff in 1:2) {
    test_that(paste0("RMSE within 7% of expectation. [Simulation", ff, "]"), {
        T1 = RESULTS[[ff]]$rmse[length(RESULTS[[ff]]$rmse)] < (check[1,ff] * 1.07)
        expect_true(T1)
        if (T1) {
            cat("[RMSE] pass\n")
        } else {
            cat(paste0("[RMSE] Failed (HIGH)", ff, ": ",
                       "\n\tExpected: ", check[1,ff],
                       " (ave)\t", check[1,ff] * 1.07, " (max)",
                       "\n\tRecieved: ",
                       RESULTS[[ff]]$rmse[length(RESULTS[[ff]]$rmse)], "\n"))
        }
    })
    
    test_that(paste0("F1 within 11% of expectation. [Simulation", ff, "]"), {
        T2 = RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)] < (check[2,ff] * 1.1)
        expect_true(T2)
        if (RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)] < (check[2,ff] * 1.1)) {
            cat("[F1] pass\n")
        } else {
            cat(paste0("[F1] Failed [HIGH]", ff, ": ",
                       "\n\tExpected: ", check[2,ff], " (ave)\t",
                       check[2,ff] * 1.1, " (max)",
                       "\n\tRecieved: ",
                       RESULTS[[ff]]$f1[length(RESULTS[[ff]]$f1)], "\n"))
        }
    })
}