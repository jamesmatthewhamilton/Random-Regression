## ----------------------------
## random_cross_fold_validation.R
##
## Author: James Matthew Hamilton
## Created: 2020-06-10
## Email:
## ---------------------------

source("R/helper.regression.R")
load(file = "data/real/RefinedXY.Rdata")
library(RandomLasso)
library(glmnet)

# ---------------- Run Random Fold Cross Validation ----------------
coef = list()
testIndex = list()
total_tests = 0
aggregate_wins_rand = 0
aggregate_wins_lasso = 0
aggregate_correct_rand = 0
aggregate_correct_lasso = 0
unique_classes = unique(y2)
y2 <- as.matrix(y2)

for (ii in 1:400) {
    testIndex[[ii]] = c(sample(1:round(length(y2) * 0.2)))
}

for(ii in 1:length(testIndex)) {
    print(testIndex[[ii]])
    testY <- y2[testIndex[[ii]], ]
    trainY <- y2[-testIndex[[ii]], ]
    testX <- x2[testIndex[[ii]], ]
    trainX <- x2[-testIndex[[ii]], ]

    rand_lasso_coef <- HiLasso(trainX, trainY, alpha = c(0, 1),
                               cores=TRUE, verbose = TRUE, nfold = 10)

    lambda.min <- cv.glmnet(trainX, trainY, type.measure = "mse",
                            nfold = 10, alpha = 0.5)$lambda.min
    coefficients <- glmnet(trainX, trainY, lambda = lambda.min, alpha = 0.5)
    hat.beta <- coef(coefficients)[-1]

    rand_lasso_pred <- as.matrix(apply(t(apply(testX, 1,
                           function(l) {l * rand_lasso_coef})), 1, sum))
    lasso_pred <- predict(coefficients, newx = testX)

    win_loss = vector()
    class_rand_lasso = vector()
    class_lasso = vector()
    correct_class_lasso = vector()
    correct_class_rand_lasso = vector()

    for (jj in 1:length(testY)) {
        if (abs(testY[jj] - rand_lasso_pred[jj]) < abs(testY[jj] - lasso_pred[jj])) {
            win_loss[jj] = 1
            aggregate_wins_rand = aggregate_wins_rand + 1
        } else {
            win_loss[jj] = 0
            aggregate_wins_lasso = aggregate_wins_lasso + 1
        }
        class_rand_lasso[jj] =
            unique_classes[which.min(abs(unique(
                unique_classes) - rand_lasso_pred[jj]))]
        class_lasso[jj] =
            unique_classes[which.min(abs(unique(
                unique_classes) - lasso_pred[jj]))]

        if (class_rand_lasso[jj] == testY[jj]) {
            correct_class_rand_lasso[jj] = 1
            aggregate_correct_rand = aggregate_correct_rand + 1
        } else {
            correct_class_rand_lasso[jj] = 0
        }

        if (class_lasso[jj] == testY[jj]) {
            correct_class_lasso[jj] = 1
            aggregate_correct_lasso = aggregate_correct_lasso + 1
        } else {
            correct_class_lasso[jj] = 0
        }
        total_tests = total_tests + 1
    }
    results = cbind(rand_lasso_pred, lasso_pred, as.matrix(as.vector(testY)),
                    win_loss, class_rand_lasso, class_lasso,
                    correct_class_rand_lasso, correct_class_lasso)
    colnames(results) <- c("Random", "Lasso", "Truth",
                           "Wins", "Class Random", "Class Lasso",
                           "Correct Random", "Correct Lasso")
    print(results)
    cat(paste0("Aggregate Wins Random: ", aggregate_wins_rand, "\n"))
    cat(paste0("Aggregate Wins Lasso: ", aggregate_wins_lasso, "\n"))
    cat(paste0("Aggregate Correct Classifications Random: ",
               aggregate_correct_rand," of ", total_tests, "\n"))
    cat(paste0("Aggregate Correct Classifications Lasso: ",
               aggregate_correct_lasso, " of ", total_tests, "\n"))
}
