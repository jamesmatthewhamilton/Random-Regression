setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../../RandomLasso/", repos = NULL, type = "source")
source("../../../RandomLasso/R/helper.regression.R")
library(RandomLasso)
library("glmnet")

x <- t(read.csv("../../res/Emory/X.csv", header = FALSE))
y <- t(read.csv("../../res/Emory/y.csv", header = FALSE))
x <- apply(x, 2, scale)

y.mean <- mean(y)
y <- y - y.mean

#x.mean <- apply(x, 2, mean)
#x.scaled <- scale(x, x.mean, FALSE)
#standard.deviation <- sqrt(apply(x.scaled ^ 2, 2, sum))
#x <- scale(x.scaled, FALSE, standard.deviation)

coef = list()
testIndex = list()
aggregate_score_rand = 0
aggregate_score_lasso = 0

for (ii in 1:40) {
    testIndex[[ii]] = c(sample(1:28, sample(1:4, 1)), sample(29:47, sample(1:4, 1)), sample(48:52, sample(1:2, 1)))
}

for(ii in 1:length(testIndex)) {
    print(testIndex[[ii]])
    testY <- y[testIndex[[ii]], ]
    trainY <- y[-testIndex[[ii]], ]
    testX <- x[testIndex[[ii]], ]
    trainX <- x[-testIndex[[ii]], ]
    
    rand_lasso_coef <- HiLasso(trainX, trainY, alpha = c(0.5, 1), cores = 16, verbose = FALSE, nfold = 10)

    lambda.min <- cv.glmnet(trainX, trainY, type.measure = "mse",
                             nfold = 10, alpha = 0.5)$lambda.min
    coefficients <- glmnet(trainX, trainY, lambda = lambda.min, alpha = 0.5)
    hat.beta <- coef(coefficients)[-1]
    
    rand_lasso_pred <- as.matrix(apply(t(apply(testX, 1, function(l) {l * rand_lasso_coef})), 1, sum))
    lasso_pred <- predict(coefficients, newx = testX)
    
    win_loss = vector()
    for (jj in 1:length(testY)) {
        if (abs(testY[jj] - rand_lasso_pred[jj]) < abs(testY[jj] - lasso_pred[jj])) {
            win_loss[jj] = 1
            aggregate_score_rand = aggregate_score_rand + 1
        } else {
            win_loss[jj] = 0
            aggregate_score_lasso = aggregate_score_lasso + 1
        }
    }
    results = cbind(rand_lasso_pred, lasso_pred, as.matrix(as.vector(testY)), win_loss)
    colnames(results) <- c("Random", "Lasso", "Truth", "Wins")
    print(results)
    cat(paste0("Aggregate Score Random: ", aggregate_score_rand, "\n"))
    cat(paste0("Aggregate Score Lasso: ", aggregate_score_lasso, "\n"))
}
