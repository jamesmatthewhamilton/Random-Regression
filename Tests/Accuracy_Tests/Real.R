setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
source("../../RandomLasso/R/helper.regression.R")
library(RandomLasso)
library("glmnet")

x <- t(read.csv("../res/Emory/X.csv", header = FALSE))
y <- t(read.csv("../res/Emory/y.csv", header = FALSE))
x <- apply(x, 2, scale)

y.mean <- mean(y)
y <- y - y.mean

#x.mean <- apply(x, 2, mean)
#x.scaled <- scale(x, x.mean, FALSE)
#standard.deviation <- sqrt(apply(x.scaled ^ 2, 2, sum))
#x <- scale(x.scaled, FALSE, standard.deviation)

print(x)
print(y)

coef = list()
final_score_rand = 0
final_score_lasso = 0
folds <- cut(seq(1,nrow(x)),breaks=5,labels=FALSE)
testIndex = list(c(1,2,3,4,29,30,31,32,48),
                 c(5,6,7,8,33,34,35,36,49),
                 c(9,10,11,12,37,38,39,40,50),
                 c(13,14,15,16,41,42,43,44,51),
                 c(17,18,19,20,22,44,45,46,52))

for(ii in 1:length(testIndex)) {
    print(testIndex[[ii]])
    testY <- y[testIndex[[ii]], ]
    trainY <- y[-testIndex[[ii]], ]
    testX <- x[testIndex[[ii]], ]
    trainX <- x[-testIndex[[ii]], ]
    
    #print("trainY")
    #print(trainY)
    #print("testY")
    #print(as.matrix(as.vector(testY)))
    
    rand_lasso_coef <- HiLasso(trainX, trainY, alpha = c(0.5, 1), cores = 16, verbose = FALSE)

    lambda.min <- cv.glmnet(trainX, trainY, type.measure = "mse",
                             nfold = 10, alpha = 0.5)$lambda.min
    coefficients <- glmnet(trainX, trainY, lambda = lambda.min, alpha = 0.5)
    hat.beta <- coef(coefficients)[-1]
    
    rand_lasso_pred <- as.matrix(apply(t(apply(testX, 1, function(l) {l * rand_lasso_coef})), 1, sum))
    lasso_pred <- predict(coefficients, newx = testX)
    
    # Checking which method performed better.
    win_loss = vector()
    for (jj in 1:length(testY)) {
        if (abs(testY[jj] - rand_lasso_pred[jj]) < abs(testY[jj] - lasso_pred[jj])) {
            win_loss[jj] = 1
            final_score_rand = final_score_rand + 1
        } else {
            win_loss[jj] = 0
            final_score_lasso = final_score_lasso + 1
        }
    }
    
    print(cbind(rand_lasso_pred, lasso_pred, as.matrix(as.vector(testY)), win_loss))
}
print(paste0("Final Score Random: ", final_score_rand))
print(paste0("Final Score Lasso: ", final_score_lasso))
stop()

lambda.min <- cv.glmnet(x, y, type.measure = "mse",
                        nfold = 10, alpha = 1)$lambda.min
coefficients <- glmnet(x, y, lambda = lambda.min, alpha = 1)
hat.beta <- coef(coefficients)[-1]
pred <- predict(coefficients, newx = x)
print(pred)
print(apply(t(apply(x, 1, function(l) {l * hat.beta})), 1, sum))

coef[[1]] <- Lasso(x, y, alpha = 1, nfold = 10)
coef[[2]] <- HiLasso(x, y, alpha = c(0.5,1), verbose = FALSE, cores = 16)

results = list()

for (ii in 1:length(coef)) {
    results[[ii]] <- apply(t(apply(x, 1, function(l) {l * coef[[ii]]})), 1, sum)
}
results