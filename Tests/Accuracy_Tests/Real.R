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

x.mean <- apply(x, 2, mean)
x.scaled <- scale(x, x.mean, FALSE)
standard.deviation <- sqrt(apply(x.scaled ^ 2, 2, sum))
x <- scale(x.scaled, FALSE, standard.deviation)

print(x)
print(y)

coef = list()

folds <- cut(seq(1,nrow(x)),breaks=5,labels=FALSE)

testIndex = list(c(1,2,3,4,29,30,31,32,49),
                 c(5,6,7,8,33,34,35,36,50),
                 c(9,10,11,12,37,38,39,40,51),
                 c(13,14,15,16,41,42,43,44,52),
                 c(17,18,19,20,22,44,45,46,49))

for(ii in 1:5) {
    print(testIndex[[ii]])
    testY <- y[testIndex[[ii]], ]
    trainY <- y[-testIndex[[ii]], ]
    testX <- x[testIndex[[ii]], ]
    trainX <- x[-testIndex[[ii]], ]
    
    print(trainY)
    print(testY)

    lambda.min <- cv.glmnet(trainX, trainY, type.measure = "mse",
                             nfold = 10, alpha = 0.5)$lambda.min
    coefficients <- glmnet(trainX, trainY, lambda = lambda.min, alpha = 1)
    hat.beta <- coef(coefficients)[-1]
    pred <- predict(coefficients, newx = testX)
    print(pred)
}

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