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

lambda.min <- cv.glmnet(x, y, type.measure = "mse",
                        nfold = 10, alpha = 1)$lambda.min
coefficients <- glmnet(x, y, lambda = lambda.min, alpha = 1)
hat.beta <- coef(coefficients)[-1]
pred <- predict(coefficients, newx = x)
print(pred)
print(apply(t(apply(x, 1, function(l) {l * hat.beta})), 1, sum))

coef[[1]] <- Lasso(x, y, alpha = 1, nfold = 10)
coef[[2]] <- HiLasso(x, y, alpha = c(0.5,1), nfold = 10, verbose = FALSE, cores = 16)

results = list()

for (ii in 1:length(coef)) {
    results[[ii]] <- apply(t(apply(x, 1, function(l) {l * coef[[ii]]})), 1, sum)
}
print(results)