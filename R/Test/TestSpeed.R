options(scipen = 999)
setwd("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Test/res/")

##  1000 Features 100 Samples:
x = as.matrix(read.csv("x")[,-1])
y = as.matrix(read.csv("y")[,-1])

detach("package:RandomLasso", unload = TRUE)
install.packages("~/Dropbox/MatthewHamilton/Hi-Lasso/R/RandomLasso/",
                 repos = NULL, type = "source")
library(RandomLasso)


# >>>>>> Generating Huge Test Data <<<<<<
map = list()
for (ii in 1:20) {map[[ii]] = x}
superX = Reduce(cbind, map)
dim(superX)

mapx = list()
mapy = list()
for (ii in 1:20) {mapx[[ii]] = superX}
for (ii in 1:20) {mapy[[ii]] = y}
superY = Reduce(rbind, mapy)
superX2 = Reduce(rbind, mapx)
dim(superY)
dim(superX2)

# >>>>>> Testing X and Y <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 30, style = 3)
for (ii in 1:30) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RandomLasso(superX2[1:(ii * 20),1:(ii * 200)],
                             superY[1:(ii * 20),],
                             alpha = c(1, 1),
                             verbose = FALSE,
                             test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/XY1Core", Sys.time(), ".csv", sep = ""))


# >>>>>> Testing Bootstraps <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 1, max = 3, style = 3)
for (ii in 1:3) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RandomLasso(superX2[1:50,1:100],
                             superY[1:50,],
                             alpha = c(1, 1),
                             bootstraps = 50 * ii,
                             verbose = FALSE,
                             test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/Bootstraps(3)", Sys.time(), ".csv", sep = ""))


# >>>>>> Parallel Testing X and Y <<<<<<
library(parallel)
detectCores()
cores <- 2
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 200, style = 3)
for (ii in 1:200) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = ParallelRandomLasso(superX2[1:(ii * 10),1:(ii * 100)],
                                     superY[1:(ii * 10),], alpha = c(1, 1),
                                     bootstraps = 5 ,
                                     verbose = FALSE,
                                     test = TRUE,
                                     cores = cores)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/XYMULTICore", Sys.time(), ".csv", sep = ""))


# >>>>>> Testing Just Features <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 200, style = 3)
for (ii in 1:100) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RandomLasso(superX2[1:100,1:(200 * ii)],
                             superY[1:100,], alpha = c(1, 1),
                             bootstraps = 10,
                             verbose = FALSE,
                             test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/Features", Sys.time(), ".csv", sep = ""))


# >>>>>> Testing Just Samples <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 200, style = 3)
for (ii in 1:100) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RandomLasso(superX2[1:(10 * ii),1:2000],
                             superY[1:(10 * ii),],
                             alpha = c(1, 1),
                             bootstraps = 10,
                             verbose = FALSE,
                             test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/Samples", Sys.time(), ".csv", sep = ""))


# >>>>>> Comparing Rapid v Random Lasso Accuracy [Features: 100] <<<<<<
time1 = list()
time2 = list()
coef1 = list()
coef2 = list()
error1 = list()
error2 = list()
iterations = 30
truth <- c(3, 3, -3, 2, 2, -2, 1.5, 1.5, 1.5, -1.5)

for (ii in 1:iterations) {
    start <- Sys.time()
    coef1[[ii]] <- RandomLasso(x,
                               y,
                               alpha = c(0.6, 0.6),
                               verbose = TRUE,
                               test = FALSE)
    error1[[ii]] <- abs(coef1[[1]][1:10] - truth)
    time1[[ii]] <- Sys.time() - start
    
    start <- Sys.time()
    coef2[[ii]] <- RapidRandomLasso(x,
                                    y,
                                    alpha = c(0.6, 0.6),
                                    verbose = TRUE,
                                    test = FALSE)
    error2[[ii]] <- abs(coef2[[1]][1:10] - truth)
    # sqrt((sd(coef2[[1]][1:10])^2/10) + sd(truth)^2/10)
    time2[[ii]] <- Sys.time() - start
}
results <- matrix(NA, nrow = 10, ncol = 7)
colnames(results) <- c("Run Time Random Lasso", "Run Time Rapid Lasso", "Truth", "Average Coef Random Lasso", "Average Coef Rapid Lasso", "Difference Random Lasso", "Difference Rapid Lasso")
results[1,1] <- Reduce('+', time1) / iterations
results[1,2] <- Reduce('+', time2) / iterations
results[1:10,3] <- truth
results[1:10,4] <- (Reduce('+', coef1) / iterations)[1:10]
results[1:10,5] <- (Reduce('+', coef2) / iterations)[1:10]
results[1:10,6] <- Reduce('+', error1) / iterations
results[1:10,7] <- Reduce('+', error2) / iterations

write.csv(x = results, paste("../log/RapidAccuracyDataset1", Sys.time(), ".csv", sep = ""))


# >>>>>> Comparing Rapid v Random Lasso Accuracy [Features: 1000] <<<<<<
time1 = list()
time2 = list()
coef1 = list()
coef2 = list()
error1 = list()
error2 = list()
iterations = 15
truth <- c(0.5285, -2.3780, -0.8379, -1.2074, -0.7173, -0.1747, -2.4928,
           -1.0483, -2.7500, 2.1934, 2.9586, -1.2497, 0.8943, 4.4044, 2.2559)

for (ii in 1:iterations) {
    start <- Sys.time()
    coef1[[ii]] <- RandomLasso(x,
                               y,
                               alpha = c(0.6, 0.6),
                               verbose = TRUE,
                               test = FALSE)
    error1[[ii]] <- abs(coef1[[1]][1:15] - truth)
    time1[[ii]] <- Sys.time() - start
    
    start <- Sys.time()
    coef2[[ii]] <- RapidRandomLasso(x,
                                    y,
                                    alpha = c(0.6, 0.6),
                                    verbose = TRUE,
                                    test = FALSE)
    error2[[ii]] <- abs(coef2[[1]][1:15] - truth)
    # sqrt((sd(coef2[[1]][1:10])^2/10) + sd(truth)^2/10)
    time2[[ii]] <- Sys.time() - start
}
results <- matrix(NA, nrow = 50, ncol = 7)
colnames(results) <- c("Run Time Random Lasso", "Run Time Rapid Lasso", "Truth", "Average Coef Random Lasso", "Average Coef Rapid Lasso", "Difference Random Lasso", "Difference Rapid Lasso")
results[1,1] <- Reduce('+', time1) / iterations
results[1,2] <- Reduce('+', time2) / iterations
results[1:15,3] <- truth
results[1:50,4] <- (Reduce('+', coef1) / iterations)[1:50]
results[1:50,5] <- (Reduce('+', coef2) / iterations)[1:50]
results[1:15,6] <- Reduce('+', error1) / iterations
results[1:15,7] <- Reduce('+', error2) / iterations

write.csv(x = results, paste("../log/RapidAccuracyDataset2", Sys.time(), ".csv", sep = ""))


# >>>>>> Testing Rapid Random Lasso <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 30, style = 3)
for (ii in 1:30) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RapidRandomLasso(superX2[1:(ii * 20),1:(ii * 200)],
                                  superY[1:(ii * 20),],
                                  alpha = c(1, 1),
                                  verbose = FALSE,
                                  test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/Rapid1Core", Sys.time(), ".csv", sep = ""))


# >>>>>> Testing Bootstraps <<<<<<
test = list()
cat("\nTesting:\n")
pb <- txtProgressBar(min = 0, max = 200, style = 3)
for (ii in 1:200) {
    setTxtProgressBar(pb, ii)
    test[[ii]] = RapidRandomLasso(superX2[1:50,1:100],
                                  superY[1:50,],
                                  alpha = c(1, 1),
                                  bootstraps = 5 * ii,
                                  verbose = FALSE,
                                  test = TRUE)
}
delist = t(sapply(test, unlist))
write.csv(x = delist, paste("../log/RapidBootstraps", Sys.time(), ".csv", sep = ""))
