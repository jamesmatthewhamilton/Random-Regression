options(scipen = 999)
setwd("~/Dropbox/MatthewHamilton/Hi-Lasso/R/Test/res/")

##  100 Features 50 Samples:
x = as.matrix(read.csv("qx")[,-1])
y = as.matrix(read.csv("qy")[,-1])
##  1000 Features 100 Samples:
# x = as.matrix(read.csv("x")[,-1])
# y = as.matrix(read.csv("y")[,-1])

detach("package:RandomLasso", unload = TRUE)
install.packages("~/Dropbox/MatthewHamilton/Hi-Lasso/R/RandomLasso/",
                 repos = NULL, type = "source")
library(RandomLasso)
ls("package:RandomLasso")

start <- Sys.time()
HiLasso(x, y, alpha = 0.5, verbose = TRUE, test = FALSE)
Sys.time() - start

start <- Sys.time()
RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
Sys.time() - start

start <- Sys.time()
RapidRandomLasso(x, y, alpha = c(0.6, 0.6), verbose = TRUE, test = FALSE)
Sys.time() - start
