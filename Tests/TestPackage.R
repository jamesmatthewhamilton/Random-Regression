options(scipen = 999) # Ctrl-Shift-Enter
setwd(dirname(parent.frame(2)$ofile))
getwd()

CORES = 12

##  100 Features 50 Samples:
x = as.matrix(read.csv("res/qx")[,-1])
y = as.matrix(read.csv("res/qy")[,-1])
##  1000 Features 100 Samples:
# x = as.matrix(read.csv("x")[,-1])
# y = as.matrix(read.csv("y")[,-1])

setwd(dirname(parent.frame(2)$ofile))
if("RandomLasso" %in% (.packages())){detach("package:RandomLasso", unload = TRUE)}
install.packages("../../RandomLasso/", repos = NULL, type = "source")
library(RandomLasso)

start <- Sys.time()
HiLasso(x, y, alpha = c(0.5, 1), verbose = TRUE, test = FALSE)
Sys.time() - start

start <- Sys.time()
RandomLasso(x, y, alpha = c(0.5, 0.5), verbose = TRUE, test = FALSE)
Sys.time() - start

start <- Sys.time()
RapidRandomLasso(x, y, alpha = c(0.6, 0.6), verbose = TRUE, test = FALSE)
Sys.time() - start
