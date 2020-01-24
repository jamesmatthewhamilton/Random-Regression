FindConfusion <- function(ground.truth, beta.hat) {
    if (beta.hat != 0) {
        if (ground.truth != 0) return("TP")
        else return("FP")
    }
    else {
        if (ground.truth != 0) return("FN")
        else return("TN")
    }
}

RMSE <- function(beta.hat, y.val, x.val)  
{
    RMSE_data <- RMSE2(as.numeric(y.val), as.numeric(x.val%*%as.vector(beta.hat))) 
    return(RMSE_data)
}

RMSE2 <- function(ground.truth, beta.hat){
    sqrt(mean((ground.truth - beta.hat)^2))
}

RunAccuracyTest <- function(s, dir="", title="", tests, iterations = 10,
                            column.names, save_files=TRUE) {

    cols = ncol(s$x[[1]])
    rows = nrow(s$x[[1]])
    coefficients = s$coef

    rmse <- rmseZ <- rmse2 <- f1 <- f2 <- dor <- TP <- FP <- FN <- TN <- TPR <- PPV <- f1.b <- f1.b.cutoff <-
    matrix(data = 0, nrow = iterations + 1, ncol = tests)

    coef.avg <- matrix(data = 0, nrow = nrow(coefficients[[1]]), ncol = tests)
    non.zero <- s$ground.truth != 0

    if (!is.na(column.names[1])) {
        colnames(rmse) <- column.names
        colnames(rmseZ) <- column.names
        colnames(rmse2) <- column.names
        colnames(f1) <- column.names
        colnames(f1.b) <- column.names
        colnames(f1.b.cutoff) <- column.names
        colnames(f2) <- column.names
        colnames(dor) <- column.names
        colnames(coef.avg) <- column.names
    }

    for (ii in 1:iterations) {
        for (jj in 1:tests) {
            beta.hat <- as.matrix(as.vector(coefficients[[jj]][,ii]))
            coef.avg[,jj] = coef.avg[,jj] + beta.hat
            rmse[ii,jj] <- RMSE(beta.hat, s$y.val[[ii]], s$x.val[[ii]])
            rmseZ[ii,jj] <- RMSE(beta.hat[non.zero], s$y.val[[ii]], s$x.val[[ii]][,non.zero])
            rmse2[ii,jj] <- RMSE2(s$ground.truth, beta.hat)
            f1.trials = vector()
            for (kk in 1:205) {
                beta.hat.trial <- beta.hat
                beta.hat.trial[abs(beta.hat) < 2 - ((kk - 1) * 0.0095)] <- 0
                confusion.values <- mapply(FindConfusion, s$ground.truth, beta.hat.trial)
                TP[ii,jj] <-.TP <- length(confusion.values[confusion.values == "TP"])
                FP[ii,jj] <- .FP <- length(confusion.values[confusion.values == "FP"])
                FN[ii,jj] <- .FN <- length(confusion.values[confusion.values == "FN"])
                TN[ii,jj] <- .TN <- length(confusion.values[confusion.values == "TN"])
                TPR[ii,jj] <- .TPR <- .TP / (.TP + .FN)
                PPV[ii,jj] <- .PPV <- .TP / (.TP + .FP)
                f1.trials[kk] <- 2 * (.PPV * .TPR) / (.PPV + .TPR)
            }
            f1.trials[is.na(f1.trials)] <- 0
            f1.b.cutoff[ii, jj] <- 2 - ((min(which(f1.trials == max(f1.trials))) - 1) * 0.0095)
            f1.b[ii,jj] <- max(f1.trials)
            
            confusion.values <- mapply(FindConfusion, s$ground.truth, beta.hat)
            TP[ii,jj] <-.TP <- length(confusion.values[confusion.values == "TP"])
            FP[ii,jj] <- .FP <- length(confusion.values[confusion.values == "FP"])
            FN[ii,jj] <- .FN <- length(confusion.values[confusion.values == "FN"])
            TN[ii,jj] <- .TN <- length(confusion.values[confusion.values == "TN"])
            TPR[ii,jj] <- .TPR <- .TP / (.TP + .FN)
            PPV[ii,jj] <- .PPV <- .TP / (.TP + .FP)
            f1[ii,jj] <- 2 * (.PPV * .TPR) / (.PPV + .TPR)
            f2[ii,jj] <- sqrt((.TP / (.TP + .FP)) * (.TP / (.TP + .FN)))
            dor[ii,jj] <- (.TP / .FP) / (.FN / .TN)
        }
    }

    coef.avg = coef.avg / iterations

    if (tests > 1) {
        rmse[iterations + 1,] <- apply(rmse[1:iterations,], 2, mean)
        rmseZ[iterations + 1,] <- apply(rmseZ[1:iterations,], 2, mean)
        rmse2[iterations + 1,] <- apply(rmse2[1:iterations,], 2, mean)
        f1.b[iterations + 1,] <- apply(f1.b[1:iterations,], 2, mean)
        f1.b.cutoff[iterations + 1,] <- apply(f1.b.cutoff[1:iterations,], 2, mean)
        f1[iterations + 1,] <- apply(f1[1:iterations,], 2, mean)
        f2[iterations + 1,] <- apply(f2[1:iterations,], 2, mean)
        dor[iterations + 1,] <- apply(dor[1:iterations,], 2, mean)
    } else {
        rmse[iterations + 1,] <- mean(rmse[1:iterations,])
        rmseZ[iterations + 1,] <- mean(rmseZ[1:iterations,])
        rmse2[iterations + 1,] <- mean(rmse2[1:iterations,])
        f1.b[iterations + 1,] <- mean(f1.b[1:iterations,])
        f1.b.cutoff[iterations + 1,] <- mean(f1.b.cutoff[1:iterations,])
        f1[iterations + 1,] <- mean(f1[1:iterations,])
        f2[iterations + 1,] <- mean(f2[1:iterations,])
        dor[iterations + 1,] <- mean(dor[1:iterations,])
    }
    
    if (save_files) {
        rows.cols <- paste0("[", rows, "x", cols, "]")
        directory <- paste0(dir, "/", title, "[", rows, "x", cols, "]", format(Sys.time(), "%F_%H:%M"))
        dir.create(directory)
    
        write.csv(x = coef.avg, paste0(directory, "/Averaged_Coefficients", rows.cols, ".csv"))
        write.csv(x = rmse, paste0(directory, "/RMSE", rows.cols, ".csv"))
        write.csv(x = rmseZ, paste0(directory, "/RMSE_Zero", rows.cols, ".csv"))
        write.csv(x = rmse2, paste0(directory, "/RMSE_Pure", rows.cols, ".csv"))
        write.csv(x = f1.b, paste0(directory, "/F1_Cutoff", rows.cols, ".csv"))
        write.csv(x = f1.b.cutoff, paste0(directory, "/Best_Cutoff_Values", rows.cols, ".csv"))
        write.csv(x = f1, paste0(directory, "/F1", rows.cols, ".csv"))
        write.csv(x = f2, paste0(directory, "/F2", rows.cols, ".csv"))
        write.csv(x = dor, paste0(directory, "/DOR", rows.cols, ".csv"))
    }
    
    return(list(rmse = rmse, rmse.Zero = rmse, rmse.pure = rmse2, f1 = f1, f1.best.cutoff = f1.b, best.cutoff.value = f1.b.cutoff, f2 = f2, dor = dor))
}
