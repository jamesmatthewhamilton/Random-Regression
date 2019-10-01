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

RunAccuracyTest <- function(loglocation, s, tests, iterations = 10, column.names, ground.truth, y.val, x.val) {
    
    cols = ncol(s$x[[1]])
    rows = nrow(s$x[[1]])
    coefficients = s$coef
    
    rmse <- rmse2 <- f1 <- f2 <- dor <- TP <- FP <- FN <- TN <- TPR <- PPV <- NPV <-
    matrix(data = 0, nrow = iterations + 1, ncol = TESTS)
    if (!is.na(column.names)) {
        colnames(rmse) <- column.names
        colnames(f1) <- column.names
    }

    coef.avg <- matrix(data = 0, nrow = nrow(coefficients[[1]]), ncol = TESTS)

    
    for (ii in 1:iterations) {
        for (jj in 1:tests) {
            beta.hat <- as.matrix(as.vector(coefficients[[jj]][,ii]))
            
            coef.avg[,jj] = coefficients[[jj]][,ii] + beta.hat
            
            rmse[ii,jj] <- RMSE(beta.hat, y.val[[ii]], x.val[[ii]])
            rmse2[ii,jj] <- RMSE2(ground.truth, beta.hat)
            
            confusion.values <- mapply(FindConfusion, ground.truth, beta.hat)
            TP[ii,jj] <-.TP <- length(confusion.values[confusion.values == "TP"])
            FP[ii,jj] <- .FP <- length(confusion.values[confusion.values == "FP"])
            FN[ii,jj] <- .FN <- length(confusion.values[confusion.values == "FN"])
            TN[ii,jj] <- .TN <- length(confusion.values[confusion.values == "TN"])
            TPR[ii,jj] <- .TPR <- .TP / (.TP + .FN)
            PPV[ii,jj] <- .PPV <- .TP / (.TP + .FP)
            NPV[ii,jj] <- .NPV <- .TN / (.TN + .FN)
            
            f1[ii,jj] <- 2 * (.PPV * .TPR) / (.PPV + .TPR)
            f2[ii,jj] <- sqrt((.TP / (.TP + .FP)) * (.TP / (.TP + .FN)))
            dor[ii,jj] <- (.TP / .FP) / (.FN / .TN)
        }
    }
    
    coef.avg = coef.avg / tests

    rmse[iterations + 1,] <- apply(rmse[1:iterations,], 2, mean)
    rmse2[iterations + 1,] <- apply(rmse2[1:iterations,], 2, mean)
    f1[iterations + 1,] <- apply(f1[1:iterations,], 2, mean)
    f2[iterations + 1,] <- apply(f2[1:iterations,], 2, mean)
    dor[iterations + 1,] <- apply(dor[1:iterations,], 2, mean)
    
    write.csv(x = coef.avg, paste0(loglocation, "_Averaged_Coefficients[", rows, "x", cols, "]", format(Sys.time(), "%F@%H-%M-%S"), ".csv"))
    write.csv(x = rmse, paste0(loglocation, "_RMSE[", rows, "x", cols, "]", format(Sys.time(), "%F@%H-%M-%S"), ".csv"))
    write.csv(x = rmse2, paste0(loglocation, "_RMSE_Raw[", rows, "x", cols, "]", format(Sys.time(), "%F@%H-%M-%S"), ".csv"))
    write.csv(x = f1, paste0(loglocation, "_F1[", rows, "x", cols, "]", format(Sys.time(), "%F@%H-%M-%S"), ".csv"))
    write.csv(x = f2, paste0(loglocation, "_F2[", rows, "x", cols, "]", format(Sys.time(), "%F@%H-%M-%S"), ".csv"))
    write.csv(x = dor, paste0(loglocation, "_DOR[", rows, "x", cols, "]", format(Sys.time(), "%F@x%H-%M-%S"), ".csv"))
    
    return(list(rmse = rmse, rmse2 = rmse2, f1 = f1, f2 = f2, dor = dor))
}
