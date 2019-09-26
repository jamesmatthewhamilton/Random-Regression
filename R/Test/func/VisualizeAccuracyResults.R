VisualizeAccuracyResults <- function(loglocation, r, s, xlab) {
    iterations = nrow(r$rmse)
    rows = nrow(s$coef[[1]])
    cols = ncol(s$coef[[1]])
    
    barplot(r$rmse[iterations,], main=paste0("RMSE[", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")), ylim = c(min(r$rmse[iterations,] * 0.98), max(r$rmse[iterations,] * 1.05)), xpd = FALSE, border = NA, xlab = xlab)
    barplot(r$f1[iterations,], main=paste0("F1[", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")), ylim = c(min(r$f1[iterations,] * 0.98), max(r$f1[iterations,] * 1.05)), xpd = FALSE, border = NA, xlab = xlab)
}