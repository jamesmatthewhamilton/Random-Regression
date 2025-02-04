VisualizeResults <- function(r, s, xlab, angle = 0) {

    directory <- r$directory
    iterations = nrow(r$rmse)
    cols = ncol(s$x[[1]])
    rows = nrow(s$x[[1]])
    rows.cols <- paste0("[", rows, "x", cols, "]")
    

    png(paste0(directory, "/Image_RMSE", rows.cols, ".png"), width = 4096, height = 2160, pointsize = 40)
    par(mar = c(8,5,8,5))
    plot <- barplot(r$rmse[iterations,], main = paste0("RMSE [", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")),
                    axisnames = FALSE)
    text(plot, par("usr")[3], labels = colnames(r$f1), srt = angle, adj = c(1.1,1.1), xpd = TRUE, cex = .9)
    plot
    dev.off()

    png(paste0(directory, "/Image_Zero_RMSE", rows.cols, ".png"), width = 4096, height = 2160, pointsize = 40)
    par(mar = c(8,5,8,5))
    plot <- barplot(r$rmseZ[iterations,], main = paste0("RMSE of Significant Values Only [", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")),
                    axisnames = FALSE)
    text(plot, par("usr")[3], labels = colnames(r$f1), srt = angle, adj = c(1.1,1.1), xpd = TRUE, cex = .9)
    plot
    dev.off()
    
    png(paste0(directory, "/Image_F1", rows.cols, ".png"), width = 4096, height = 2160, pointsize = 40)
    par(mar = c(8,5,8,5))
    plot <- barplot(r$f1[iterations,], main = paste0("F1 [", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")),
                    axisnames = FALSE)
    text(plot, par("usr")[3], labels = colnames(r$f1), srt = angle, adj = c(1.1,1.1), xpd = TRUE, cex = .9)
    plot
    dev.off()

    png(paste0(directory, "/Image_Pure_F1", rows.cols, ".png"), width = 4096, height = 2160, pointsize = 40)
    par(mar = c(8,5,8,5))
    plot <- barplot(r$f1.pure[iterations,], main = paste0("F1 (No Cutoff) [", rows, "x", cols, "] Date: ", format(Sys.time(), "%F@%H-%M-%S")),
            axisnames = FALSE)
    text(plot, par("usr")[3], labels = colnames(r$f1), srt = angle, adj = c(1.1,1.1), xpd = TRUE, cex = .9)
    plot
    dev.off()
}
