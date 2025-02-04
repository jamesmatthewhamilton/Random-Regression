.start.progress.bar <- function(pb, max, min = 0, style = 3) {
    pb <- txtProgressBar(min=min, max=max, style=style)
    start <<- as.numeric(Sys.time())
    return(list(pb = pb, start = start))
}

.continue.progress.bar <- function(pb, start_time, current_increment,
                                   end_increment) {
    setTxtProgressBar(pb, current_increment)
    passed <- as.numeric(Sys.time()) - start_time
    remaining <- (passed / (current_increment / end_increment)) - passed
    hr <- floor(remaining / 3600)
    min <- floor(remaining / 60) - (hr * 60)
    sec <- floor(remaining) - (hr * 3600) - (min * 60)
    if (hr < 10) hr <- paste("0", hr, sep = "")
    if (min < 10) min <- paste("0", min, sep = "")
    if (sec < 10) sec <- paste("0", sec, sep = "")

    cat("\r", paste("[",hr, ":", min, ":", sec, "] |", sep=""))
}