findConfusion <- function(ground_truth, beta_hat) {
  if (beta_hat != 0) {
    if (ground_truth != 0) return("TP")
    else return("FP")
  }
  else {
    if (ground_truth != 0) return("FN")
    else return("TN")
  }
}

findF1 <- function(beta_hat, ground_truth) {
  confusion.values <- mapply(findConfusion, ground_truth, beta_hat)
  TP <- length(confusion.values[confusion.values == "TP"])
  FP <- length(confusion.values[confusion.values == "FP"])
  FN <- length(confusion.values[confusion.values == "FN"])
  TPR <- TP / (TP + FN)
  PPV <- TP / (TP + FP)
  f1 <- 2 * (PPV * TPR) / (PPV + TPR)
  return(f1)
}
