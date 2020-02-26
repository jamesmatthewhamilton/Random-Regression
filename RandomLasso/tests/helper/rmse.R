findRMSE <- function(beta_hat, x_val, y_val) {
  return(sqrt(mean((as.numeric(y_val) - as.numeric(x_val%*%as.vector(beta_hat)))^2)))
}
