# Export simulation data to .csv files.
for (ii in 1:10) {

  write.csv(matrix(unlist(sim_data[[ii]][2]), ncol = 1),
            paste0("data/sim4/sim4_", ii, "_x.csv"))
  write.csv(matrix(unlist(sim_data[[ii]][3]), ncol = n_feature, byrow = FALSE),
            paste0("data/sim4/sim4_", ii, "_y.csv"))
  write.csv(matrix(unlist(sim_data[[ii]][6]), ncol = 1),
            paste0("data/sim4/sim4_", ii, "_xval.csv"))
  write.csv(matrix(unlist(sim_data[[ii]][7]), ncol = n_feature, byrow = FALSE),
            paste0("data/sim4/sim4_", ii, "_yval.csv"))
  write.csv(matrix(unlist(sim_data[[ii]][8]), ncol = n_feature, byrow = FALSE),
            paste0("data/sim4/sim4_", ii, "_xcov.csv"))
}
write.csv(as.matrix(beta0),  paste0("data/sim4/sim4_ground_truth.csv"))

# Export simulation data to .RData binary files.
x <- y <- x_val <- y_val <- x_cov <- list()
for (ii in 1:10) {
  x[[ii]] <- matrix(unlist(sim_data[[ii]][3]),
                    ncol = n_feature, byrow = FALSE)
  y[[ii]] <- matrix(unlist(sim_data[[ii]][2]), ncol = 1)
  x_val[[ii]] <- matrix(unlist(sim_data[[ii]][7]),
                        ncol = n_feature, byrow = FALSE)
  y_val[[ii]] <- matrix(unlist(sim_data[[ii]][6]), ncol = 1)
  x_cov[[i]] <- matrix(unlist(sim_data[[ii]][8]),
                       ncol = n_feature, byrow = FALSE)
}
beta0 <- as.matrix(beta0)
save(x, y, x_val, y_val, x_cov, beta0, file = "data/sim2.RData")
