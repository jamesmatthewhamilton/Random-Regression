SimulateTestData <- function(dataset, tests, iterations = 10) {
    load(dataset)
    
    coef <- vector("list", tests)
    for (ii in 1:tests) {
        coef[[ii]] <- matrix(0, nrow = n_feature, ncol = iterations)
    }
    
    y <- x <- y.test <- x.test <- y.val <- x.val <- cov.x <- ground.truth <- list()
    for (ii in 1:iterations) {
        
        y[[ii]] <- matrix(unlist(sim_data[[ii]][2]), ncol = 1)
        x[[ii]] <- matrix(unlist(sim_data[[ii]][3]), ncol = n_feature, byrow = FALSE)
        y.test[[ii]] <- matrix(unlist(sim_data[[ii]][4]), ncol = 1)
        x.test[[ii]] <- matrix(unlist(sim_data[[ii]][5]), ncol = n_feature, byrow = FALSE)
        y.val[[ii]] <- matrix(unlist(sim_data[[ii]][6]), ncol = 1)
        x.val[[ii]] <- matrix(unlist(sim_data[[ii]][7]), ncol = n_feature, byrow = FALSE)
        cov.x[[ii]] <- matrix(unlist(sim_data[[ii]][8]), ncol = n_feature, byrow = FALSE)
        
        colnames(x[[ii]]) <- colnames(x.val[[ii]]) <- colnames(x.test[[ii]]) <- paste0('Gene', seq(1:n_feature))
    }
    ground.truth <- as.matrix(beta0)
    
    return(list(coef = coef,
                x = x,
                y = y,
                y.test = y.test,
                x.test = x.test,
                y.val = y.val,
                x.val = x.val,
                cov.x = cov_x,
                ground.truth = ground.truth))
}