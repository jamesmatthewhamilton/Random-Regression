
## Advanced Random Lasso 
## Random Lasso considering selction weight
RandomLasso3 <- function(x, y, Importance_weight = Importance_weight, 
                         NumOfFeatures = q1, repeat_Boostrapping = B, step2 = 'FALSE', Method = 'OLS')
{    
    x_names  <- colnames(x)
    num_x    <- ncol(x)        # number of genes
    n_sample <- nrow(x)
    tmp_denominator <- rep(1e-10, num_x)  # for non-zero weight in adaptive lasso
    
    Adj_gene_net_b <- matrix(0, nrow = num_x, ncol = 5) # beta hat matrix for random Lasso with Lasso
    rownames(Adj_gene_net_b) <- x_names
    colnames(Adj_gene_net_b) <- c('beta', 'Importance', 'C_j', 'p_value', 'zero_prob')
    
    
    # gene selection weight : In Proposal, Use sample selection probability of step1 and step2,
    select_prob <-  tmp_denominator + Importance_weight 
    names(select_prob) <- x_names
    
    # step 1 of Random LASSO < making improtance measures of genes >
    ## call LASSO ftn for step 1 ( ) : how to process W_p and importance 
    
    Adj_temp <- matrix(0, nrow = num_x, ncol = repeat_Boostrapping)  # temporary storage for coefficient
    rownames(Adj_temp) <- x_names
    
    for (j in 1 : repeat_Boostrapping)
    {
        print("j= ")
        print(j)
        
        # select genes by sampling with selection probability
        sample_x <- sample(1:num_x, NumOfFeatures, replace = FALSE, prob = select_prob)
        sample_y <- sample(1:n_sample, n_sample, replace = TRUE)
        
        XX.tr <- x[sample_y, sample_x]
        YY.tr <- y[sample_y]
        
        
        mean_y <- mean(YY.tr)
        YY     <- YY.tr - mean_y
        
        mean_x <- apply(XX.tr, 2, mean)
        XX.tr  <- scale(XX.tr, mean_x, FALSE)
        sd_x   <- sqrt(apply(XX.tr^2, 2, sum))
        XX     <- scale(XX.tr, FALSE, sd_x)
        
        Adj_temp[-sample_x, j] <-  NA
        
        if (step2 == 'FALSE') 
        {
            if ( Method == 'OLS')
            {
                # ## Step 1: OLS estimates
                
                fit.lm <- lm(YY ~ XX) # . , data = data.frame(XX))
                hat_beta <- summary(fit.lm)$coefficients[, 1]
                hat_beta <- as.numeric(hat_beta[-1])
                hat_beta <- hat_beta/sd_x
                Adj_temp[names(hat_beta), j] <-  hat_beta
                print("Proposed : Step1 = OLS")
            }
            
            if ( Method == 'Ridge')
            {
                # ## Step 1: Ridge estimates
                
                fit <- glmnet(XX, YY, standardize = FALSE,  alpha = 0,  
                              lambda=cv.glmnet(XX, YY, type.measure = "mse", nfold = 5, alpha = 0)$lambda.min, intercept=FALSE)
                
                r_names <- rownames(coef(fit))[-1]
                hat_beta <- coef(fit)[-1]
                names(hat_beta) <- r_names
                hat_beta <- hat_beta/sd_x
                Adj_temp[names(hat_beta), j] <-  hat_beta
                print("Proposed : Step1 = Ridge")
            }
            
            if ( Method == 'LASSO')
            {
                ## Step 1: Random Lasso with lasso
                
                fit <- glmnet(as.matrix(XX), YY, standardize = FALSE, alpha = 1,
                              lambda=cv.glmnet(as.matrix(XX), YY, type.measure = "mse", nfold = 5, alpha = 1)$lambda.min, intercept=FALSE)
                # alpha =1 ; LASSO, alpha = 0 ; ridge
                r_names <- rownames(coef(fit))[-1]
                hat_beta <- coef(fit)[-1]
                names(hat_beta) <- r_names
                
                Adj_temp[names(hat_beta), j] <-  hat_beta / sd_x 
                print("Proposed : step1 = LASSO")
            }
            
            if (Method == 'Enet')
            {
                ## Step 1: Random Lasso with Enet
                lambdas = list()
                mses = list()
                for (i in seq(0, 1, 0.1))
                {
                    cv_glm <- cv.glmnet(as.matrix(XX), YY, type.measure="mse", nfold = 5, alpha = i ) 
                    lambdas = rbind(lambdas, cv_glm$lambda.min)
                    mses = rbind(mses, cv_glm$cvm[which.min(cv_glm$cvm)])
                }
                opt_lambda = as.numeric(lambdas[which.min(mses)])
                opt_alpha = as.numeric(seq(0, 1, 0.1)[which.min(mses)])
                fit <- glmnet(as.matrix(XX), YY, standardize = FALSE,  
                              lambda = opt_lambda, alpha = opt_alpha, intercept = FALSE)
                
                r_names <- rownames(coef(fit))[-1]
                hat_beta <- coef(fit)[-1]
                names(hat_beta) <- r_names
                Adj_temp[names(hat_beta), j] <-  hat_beta / sd_x 
                print("Proposed : step1 = Elastic-net")
            }
        }
        
        if (step2 == 'TRUE')
        {
            # weight_Adaptive1 <- Importance_weight[colnames(XX)]  
            # tmp_denominator <- rep(1e-10, length(weight_Adaptive1)) 
            # weight_Adaptive <- weight_Adaptive1 + tmp_denominator
            # 
            weight_Adaptive <- select_prob[colnames(XX)]  
            
            # validataion....part... adaptive
            alasso <- glmnet(as.matrix(XX), YY, standardize = FALSE, alpha = 1, penalty.factor = 1 / weight_Adaptive,
                             lambda=cv.glmnet(as.matrix(XX), YY, type.measure = "mse", nfold = 5, alpha = 1)$lambda.min, intercept=FALSE)
            
            r_names <- rownames(coef(alasso))[-1]
            hat_beta <- coef(alasso)[-1]
            names(hat_beta) <- r_names
            Adj_temp[names(hat_beta), j] <- hat_beta/sd_x 
            print("propose step2 : Adaptive LASSO")
        }
    }# end of for bootstrapping 
    # average the coefficient 
    
    Adj_gene_net_b[, 1] <- rowSums(Adj_temp,      na.rm = TRUE) / repeat_Boostrapping # beta hat
    Adj_gene_net_b[, 2] <- rowSums(abs(Adj_temp), na.rm = TRUE) / repeat_Boostrapping # importance
    
    #Adj_gene_net_b[, 1] <- rowMeans(Adj_temp,      na.rm = TRUE)  # beta hat
    #Adj_gene_net_b[, 2] <- rowMeans(abs(Adj_temp), na.rm = TRUE)  # importance
    
    Adj_gene_net_b[, 3] <- rowSums(Adj_temp!=0, na.rm = TRUE) # C_j 
    n_boot <- rowSums(Adj_temp == 0, na.rm = TRUE) + rowSums(Adj_temp !=0, na.rm = TRUE)
    n_boot[is.na(n_boot)  == TRUE] <- 0
    
    if ( ((num_x*repeat_Boostrapping - sum(is.na(Adj_temp))) == 0)) 
    {  
        Adj_gene_net_b[, 4] <- 1 
        Adj_gene_net_b[, 5] <- 1
        
    }else
    {
        hat_pi <- sum(Adj_temp!=0, na.rm = TRUE)/((num_x*repeat_Boostrapping) - sum(is.na(Adj_temp)))
        Adj_gene_net_b[, 4] <- pbinom(Adj_gene_net_b[, 3], repeat_Boostrapping,  hat_pi, lower.tail = FALSE, log.p = FALSE)# p_value 
        Adj_gene_net_b[, 5] <- rowSums(Adj_temp == 0,  na.rm = TRUE) / (rowSums(Adj_temp == 0, na.rm = TRUE) + rowSums(Adj_temp !=0, na.rm = TRUE))
    }
    Adj_gene_net_b[, 4][n_boot[is.na(n_boot) == TRUE]] <- 1
    
    return(Adj_gene_net_b)
} # end of randomLasso ftn  
