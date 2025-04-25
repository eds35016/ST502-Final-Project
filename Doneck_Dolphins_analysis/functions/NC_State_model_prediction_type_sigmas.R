model_prediction_type_sigmas <- function(player = players, type="EFF", match = tt){
  #player is numeric
  # 1. Loading posterior iterations-------------------------
  library(rjags)
  library(MCMCvis)
  
  #type in ("EFF", "Winscore", "TS")
  #input----------
  
  # load(paste('./results/res_mixed_model_Trier_', type, '2.RData', sep = ''))
  # load('./data/Trier_data_clean_new.RData')
  
  load(paste('../../results/NC_State_data/NC_State_res_mixed_model_Trier_', type, '2.RData', sep = ''))
  load('../../../new_data/NC_State_Trier_data_clean_new.RData')
  
  
  sex <- c()
  class <- c()
  
  T <- match
  C <- 3
  I <- 1000
  
  b0 <- matrix(data = NA, nrow = length(player), ncol = I * C)
  b1 <- matrix(data = NA, nrow = length(player), ncol = I * C)
  sigma <- matrix(data = NA, nrow = length(player), ncol = I * C)
  
  predictive <- matrix(data = NA, nrow = length(player), ncol = I * C)
  
  beta0 <- as.vector(unlist(MCMCchains(rsamps,
                                       params = "beta0", ISB = F)))
  
  beta1 <- as.vector(unlist(MCMCchains(rsamps,
                                       params = "beta1", ISB = F)))
  betaC <- as.vector(unlist(MCMCchains(rsamps,
                                       params = "betaC", ISB = F)))
  betaH <- as.vector(unlist(MCMCchains(rsamps,
                                       params = "betaH", ISB = F)))
  betaW <- as.vector(unlist(MCMCchains(rsamps,
                                       params = "betaW", ISB = F)))
  
  sigma0m <- as.vector(unlist(MCMCchains(rsamps,
                                         params = 'sigma0m', ISB = F)))
  
  
  
  for(p in 1 : length(player)){
    
    sex[p] <- unique(Trier_data$sex[Trier_data$id_player == player[p]])
    class[p] <- unique(Trier_data$class[Trier_data$id_player == player[p]])
    
    
    
    b0[p,] <- as.vector(unlist(MCMCchains(rsamps,
                                          params = paste('b0[',p,']', sep = ''), ISB = F)))
    b1[p,] <- as.vector(unlist(MCMCchains(rsamps,
                                          params = paste('b1[',p,']', sep = ''), ISB = F)))
    
    sigma[p,] <- as.vector(unlist(MCMCchains(rsamps,
                                       params = paste('sigma[',p,']', sep = ''), ISB = F)))
    
    
    for(i in 1 : (I * C)){
      m <- rnorm(1, 0, sd = sigma0m[i])
      predictive[p, i] <- rnorm(1, beta0[i] + b0[p, i] + m + betaW[i] * sex[p]
                                + betaC[i] * class[p] + betaH[i] + (beta1[i] + b1[p, i]) * (T), 
                                sd = sigma[p, i])
    }
    
  }
  
  return(predictive)
}
