library(rjags)
source('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/scripts/PIT2/draws_PIR.R') #saved in draws_posterior

load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/data/Trier_data_clean_new.RData')
Trier_data$home <- as.numeric(Trier_data$home) # was character 

source('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/functions/suma_logaritmica_de_elementos.R') #function

size_data <- dim(Trier_data)[1]
size_draws <- dim(draws_posterior)[1]

# weights

log_weights_i <- function(data_i, draws, log = TRUE) {
  mu <- c()
  sigma <- c()
  
  
  for(i in 1 : dim(draws)[1]){
    #print(i/3000*100)
    mu[i] <-  draws[i, 1] + draws[i, 8 + data_i$id_player] + # beta0 + b0 +
      draws[i, 26 + data_i$id_player] + draws[i, 5] * data_i$sex + # b0j +betaW
      draws[i, 3] * data_i$class + draws[i, 4] * data_i$home + # betaC + betaH
      (draws[i, 2] + draws[i, 17 + data_i$id_player]) * data_i$id_game
    sigma[i] <- draws[i, 44 + data_i$id_player]
    
  }
  
  log_likelihood <- dnorm(x = data_i$PIR_per_min, mean = mu, sd = sigma, 
                          log = log)
  
  inv_loglik <- - log_likelihood
  
  log_w <- inv_loglik - suma_logaritmica_de_elementos(inv_loglik)
  
  
  
  return(log_w) 
}






# una obs.
size_pred <- 1000
y_pred <- matrix(data = NA, nrow = size_data, ncol = size_pred)

PIT <- c()
for(obs in 1 : size_data){
  
  
  
  prob_w <- exp(log_weights_i(Trier_data[obs, ], draws_posterior)) 
  
  
  for(i in 1 : size_pred){
    idx <- sample(1:dim(draws_posterior)[1],1, prob = prob_w)
    
    sigma <- draws_posterior[idx, 44 + Trier_data$id_player[obs]]
    
    
    mu <-  draws_posterior[idx, 1] + draws_posterior[idx, 8 + Trier_data$id_player[obs]] + # beta0 + b0 +
      draws_posterior[i, 26 + Trier_data$id_player[obs]] + draws_posterior[i, 5] * Trier_data$sex[obs] + # b0j +betaW
      draws_posterior[i, 3] * Trier_data$class[obs] + draws_posterior[i, 4] * Trier_data$home[obs] + # betaC + betaH
      (draws_posterior[i, 2] + draws_posterior[i, 17 + Trier_data$id_player[obs]]) * Trier_data$id_game[obs]
    
    
    
    y_pred[obs,i] <- rnorm(1, mean = mu, sd = sigma)
    
    
    
  }
  
  PIT[obs] <- sum(y_pred[obs,] <= Trier_data$PIR_per_min[obs]) / size_pred
}

#plots
hist(PIT, freq = F, col = 'white', cex.lab=1.5, cex.axis=1.5, main = NULL)

par(mar=c(5,5,2,2)+0.1)
plot(Trier_data$id_game, PIT, cex.lab=1.5, cex.axis=1.5, main = NULL,
     xlab = 'IAP', lwd = 3)

PIT_PIR <- PIT
save(PIT_PIR, file = '/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/PIT_PIR2.RData')
