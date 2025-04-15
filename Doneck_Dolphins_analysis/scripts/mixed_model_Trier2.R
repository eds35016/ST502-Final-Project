load('./data/Trier_data_clean_new.RData')
set.seed(56888)

# jags model PIR ----------------

library('rjags')
library(mcmcplots)
useful <- which(!is.na(Trier_data$PIR_per_min)) 
data_model <- list(Y = Trier_data$PIR_per_min,  
                   N = max(unique(Trier_data$id_player)), useful = useful,
                   id_player = Trier_data$id_player, 
                   id_game = Trier_data$id_game,
                   W = Trier_data$sex, Class = Trier_data$class, 
                   home = Trier_data$home)
inits <- function() {
  list( beta0 = runif(1, 4, 8), beta1 = runif(1, -5, 5),
        sigma = runif(9, 0, 2), sigma0 = runif(1, 0, 2), 
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics
summary(rsamps)$quantiles

summary(rsamps)$statistics[c(37:53),]
summary(rsamps)$quantiles[37:53,]

save(rsamps, file = './results/res_mixed_model_Trier_PIR2.RData')
# load('./results/res_mixed_model_Trier_PIR2.RData')
# there is differences among players in the overall level of performance

# jags model EFF ----------------

library(rjags)
library(mcmcplots)
useful <- which(!is.na(Trier_data$EFF_per_min)) 
data_model <- list(Y = Trier_data$EFF_per_min,  
                   N = max(unique(Trier_data$id_player)), useful = useful,
                   id_player = Trier_data$id_player, 
                   id_game = Trier_data$id_game,
                   W = Trier_data$sex, Class = Trier_data$class, 
                   home = Trier_data$home)
inits <- function() {
  list( beta0 = runif(1, 4, 8), beta1 = runif(1, -5, 5),
        sigma = runif(9, 0, 2), sigma0 = runif(1, 0, 2), 
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics[37:53,]
summary(rsamps)$quantiles[37:53,]
#dic <- dic.samples(result_ar1, n.iter = 100000, thin = 100)

save(rsamps, file = './results/res_mixed_model_Trier_EFF2.RData')
# load('./results/res_mixed_model_Trier_EFF.RData')
# there is differences among players in the overall level of performance

# jags model Winscore ----------------

library(rjags)
library(mcmcplots)
useful <- which(!is.na(Trier_data$Winscore_min)) 
data_model <- list(Y = Trier_data$Winscore_min,  
                   N = max(unique(Trier_data$id_player)), useful = useful,
                   id_player = Trier_data$id_player, 
                   id_game = Trier_data$id_game,
                   W = Trier_data$sex, Class = Trier_data$class, 
                   home = Trier_data$home)
inits <- function() {
  list( beta0 = runif(1, 4, 8), beta1 = runif(1, -5, 5),
        sigma = runif(9, 0, 2), sigma0 = runif(1, 0, 2), 
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics[37:53,]
summary(rsamps)$quantiles[37:53,]
#dic <- dic.samples(result_ar1, n.iter = 100000, thin = 100)

save(rsamps, file = './results/res_mixed_model_Trier_Winscore2.RData')
# load('./results/res_mixed_model_Trier_Winscore2.RData')