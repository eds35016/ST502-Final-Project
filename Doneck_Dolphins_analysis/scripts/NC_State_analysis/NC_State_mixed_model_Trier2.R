#load('./data/Trier_data_clean_new.RData')
load('../../../new_data/NC_State_Trier_data_clean_new.RData')
set.seed(56888)

# 1. unique men's games, ordered by their numeric part
men <- unique(Trier_data$id_game[grepl("^M", Trier_data$id_game)])
men <- men[order(as.integer(sub("^M", "", men)))]

# 2. same for women's games
women <- unique(Trier_data$id_game[grepl("^W", Trier_data$id_game)])
women <- women[order(as.integer(sub("^W", "", women)))]

# 3. all games, men first then women
all_games <- c(men, women)

# 4. create new sequential index
Trier_data$game_number <- as.integer(factor(Trier_data$id_game, levels = all_games))
Trier_data$id_game <- Trier_data$game_number

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
        sigma = runif(10, 0, 2), sigma0 = runif(1, 0, 2), # sigma changed to 10 since we have 10 players
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


# results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
#                       n.adapt = 0)
results <- jags.model("../../models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics[37:53,]
summary(rsamps)$quantiles[37:53,]
#dic <- dic.samples(result_ar1, n.iter = 100000, thin = 100)

#save(rsamps, file = './results/res_mixed_model_Trier_EFF2.RData')
save(rsamps, file = '../../results/NC_State_data/NC_State_res_mixed_model_Trier_EFF2.RData')
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
        sigma = runif(10, 0, 2), sigma0 = runif(1, 0, 2), # sigma changed to 10 since we have 10 players
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


# results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
#                       n.adapt = 0)
results <- jags.model("../../models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics[37:53,]
summary(rsamps)$quantiles[37:53,]
#dic <- dic.samples(result_ar1, n.iter = 100000, thin = 100)

#save(rsamps, file = './results/res_mixed_model_Trier_Winscore2.RData')
save(rsamps, file = '../../results/NC_State_data/NC_State_res_mixed_model_Trier_Winscore2.RData')

# load('./results/res_mixed_model_Trier_Winscore2.RData')

# jags model True Shooting % ----------------

library(rjags)
library(mcmcplots)
useful <- which(!is.na(Trier_data$TS_min)) 
data_model <- list(Y = Trier_data$TS_min,  
                   N = max(unique(Trier_data$id_player)), useful = useful,
                   id_player = Trier_data$id_player, 
                   id_game = Trier_data$id_game,
                   W = Trier_data$sex, Class = Trier_data$class, 
                   home = Trier_data$home)
inits <- function() {
  list( beta0 = runif(1, 4, 8), beta1 = runif(1, -5, 5),
        sigma = runif(10, 0, 2), sigma0 = runif(1, 0, 2), # sigma changed to 10 since we have 10 players
        sigma1 = runif(1, 0, 2))
}
parameters <- c( "beta0", "beta1", "betaW", "betaC", "betaH", "b0", "b0m",  "b1", 
                 "sigma", "sigma0",  "sigma0m", "sigma1")


# results <- jags.model("./models/mixed_model_Trier2", data_model, inits, n.chains = 3,
#                       n.adapt = 0)
results <- jags.model("../../models/mixed_model_Trier2", data_model, inits, n.chains = 3,
                      n.adapt = 0)

update(results, n.iter = 300000)
rsamps <- coda.samples(results, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps)
summary(rsamps)$statistics[37:53,]
summary(rsamps)$quantiles[37:53,]
#dic <- dic.samples(result_ar1, n.iter = 100000, thin = 100)

#save(rsamps, file = './results/res_mixed_model_Trier_TS2.RData')
save(rsamps, file = '../../results/NC_State_data/NC_State_res_mixed_model_Trier_TS2.RData')
# load('./results/res_mixed_model_Trier_TS2.RData')