#draws
# 1. Saving posterior iterations-------------------------
library(rjags)
library(MCMCvis)

load('./results/res_mixed_model_Trier_PIR2.RData') #rsamps

n <- 9
matches <- 18
I <- 1000
C <- 3

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

sigma0 <- as.vector(unlist(MCMCchains(rsamps,
                                         params = "sigma0", ISB = F)))
sigma0m <- as.vector(unlist(MCMCchains(rsamps,
                                         params = "sigma0m", ISB = F)))
sigma1 <- as.vector(unlist(MCMCchains(rsamps,
                                     params = "sigma1", ISB = F)))
b0 <- matrix(data = NA, nrow = n, ncol = C * I)
b1 <- matrix(data = NA, nrow = n, ncol = C * I)
sigma <- matrix(data = NA, nrow = n, ncol = C * I)
b0m <- matrix(data = NA, nrow = matches, ncol = C * I)


for(nn in 1 : n){
  b0[nn, ] <- as.vector(unlist(MCMCchains(rsamps,
                                           params = paste('b0[', nn, ']', sep = ''), 
                                           ISB = F)))
  b1[nn, ] <- as.vector(unlist(MCMCchains(rsamps,
                                           params = paste('b1[', nn, ']', sep = ''), 
                                           ISB = F)))
  sigma[nn, ] <- as.vector(unlist(MCMCchains(rsamps,
                                             params = paste('sigma[', nn, ']', sep = ''), 
                                             ISB = F)))
}

for(nn in 1 : matches){
  b0m[nn, ] <- as.vector(unlist(MCMCchains(rsamps,
                                          params = paste('b0m[', nn, ']', sep = ''), 
                                          ISB = F)))
}

draws_posterior <- cbind(beta0, beta1, betaC, betaH, betaW, sigma0,
                         sigma0m, sigma1, t(b0), t(b1), t(b0m), t(sigma))

#draws_posterior
# beta0 column 1
# beta1 column 2
# betaC column 3
# betaH column 4
# betaW column 5

# sigma0 column 6
# sigma0m column 7
# sigma1 column 8

# b0 columns 9 - 17
# b1 columns 18 - 26
# b0m columns 27-44
# sigma columns 45-53

rm(rsamps, beta0, beta1, betaC, betaH, betaW, sigma, sigma0, sigma0m, sigma1, 
   b0, b1, b0m, C, I, matches, n, nn)
