# load('./results/lineups_EFF.RData')
# load('./results/lineups_PIR.RData')
# load('./results/lineups_winscore.RData')
# load('./results/lineups_TS.RData')

load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_EFF_sigmas.RData')
#load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_PIR_sigmas.RData')
#load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_winscore_sigmas.RData')
#load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_TS_sigmas.RData')


# Probability of being in the lineup
posterior <- posterior_EFF
Lineups <- matrix(data = NA, nrow = 3000, ncol = 9)
for(i in 1 : 3000){ #all the lineups
  Lineups[i, ] <- as.numeric(substring(posterior[i], seq(1,17, by = 2), 
                                       seq(1,17, by = 2)))
}
# probabilities
players <- c(4) # indices of the players
count <- 0
for(i in 1 : 3000){
  logic <- as.numeric(Lineups[i, players] == rep(1, length(players)))
  if(sum(logic) == length(players)){count <- count + 1}
}
prob <- count / 3000 # 0.9796667


players <- c(6, 7, 4) # indices of the players
count <- 0
for(i in 1 : 3000){
  logic <- as.numeric(Lineups[i, players] == c(1,1,0))
  if(sum(logic) == length(players)){count <- count + 1}
}
prob2 <- count / 3000 # 0.014333333

prob2 / (1-prob) # conditional probabilities 0.7049

players <- c(6, 7) # indices of the players
count <- 0
for(i in 1 : 3000){
  logic <- as.numeric(Lineups[i, players] == c(1,1))
  if(sum(logic) == length(players)){count <- count + 1}
}
prob2 <- count / 3000 #0.1663333
