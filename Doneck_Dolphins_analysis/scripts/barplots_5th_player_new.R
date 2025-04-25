# 1. Loading data-----------
#load('./results/lineups_PIR_sigmas.RData')
#load('./results/lineups_EFF_sigmas.RData')
#load('./results/lineups_Winscore_sigmas.RData')
#load('./results/lineups_TS_sigmas.RData')

load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_PIR_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_EFF_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_Winscore_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_TS_sigmas.RData')


# 2. Posterior probability of compatibility-----------

# Probability of being in the lineup
posterior <- posterior_TS
Lineups <- matrix(data = NA, nrow = 3000, ncol = 9)
for(i in 1 : 3000){ #all the lineups
  Lineups[i, ] <- as.numeric(substring(posterior[i], seq(1,17, by = 2), 
                                       seq(1,17, by = 2)))
}

players <- c(1, 2, 4, 9) # indices of the players
count <- 0
for(i in 1 : 3000){
  logic <- as.numeric(Lineups[i, players] == rep(1, length(players)))
  if(sum(logic) == length(players)){count <- count + 1}
}
prob <- count / 3000

# to get the probability values in plots below, rerun for conditional prob
# by changing the last player number in this players vector below
# test the last player as 3, 5, 6, 7, and 8 and record cond prob
players <- c(1, 2, 4, 9, 8) # indices of the players
count <- 0
for(i in 1 : 3000){
  logic <- as.numeric(Lineups[i, players] == rep(1, length(players)))
  if(sum(logic) == length(players)){count <- count + 1}
}
prob2 <- count / 3000

prob2 / prob # conditional probabilities

# plot-----------

p_player <- data.frame(Player = rep(c("D. Green", "L. Jung", "N. Passivan", 
                                      "P. Dorner", "S. Erni"), 4),
                       Probability = c(0.16, 0.01, 0.24, 0.47, 0.12,
                                       0.19, 0.02, 0.25, 0.44, 0.10,
                                       0.15, 0.04, 0.25, 0.34, 0.22,
                                       0.23, 0.07, 0.23, 0.24, 0.22),
                       Metric=c(rep("EFF",5), rep("PIR",5), rep("Win Score",5), rep("TS%",5)))

library(ggplot2)
library(viridis)

ggplot(p_player, aes(x=Player, y=Probability, fill = Metric)) + 
  geom_bar(stat = "identity", position = position_dodge())  +
  scale_fill_viridis(discrete = TRUE, option = "D")+
  theme_test(base_size = 20, base_line_size = 15/20) +
  labs(x=element_blank())
