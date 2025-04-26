load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/PIT_EFF2.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/PIT_PIR2.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/PIT_WIN2.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/PIT_TS2.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/data/Trier_data_clean_new.RData')


# Final plot-------------------
plot(Trier_data$id_game, PIT_EFF, cex.lab=1.5, cex.axis=1.5, main = NULL,
     xlab = 'Match', ylab = 'PIT', lwd = 3, ylim = c(0,1), pch = 2, col='#fde725')
lines(Trier_data$id_game, PIT_PIR, cex.lab=1.5, cex.axis=1.5, main = NULL,
      xlab = 'Match', ylab = 'PIT', lwd = 3, ylim = c(0,1), pch = 3, 
      col='#21918c', type = 'p')
lines(Trier_data$id_game, PIT_WIN, cex.lab=1.5, cex.axis=1.5, main = NULL,
      xlab = 'Match', ylab = 'PIT', lwd = 3, ylim = c(0,1), pch = 4, 
      col='#440154', type = 'p')
lines(Trier_data$id_game, PIT_TS, cex.lab=1.5, cex.axis=1.5, main = NULL,
      xlab = 'Match', ylab = 'PIT', lwd = 3, ylim = c(0,1), pch = 4, 
      col='#FF69B4', type = 'p')
legend('top', legend=c("EFF", "PIR", "Win Score", "TS%"),
       col=c("#fde725", "#21918c", '#440154', '#FF69B4'),pch=2:4, cex=1.1,
       horiz = T)
abline(h = 0.5, col = 'blue', lty = 2, lwd = 3)


# Plots by player-----------------
for(i in 1:9){
  plot(Trier_data$id_game[Trier_data$id_player==i], 
       PIT_EFF[Trier_data$id_player==i], cex.lab=1.5, cex.axis=1.5,
       xlab = 'Match', ylab = 'PIT', lwd = 3, ylim = c(0,1), pch = 2, 
       col='#fde725', main=paste('player',i))
}

# Find prob of ind players
# Function to calculate lineup matrix and probabilities for all players
calc_all_probs <- function(posterior) {
  Lineups <- matrix(data = NA, nrow = 3000, ncol = 9)
  for(i in 1:3000) {
    Lineups[i, ] <- as.numeric(substring(posterior[i], seq(1,17, by=2), seq(1,17, by=2)))
  }
  probs <- colMeans(Lineups)  # Get probability for each player (1-9)
  return(probs)
}

# Calculate for each metric
probs_PIR <- calc_all_probs(posterior_PIR)
probs_EFF <- calc_all_probs(posterior_EFF)
probs_Winscore <- calc_all_probs(posterior_Winscore)
probs_TS <- calc_all_probs(posterior_TS)

# Create a dataframe
probs_named <- data.frame(
  player = rep(1:9, times = 4),
  prob = c(probs_PIR, probs_EFF, probs_Winscore, probs_TS),
  metric = rep(c("PIR", "EFF", "Winscore", "TS%"), each = 9)
)

library(ggplot2)

player_names <- c(
  "A. Breuer", "C. Rossi", "D. Green", "D. Passivan", "L. Jung",
  "N. Passivan", "P. Dorner", "S. Erni", "W. Vlaanderen"
)

p <- ggplot(probs_named,
            aes(x = factor(player), y = prob, shape = metric, color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_shape_manual(
    values = c("EFF" = 16, "Winscore" = 15, "TS%" = 17, "PIR" = 18)
  ) +
  scale_y_continuous("Selection Probability", labels = scales::number_format(accuracy = 0.01)) +
  scale_x_discrete(labels = player_names) +
  labs(x = NULL, color = "Metric", shape = "Metric") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p)


# skip conditional probabilities for now

#players <- c(6, 7, 4) # indices of the players
#count <- 0
#for(i in 1 : 3000){
#  logic <- as.numeric(Lineups[i, players] == c(1,1,0))
#  if(sum(logic) == length(players)){count <- count + 1}
#}
#prob2 <- count / 3000 # 0.014333333

#prob2 / (1-prob) # conditional probabilities 0.7049

#players <- c(6, 7) # indices of the players
#count <- 0
#for(i in 1 : 3000){
#  logic <- as.numeric(Lineups[i, players] == c(1,1))
#  if(sum(logic) == length(players)){count <- count + 1}
#}
#prob2 <- count / 3000 #0.1663333
