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

# Compute mean PIT per player for each metric (Maddy's code - not sure this is right)
mean_EFF <- tapply(PIT_EFF, Trier_data$id_player, mean)
mean_PIR <- tapply(PIT_PIR, Trier_data$id_player, mean)
mean_WIN <- tapply(PIT_WIN, Trier_data$id_player, mean)
mean_TS  <- tapply(PIT_TS,  Trier_data$id_player, mean)

# Set up x-axis
players <- as.factor(names(mean_EFF))

# Plot setup
plot(players, mean_EFF, ylim = c(0,1), type = 'b', pch = 2, col = '#fde725',
     xlab = 'Player', ylab = 'Mean PIT', lwd = 2, cex.lab = 1.5, cex.axis = 1.2)

# Add lines for other metrics
points(players, mean_PIR, type = 'b', pch = 3, col = '#21918c', lwd = 2)
points(players, mean_WIN, type = 'b', pch = 4, col = '#440154', lwd = 2)
points(players, mean_TS,  type = 'b', pch = 5, col = '#FF69B4', lwd = 2)

# Add legend
legend('topright', legend = c("EFF", "PIR", "Win Score", "TS%"),
       col = c("#fde725", "#21918c", "#440154", "#FF69B4"), pch = 2:5, lwd = 2)
