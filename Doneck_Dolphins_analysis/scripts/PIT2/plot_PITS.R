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
