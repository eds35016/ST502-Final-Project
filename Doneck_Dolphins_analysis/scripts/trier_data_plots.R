setwd("/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/data")
load('./data/Trier_data_clean_new.RData')
#load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/data/Trier_data_clean_new.RData')
library(ggplot2)

Trier_data$id_player <- as.factor(Trier_data$id_player)

player_abv <- c()
for (i in 1 : length(Trier_data[, 1])) {
  if(Trier_data$id_player[i] == 1){player_abv[i] <- "A.B."}
  if(Trier_data$id_player[i] == 2){player_abv[i] <- "C.R."}
  if(Trier_data$id_player[i] == 3){player_abv[i] <- "D.G."}
  if(Trier_data$id_player[i] == 4){player_abv[i] <- "D.P."}
  if(Trier_data$id_player[i] == 5){player_abv[i] <- "L.J."}
  if(Trier_data$id_player[i] == 6){player_abv[i] <- "N.P."}
  if(Trier_data$id_player[i] == 7){player_abv[i] <- "P.D."}
  if(Trier_data$id_player[i] == 8){player_abv[i] <- "S.E."}
  if(Trier_data$id_player[i] == 9){player_abv[i] <- "W.V."}
}

Trier_data$Player <- as.factor(player_abv)
# 1. Plot--------------




# PIR-----------
library(RColorBrewer)
ggplot(Trier_data, aes(id_game, PIR_per_min, group = Player, colour = Player)) +
  geom_point(size = 5) +
  geom_line(size = 3, linetype = 'solid') +
  labs(x="Game", y=expression("PIR/min")) +
  theme_classic(base_size = 20, base_line_size = 15/20) +
  theme(#plot.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        #panel.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        panel.grid.major = element_line(colour = rgb(212/255, 211/255, 217/255)),
        #legend.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        legend.position = "right", 
        legend.text = element_text( size = 14)) +
  scale_colour_brewer(palette = "Set1", guide = guide_legend(ncol=1))+
  scale_x_continuous(breaks=seq(0, 18, 2))+
  scale_y_continuous(breaks=seq(-1.25, 2.25, .25), limits = c(-1, 2))



# EFF--------------

ggplot(Trier_data, aes(id_game, EFF_per_min, group = Player, colour = Player)) +
  geom_point(size = 5) +
  geom_line(size = 3, linetype = 'solid') +
  labs(x="Game", y=expression("EFF/min")) +
  theme_classic(base_size = 20, base_line_size = 15/20) +
  theme(#plot.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        #panel.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        panel.grid.major = element_line(colour = rgb(212/255, 211/255, 217/255)),
        #legend.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        legend.position = "right", 
        legend.text = element_text( size = 14)) +
  scale_colour_brewer(palette = "Set1", guide = guide_legend(ncol=1))+
  scale_x_continuous(breaks=seq(0, 18, 2))+
  scale_y_continuous(breaks=seq(-1.25, 2.25, .25), limits = c(-1, 2))


# Winscore-------------------

ggplot(Trier_data, aes(id_game, Winscore_min, group = Player, colour = Player)) +
  geom_point(size = 5) +
  geom_line(size = 3, linetype = 'solid') +
  labs(x="Game", y=expression("Win Score/min")) +
  theme_classic(base_size = 20, base_line_size = 15/20) +
  theme(#plot.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        #panel.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        panel.grid.major = element_line(colour = rgb(212/255, 211/255, 217/255)),
        #legend.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
        legend.position = "right",
        legend.text = element_text( size = 14)) +
  scale_colour_brewer(palette = "Set1", guide = guide_legend(ncol=1))+
  scale_x_continuous(breaks=seq(0, 18, 2))+
  scale_y_continuous(breaks=seq(-1.25, 2.25, .25), limits = c(-1, 2))


# TrueShooting%-------------------

ggplot(Trier_data, aes(id_game, TS_min, group = Player, colour = Player)) +
  geom_point(size = 5) +
  geom_line(size = 3, linetype = 'solid') +
  labs(x="Game", y=expression("True Shooting % /min")) +
  theme_classic(base_size = 20, base_line_size = 15/20) +
  theme(#plot.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    #panel.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    panel.grid.major = element_line(colour = rgb(212/255, 211/255, 217/255)),
    #legend.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    legend.position = "right",
    legend.text = element_text( size = 14)) +
  scale_colour_brewer(palette = "Set1", guide = guide_legend(ncol=1))+
  scale_x_continuous(breaks=seq(0, 18, 2))+
  scale_y_continuous(breaks=seq(-1.25, 2.25, .25), limits = c(-1, 2))