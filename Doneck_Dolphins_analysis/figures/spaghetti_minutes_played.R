# Chart minutes played by each player in each game-------------

# 0. Reading data--------------
library("readxl")
# xls files
trier_data <- read_excel("./data/data_trier.xlsx")
MP <- gsub("^.*?00","00",as.character(trier_data$MP))

parse_time <- function(x) {
  res <- do.call(rbind, strsplit(x, ":", TRUE))
  mode(res) <- "numeric"
  c(res %*% (1/c(1/60, 1, 60)))
}

trier_data$MP <- parse_time(MP)

# 1. Cleaning_data--------

# 1. 1. EFF 

trier_data$EFF <- trier_data$points + trier_data$rebounds + trier_data$assists +
  trier_data$steals + trier_data$blocks - trier_data$misses - trier_data$misses_ft -
  trier_data$turnovers

# 1. 2. PIR 

trier_data$PIR <- trier_data$points + trier_data$rebounds + trier_data$assists +
  trier_data$steals + trier_data$blocks + trier_data$rec_fouls - trier_data$misses - 
  trier_data$misses_ft -  trier_data$turnovers - trier_data$rec_blocks - 
  trier_data$fouls

# 1. 3. Win score 

trier_data$Winscore <- trier_data$points + trier_data$rebounds + .5 * trier_data$assists +
  trier_data$steals + .5 * trier_data$blocks - trier_data$turnovers - 
  .5 * trier_data$fouls - trier_data$FGA - .5 * trier_data$FTA # falta -FGA - .5 * FTA


# Cleaning

trier_data$EFF_per_min <- trier_data$EFF / trier_data$MP
trier_data$PIR_per_min <- trier_data$PIR / trier_data$MP
trier_data$Winscore_min <- trier_data$Winscore / trier_data$MP



trier_data$EFF_per_min[is.nan(trier_data$EFF_per_min)]<-NA
trier_data$PIR_per_min[is.nan(trier_data$PIR_per_min)]<-NA
trier_data$Winscore_min[is.nan(trier_data$Winscore_min)]<-NA



# 2. Factorizing home variable

trier_data$home[trier_data$home == 'Y'] <- 1
trier_data$home[trier_data$home == 'N'] <- 0

# 3. Player

trier_data$player[trier_data$player == 'Natalie Passiwan'] <-
  'Nathalie Passiwan'
trier_data$player[trier_data$player == 'Nathalie Passivan'] <-
  'Nathalie Passiwan'
trier_data$player[trier_data$player == 'Dirk Passivan'] <-
  'Dirk Passiwan'
trier_data$player[trier_data$player == 'Natalie Passivan'] <-
  'Nathalie Passiwan'
trier_data$player[trier_data$player == 'Walter Vlandeeren'] <-
  'Walter Vlaanderen'

unique(trier_data$player)

trier_data$player <- as.factor(trier_data$player)

levels(trier_data$player)

trier_data$id_player <- as.numeric(trier_data$player)

# 3. Sex

trier_data$sex[trier_data$id_player == 1] <- 1
trier_data$sex[trier_data$id_player == 7] <- 1
trier_data$sex[trier_data$id_player == 9] <- 1
trier_data$sex[trier_data$id_player == 11] <- 1
trier_data$sex[trier_data$id_player == 12] <- 1
trier_data$sex[is.na(trier_data$sex)] <- 0

# 4. Classification

trier_data$class[trier_data$id_player == 1] <- 1.5
trier_data$class[trier_data$id_player == 2] <- 4.5
trier_data$class[trier_data$id_player == 3] <- 2
trier_data$class[trier_data$id_player == 4] <- 3.5
trier_data$class[trier_data$id_player == 5] <- 4.5
trier_data$class[trier_data$id_player == 6] <- NA #2.5 
trier_data$class[trier_data$id_player == 7] <- 2.5 #1
trier_data$class[trier_data$id_player == 8] <- 1 #4.5
trier_data$class[trier_data$id_player == 9] <- 4.5 #3.5
trier_data$class[trier_data$id_player == 10] <- 3.5
trier_data$class[trier_data$id_player == 11] <- 3.5
trier_data$class[trier_data$id_player == 12] <- 2.5
trier_data$class[trier_data$id_player == 13] <- 4.5


# 5. Checking minutes played

library(sqldf)

mp_player <- sqldf("SELECT sum(MP), player FROM trier_data GROUP BY player")

Trier_data <- subset(trier_data, 
                     subset = !(player %in% mp_player$player[mp_player$`sum(MP)`< 40]) )

# mp_player_nw <- sqldf("SELECT sum(MP), player FROM Trier_data GROUP BY player")

Trier_data$player <- as.character(Trier_data$player)

Trier_data$player <- as.factor(Trier_data$player)

levels(Trier_data$player)

Trier_data$id_player <- as.numeric(Trier_data$player)

Trier_data <- subset(Trier_data, 
                     subset = MP>2 )

# Players with less than 40 minutes of play have been removed, and metrics 
# calculated with less than 2 minutes...

# 6. Save data

Trier_data <- data.frame(id_game = Trier_data$id_game, player = Trier_data$player,
                         id_player = Trier_data$id_player, 
                         PIR_per_min = Trier_data$PIR_per_min, 
                         EFF_per_min = Trier_data$EFF_per_min,
                         Winscore_min = Trier_data$Winscore_min,
                         home = Trier_data$home, sex = Trier_data$sex, 
                         class = Trier_data$class, MP = Trier_data$MP)



# 7. Final cleaning

library(GLDEX)

# Trier_data <- Trier_data[-which.na(Trier_data$PIR_per_min),]



Trier_data$id_player <- as.numeric(as.factor(as.character(Trier_data$player)))

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




Trier_data$id_event <- paste(Trier_data$id_game, Trier_data$player)

#univers
gamess <- c(1:13,15:18)
playerss <- c('Annabel Breuer', 'Correy Rossi', 'Dejon Green', 'Dirk Passiwan',
              'Lukas Jung', 'Nathalie Passiwan', 'Patrick Dorner', 'Svenja Erni', 
              'Walter Vlaanderen')
univers <- c()


for(i in gamess){
  for(j in playerss){
    univers <- c(univers, paste(i,j))
  }
}

setdiff(univers, Trier_data$id_event)

Trier_data[135:153, 12] <- setdiff(univers, Trier_data$id_event)
Trier_data[135:153, 10] <- rep(0, 19)


Trier_data$Player[135:153] <- as.factor(c('L.J.', 'S.E.', 'L.J.', 'S.E.', 'A.B.', 
                                          'N.P.', 'L.J.', 'S.E.', 'S.E.', 'D.P.', 
                                          'N.P.', 'L.J.', 'N.P.', 'S.E.', 'L.J.', 
                                          'S.E.', 'N.P.', 'D.G.', 'S.E.'))

Trier_data$id_game[135:153] <- c(1, 1, 5, 5, 7, 7, 8, 8, 9, 10, 10, 12, 12, 12, 
                                 13, 13, 15, 16, 17)

levels(Trier_data$Player)

Trier_data$Player <- as.factor(Trier_data$Player)


# 2. Plot-------------------------
library(ggplot2)

ggplot(Trier_data, aes(id_game, MP,
                                                      colour = Player)) +
  geom_point(size = 3) +
  geom_line(size = 2, linetype = 'solid') +
  labs(x="Game", y="Minutes played")+
  coord_cartesian(ylim = c(0,40))+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(legend.position = "right")+
  theme(axis.line = element_line(color="black", size = 0.5))+
  theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 16),
    plot.margin = margin(0.5, -0.2, 0, 0, "cm") )+
  theme_classic(base_size = 20, base_line_size = 15/20) +
  theme(#plot.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    #panel.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    panel.grid.major = element_line(colour = rgb(212/255, 211/255, 217/255)),
    #legend.background = element_rect(fill = rgb(212/255, 211/255, 217/255)),
    legend.position = "right", 
    legend.text = element_text( size = 14)) +
  scale_colour_brewer(palette = "Set1", guide = guide_legend(ncol=1))
