library("readxl")
# xls files
# Use setwd("/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/scripts")
#trier_data <- read_excel("../data/data_trier.xlsx")
#trier_data <- read_excel("/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/data/data_trier.xlsx")

trier_data_men   <- read.csv("../../../new_data/mens_player_stats_across_games_formatted.csv",   stringsAsFactors = FALSE)
trier_data_women <- read.csv("../../../new_data/womens_player_stats_across_games_formatted.csv", stringsAsFactors = FALSE)

# prepend the prefixes
trier_data_men$id_game   <- paste0("M", trier_data_men$id_game)
trier_data_women$id_game <- paste0("W", trier_data_women$id_game)

trier_data_men$sex   <- 0
trier_data_women$sex <- 1

# stack them
trier_data <- rbind(trier_data_men, trier_data_women)

MP <- gsub("^.*?00","00",as.character(trier_data$MP))

parse_time <- function(x) {
  res <- do.call(rbind, strsplit(x, ":", TRUE))
  mode(res) <- "numeric"
  c(res %*% (1/c(1/60, 1, 60)))
}

trier_data$MP <- parse_time(MP)

# 1. Ratings----------------------

# 1. 1. EFF ----------------

trier_data$EFF <- trier_data$points + trier_data$rebounds + trier_data$assists +
  trier_data$steals + trier_data$blocks - trier_data$misses - trier_data$misses_ft -
  trier_data$turnovers

# 1. 2. PIR -------------- # some stats do not exist in the NC State data, so we can skip this metric

#trier_data$PIR <- trier_data$points + trier_data$rebounds + trier_data$assists +
#  trier_data$steals + trier_data$blocks + trier_data$rec_fouls - trier_data$misses - 
#  trier_data$misses_ft -  trier_data$turnovers - trier_data$rec_blocks - 
#  trier_data$fouls

# 1. 3. Win score --------------

trier_data$Winscore <- trier_data$points + trier_data$rebounds + .5 * trier_data$assists +
  trier_data$steals + .5 * trier_data$blocks - trier_data$turnovers - 
  .5 * trier_data$fouls - trier_data$FGA - .5 * trier_data$FTA # falta -FGA - .5 * FTA

# 1. 4. True Shooting % -----------

trier_data$TS <- (trier_data$points) / (2*(trier_data$FGA + (0.44*trier_data$FTA)))

# Cleaning

trier_data$EFF_per_min <- trier_data$EFF / trier_data$MP
#trier_data$PIR_per_min <- trier_data$PIR / trier_data$MP
trier_data$Winscore_min <- trier_data$Winscore / trier_data$MP
trier_data$TS_min <- trier_data$TS / trier_data$MP



trier_data$EFF_per_min[is.nan(trier_data$EFF_per_min)]<-NA
#trier_data$PIR_per_min[is.nan(trier_data$PIR_per_min)]<-NA
trier_data$Winscore_min[is.nan(trier_data$Winscore_min)]<-NA
trier_data$TS_min[is.nan(trier_data$TS_min)]<-NA


# 2. Factorizing home variable-------------

# Already done for NC State data

# 3. Player-----------

unique(trier_data$player)

trier_data$player <- as.factor(trier_data$player)

levels(trier_data$player)

trier_data$id_player <- as.numeric(trier_data$player)

# 4. Classification----------------

player_ids <- unique(trier_data[c("player","id_player")])
print(player_ids)

trier_data$class[trier_data$id_player == 1] <- 3 # junior
trier_data$class[trier_data$id_player == 2] <- 4 # senior
trier_data$class[trier_data$id_player == 3] <- 2 # sophomore
trier_data$class[trier_data$id_player == 4] <- 4
trier_data$class[trier_data$id_player == 5] <- 4
trier_data$class[trier_data$id_player == 6] <- 4
trier_data$class[trier_data$id_player == 7] <- 3
trier_data$class[trier_data$id_player == 8] <- 1 # freshman
trier_data$class[trier_data$id_player == 9] <- 1
trier_data$class[trier_data$id_player == 10] <- 2

# 5. Checking minutes played------------

library(sqldf)

mp_player <- sqldf("SELECT sum(MP), player FROM trier_data GROUP BY player")

trier_data <- subset(trier_data, 
                     subset = !(player %in% mp_player$player[mp_player$`sum(MP)`< 40]) )

# mp_player_nw <- sqldf("SELECT sum(MP), player FROM trier_data GROUP BY player")

trier_data$player <- as.character(trier_data$player)

trier_data$player <- as.factor(trier_data$player)

levels(trier_data$player)

trier_data$id_player <- as.numeric(trier_data$player)

trier_data <- subset(trier_data, 
                     subset = MP>2 )

# Players with less than 40 minutes of play have been removed, and metrics 
# calculated with less than 2 minutes...

# 6. Save data---------------

Trier_data <- data.frame(id_game = trier_data$id_game, player = trier_data$player,
                         id_player = trier_data$id_player, 
                         EFF_per_min = trier_data$EFF_per_min,
                         Winscore_min = trier_data$Winscore_min,
                         TS_min = trier_data$TS_min,
                         home = trier_data$home, sex = trier_data$sex, class = trier_data$class)



# 7. Final cleaning-----------------

library(GLDEX)

# Trier_data <- Trier_data[-which.na(Trier_data$PIR_per_min),]



Trier_data$id_player <- as.numeric(as.factor(as.character(Trier_data$player)))

# Set NA values to 0 for the EFF_per_min, Winscore_min and TS_min variables
Trier_data$EFF_per_min[is.na(Trier_data$EFF_per_min)] <- 0
Trier_data$Winscore_min[is.na(Trier_data$Winscore_min)] <- 0
Trier_data$TS_min[is.na(Trier_data$TS_min)] <- 0

save(Trier_data, file = '../../../new_data/NC_State_Trier_data_clean_new.RData')

write.table(Trier_data, file = '../../../new_data/NC_State_Trier_data_clean_new.txt', 
            sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Subset to just player, sex and class for data_players info
player_info <- unique(Trier_data[, c("player", "sex", "class")])

write.table(
  player_info,
  file = "../../../new_data/NC_State_data_players.tsv",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
