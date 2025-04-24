# read in players file
data_players <- read.delim("../../../new_data/NC_State_data_players.tsv",
                           stringsAsFactors = FALSE)

# load combinations()
library(gtools)

# number of players
n_players <- nrow(data_players)

# all 5-player combinations of the n_players
combos <- combinations(n = n_players,
                       r = 5,
                       v = seq_len(n_players))

n_lineups <- nrow(combos)  # should be choose(n_players, 5)

# pre-allocate
total_points <- numeric(n_lineups)
n_women      <- numeric(n_lineups)

# compute for each lineup
for (i in seq_len(n_lineups)) {
  idx <- combos[i, ]
  total_points[i] <- sum(data_players$class[idx])
  n_women[i]      <- sum(data_players$sex[idx])
}

# apply thresholds:
#   if 0 women: max 12 points
#   if 1 woman: max 14 points
#   if ≥2 women: max 16 points
allowed <- numeric(n_lineups)
for (i in seq_len(n_lineups)) {
  pts <- total_points[i]
  w   <- n_women[i]
  if      (w == 0 && pts <= 12)        allowed[i] <- 1
  else if (w == 1 && pts <= 14)        allowed[i] <- 1
  else if (w >= 2 && pts <= 16)        allowed[i] <- 1
  # otherwise remains 0
}

# quick counts
cat("Total possible lineups: ", n_lineups, "\n")
cat("Allowed total:        ", sum(allowed), "\n\n")

cat("By # women:\n")
for (w in 0:3) {
  count_all   <- sum(n_women == w)
  count_allow <- sum(allowed[n_women == w])
  cat(sprintf("  %d women: %3d total, %3d allowed\n",
              w, count_all, count_allow))
}

# build the full data.frame of lineups
line_ups <- data.frame(
  player1      = data_players$player[combos[,1]],
  player2      = data_players$player[combos[,2]],
  player3      = data_players$player[combos[,3]],
  player4      = data_players$player[combos[,4]],
  player5      = data_players$player[combos[,5]],
  number_women = n_women,
  total_points = total_points,
  allowed      = allowed
)

# save for later
save(line_ups, file = "../../../new_data/NC_State_line_ups.RData")

write.table(line_ups, file = "../../../new_data/NC_State_line_ups.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)