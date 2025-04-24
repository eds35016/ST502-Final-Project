# 0. Packages
if (!require(lpSolve)) install.packages("lpSolve")
library(lpSolve)

# 1. Load cleaned Trier_data
load("../../../new_data/NC_State_Trier_data_clean_new.RData")

# 2. Map games and players to integer indices
game_levels        <- unique(Trier_data$id_game)
Trier_data$game_ix <- match(Trier_data$id_game, game_levels)

player_levels        <- unique(as.character(Trier_data$player))
Trier_data$player_ix <- match(as.character(Trier_data$player), player_levels)

n_games   <- length(game_levels)
n_players <- length(player_levels)

# 3. Build player × game matrices for each metric
mat_EFF <- matrix(NA, nrow = n_players, ncol = n_games)
mat_WIN <- matrix(NA, nrow = n_players, ncol = n_games)
mat_TS  <- matrix(NA, nrow = n_players, ncol = n_games)

for (i in seq_len(nrow(Trier_data))) {
  pid <- Trier_data$player_ix[i]
  gid <- Trier_data$game_ix[i]
  mat_EFF[pid, gid] <- Trier_data$EFF_per_min[i]
  mat_WIN[pid, gid] <- Trier_data$Winscore_min[i]
  mat_TS [pid, gid] <- Trier_data$TS_min[i]
}

# 4. Compute each player's mean performance
mean_EFF <- rowMeans(mat_EFF, na.rm = TRUE)
mean_WIN <- rowMeans(mat_WIN, na.rm = TRUE)
mean_TS  <- rowMeans(mat_TS,  na.rm = TRUE)

# 5. Build constant rows for LP constraints
ids       <- seq_len(n_players)
all5      <- rep(1, n_players)   # team size = 5
class_pts <- tapply(Trier_data$class,    Trier_data$player_ix, unique)
female    <- tapply(Trier_data$sex,      Trier_data$player_ix, unique)
Iblock    <- diag(1, n_players)         # each x_i ≤ 1

# 6. Define the three class‐vs‐women scenarios
scenarios <- list(
  list(min_women = 2, class_rhs = 16, fem_dir = ">="),
  list(min_women = 1, class_rhs = 14, fem_dir = "="),
  list(min_women = 0, class_rhs = 12, fem_dir = "=")
)

# 7. Function to solve one lineup optimization given an objective vector
solve_lineup <- function(obj_vec) {
  best_val <- -Inf
  best_x   <- integer(n_players)
  
  for (sc in scenarios) {
    Cmat <- rbind(
      all5,            # row 1: sum(x) == 5
      class_pts,       # row 2: total class‐points ≤ class_rhs
      female,          # row 3: #women ≥/=/= min_women
      Iblock           # rows 4–(3+n_players): x_i ≤ 1
    )
    Rhs <- c(
      5,
      sc$class_rhs,
      sc$min_women,
      rep(1, n_players)
    )
    Dir <- c(
      "=",
      "<=",
      sc$fem_dir,
      rep("<=", n_players)
    )
    
    sol <- lp(
      direction    = "max",
      objective.in = obj_vec,
      const.mat    = Cmat,
      const.dir    = Dir,
      const.rhs    = Rhs,
      int.vec      = ids
    )
    
    if (sol$status == 0 && sol$objval > best_val) {
      best_val <- sol$objval
      best_x   <- sol$solution
    }
  }
  
  list(selection = best_x, objval = best_val)
}

# 8. Solve for each metric
res_EFF <- solve_lineup(mean_EFF)
res_WIN <- solve_lineup(mean_WIN)
res_TS  <- solve_lineup(mean_TS)

# 9. Helper to map selection back to player names
get_lineup_names <- function(sel) {
  player_levels[which(sel == 1)]
}

# 10. Print best lineups
cat("Best EFF lineup (value =", round(res_EFF$objval, 3), "):\n",
    paste(get_lineup_names(res_EFF$selection), collapse = ", "), "\n\n")

cat("Best Winscore lineup (value =", round(res_WIN$objval, 3), "):\n",
    paste(get_lineup_names(res_WIN$selection), collapse = ", "), "\n\n")

cat("Best TS lineup (value =", round(res_TS$objval, 3), "):\n",
    paste(get_lineup_names(res_TS$selection), collapse = ", "), "\n")
