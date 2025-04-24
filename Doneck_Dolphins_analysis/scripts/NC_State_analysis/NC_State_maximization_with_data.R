#################################################
# LINEUP OPTIMIZATION (dynamic for 10 players)
# Based on the original scripts by Calvo et al.
#################################################

# load cleaned boxscore and lineups
load('../../../new_data/NC_State_Trier_data_clean_new.RData')
load('../../../new_data/NC_State_line_ups.RData')

library(lpSolve)

# figure out how many players and games
# (we do this dynamically rather than hardcoded to avoid crazy headaches)
n_players <- max(Trier_data$id_player)
games <- sort(unique(Trier_data$id_game))
n_games <- length(games)

# extract each players class and sex once
class_by_id <- tapply(Trier_data$class, Trier_data$id_player, unique)
sex_by_id <- tapply(Trier_data$sex, Trier_data$id_player, unique)

# build per‐game performance matrices
data_matrix_EFF <- matrix(NA, n_players, n_games,
                         dimnames = list(1:n_players, games))
data_matrix_WIN <- matrix(NA, n_players, n_games,
                         dimnames = list(1:n_players, games))
data_matrix_TS <- matrix(NA, n_players, n_games,
                         dimnames = list(1:n_players, games))

for(i in seq_len(nrow(Trier_data))) {
  pid <- Trier_data$id_player[i]
  gid <- Trier_data$id_game[i]
  j <- match(gid, games)
  data_matrix_EFF[pid, j] <- Trier_data$EFF_per_min[i]
  data_matrix_WIN[pid, j] <- Trier_data$Winscore_min[i]
  data_matrix_TS[pid, j] <- Trier_data$TS_min[i]
}

mean_EFF <- rowMeans(data_matrix_EFF, na.rm = TRUE)
mean_WIN <- rowMeans(data_matrix_WIN, na.rm = TRUE)
mean_TS <- rowMeans(data_matrix_TS,  na.rm = TRUE)

# function to solve the best lineup based on a given metric
solve_lineup <- function(f.obj, class_vec, sex_vec,
                         min_women, max_points) {
  n <- length(f.obj)
  # build constraint matrix:  sum(x)=5, sum(class*x)<=..., sum(sex*x) >=/=/<=..., xi<=1
  base_mat <- rbind(
    rep(1, n),
    class_vec,
    sex_vec
  )
  f.con  <- rbind(base_mat, diag(n))
  # directions
  dir_w <- if(min_women==0) "=" else if(min_women==1) "=" else ">="
  f.dir  <- c("=", "<=", dir_w, rep("<=", n))
  f.rhs  <- c(5, max_points, min_women, rep(1, n))
  sol    <- lp("max", f.obj, f.con, f.dir, f.rhs,
               int.vec = seq_len(n))
  list(selection = sol$solution, value = sol$objval)
}


############################################
# CALCULATE PERFORMANCE OF ALLOWED LINEUPS
############################################

# filter and re-ID
allowed <- subset(line_ups, allowed == 1)
allowed$id <- paste0("L", seq_len(nrow(allowed)))

# map player name to id_player
id_map <- unique(Trier_data[, c("id_player","player")])
id_by_name <- setNames(id_map$id_player, id_map$player)

# sum up each lineups mean performance for each metric
for(metric in c("EFF","WIN","TS")) {
  mean_vec <- get(paste0("mean_", metric))
  perf <- apply(
    allowed[, paste0("player",1:5)],
    1,
    function(pls) sum(mean_vec[id_by_name[pls]])
  )
  allowed[[paste0("mean", metric, "performance")]] <- perf
}

###############################################
# SOLVE AND PRINT BEST LINEUP FOR EACH METRIC
###############################################

# run each of the three cases using mean_EFF
res2w <- solve_lineup(mean_EFF, class_by_id, sex_by_id,
                      min_women = 2, max_points = 16)
res1w <- solve_lineup(mean_EFF, class_by_id, sex_by_id,
                      min_women = 1, max_points = 14)
res0w <- solve_lineup(mean_EFF, class_by_id, sex_by_id,
                      min_women = 0, max_points = 12)

# pick the best of the three
vals <- c(res2w$value, res1w$value, res0w$value)
best <- which.max(vals)
best_out <- list(res2w, res1w, res0w)[[best]]
selected_ids <- which(best_out$selection == 1)
selected_names <- unique(
  Trier_data$player[Trier_data$id_player %in% selected_ids]
)
# combine names 1 through 5 into a comma separated string for each row
allowed_names <- apply(
  allowed[, paste0("player",1:5)],
  1,
  function(x) paste(x, collapse = ", ")
)
allowed_row <- allowed[allowed_names == paste(selected_names, collapse = ", "), ]
cat("Metric: EFF\n")
cat("Best scenario:", c("≥2 women","1 woman","0 women")[best], "\n")
cat("Objective value:", max(vals), "\n")
cat("Player names in the optimal lineup:",
    paste(selected_names, collapse = ", "), "\n")
cat("Player IDs in the optimal lineup:",
    paste(selected_ids, collapse = ", "), "\n")
cat("Full Row: \n")
print(allowed_row)
cat("\n")

# run each of the three cases using mean_WIN
res2w <- solve_lineup(mean_WIN, class_by_id, sex_by_id,
                      min_women = 2, max_points = 16)
res1w <- solve_lineup(mean_WIN, class_by_id, sex_by_id,
                      min_women = 1, max_points = 14)
res0w <- solve_lineup(mean_WIN, class_by_id, sex_by_id,
                      min_women = 0, max_points = 12)

# pick the best of the three
vals <- c(res2w$value, res1w$value, res0w$value)
best <- which.max(vals)
best_out <- list(res2w, res1w, res0w)[[best]]
selected_ids <- which(best_out$selection == 1)
selected_names <- unique(
  Trier_data$player[Trier_data$id_player %in% selected_ids]
)
# combine names 1 through 5 into a comma separated string for each row
allowed_names <- apply(
  allowed[, paste0("player",1:5)],
  1,
  function(x) paste(x, collapse = ", ")
)
allowed_row <- allowed[allowed_names == paste(selected_names, collapse = ", "), ]
cat("Metric: WIN\n")
cat("Best scenario:", c("≥2 women","1 woman","0 women")[best], "\n")
cat("Objective value:", max(vals), "\n")
cat("Player names in the optimal lineup:",
    paste(selected_names, collapse = ", "), "\n")
cat("Player IDs in the optimal lineup:",
    paste(selected_ids, collapse = ", "), "\n")
cat("Full Row: \n")
print(allowed_row)
cat("\n")

# run each of the three cases using mean_TS
res2w <- solve_lineup(mean_TS, class_by_id, sex_by_id,
                      min_women = 2, max_points = 16)
res1w <- solve_lineup(mean_TS, class_by_id, sex_by_id,
                      min_women = 1, max_points = 14)
res0w <- solve_lineup(mean_TS, class_by_id, sex_by_id,
                      min_women = 0, max_points = 12)

# pick the best of the three
vals <- c(res2w$value, res1w$value, res0w$value)
best <- which.max(vals)
best_out <- list(res2w, res1w, res0w)[[best]]
selected_ids <- which(best_out$selection == 1)
selected_names <- unique(
  Trier_data$player[Trier_data$id_player %in% selected_ids]
)
# combine names 1 through 5 into a comma separated string for each row
allowed_names <- apply(
  allowed[, paste0("player",1:5)],
  1,
  function(x) paste(x, collapse = ", ")
)
allowed_row <- allowed[allowed_names == paste(selected_names, collapse = ", "), ]
cat("Metric: TS\n")
cat("Best scenario:", c("≥2 women","1 woman","0 women")[best], "\n")
cat("Objective value:", max(vals), "\n")
cat("Player names in the optimal lineup:",
    paste(selected_names, collapse = ", "), "\n")
cat("Player IDs in the optimal lineup:",
    paste(selected_ids, collapse = ", "), "\n")
cat("Full Row: \n")
print(allowed_row)
cat("\n")
