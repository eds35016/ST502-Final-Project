#################################
# BAYESIAN LINEUP OPTIMIZATION
#################################

library(lpSolve)

# 1) load box‐score data (to get class & sex)
load('../../../new_data/NC_State_Trier_data_clean_new.RData')

# derive the actual player IDs present in the dataset
player_ids   <- sort(unique(Trier_data$id_player))
n_players    <- length(player_ids)

# build class_by_id and sex_by_id in the same order as player_ids
class_by_id <- tapply(Trier_data$class, Trier_data$id_player, unique)
sex_by_id   <- tapply(Trier_data$sex,   Trier_data$id_player, unique)
# now subset and coerce to numeric
class_by_id <- as.numeric(class_by_id[as.character(player_ids)])
sex_by_id   <- as.numeric(sex_by_id[  as.character(player_ids)])

# 2) source the JAGS‐posterior prediction function
source('../../functions/NC_State_model_prediction_type_sigmas.R')

# 3) get posterior samples of True‐Shooting for each player
set.seed(9257)
predictions <- model_prediction_type_sigmas(
  player = seq_len(n_players),
  type   = 'TS',
  match  = 33 # number of matches in the season + 1
)
n_sims <- ncol(predictions)

# 4) generic solver for “best lineup” under
#    - exactly 5 players
#    - ≤ max_points total class
#    - ≥ / = / = min_women total women
#    - each xi ∈ {0,1}
solve_lineup <- function(f.obj, class_vec, sex_vec, min_women, max_points) {
  n     <- length(f.obj)
  # rows: 1) sum(x)=5   2) sum(class*x) ≤ max_points   3) sum(sex*x) ≥/=/≤ min_women
  base_mat <- rbind(
    rep(1, n),
    class_vec,
    sex_vec
  )
  f.con <- rbind(base_mat, diag(n))
  # pick operator for the women‐constraint
  dir_w <- if (min_women == 0) "=" else if (min_women == 1) "=" else ">="
  f.dir <- c("=", "<=", dir_w, rep("<=", n))
  f.rhs <- c(5, max_points, min_women, rep(1, n))
  sol   <- lp(
    direction   = "max",
    objective.in = f.obj,
    const.mat   = f.con,
    const.dir   = f.dir,
    const.rhs   = f.rhs,
    int.vec     = seq_len(n)
  )
  list(selection = sol$solution, value = sol$objval)
}

# 5) loop over posterior draws, solve all 3 scenarios, record best
posterior <- character(n_sims)
for (i in seq_len(n_sims)) {
  f.obj <- predictions[, i]

  # scenario A: ≥2 women, class‐budget 16
  res2w <- solve_lineup(f.obj, class_by_id, sex_by_id,
                        min_women = 2, max_points = 16)
  # scenario B: exactly 1 woman, class‐budget 14
  res1w <- solve_lineup(f.obj, class_by_id, sex_by_id,
                        min_women = 1, max_points = 14)
  # scenario C: 0 women, class‐budget 12
  res0w <- solve_lineup(f.obj, class_by_id, sex_by_id,
                        min_women = 0, max_points = 12)

  vals <- c(res2w$value, res1w$value, res0w$value)
  best <- which.max(vals)
  sel  <- list(res2w$selection, res1w$selection, res0w$selection)[[best]]
  print(paste(which(sel == 1), collapse = " "))

  # record the 5 selected player‐IDs as a space‐separated string
  posterior[i] <- paste(which(sel == 1), collapse = " ")
}

# 6) summarize & save
summary_TS   <- summary(as.factor(posterior))
posterior_TS <- posterior

save(summary_TS, posterior_TS,
     file = '../../results/NC_State_data/NC_State_lineups_TS_sigmas.RData')
