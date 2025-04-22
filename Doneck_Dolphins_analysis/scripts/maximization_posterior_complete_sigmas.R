library(lpSolve)
#source('./functions/model_prediction_type_sigmas.R')
source('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/functions/model_prediction_type_sigmas.R')

set.seed(9257)
predictions <- model_prediction_type_sigmas(player = 1:9, type = 'TS', match = 19)

posterior <- c()
for(i in 1 : length(predictions[1, ])){
  
  # funcion objetivo: Pir1 x x1 + Pir2 x x2...
  f.obj <- predictions[, i]
  
  # 1. 2 or more women--------------------------
  # restricciones matriz
  # la suma es 5
  f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
                    1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 17.5 puntuación
                    1, 0, 0, 0, 0, 1, 0, 1, 0,  # female players
                    1, 0, 0, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 1
                    0, 1, 0, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 2
                    0, 0, 1, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 3
                    0, 0, 0, 1, 0, 0, 0, 0, 0, # o 0 o 1 jugador 4
                    0, 0, 0, 0, 1, 0, 0, 0, 0, # o 0 o 1 jugador 5
                    0, 0, 0, 0, 0, 1, 0, 0, 0, # o 0 o 1 jugador 6
                    0, 0, 0, 0, 0, 0, 1, 0, 0, # o 0 o 1 jugador 7
                    0, 0, 0, 0, 0, 0, 0, 1, 0, # o 0 o 1 jugador 8
                    0, 0, 0, 0, 0, 0, 0, 0, 1), # o 0 o 1 jugador 9
                  nrow = 12, byrow = T)# o 0 o 1 jugador 11
  
  
  
  #restricciones
  f.rhs <- c(5, 17.5, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  f.dir <- c('=', '<=', '>=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', 
             '<=')
  #enteros
  f.intvec <- c(1 : 9)
  
  
  solution <- lp(direction = "max",
                 objective.in = f.obj,
                 const.mat = f.con, 
                 const.dir = f.dir, 
                 const.rhs = f.rhs,
                 int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
                 #all.int = FALSE
  )
  
  sol1 <- solution$solution
  val1 <- solution$objval
  
  
  # 2. 1 woman------------------
  f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
                    1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 16 puntuaction
                    1, 0, 0, 0, 0, 1, 0, 1, 0,  # female players
                    1, 0, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 1
                    0, 1, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 2
                    0, 0, 1, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 3
                    0, 0, 0, 1, 0, 0, 0, 0, 0,# o 0 o 1 jugador 4
                    0, 0, 0, 0, 1, 0, 0, 0, 0,# o 0 o 1 jugador 5
                    0, 0, 0, 0, 0, 1, 0, 0, 0,# o 0 o 1 jugador 6
                    0, 0, 0, 0, 0, 0, 1, 0, 0,# o 0 o 1 jugador 7
                    0, 0, 0, 0, 0, 0, 0, 1, 0,# o 0 o 1 jugador 8
                    0, 0, 0, 0, 0, 0, 0, 0, 1),# o 0 o 1 jugador 9
                  nrow = 12, byrow = T)# 
  
  
  
  #restricciones
  f.rhs <- c(5, 16, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  f.dir <- c('=', '<=', '=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', 
             '<=')
  #enteros
  f.intvec <- c(1 : 9)
  
  solution2 <- lp(direction = "max",
                  objective.in = f.obj,
                  const.mat = f.con, 
                  const.dir = f.dir, 
                  const.rhs = f.rhs,
                  int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
                  #all.int = FALSE
  )
  
  sol2 <- solution2$solution
  val2 <- solution2$objval
  
  # 3. no women----------------------
  f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
                    1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 14.5 puntuación
                    1, 0, 0, 0, 0, 0, 0, 0, 0,# o 0  jugador 1
                    0, 1, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 2
                    0, 0, 1, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 3
                    0, 0, 0, 1, 0, 0, 0, 0, 0,# o 0 o 1 jugador 4
                    0, 0, 0, 0, 1, 0, 0, 0, 0,# o 0 o 1 jugador 5
                    0, 0, 0, 0, 0, 1, 0, 0, 0,# o 0  jugador 6
                    0, 0, 0, 0, 0, 0, 1, 0, 0,# o 0 o 1 jugador 7
                    0, 0, 0, 0, 0, 0, 0, 1, 0,# o 0  jugador 8
                    0, 0, 0, 0, 0, 0, 0, 0, 1),# o 0  jugador 9
                  nrow = 11, byrow = T)# o 0 o 1 jugador 11
  
  
  
  #restricciones
  f.rhs <- c(5, 14.5,  0, 1, 1, 1, 1, 0, 1, 0, 1)
  
  f.dir <- c('=', '<=', '=', '<=', '<=', '<=', '<=', '=', '<=', '=', 
             '<=')
  #enteros
  f.intvec <- c(1 : 9)
  
  
  solution3 <- lp(direction = "max",
                  objective.in = f.obj,
                  const.mat = f.con, 
                  const.dir = f.dir, 
                  const.rhs = f.rhs,
                  int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
                  #all.int = FALSE
  )
  
  sol3 <- solution3$solution
  val3 <- solution3$objval
  
  
  sol <- c(paste(sol1, collapse = " "), paste(sol2, collapse = " "), 
           paste(sol3, collapse = " "))
  val <- c(val1, val2, val3)
  
  
  posterior[i] <- sol[val == max(val)]
}

summary_TS <- summary(as.factor(posterior))
levels(as.factor(posterior))
sort(summary_TS)
posterior_TS <- posterior
save(summary_TS, posterior_TS, 
     file = '/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_TS_sigmas.RData')
