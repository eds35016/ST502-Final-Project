load('./data/Trier_data_clean_new.RData')
library(lpSolve)

data_matrix_EFF <- matrix(data = NA, nrow = 9, ncol = 18)
data_matrix_PIR <- matrix(data = NA, nrow = 9, ncol = 18)
data_matrix_WIN <- matrix(data = NA, nrow = 9, ncol = 18)

# 1. EFF-------------------
for(i in 1 : length(Trier_data$id_game)){
  data_matrix_EFF[Trier_data$id_player[i], Trier_data$id_game[i]] <-
    Trier_data$EFF_per_min[i]
}

mean_EFF <- apply(data_matrix_EFF, FUN = mean, MARGIN = 1, na.rm=TRUE) #hacer para PIR, Winscore
# data_matrix_EFF[is.na(data_matrix_EFF)]<- -2 #NA lo peor

# posterior <- c()
# for(i in 15){ #game
#   
#   # funcion objetivo: Pir1 x x1 + Pir2 x x2...
#   f.obj <- data_matrix_EFF[, i]
#   
#   #  2 or more women
#   # restricciones matriz
#   # la suma es 5
#   f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
#                     1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 17.5 puntuación
#                     1, 0, 0, 0, 0, 1, 0, 1, 0,  # female players
#                     1, 0, 0, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 1
#                     0, 1, 0, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 2
#                     0, 0, 1, 0, 0, 0, 0, 0, 0,  # o 0 o 1 jugador 3
#                     0, 0, 0, 1, 0, 0, 0, 0, 0, # o 0 o 1 jugador 4
#                     0, 0, 0, 0, 1, 0, 0, 0, 0, # o 0 o 1 jugador 5
#                     0, 0, 0, 0, 0, 1, 0, 0, 0, # o 0 o 1 jugador 6
#                     0, 0, 0, 0, 0, 0, 1, 0, 0, # o 0 o 1 jugador 7
#                     0, 0, 0, 0, 0, 0, 0, 1, 0, # o 0 o 1 jugador 8
#                     0, 0, 0, 0, 0, 0, 0, 0, 1), # o 0 o 1 jugador 9
#                   nrow = 12, byrow = T)# o 0 o 1 jugador 11
#   
#   
#   
#   #restricciones
#   f.rhs <- c(5, 17.5, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#   
#   f.dir <- c('=', '<=', '>=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', 
#              '<=')
#   #enteros
#   f.intvec <- c(1 : 9)
#   
#   
#   solution <- lp(direction = "max",
#                  objective.in = f.obj,
#                  const.mat = f.con, 
#                  const.dir = f.dir, 
#                  const.rhs = f.rhs,
#                  int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
#                  #all.int = FALSE
#   )
#   
#   sol1 <- solution$solution
#   val1 <- solution$objval
#   
#   
#   # 1 woman
#   f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
#                     1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 16 puntuaction
#                     1, 0, 0, 0, 0, 1, 0, 1, 0,  # female players
#                     1, 0, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 1
#                     0, 1, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 2
#                     0, 0, 1, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 3
#                     0, 0, 0, 1, 0, 0, 0, 0, 0,# o 0 o 1 jugador 4
#                     0, 0, 0, 0, 1, 0, 0, 0, 0,# o 0 o 1 jugador 5
#                     0, 0, 0, 0, 0, 1, 0, 0, 0,# o 0 o 1 jugador 6
#                     0, 0, 0, 0, 0, 0, 1, 0, 0,# o 0 o 1 jugador 7
#                     0, 0, 0, 0, 0, 0, 0, 1, 0,# o 0 o 1 jugador 8
#                     0, 0, 0, 0, 0, 0, 0, 0, 1),# o 0 o 1 jugador 9
#                   nrow = 12, byrow = T)# 
#   
#   
#   
#   #restricciones
#   f.rhs <- c(5, 16, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#   
#   f.dir <- c('=', '<=', '=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', '<=', 
#              '<=')
#   #enteros
#   f.intvec <- c(1 : 9)
#   
#   solution2 <- lp(direction = "max",
#                   objective.in = f.obj,
#                   const.mat = f.con, 
#                   const.dir = f.dir, 
#                   const.rhs = f.rhs,
#                   int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
#                   #all.int = FALSE
#   )
#   
#   sol2 <- solution2$solution
#   val2 <- solution2$objval
#   
#   # no women
#   f.con <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1,  #5 jugadores
#                     1.5, 2, 3.5, 4.5, 1, 4.5, 3.5, 3.5, 4.5,# 14.5 puntuación
#                     1, 0, 0, 0, 0, 0, 0, 0, 0,# o 0  jugador 1
#                     0, 1, 0, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 2
#                     0, 0, 1, 0, 0, 0, 0, 0, 0,# o 0 o 1 jugador 3
#                     0, 0, 0, 1, 0, 0, 0, 0, 0,# o 0 o 1 jugador 4
#                     0, 0, 0, 0, 1, 0, 0, 0, 0,# o 0 o 1 jugador 5
#                     0, 0, 0, 0, 0, 1, 0, 0, 0,# o 0  jugador 6
#                     0, 0, 0, 0, 0, 0, 1, 0, 0,# o 0 o 1 jugador 7
#                     0, 0, 0, 0, 0, 0, 0, 1, 0,# o 0  jugador 8
#                     0, 0, 0, 0, 0, 0, 0, 0, 1),# o 0  jugador 9
#                   nrow = 11, byrow = T)# o 0 o 1 jugador 11
#   
#   
#   
#   #restricciones
#   f.rhs <- c(5, 14.5,  0, 1, 1, 1, 1, 0, 1, 0, 1)
#   
#   f.dir <- c('=', '<=', '=', '<=', '<=', '<=', '<=', '=', '<=', '=', 
#              '<=')
#   #enteros
#   f.intvec <- c(1 : 9)
#   
#   
#   solution3 <- lp(direction = "max",
#                   objective.in = f.obj,
#                   const.mat = f.con, 
#                   const.dir = f.dir, 
#                   const.rhs = f.rhs,
#                   int.vec = f.intvec#,    # especificar este parámetro de función indica qué variables son enteros
#                   #all.int = FALSE
#   )
#   
#   sol3 <- solution3$solution
#   val3 <- solution3$objval
#   
#   
#   sol <- c(paste(sol1, collapse = " "), paste(sol2, collapse = " "), 
#            paste(sol3, collapse = " "))
#   val <- c(val1, val2, val3)
#   
#   
#   posterior[i] <- sol[val == max(val)]
# }
# 
# #es un buen ejemplo por Svenja Erni...
# 
# 
# 
# 
# 
# 
# 



# EXAMPLE WITH MEANS

{ #game
  
  # funcion objetivo: Pir1 x x1 + Pir2 x x2...
  f.obj <- mean_EFF
  
  # 2 or more women
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
  
  
  # 1 woman
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
  
  # no women
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
  
  
  res <- sol[val == max(val)]
}



# PERFORMANCES OF THE LINEUPS

load('./data/line_ups.RData')
line_ups <- line_ups[which(line_ups$allowed==1),]
line_ups <- data.frame(id=paste('L',1:91, sep = ''), line_ups)

line_ups$meanEFFperformance <- rep(NA, 91)
for(j in 1 : length(line_ups[,1])){
  line_ups$meanEFFperformance[j] <- mean_EFF[line_ups$X1[j]] + #mean performance of the first player of Lj
    mean_EFF[line_ups$X2[j]] + #mean performance of the 2nd player of Lj
    mean_EFF[line_ups$X3[j]] + #mean performance of the 3rd player of Lj
    mean_EFF[line_ups$X4[j]] + #mean performance of the 4th player of Lj
    mean_EFF[line_ups$X5[j]] #mean performance of the 5th player of Lj
}


# 2. PIR------------------

for(i in 1 : length(Trier_data$id_game)){
  data_matrix_PIR[Trier_data$id_player[i], Trier_data$id_game[i]] <-
    Trier_data$PIR_per_min[i]
}

mean_PIR <- apply(data_matrix_PIR, FUN = mean, MARGIN = 1, na.rm=TRUE)

# EXAMPLE WITH MEANS

{ #game
  
  # funcion objetivo: Pir1 x x1 + Pir2 x x2...
  f.obj <- mean_PIR
  
  # 2 or more women
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
  
  
  # 1 woman
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
  
  # no women
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
  
  
  res <- sol[val == max(val)]
}



line_ups$meanPIRperformance <- rep(NA, 91)
for(j in 1 : length(line_ups[,1])){
  line_ups$meanPIRperformance[j] <- mean_PIR[line_ups$X1[j]] + #mean performance of the first player of Lj
    mean_PIR[line_ups$X2[j]] + #mean performance of the 2nd player of Lj
    mean_PIR[line_ups$X3[j]] + #mean performance of the 3rd player of Lj
    mean_PIR[line_ups$X4[j]] + #mean performance of the 4th player of Lj
    mean_PIR[line_ups$X5[j]] #mean performance of the 5th player of Lj
}



# 2. Winscore------------------

for(i in 1 : length(Trier_data$id_game)){
  data_matrix_WIN[Trier_data$id_player[i], Trier_data$id_game[i]] <-
    Trier_data$Winscore_min[i]
}

mean_WIN <- apply(data_matrix_WIN, FUN = mean, MARGIN = 1, na.rm=TRUE)

# EXAMPLE WITH MEANS

{ #game
  
  # funcion objetivo: Pir1 x x1 + Pir2 x x2...
  f.obj <- mean_WIN
  
  # 2 or more women
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
  
  
  # 1 woman
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
  
  # no women
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
  
  
  res <- sol[val == max(val)]
}



line_ups$meanWINperformance <- rep(NA, 91)
for(j in 1 : length(line_ups[,1])){
  line_ups$meanWINperformance[j] <- mean_WIN[line_ups$X1[j]] + #mean performance of the first player of Lj
    mean_WIN[line_ups$X2[j]] + #mean performance of the 2nd player of Lj
    mean_WIN[line_ups$X3[j]] + #mean performance of the 3rd player of Lj
    mean_WIN[line_ups$X4[j]] + #mean performance of the 4th player of Lj
    mean_WIN[line_ups$X5[j]] #mean performance of the 5th player of Lj
}