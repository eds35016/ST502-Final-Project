# Line-ups count
library("readxl")
# xls files
data_players <- read_excel("./data/data_players.xlsx")

library(gtools)

c <- combinations(9, 5, c(1:9)) # all the combinations of 5 players

puntos <- c()
women <- c()
for(i in 1 :126){
  puntos[i] <- sum(data_players$Class[c[i,]]) # total classification points for the lineup i
  women[i] <- sum(data_players$Women[c[i,]])  # number of women players in the lineup i
}

lineup <- rep(1, 126) # 1 if lineup i is alowed, 0 otherwise
for(i in 1 : 126){
  if(women[i]==0){
    if(puntos[i]>14.5){lineup[i] <- 0}
  }
  if(women[i]==1){
    if(puntos[i]>16){lineup[i] <- 0}
  }
  if(women[i]>=2){
    if(puntos[i]>17.5){lineup[i] <- 0}
  }
}


sum(lineup) # number of permitted lineups, 91

line_ups <- data.frame(c[,1:5], number_women = (women), total_points = puntos, 
                       allowed = lineup)

sum(women==0) # lineups with all men players, 6
sum(women==1) # lineups with a woman player, 45
sum(women==2) # lineups with two women player, 60
sum(women==3) # lineups with three women player, 15

sum(lineup[women==0]) # allowed lineups with all men players, 2
sum(lineup[women==1]) # allowed lineups with a woman player, 27
sum(lineup[women==2]) # allowed lineups with two women player, 48
sum(lineup[women==3]) # allowed lineups with three women player, 14


save(line_ups, file = './data/line_ups.RData')
