library(lpSolve)
source('./functions/model_prediction_type_sigmas.R')


# 1. EFF---------------
set.seed(9257)
predictions <- model_prediction_type_sigmas(player = 1:9, type = 'EFF', match = 19) #performance predictions
load('./data/line_ups.RData')
line_ups <- line_ups[which(line_ups$allowed==1),]
line_ups <- data.frame(id=paste('L',1:91, sep = ''), line_ups)

# Building matrix with posterior predictive sample of the lineups-
post_pred_matrix <- matrix(data = NA, nrow = length(predictions[1,]), 
                           ncol = length(line_ups[,1]))
for(i in 1 : length(predictions[1,])){
  for(j in 1 : length(line_ups[,1])){
    post_pred_matrix[i, j] <- predictions[line_ups$X1[j], i] + #sample i of the prediction of the first player of Lj
      predictions[line_ups$X2[j], i] + #sample i of the prediction of the 2nd player of Lj
      predictions[line_ups$X3[j], i] + #sample i of the prediction of the 3rd player of Lj
      predictions[line_ups$X4[j], i] + #sample i of the prediction of the 4th player of Lj
      predictions[line_ups$X5[j], i] #sample i of the prediction of the 5th player of Lj
  }
}

post_pred_combination_EFF <- as.data.frame(post_pred_matrix)
colnames(post_pred_combination_EFF) <- paste('L',1:91, sep = '')


hist(post_pred_combination_EFF[,1])



means <- apply(post_pred_combination_EFF, FUN =mean, MARGIN = 2)
sd <- apply(post_pred_combination_EFF, FUN =sd, MARGIN = 2)

sort(means)


# 2. PIR---------------
set.seed(9257)
predictions <- model_prediction_type_sigmas(player = 1:9, type = 'PIR', match = 19) #performance predictions
load('./data/line_ups.RData')
line_ups <- line_ups[which(line_ups$allowed==1),]
line_ups <- data.frame(id=paste('L',1:91, sep = ''), line_ups)

# Building matrix with posterior predictive sample of the lineups-
post_pred_matrix <- matrix(data = NA, nrow = length(predictions[1,]), 
                           ncol = length(line_ups[,1]))
for(i in 1 : length(predictions[1,])){
  for(j in 1 : length(line_ups[,1])){
    post_pred_matrix[i, j] <- predictions[line_ups$X1[j], i] + #sample i of the prediction of the first player of Lj
      predictions[line_ups$X2[j], i] + #sample i of the prediction of the 2nd player of Lj
      predictions[line_ups$X3[j], i] + #sample i of the prediction of the 3rd player of Lj
      predictions[line_ups$X4[j], i] + #sample i of the prediction of the 4th player of Lj
      predictions[line_ups$X5[j], i] #sample i of the prediction of the 5th player of Lj
  }
}

post_pred_combination_PIR <- as.data.frame(post_pred_matrix)
colnames(post_pred_combination_PIR) <- paste('L',1:91, sep = '')


hist(post_pred_combination_PIR[,1])



means <- apply(post_pred_combination_PIR, FUN =mean, MARGIN = 2)
sd <- apply(post_pred_combination_PIR, FUN =sd, MARGIN = 2)

sort(means)


# 3. Winscore---------------
set.seed(9257)
predictions <- model_prediction_type_sigmas(player = 1:9, type = 'Winscore', match = 19) #performance predictions
load('./data/line_ups.RData')
line_ups <- line_ups[which(line_ups$allowed==1),]
line_ups <- data.frame(id=paste('L',1:91, sep = ''), line_ups)

# Building matrix with posterior predictive sample of the lineups-
post_pred_matrix <- matrix(data = NA, nrow = length(predictions[1,]), 
                           ncol = length(line_ups[,1]))
for(i in 1 : length(predictions[1,])){
  for(j in 1 : length(line_ups[,1])){
    post_pred_matrix[i, j] <- predictions[line_ups$X1[j], i] + #sample i of the prediction of the first player of Lj
      predictions[line_ups$X2[j], i] + #sample i of the prediction of the 2nd player of Lj
      predictions[line_ups$X3[j], i] + #sample i of the prediction of the 3rd player of Lj
      predictions[line_ups$X4[j], i] + #sample i of the prediction of the 4th player of Lj
      predictions[line_ups$X5[j], i] #sample i of the prediction of the 5th player of Lj
  }
}

post_pred_combination_WIN <- as.data.frame(post_pred_matrix)
colnames(post_pred_combination_WIN) <- paste('L',1:91, sep = '')


hist(post_pred_combination_WIN[,1])



means <- apply(post_pred_combination_WIN, FUN =mean, MARGIN = 2)
sd <- apply(post_pred_combination_WIN, FUN =sd, MARGIN = 2)

sort(means)




# Results (trabajar con las 5 primeras del articulo?)
# EFF
# 1. Breuer, Rossi, DPassivan, Dorner, Walter   L24                 L1
# 2. Breuer, DPassivan, Dorner, Svenja, Walter   L61                L2
# 3. Breuer, Rossi, DPassivan, NPassivan, Walter  L22               L3
# 4. Breuer, Rossi, Green, DPassivan, Walter     L5                 L4
# 5. Breuer, Rossi, DPassivan, NPassivan, Dorner  L20               L5
# 6. Breuer, Rossi, DPassivan, Svenja, Walter     L25
# 7. Breuer, Rossi, Green, DPassivan, Dorner     L3
# 
# PIR
# 1. Breuer, Rossi, DPassivan, Dorner, Walter   L24
# 2. Breuer, DPassivan, Dorner, Svenja, Walter   L61
# 3. Breuer, Rossi, DPassivan, NPassivan, Walter  L22            
# 4. Breuer, Rossi, Green, DPassivan, Walter     L5
# 5. Breuer, Rossi, DPassivan, Svenja, Walter     L25
# 6. Breuer, Rossi, DPassivan, NPassivan, Dorner  L20             L5
# 7. Breuer, Rossi, Green, DPassivan, Dorner     L3
# 
# Winscore
# 1. Breuer, Rossi, DPassivan, Dorner, Walter   L24   L1
# 2. Breuer, DPassivan, Dorner, Svenja, Walter   L61  L2
# 3. Breuer, Rossi, DPassivan, Svenja, Walter     L25
# 4. Breuer, Rossi, DPassivan, NPassivan, Walter  L22   L3
# 5. Breuer, Rossi, Green, DPassivan, Walter     L5     L4
# 
# 12. Breuer, Rossi, DPassivan, NPassivan, Dorner  L20 L5
# 16. Breuer, Rossi, Green, DPassivan, Dorner     L3