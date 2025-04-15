SOFTWARE NEEDED: R and JAGS. All codes are in R language.

This folder contains the statistical analysis carried out in the real case application of the paper "Selecting the best compositions of a wheelchair basketball team: a mathematical approach". The material available here is as follows.

1. Folder "data": it includes four elements, i. e., data_players.xlsx, data_trier.xlsx, line_ups.RData and Trier_data_clean.RData. 
data_players.xlsx: Functional classification and sex of the players (excel format).
data_trier.xlsx: Performance data of the players (excel format). 
line_ups.RData: Number of female players and total functional points for each possible team line-up (R format).
Trier_data_clean_new.RData: Performance data of the players (R format).

2. Folder "figures": all of the figures presented in the paper in PDF format. In addition, the R script entitled "spaghetti_minutes_played.R" required to produce the subsequent chart plot.

3. Folder "functions": it includes the function used for predicting (model_prediction_type_sigmas.R) and a logarithmic summation function (suma_logaritmica_de_elementos.R).

4. Folder "models": the Bayesian longitudinal  model used in the case study in JAGS format.

5. Folder "results": it contains samples from JAGS of the posterior distribution of the parameters (res_mixed_model_Trier_EFF2.RData, res_mixed_model_Trier_PIR2.RData and res_mixed_model_Trier_Winscore2.RData), the obtained PIT values for each model (PIT_EFF2.RData, PIT_PIR2.RData and PIT_WIN2.RData) and the results of the linear integer problem (lineups_EFF_sigmas.RData, lineups_PIR_sigmas.RData and lineups_winscore_sigmas.RData).

6. Folder "scripts": it contains nine general scripts for the analysis, i. e., maximization_posterior_complete_sigmas.R, maximization_with_data.R, barplot_sigmas.R, barplot_5th_player_new.R, mixed_model_Trier2.R, posterior_predictive_performance_for_lineups.R, posterior_probabilities_players2.R, Trier_data_clean_new.R and trier_data_plots.R. In addition, the PIT folder contains all the code needed to compute the PIT vales.
maximization_posterior_complete_sigmas.R: code for solving the optimisation problem from posterior predictive samples. 
maximization_with_data.R: code for solving the optimisation problem from data. 
barplot_sigmas.R: code for ploting the optimisation results, the most optimal line-ups. 
barplot_5th_player_new.R: code for Figure 4. 
mixed_model_Trier2.R: code for computing the JAGS model and seeing the posterior results.
posterior_predictive_performance_for_lineups.R: code to obtain the results summarised in Table 2.
posterior_probabilities_players2.R: code for Figure 4.
Trier_data_clean_new.R: code for adapting the data to our analysis.
trier_data_plots.R: code for Figure 2.