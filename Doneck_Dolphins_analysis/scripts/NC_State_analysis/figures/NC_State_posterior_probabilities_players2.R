# load('./results/lineups_EFF.RData')
# load('./results/lineups_PIR.RData')
# load('./results/lineups_winscore.RData')
# load('./results/lineups_TS.RData')

library(dplyr)
library(tidyr)
library(ggplot2)

load('../../results/NC_State_data/NC_State_lineups_TS_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_EFF_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_Winscore_sigmas.RData')

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# ─── 1) read data_trier file, extract mapping ─────────────────────────────
data_trier <- read_tsv("../../../new_data/NC_State_Trier_data_clean_new.txt", show_col_types=FALSE)
id_map <- data_trier %>%
  select(id_player, player) %>%  # columns in your file
  distinct()                      # one row per player

# ─── 2) assemble the three posteriors into one tibble ──────────────────
posteriors <- list(
  "EFF"      = posterior_EFF,
  "Winscore" = posterior_Winscore,
  "log(TS)"       = posterior_TS
)

probs_df <- imap_dfr(posteriors, ~ tibble(
    metric = .y,
    lineup = .x
  )) %>%
  separate_rows(lineup, sep = " ") %>%      # one row per id
  mutate(id_player = as.integer(lineup)) %>%
  count(metric, id_player, name="count") %>%
  mutate(prob = count / length(posterior_EFF)) %>%  # divide by 3000
  select(metric, id_player, prob)

# ─── 3) join on the names ───────────────────────────────────────────────
probs_named <- probs_df %>%
  left_join(id_map, by = "id_player") %>%
  mutate(player = factor(player, levels = unique(id_map$player)))

# ─── 4) plot ────────────────────────────────────────────────────────────

probs_named <- probs_named %>%
  mutate(metric = factor(metric, levels = c("EFF", "Winscore", "log(TS)")))

pdf(file = "../../figures/NC_State_data/NC_State_posterior_prob_players.pdf", width = 7, height = 4)

p <- ggplot(probs_named,
       aes(x = player, y = prob, shape = metric, color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_shape_manual(
    values = c("EFF" = 16, "Winscore" = 15, "log(TS)" = 17),
  ) +
  scale_y_continuous("Selection Probability", labels = scales::number_format(accuracy = 0.01)) +
  labs(x = NULL, color = "Metric", shape = "Metric") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p)

dev.off()

# skip conditional probabilities for now

#players <- c(6, 7, 4) # indices of the players
#count <- 0
#for(i in 1 : 3000){
#  logic <- as.numeric(Lineups[i, players] == c(1,1,0))
#  if(sum(logic) == length(players)){count <- count + 1}
#}
#prob2 <- count / 3000 # 0.014333333

#prob2 / (1-prob) # conditional probabilities 0.7049

#players <- c(6, 7) # indices of the players
#count <- 0
#for(i in 1 : 3000){
#  logic <- as.numeric(Lineups[i, players] == c(1,1))
#  if(sum(logic) == length(players)){count <- count + 1}
#}
#prob2 <- count / 3000 #0.1663333
