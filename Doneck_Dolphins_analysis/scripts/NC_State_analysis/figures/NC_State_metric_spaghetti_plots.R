# 0)–– INSTALL/LOAD PACKAGES -----------------------------------------------
if (!requireNamespace("ggplot2",  quietly=TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr",    quietly=TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr",    quietly=TRUE)) install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

# 1)–– LOAD DATA -------------------------------------------------------
load("../../../new_data/NC_State_Trier_data_clean_new.RData")  

# 2)–– COMPUTE game_number -------------------------------------------------
#  a) unique men's games in numeric order
men_games   <- Trier_data %>%
  filter(grepl("^M", id_game)) %>%
  pull(id_game) %>% unique() %>%
  {.[order(as.integer(sub("^M", "", .)))]}

#  b) unique women's games in numeric order
women_games <- Trier_data %>%
  filter(grepl("^W", id_game)) %>%
  pull(id_game) %>% unique() %>%
  {.[order(as.integer(sub("^W", "", .)))]}

#  c) assign a unified game_number
Trier_data <- Trier_data %>%
  mutate(
    game_number = ifelse(
      grepl("^M", id_game),
      match(id_game, men_games),
      match(id_game, women_games)
    )
  )

# 1a) log‐transform (add a small offset to eliminate zero values)
Trier_data$TS_trans <- log( Trier_data$TS_min + 1e-1 )
min_TS <- min(Trier_data$TS_trans)
Trier_data$TS_trans <- Trier_data$TS_trans - min_TS + 1e-6 # to make it positive


# 3)–– PLOT AND SAVE -------------------------------------------------------
players  <- unique(Trier_data$player)
players  <- as.character(players)
initials <- vapply(
  strsplit(players, " "),
  function(x) paste0(substr(x, 1, 1), collapse = ""),
  FUN.VALUE = ""
)
names(initials) <- players

library(scales)
pal        <- hue_pal()(length(players))
player_cols <- setNames(pal, players)

pdf("../../figures/NC_State_data/NC_State_metrics_spaghetti_plots.pdf", width=12, height=6)

make_spaghetti <- function(df, metric) {
  metric_title <- switch(metric,
    TS_trans    = "log(TS/min)",
    Winscore_min= "Winscore/min",
    EFF_per_min = "EFF/min",
    TS_min      = "TS/min"
  )

  p <- ggplot(df, aes(
        x     = game_number,
        y     = .data[[metric]],
        group = player,
        color = player
      )) +
    geom_line(alpha = 0.7, size = 0.6) +
    geom_point(size = 1.5) +
    labs(
      title = paste0(metric_title, " by Game"),
      x     = "Game Number",
      y     = metric_title,
      color = "Player"
    ) +
    scale_color_manual(
      values = player_cols, 
      labels = initials
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title     = element_text(hjust = 0.5)
    )

  print(p)
}

# generate all four plots
make_spaghetti(Trier_data, "EFF_per_min")
make_spaghetti(Trier_data, "Winscore_min")
make_spaghetti(Trier_data, "TS_min")
make_spaghetti(Trier_data, "TS_trans")

dev.off()