# load('./results/lineups_PIR_sigmas.RData')
# load('./results/lineups_EFF_sigmas.RData')
# load('./results/lineups_Winscore_sigmas.RData')
# load('./results/lineups_TS_sigmas.RData')

load('../../results/NC_State_data/NC_State_lineups_EFF_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_Winscore_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_TS_sigmas.RData')

library(readr)
library(dplyr)

data_trier <- read_tsv("../../../new_data/NC_State_Trier_data_clean_new.txt",
                       show_col_types = FALSE)

id_map <- data_trier %>%
  select(id_player, player) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    initials = {
      parts <- strsplit(player, "\\s+")[[1]]
      paste0(
        substr(parts[1],         1, 1),          # first name initial
        substr(parts[length(parts)], 1, 1)       # last name initial
      )
    }
  ) %>%
  ungroup() %>%
  select(id_player, initials)

pdf(file = '../../figures/NC_State_data/NC_State_lineups_sigmas_barplots_by_metric.pdf', width = 10, height = 8)
plot_top7 <- function(summary_vec, main.title){
  # ── 1) filter & pick top 7 ───────────────────────────────────────────────
  summary_vec <- summary_vec[names(summary_vec) != "(Other)"]
  rel   <- prop.table(summary_vec)
  top7  <- head(sort(rel, decreasing = TRUE), 7)
  
  # ── 2) build initials with a comma + space ─────────────────────────────
  raw_labels  <- names(top7)
  init_labels <- sapply(raw_labels, function(lbl){
    ids <- as.integer(strsplit(lbl, " ")[[1]])
    paste(id_map$initials[ match(ids, id_map$id_player) ], collapse = ", ")
  })
  
  # ── 3) create simple codes 1:7 ─────────────────────────────────────────
  codes <- seq_along(init_labels)
  
  # ── 4) expand right margin & allow drawing into it ────────────────────
  old_p  <- par(mar = c(5, 4, 4, 15) + 0.1, xpd = TRUE)
  on.exit(par(old_p), add = TRUE)
  
  # ── 5) draw the bars ─────────────────────────────────────────────────────
  bp <- barplot(
    top7,
    names.arg = parse(text = paste0("L[", codes, "]")),
    col       = rgb(0.8, 0, 0, 0.6),
    ylim      = c(0, max(top7) * 1.1),
    cex.names = 1.5,
    cex.axis  = 1.5,
    ylab      = "Relative frequency",
    cex.lab   = 1.5,
    main      = main.title,
    cex.main  = 1.5,
    xlab      = "Lineup",
  )
  
  # ── 6) compute legend position at plot’s vertical center ────────────────
  u     <- par("usr")
  y_mid <- mean(u[3:4])
  x_out <- u[2] + -0.02 * diff(u[1:2])


        leg_txt  <- paste0(
  "L[", codes, "]~\":\"~\"",
   init_labels,
  "\""
)
leg_expr <- parse(text = leg_txt)

  # ── 7) draw legend/key ───────────────────────────────────────────────────
  legend(
    x        = x_out,
    y        = y_mid,
    legend   = leg_expr,
    title    = "Lineups",
    bty      = "n",
    yjust    = 0.5,
    xpd      = TRUE,
    cex      = 1.3,
    text.font= 1,
    title.cex = 1.5,
    y.intersp = 1.3
  )
}


plot_top7(summary_EFF,      "EFF lineups (Top 7 by EFF)")
plot_top7(summary_Winscore, "Winscore lineups (Top 7 by Winscore)")
plot_top7(summary_TS,       "log(TS) lineups (Top 7 by log(TS))")

dev.off()