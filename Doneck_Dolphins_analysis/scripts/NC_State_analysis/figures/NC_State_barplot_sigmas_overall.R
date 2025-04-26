# 0)–– LOAD SAVED SUMMARIES ----------------------------------------
load('../../results/NC_State_data/NC_State_lineups_EFF_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_Winscore_sigmas.RData')
load('../../results/NC_State_data/NC_State_lineups_TS_sigmas.RData')

# 1)–– MAP PLAYER IDs TO INITIALS -------------------------------------
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
      paste0(substr(parts[1], 1, 1),
             substr(parts[length(parts)], 1, 1))
    }
  ) %>%
  ungroup() %>%
  select(id_player, initials)

## 2)–– TABULATE COUNTS ---------------------------------------------------
counts_EFF      <- table(posterior_EFF)
counts_Winscore <- table(posterior_Winscore)
counts_TS       <- table(posterior_TS)

# union of every lineup string
all_lineups <- sort(
  Reduce(union, list(
    names(counts_EFF),
    names(counts_Winscore),
    names(counts_TS)
  ))
)

# helper to pad any table to the full universe
pad_counts <- function(ct){
  x <- integer(length(all_lineups))
  names(x) <- all_lineups
  x[names(ct)] <- as.integer(ct)
  x
}

counts_EFF_full      <- pad_counts(counts_EFF)
counts_Winscore_full <- pad_counts(counts_Winscore)
counts_TS_full       <- pad_counts(counts_TS)


# 3)–– PICK THE GLOBAL TOP 7 ---------------------------------------------
global_counts <- counts_EFF_full +
                 counts_Winscore_full +
                 counts_TS_full

selected <- names(
  head(sort(global_counts, decreasing = TRUE), 7)
)


# 4)–– PLOTTING FUNCTION FOR THOSE SAME 7 -------------------------------
plot_selected7 <- function(counts_full, posterior_vec, main.title){
  # relative freq for *exactly* our 7
  rel7 <- counts_full[selected] / length(posterior_vec)

  # build “AB, CD, EF” labels
  init_labels <- sapply(selected, function(lbl){
    ids <- as.integer(strsplit(lbl, " ")[[1]])
    paste(
      id_map$initials[ match(ids, id_map$id_player) ],
      collapse = ", "
    )
  })

  codes <- seq_along(init_labels)

  # expand margin
  oldp <- par(mar = c(5,4,4,15) + 0.1, xpd = TRUE)
  on.exit(par(oldp), add = TRUE)

  # draw bars
  barplot(
    rel7,
    names.arg = parse(text = paste0("L[", codes, "]")),
    ylim      = c(0, max(rel7)*1.1),
    cex.names = 1.5, cex.axis = 1.5,
    ylab      = "Relative frequency", cex.lab = 1.5,
    main      = main.title,        cex.main = 1.5,
    col       = rgb(0.8,0,0,0.6)
  )

  # legend out to the right
  u     <- par("usr"); y_mid <- mean(u[3:4])
  x_out <- u[2] - 0.02 * diff(u[1:2])

  leg_txt <- paste0(
    "L[", codes, "]~\":\"~\"",
    init_labels, "\""
  )

  legend(
    x        = x_out, y = y_mid,
    legend   = parse(text = leg_txt),
    title    = "Lineups",       bty = "n",
    yjust    = 0.5,             xpd = TRUE,
    cex      = 1.3,             title.cex = 1.5,
    y.intersp= 1.3
  )
}


# 5)–– RENDER ALL THREE TO PDF ------------------------------------------
pdf("../../figures/NC_State_data/NC_State_lineups_sigmas_barplots_overall.pdf",
    width = 10, height = 8)

plot_selected7(
  counts_EFF_full, posterior_EFF,
  "EFF lineups (Top 7 overall)"
)
plot_selected7(
  counts_Winscore_full, posterior_Winscore,
  "Winscore lineups (Top 7 overall)"
)
plot_selected7(
  counts_TS_full, posterior_TS,
  "log(TS) lineups (Top 7 overall)"
)

dev.off()