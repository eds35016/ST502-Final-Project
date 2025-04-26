lineups <- read.table("../../../new_data/NC_State_line_ups.tsv",
                      header = TRUE,
                      sep    = "\t",
                      stringsAsFactors = FALSE)

# Turn 0/1 into a factor with human-readable labels
lineups$allowed_factor <- factor(lineups$allowed,
                                 levels = c(1, 0),
                                 labels = c("Allowed", "Disallowed"))

# Get the counts in that order:
counts <- table(lineups$allowed_factor)
props  <- prop.table(counts)

pdf("../../figures/NC_State_data/NC_State_allowed_lineups_barplot.pdf", width = 4, height = 4)
bp <- barplot(counts,
              col   = c("#009E73", "#D55E00"),
              main  = "Allowed vs Disallowed Lineups (NC State)",
              cex.main= 0.9,
              ylab  = "Number of Lineups",
              ylim  = c(0, max(counts)))

labels <- sprintf("%d (%.2f)", counts, props)

# place each count label centered inside its bar
text(x      = bp,
     y      = counts / 2,
     labels = labels,
     cex    = 0.8)
dev.off()
