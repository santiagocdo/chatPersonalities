# figure2_interaction_volume.R
#
# Figure 2: Impact of interaction frequency and verbal density on BFI
# estimation accuracy (Squared Error) across N=390 participants.
#
# Usage:
#   Rscript claude_code_scripts/figure2_interaction_volume.R

rm(list = ls(all = TRUE))

library(ggplot2)
library(viridisLite)
library(reshape2)
library(cowplot)

# ── paths ─────────────────────────────────────────────────────────────────────
if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) {
  SCRIPT_DIR <- dirname(normalizePath(sys.frame(1)$ofile))
} else {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg)) {
    SCRIPT_DIR <- dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    SCRIPT_DIR <- file.path(getwd(), "llm_infer_personalities", "claude_code_scripts")
  }
}
BASE_DIR <- dirname(SCRIPT_DIR)
OUT_DIR  <- file.path(SCRIPT_DIR, "outputs")

DIMS <- c("Extroversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")
DIM_LABELS <- c(
  Extroversion      = "Extraversion",
  Agreeableness     = "Agreeableness",
  Conscientiousness = "Conscientiousness",
  Neuroticism       = "Neuroticism",
  Openness          = "Openness"
)

# ── 1. Load combined predictions + ground truth (already normalised 0-1) ─────
combined <- rbind(
  read.csv(file.path(OUT_DIR, "combined_exp1.csv")),
  read.csv(file.path(OUT_DIR, "combined_exp2.csv")),
  read.csv(file.path(OUT_DIR, "combined_exp3.csv")),
  read.csv(file.path(OUT_DIR, "combined_exp4.csv"))
)
combined$subjectId <- as.character(combined$subjectId)
message(sprintf("Combined predictions loaded: N = %d", nrow(combined)))

# ── 2. Load interaction summaries & aggregate per participant ─────────────────
load_summary <- function(fname, pid_col) {
  df <- read.csv(file.path(BASE_DIR, fname))
  df$pid <- as.character(df[[pid_col]])
  # Aggregate across chats: total interactions, weighted mean word count
  agg <- aggregate(
    cbind(num_interactions, user_mean_words) ~ pid,
    data = df,
    FUN = function(x) x  # placeholder — handled below
  )
  # Manual aggregation for weighted mean + total words
  agg_list <- split(df, df$pid)
  rows <- lapply(agg_list, function(sub) {
    n_int   <- sum(sub$num_interactions)
    wmean   <- weighted.mean(sub$user_mean_words, sub$num_interactions)
    # total words = sum across chats of (num_interactions * user_mean_words)
    tot_w   <- sum(sub$num_interactions * sub$user_mean_words)
    data.frame(
      subjectId        = sub$pid[1],
      num_interactions = n_int,
      user_mean_words  = wmean,
      total_words      = tot_w
    )
  })
  do.call(rbind, rows)
}

summaries <- rbind(
  load_summary("exp1_n89_inter_summary.csv",  "Participant.Private.ID"),
  load_summary("exp2_n97_inter_summary.csv",  "Participant.Private.ID"),
  load_summary("exp3_n100_inter_summary.csv", "participant_ID"),
  load_summary("exp4_n104_inter_summary.csv", "participant_ID")
)
summaries$subjectId <- as.character(summaries$subjectId)
message(sprintf("Summaries loaded: N = %d participants", nrow(summaries)))

# ── 3. Merge ──────────────────────────────────────────────────────────────────
dat <- merge(combined, summaries, by = "subjectId")
message(sprintf("Merged dataset: N = %d", nrow(dat)))

# ── 4. Compute Squared Error per trait x prompt ──────────────────────────────
se_rows <- list()
for (p in 1:3) {
  for (d in DIMS) {
    gt_col   <- paste0("gt_", d)
    pred_col <- paste0("p", p, "_", d)
    se_col   <- paste0("se_p", p, "_", d)
    dat[[se_col]] <- (dat[[pred_col]] - dat[[gt_col]])^2
  }
}

# ── 5. Reshape to long format ────────────────────────────────────────────────
prompt_labels <- c(
  "1" = "Prompt 1 (Lexical)",
  "2" = "Prompt 2 (CoT)",
  "3" = "Prompt 3 (Persona)"
)

long_list <- list()
for (p in 1:3) {
  for (d in DIMS) {
    se_col <- paste0("se_p", p, "_", d)
    long_list[[length(long_list) + 1]] <- data.frame(
      subjectId        = dat$subjectId,
      num_interactions = dat$num_interactions,
      user_mean_words  = dat$user_mean_words,
      total_words      = dat$total_words,
      prompt           = prompt_labels[as.character(p)],
      dimension        = d,
      SE               = dat[[se_col]]
    )
  }
}
long_df <- do.call(rbind, long_list)
long_df$dimension <- factor(long_df$dimension, levels = DIMS, labels = DIM_LABELS)
long_df$prompt    <- factor(long_df$prompt,
                            levels = c("Prompt 1 (Lexical)",
                                       "Prompt 2 (CoT)",
                                       "Prompt 3 (Persona)"))

# ── 6. Figure 2 (3 rows): interactions, mean words, total words ──────────────

long_row1 <- long_df
long_row1$row_label <- "Number of Interactions"
long_row1$x_var     <- long_row1$num_interactions

long_row2 <- long_df
long_row2$row_label <- "Mean Words per Message"
long_row2$x_var     <- long_row2$user_mean_words

long_row3 <- long_df
long_row3$row_label <- "Total Words in Session"
long_row3$x_var     <- long_row3$total_words

long_3row <- rbind(long_row1, long_row2, long_row3)
long_3row$row_label <- factor(long_3row$row_label,
                              levels = c("Number of Interactions",
                                         "Mean Words per Message",
                                         "Total Words in Session"))

# Helper: convert p-value to significance stars
star_corr <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*", "")))
}

# Compute slope p-values for annotation in each panel
compute_stars <- function(data, x_col) {
  data$x <- data[[x_col]]
  prompt_levels <- levels(data$prompt)
  cols <- viridis(3, option = "D")
  names(cols) <- prompt_levels

  ann_list <- list()
  for (d in levels(data$dimension)) {
    sub_d <- data[data$dimension == d, ]
    y_max <- quantile(sub_d$SE, 0.98, na.rm = TRUE)
    for (i in seq_along(prompt_levels)) {
      pl <- prompt_levels[i]
      sub <- sub_d[sub_d$prompt == pl, ]
      fit <- lm(SE ~ x, data = sub)
      pval <- summary(fit)$coefficients["x", "Pr(>|t|)"]
      s <- star_corr(pval)
      if (nchar(s) > 0) {
        ann_list[[length(ann_list) + 1]] <- data.frame(
          dimension = d,
          prompt    = pl,
          label     = s,
          x_pos     = max(sub$x, na.rm = TRUE),
          y_pos     = y_max - (i - 1) * y_max * 0.08
        )
      }
    }
  }
  if (length(ann_list) == 0) return(NULL)
  do.call(rbind, ann_list)
}

# Build one row-plot per predictor, each with its own free x-axis
make_row <- function(data, x_col, x_label, show_legend = FALSE, show_title = TRUE) {
  data$x <- data[[x_col]]
  stars <- compute_stars(data, x_col)

  p <- ggplot(data, aes(x = x, y = SE, colour = prompt)) +
    geom_point(alpha = 0.15, shape = 16, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    scale_colour_manual(values = viridis(3, option = "D")) +
    labs(y = "Squared Error (SE)", x = x_label, colour = "Prompt strategy:") +
    facet_wrap(~ dimension, nrow = 1, scales = "free_x") +
    theme_bw(base_size = 16) +
    theme(
      strip.background = element_rect(fill = "grey92"),
      panel.grid.minor = element_blank()
    )

  if (!is.null(stars) && nrow(stars) > 0) {
    p <- p + geom_text(
      data    = stars,
      aes(x = x_pos, y = y_pos, label = label, colour = prompt),
      inherit.aes = FALSE,
      size = 6, fontface = "bold", hjust = 1, show.legend = FALSE
    ) +
    scale_colour_manual(values = viridis(3, option = "D"))
  }

  if (!show_title) {
    p <- p + theme(strip.text = element_blank())
  }
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "bottom")
  }
  p
}

p_row1 <- make_row(long_df, "num_interactions", "Number of Interactions",
                   show_legend = FALSE, show_title = TRUE)
p_row2 <- make_row(long_df, "user_mean_words",  "Mean Words per Message",
                   show_legend = FALSE, show_title = FALSE)
p_row3 <- make_row(long_df, "total_words",      "Total Words in Session",
                   show_legend = TRUE,  show_title = FALSE)

p_fig2 <- plot_grid(p_row1, p_row2, p_row3, ncol = 1, rel_heights = c(1, 0.85, 1.05),
                    align = "v", axis = "lr")

fig2_pdf <- file.path(OUT_DIR, "figure2_interaction_volume.pdf")
fig2_png <- file.path(OUT_DIR, "figure2_interaction_volume.png")
ggsave(fig2_pdf, p_fig2, width = 18, height = 12)
ggsave(fig2_png, p_fig2, width = 18, height = 12, dpi = 300)
message("Figure 2 saved -> ", fig2_pdf)
message("Figure 2 saved -> ", fig2_png)

# ── 7. Summary: does verbosity reduce MSE? ──────────────────────────────────
message("\n--- Linear model: SE ~ num_interactions + user_mean_words + total_words (by prompt x trait) ---\n")

summary_rows <- list()
for (p_label in levels(long_df$prompt)) {
  for (d_label in levels(long_df$dimension)) {
    sub <- long_df[long_df$prompt == p_label & long_df$dimension == d_label, ]
    fit <- lm(SE ~ num_interactions + user_mean_words + total_words, data = sub)
    cf  <- summary(fit)$coefficients
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      prompt    = p_label,
      dimension = d_label,
      beta_interactions  = cf["num_interactions", "Estimate"],
      p_interactions     = cf["num_interactions", "Pr(>|t|)"],
      beta_mean_words    = cf["user_mean_words", "Estimate"],
      p_mean_words       = cf["user_mean_words", "Pr(>|t|)"],
      beta_total_words   = cf["total_words", "Estimate"],
      p_total_words      = cf["total_words", "Pr(>|t|)"],
      R2                 = summary(fit)$r.squared,
      MSE                = mean(sub$SE, na.rm = TRUE)
    )
  }
}

summary_tbl <- do.call(rbind, summary_rows)
summary_tbl$sig_interactions <- ifelse(summary_tbl$p_interactions < 0.05, "*", "")
summary_tbl$sig_mean_words   <- ifelse(summary_tbl$p_mean_words  < 0.05, "*", "")
summary_tbl$sig_total_words  <- ifelse(summary_tbl$p_total_words < 0.05, "*", "")

write.csv(summary_tbl, file.path(OUT_DIR, "figure2_summary_stats.csv"), row.names = FALSE)
message("Summary stats saved -> ", file.path(OUT_DIR, "figure2_summary_stats.csv"))

message("\n--- MSE by prompt (averaged across traits) ---")
mse_by_prompt <- aggregate(MSE ~ prompt, data = summary_tbl, FUN = mean)
print(mse_by_prompt, digits = 4, row.names = FALSE)

message("\n--- Significant predictors (p < .05) ---")
sig <- summary_tbl[summary_tbl$sig_interactions == "*" |
                   summary_tbl$sig_mean_words   == "*" |
                   summary_tbl$sig_total_words  == "*",
                   c("prompt", "dimension",
                     "beta_interactions", "p_interactions",
                     "beta_mean_words",   "p_mean_words",
                     "beta_total_words",  "p_total_words")]
if (nrow(sig) > 0) {
  print(sig, digits = 3, row.names = FALSE)
} else {
  message("No significant predictors found at p < .05.")
}
