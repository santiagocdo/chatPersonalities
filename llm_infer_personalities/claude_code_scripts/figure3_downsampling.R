# figure3_downsampling.R
#
# Figure 3: Personality estimation accuracy by window size for Experiment 4
# (N=104). Chronological rolling-window design — for each (participant, k),
# the prediction is the mean across all windows of that size; correlation is
# then computed across the 104 participants.
#
# Run AFTER infer_interactions_exp4.py:
#   python3 claude_code_scripts/infer_interactions_exp4.py
#
# Then:
#   Rscript claude_code_scripts/figure3_downsampling.R

rm(list = ls(all = TRUE))

library(ggplot2)
library(viridisLite)

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
PROMPT_LABELS <- c(
  "1" = "Prompt 1 (Lexical)",
  "2" = "Prompt 2 (CoT)",
  "3" = "Prompt 3 (Persona)"
)
PROMPT_LEVELS <- unname(PROMPT_LABELS)

# ── 1. Ground truth (0-5 -> 0-1) ──────────────────────────────────────────────
bfi <- read.csv(file.path(BASE_DIR, "exp4_n104_bfi.csv"))
STANDARD_COLS <- c("participant_ID", "Extroversion", "Agreeableness",
                   "Conscientiousness", "Neuroticism", "Openness")
colnames(bfi)[1:6] <- STANDARD_COLS
bfi[, -1] <- bfi[, -1] / 5
bfi$participant_ID <- as.character(bfi$participant_ID)
message(sprintf("Ground truth loaded: N = %d", nrow(bfi)))

# ── 2. Per-window predictions ────────────────────────────────────────────────
preds <- read.csv(file.path(OUT_DIR, "interaction_pred_exp4.csv"))
preds$participant_ID <- as.character(preds$participant_ID)
# Normalize LLM scores 0-10 -> 0-1
for (p in 1:3) {
  for (d in DIMS) {
    col <- paste0("p", p, "_", d)
    preds[[col]] <- preds[[col]] / 10
  }
}
message(sprintf("Per-window predictions loaded: %d rows", nrow(preds)))

k_levels <- sort(unique(preds$k), decreasing = TRUE)

# ── 3. Per-participant means across windows ──────────────────────────────────
# For each (participant, k), average all per-window predictions for that k.
# Also aggregate lexical metrics (mean across windows).
agg_cols <- c(
  paste0(rep(paste0("p", 1:3, "_"), each = length(DIMS)), DIMS),
  "total_words", "mean_words", "total_chars", "mean_chars",
  "unique_words", "ttr", "mean_sentence_len"
)

means <- aggregate(
  preds[, agg_cols],
  by  = list(participant_ID = preds$participant_ID, k = preds$k),
  FUN = function(x) mean(x, na.rm = TRUE)
)
# Window counts (1, 11, 21, 26 per participant per k)
nwin <- aggregate(
  preds$window_start,
  by  = list(participant_ID = preds$participant_ID, k = preds$k),
  FUN = length
)
colnames(nwin)[3] <- "n_windows"
means <- merge(means, nwin, by = c("participant_ID", "k"))

write.csv(means, file.path(OUT_DIR, "interaction_pred_exp4_means.csv"), row.names = FALSE)
message(sprintf("Per-participant means written: %d rows", nrow(means)))

# ── 4. Correlations (one r per prompt x k x trait) ───────────────────────────
corr_rows <- list()
for (pid_p in 1:3) {
  for (kk in k_levels) {
    sub <- means[means$k == kk, ]
    m <- merge(sub, bfi, by = "participant_ID", suffixes = c("_pred", "_gt"))
    for (d in DIMS) {
      pred_col <- paste0("p", pid_p, "_", d, "_pred")
      gt_col   <- paste0(d, "_gt")
      # If merge didn't rename (no name clash), fall back to raw column name
      if (!pred_col %in% names(m)) pred_col <- paste0("p", pid_p, "_", d)
      if (!gt_col   %in% names(m)) gt_col   <- d
      ok <- !is.na(m[[pred_col]]) & !is.na(m[[gt_col]])
      if (sum(ok) < 5) next
      ct <- cor.test(m[[pred_col]][ok], m[[gt_col]][ok])
      corr_rows[[length(corr_rows) + 1]] <- data.frame(
        prompt    = PROMPT_LABELS[as.character(pid_p)],
        k         = kk,
        dimension = d,
        r         = as.numeric(ct$estimate),
        ci_low    = ct$conf.int[1],
        ci_high   = ct$conf.int[2],
        p_value   = ct$p.value,
        n_part    = sum(ok)
      )
    }
  }
}
correls <- do.call(rbind, corr_rows)
correls$prompt    <- factor(correls$prompt,    levels = PROMPT_LEVELS)
correls$dimension <- factor(correls$dimension, levels = DIMS, labels = DIM_LABELS)

write.csv(correls, file.path(OUT_DIR, "figure3_correlations.csv"), row.names = FALSE)
message("Correlations written.")

# ── 5. Figure 3 ─────────────────────────────────────────────────────────────
p_fig3 <- ggplot(correls, aes(x = k, y = r, colour = prompt, fill = prompt)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = viridis(3, option = "D")) +
  scale_fill_manual(values   = viridis(3, option = "D")) +
  scale_x_continuous(breaks = sort(k_levels)) +
  labs(
    title  = "Figure 3: Personality estimation accuracy by rolling-window size (Exp 4, N=104)",
    x      = "Window size (k interactions)",
    y      = "Pearson r (estimated vs. ground truth)",
    colour = "Prompt strategy:",
    fill   = "Prompt strategy:"
  ) +
  facet_wrap(~ dimension, nrow = 1) +
  theme_bw(base_size = 16) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

fig3_pdf <- file.path(OUT_DIR, "figure3_downsampling.pdf")
fig3_png <- file.path(OUT_DIR, "figure3_downsampling.png")
ggsave(fig3_pdf, p_fig3, width = 18, height = 5.5)
ggsave(fig3_png, p_fig3, width = 18, height = 5.5, dpi = 300)
message("Figure 3 saved -> ", fig3_pdf)
message("Figure 3 saved -> ", fig3_png)

# ── 6. Stdout summaries ─────────────────────────────────────────────────────
message("\n--- Mean r by prompt x k (averaged across traits) ---")
agg_r <- aggregate(r ~ prompt + k, data = correls, FUN = mean)
agg_r <- agg_r[order(agg_r$prompt, -agg_r$k), ]
print(agg_r, digits = 3, row.names = FALSE)

message("\n--- Mean lexical metrics by k (across participants & windows) ---")
lex_cols <- c("total_words", "mean_words", "total_chars", "mean_chars",
              "unique_words", "ttr", "mean_sentence_len")
lex_summary <- aggregate(preds[, lex_cols], by = list(k = preds$k), FUN = mean, na.rm = TRUE)
lex_summary <- lex_summary[order(-lex_summary$k), ]
print(lex_summary, digits = 3, row.names = FALSE)
