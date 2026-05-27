# analyze_results.R
#
# Merges ground-truth BFI scores with LLM-predicted Big Five scores (two prompts,
# three experiments), writes combined CSVs, and plots a correlation figure.
#
# Run AFTER infer_personalities.py has completed all experiments:
#   python3 claude_code_scripts/infer_personalities.py
#
# Then run this script from the repo root:
#   Rscript llm_infer_personalities/claude_code_scripts/analyze_results.R

rm(list = ls(all = TRUE))

library(ggplot2)
library(viridisLite)

# ── paths ─────────────────────────────────────────────────────────────────────
# Resolve BASE_DIR whether called via Rscript or source()
if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) {
  SCRIPT_DIR <- dirname(normalizePath(sys.frame(1)$ofile))
} else {
  # Called via Rscript: use commandArgs
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg)) {
    SCRIPT_DIR <- dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    SCRIPT_DIR <- file.path(getwd(), "llm_infer_personalities", "claude_code_scripts")
  }
}
BASE_DIR <- dirname(SCRIPT_DIR)
OUT_DIR  <- file.path(SCRIPT_DIR, "outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── load ground-truth BFI ─────────────────────────────────────────────────────
# exp1: Participant.Private.ID, bfi10_extraversion, bfi10_agreeableness,
#        bfi10_conscientiousness, bfi10_neuroticism, bfi10_openness  (1–7 scale)
# exp2: Participant.Private.ID, bfi44_extraversion, bfi44_agreeableness,
#        bfi44_conscientiousness, bfi44_neuroticism, bfi44_openness  (1–5 scale)
# exp3: participant_ID, extraversion_score, agreeableness_score,
#        conscientiousness_score, neuroticism_score, openness_score  (1–5 scale)
# exp4: same column structure as exp3 (1–5 scale)
bfi_e1 <- read.csv(file.path(BASE_DIR, "exp1_n89_bfi.csv"))
bfi_e2 <- read.csv(file.path(BASE_DIR, "exp2_n97_bfi.csv"))
bfi_e3 <- read.csv(file.path(BASE_DIR, "exp3_n100_bfi.csv"))
bfi_e4 <- read.csv(file.path(BASE_DIR, "exp4_n104_bfi.csv"))

# Standardise column names (col order is the same across all three files)
STANDARD_COLS <- c("participant_ID", "Extroversion", "Agreeableness",
                   "Conscientiousness", "Neuroticism", "Openness")
colnames(bfi_e1)[1:6] <- STANDARD_COLS
colnames(bfi_e2)[1:6] <- STANDARD_COLS
colnames(bfi_e3)[1:6] <- STANDARD_COLS
colnames(bfi_e4)[1:6] <- STANDARD_COLS

# Normalise to 0–1 (exp1 uses 1–7, exp2/3/4 use 1–5)
bfi_e1[, -1] <- bfi_e1[, -1] / 7
bfi_e2[, -1] <- bfi_e2[, -1] / 5
bfi_e3[, -1] <- bfi_e3[, -1] / 5
bfi_e4[, -1] <- bfi_e4[, -1] / 5

# ── helper: load a prediction CSV ─────────────────────────────────────────────
load_pred <- function(prompt_id, exp_num) {
  path <- file.path(OUT_DIR, sprintf("llm_pred_p%d_exp%d.csv", prompt_id, exp_num))
  if (!file.exists(path)) {
    stop(sprintf("Missing prediction file: %s\nRun infer_personalities.py first.", path))
  }
  df <- read.csv(path)
  df$Participant_ID <- as.character(df$Participant_ID)
  df[order(df$Participant_ID), ]
}

DIMS <- c("Extroversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")

# ── build combined CSV per experiment ─────────────────────────────────────────
build_combined <- function(bfi, exp_num) {
  # Cast IDs to character before sorting to ensure consistent string sort
  # (avoids numeric vs. string sort mismatch for mixed-length IDs in Exp 1)
  bfi$participant_ID <- as.character(bfi$participant_ID)
  bfi <- bfi[order(bfi$participant_ID), ]

  p1 <- load_pred(1, exp_num)
  p2 <- load_pred(2, exp_num)
  p3 <- load_pred(3, exp_num)

  # Verify IDs match ground truth exactly
  stopifnot(
    "P1 IDs do not match BFI IDs" = all(bfi$participant_ID == p1$Participant_ID),
    "P2 IDs do not match BFI IDs" = all(bfi$participant_ID == p2$Participant_ID),
    "P3 IDs do not match BFI IDs" = all(bfi$participant_ID == p3$Participant_ID)
  )

  out <- data.frame(subjectId = bfi$participant_ID)

  # Ground truth (already normalised to 0–1)
  for (d in DIMS) out[[paste0("gt_", d)]] <- bfi[[d]]

  # LLM predictions normalised from 0–10 to 0–1
  for (d in DIMS) out[[paste0("p1_", d)]] <- p1[[d]] / 10
  for (d in DIMS) out[[paste0("p2_", d)]] <- p2[[d]] / 10
  for (d in DIMS) out[[paste0("p3_", d)]] <- p3[[d]] / 10

  out
}

combined_e1 <- build_combined(bfi_e1, 1)
combined_e2 <- build_combined(bfi_e2, 2)
combined_e3 <- build_combined(bfi_e3, 3)
combined_e4 <- build_combined(bfi_e4, 4)

write.csv(combined_e1, file.path(OUT_DIR, "combined_exp1.csv"), row.names = FALSE)
write.csv(combined_e2, file.path(OUT_DIR, "combined_exp2.csv"), row.names = FALSE)
write.csv(combined_e3, file.path(OUT_DIR, "combined_exp3.csv"), row.names = FALSE)
write.csv(combined_e4, file.path(OUT_DIR, "combined_exp4.csv"), row.names = FALSE)
message("Combined CSVs written.")

# ── correlation helpers ────────────────────────────────────────────────────────
getCorrels <- function(gt_df, pred_df, dims = DIMS) {
  rows <- lapply(dims, function(d) {
    test <- cor.test(gt_df[[d]], pred_df[[d]], use = "complete.obs")
    data.frame(
      dimension = d,
      cor       = as.numeric(test$estimate),
      ci_low    = test$conf.int[1],
      ci_high   = test$conf.int[2],
      p_value   = test$p.value
    )
  })
  do.call(rbind, rows)
}

correls_from_combined <- function(combined, prompt_prefix, exp_label, prompt_label) {
  gt   <- combined[, paste0("gt_", DIMS), drop = FALSE]
  pred <- combined[, paste0(prompt_prefix, "_", DIMS), drop = FALSE]
  colnames(gt)   <- DIMS
  colnames(pred) <- DIMS
  result <- getCorrels(gt, pred)
  cbind(data.frame(prompt = prompt_label, exp = exp_label), result)
}

# ── compute correlations ───────────────────────────────────────────────────────
corr_list <- list(
  correls_from_combined(combined_e1, "p1", "Expt. 1 (n=89)",   "Prompt 1 (Lexical)"),
  correls_from_combined(combined_e1, "p2", "Expt. 1 (n=89)",   "Prompt 2 (CoT)"),
  correls_from_combined(combined_e1, "p3", "Expt. 1 (n=89)",   "Prompt 3 (Persona)"),
  correls_from_combined(combined_e2, "p1", "Expt. 2 (n=97)",   "Prompt 1 (Lexical)"),
  correls_from_combined(combined_e2, "p2", "Expt. 2 (n=97)",   "Prompt 2 (CoT)"),
  correls_from_combined(combined_e2, "p3", "Expt. 2 (n=97)",   "Prompt 3 (Persona)"),
  correls_from_combined(combined_e3, "p1", "Expt. 3 (n=100)",  "Prompt 1 (Lexical)"),
  correls_from_combined(combined_e3, "p2", "Expt. 3 (n=100)",  "Prompt 2 (CoT)"),
  correls_from_combined(combined_e3, "p3", "Expt. 3 (n=100)",  "Prompt 3 (Persona)"),
  correls_from_combined(combined_e4, "p1", "Expt. 4 (n=104)",  "Prompt 1 (Lexical)"),
  correls_from_combined(combined_e4, "p2", "Expt. 4 (n=104)",  "Prompt 2 (CoT)"),
  correls_from_combined(combined_e4, "p3", "Expt. 4 (n=104)",  "Prompt 3 (Persona)")
)

# Combined across all experiments (n = 89 + 97 + 100 + 104 = 390)
combined_all <- rbind(combined_e1, combined_e2, combined_e3, combined_e4)
corr_list[[13]] <- correls_from_combined(combined_all, "p1", "All Expts (n=390)", "Prompt 1 (Lexical)")
corr_list[[14]] <- correls_from_combined(combined_all, "p2", "All Expts (n=390)", "Prompt 2 (CoT)")
corr_list[[15]] <- correls_from_combined(combined_all, "p3", "All Expts (n=390)", "Prompt 3 (Persona)")

correls       <- do.call(rbind, corr_list)
correls$sig   <- ifelse(correls$p_value < 0.05, "sig", "ns")

# Order experiments left-to-right
correls$exp <- factor(correls$exp,
                      levels = c("Expt. 1 (n=89)", "Expt. 2 (n=97)",
                                 "Expt. 3 (n=100)", "Expt. 4 (n=104)",
                                 "All Expts (n=390)"))

# ── plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(correls, aes(x = cor, y = dimension, col = prompt, shape = sig)) +
  labs(
    title = "LLM prediction of user personality from chat interactions",
    y     = "Personality Dimension",
    x     = "Pearson r with 95% Confidence Intervals",
    col   = "Prompt:",
    shape = "Significance:"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_color_manual(values = viridis(3, option = "D")) +
  scale_shape_manual(
    values = c(ns = 21, sig = 19),
    labels = c(ns = "n.s.", sig = "p < .05")
  ) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    position = position_dodge(0.5), width = 0.25
  ) +
  geom_point(fill = "white", size = 3, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.25), limits = c(-0.6, 0.8)) +
  scale_y_discrete(labels = c(
    Extroversion      = "Extraversion",
    Agreeableness     = "Agreeableness",
    Conscientiousness = "Conscientiousness",
    Neuroticism       = "Neuroticism",
    Openness          = "Openness"
  )) +
  facet_grid(. ~ exp) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

fig_pdf <- file.path(OUT_DIR, "correlations_figure.pdf")
fig_png <- file.path(OUT_DIR, "correlations_figure.png")
ggsave(fig_pdf, p, width = 18, height = 5)
ggsave(fig_png, p, width = 18, height = 5, dpi = 150)
message("Figure saved → ", fig_pdf)
message("Figure saved → ", fig_png)

# ── summary table ──────────────────────────────────────────────────────────────
message("\n--- Correlation Summary ---")
print(
  correls[, c("exp", "prompt", "dimension", "cor", "ci_low", "ci_high", "p_value", "sig")],
  digits = 3, row.names = FALSE
)
