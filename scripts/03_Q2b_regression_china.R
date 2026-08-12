# ============================================================
# Q2(b): Predictor-outcome correlations, full regression models,
# and backward elimination for China (CHN)
# ============================================================

outcome_vars <- c(
  "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
)

predictor_vars <- c(
  "ILFam", "ILFriends", "ILLeisure",
  "ICQHardWork", "ICQResonsibility", "ICQImagination", "ICQTolerance",
  "ICQDetermination", "ICQFaith", "ICQObedience",
  "ACTReligion", "ACTUnions", "ACTEnvOrg", "ACTHumanitarian",
  "Happy", "LifeSatis", "FutureRespect", "IncomeEquality", "HardWork",
  "PolPetition", "PolDemons", "PolLeader", "PolExperts", "PolDemoc",
  "Sex"
)

# ---- Figure 7: Predictor-outcome correlation heatmap (CHN) ----
# {r, fig.width=10, fig.height=7}
focus_country <- "CHN"
screen_df <- focus_data[, c(outcome_vars, predictor_vars)]

screen_corr <- cor(
  screen_df[, predictor_vars],
  screen_df[, outcome_vars],
  use = "pairwise.complete.obs"
)
round(screen_corr, 2)

screen_corr_long <- melt(screen_corr)
names(screen_corr_long) <- c("Predictor", "Outcome", "Correlation")
screen_corr_long <- screen_corr_long[!is.na(screen_corr_long$Correlation), ]

pred_order <- rownames(screen_corr)[order(apply(abs(screen_corr), 1, max, na.rm = TRUE), decreasing = TRUE)]
screen_corr_long$Predictor <- factor(screen_corr_long$Predictor, levels = rev(pred_order))
screen_corr_long$Outcome <- factor(screen_corr_long$Outcome, levels = outcome_vars)

max_abs_corr <- max(abs(screen_corr_long$Correlation), na.rm = TRUE)

ggplot(screen_corr_long, aes(x = Outcome, y = Predictor, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "firebrick3", mid = "white", high = "dodgerblue4",
    midpoint = 0, limits = c(-max_abs_corr, max_abs_corr)
  ) +
  labs(
    title = paste("Predictor–Outcome Correlations (CHN)"),
    x = "Confidence outcome",
    y = "Predictor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ---- Full multiple regression models (CHN) ----
for (outcome in outcome_vars) {
  temp_df <- focus_data[, c(outcome, predictor_vars)]
  temp_df <- na.omit(temp_df)
  cat(outcome, ":", nrow(temp_df), "complete rows\n")
}

outcome_vars_reg <- c(
  "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt"
)

full_model_results <- list()
for (outcome in outcome_vars_reg) {
  full_formula <- as.formula(
    paste(outcome, "~", paste(predictor_vars, collapse = " + "))
  )
  full_df <- focus_data[, c(outcome, predictor_vars)]
  full_df <- na.omit(full_df)
  full_model <- lm(full_formula, data = full_df)
  full_model_results[[outcome]] <- summary(full_model)
}

full_model_stats_b <- data.frame()
for (outcome in names(full_model_results)) {
  fit_sum <- full_model_results[[outcome]]
  fstat <- fit_sum$fstatistic
  full_model_stats_b <- rbind(
    full_model_stats_b,
    data.frame(
      Outcome = outcome,
      R2 = fit_sum$r.squared,
      Adj_R2 = fit_sum$adj.r.squared,
      F_stat = unname(fstat[1]),
      F_pvalue = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    )
  )
}

full_predictor_stats_b <- data.frame()
for (outcome in names(full_model_results)) {
  coef_table <- as.data.frame(full_model_results[[outcome]]$coefficients)
  coef_table$Predictor <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[coef_table$Predictor != "(Intercept)", , drop = FALSE]
  names(coef_table) <- c("Estimate", "Std_Error", "t_value", "p_value", "Predictor")
  coef_table$Outcome <- outcome
  coef_table$Abs_t <- abs(coef_table$t_value)
  coef_table <- coef_table[, c("Outcome", "Predictor", "Estimate", "Std_Error", "t_value", "Abs_t", "p_value")]
  full_predictor_stats_b <- rbind(full_predictor_stats_b, coef_table)
}

# ---- Figure 8: Model predictability (R²), full model ----
full_model_stats_b %>%
  arrange(R2) %>%
  ggplot(aes(x = R2, y = reorder(Outcome, R2))) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = round(R2, 3)), hjust = -0.1, size = 4) +
  expand_limits(x = max(full_model_stats_b$R2) + 0.01) +
  labs(
    title = "Model Predictability (R²) China",
    x = expression(R^2),
    y = "Confidence outcome"
  ) +
  theme_minimal()

# ---- Figure 9: Predictor importance heatmap, full model ----
heatmap_full_b <- full_predictor_stats_b %>%
  group_by(Outcome) %>%
  mutate(Scaled_Importance = Abs_t / max(Abs_t, na.rm = TRUE)) %>%
  ungroup()

pred_order <- heatmap_full_b %>%
  group_by(Predictor) %>%
  summarise(Max_Importance = max(Scaled_Importance, na.rm = TRUE)) %>%
  arrange(desc(Max_Importance)) %>%
  pull(Predictor)

heatmap_full_b$Predictor <- factor(heatmap_full_b$Predictor, levels = rev(pred_order))
heatmap_full_b$Outcome <- factor(heatmap_full_b$Outcome, levels = outcome_vars_reg)

ggplot(heatmap_full_b, aes(x = Outcome, y = Predictor, fill = Scaled_Importance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Scaled_Importance)), size = 3) +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#4575b4",
    midpoint = 0.5, limits = c(0, 1)
  ) +
  labs(
    title = "Predictor Importance (China)",
    x = "Confidence outcome", y = "Predictor", fill = "Scaled importance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ---- Backward elimination function ----
backward_elimination <- function(data, outcome, predictors, alpha = 0.05) {
  work_df <- data[, c(outcome, predictors), drop = FALSE]
  work_df <- na.omit(work_df)

  if (nrow(work_df) == 0) {
    return(list(
      model_stats = data.frame(
        Outcome = outcome, Num_Predictors = NA, Residual_SE = NA, DF_Residual = NA,
        R2 = NA, Adj_R2 = NA, F_stat = NA, DF_Model = NA, DF_Error = NA,
        F_pvalue = NA, N_used = 0
      ),
      predictor_stats = data.frame(),
      model = NULL
    ))
  }

  current_predictors <- predictors
  repeat {
    if (length(current_predictors) == 0) break
    form <- as.formula(paste(outcome, "~", paste(current_predictors, collapse = " + ")))
    fit <- lm(form, data = work_df)
    fit_sum <- summary(fit)
    coef_table <- fit_sum$coefficients
    coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
    if (nrow(coef_table) == 0) break
    pvals <- coef_table[, "Pr(>|t|)"]
    if (max(pvals, na.rm = TRUE) < alpha) break
    remove_var <- names(which.max(pvals))
    current_predictors <- setdiff(current_predictors, remove_var)
  }

  if (length(current_predictors) == 0) {
    final_formula <- as.formula(paste(outcome, "~ 1"))
  } else {
    final_formula <- as.formula(paste(outcome, "~", paste(current_predictors, collapse = " + ")))
  }

  final_model <- lm(final_formula, data = work_df)
  final_summary <- summary(final_model)
  fstat <- final_summary$fstatistic

  model_stats <- data.frame(
    Outcome = outcome,
    Num_Predictors = length(current_predictors),
    Residual_SE = final_summary$sigma,
    DF_Residual = final_model$df.residual,
    R2 = final_summary$r.squared,
    Adj_R2 = final_summary$adj.r.squared,
    F_stat = ifelse(length(fstat) > 0, unname(fstat[1]), NA),
    DF_Model = ifelse(length(fstat) > 0, unname(fstat[2]), NA),
    DF_Error = ifelse(length(fstat) > 0, unname(fstat[3]), NA),
    F_pvalue = ifelse(length(fstat) > 0, pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE), NA),
    N_used = nrow(work_df)
  )

  coef_table <- as.data.frame(final_summary$coefficients)
  coef_table$Predictor <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[coef_table$Predictor != "(Intercept)", , drop = FALSE]

  if (nrow(coef_table) > 0) {
    names(coef_table) <- c("Estimate", "Std_Error", "t_value", "p_value", "Predictor")
    coef_table$Outcome <- outcome
    coef_table$Abs_t <- abs(coef_table$t_value)
    coef_table <- coef_table[, c("Outcome", "Predictor", "Estimate", "Std_Error", "t_value", "Abs_t", "p_value")]
  }

  return(list(
    model_stats = model_stats,
    predictor_stats = coef_table,
    model = final_model
  ))
}

# ---- Apply backward elimination (CHN) ----
all_results <- list()
for (outcome in outcome_vars_reg) {
  all_results[[outcome]] <- backward_elimination(
    data = focus_data,
    outcome = outcome,
    predictors = predictor_vars,
    alpha = 0.05
  )
}

all_model_stats <- do.call(rbind, lapply(all_results, function(x) x$model_stats))
all_predictor_stats <- do.call(rbind, lapply(all_results, function(x) x$predictor_stats))

make_predictor_stats_gt <- function(predictor_stats, title_text) {
  predictor_stats$p_value <- signif(predictor_stats$p_value, 4)
  predictor_stats |>
    gt::gt() |>
    gt::cols_label(
      Outcome = "Outcome", Predictor = "Predictor", Estimate = "Estimate",
      Std_Error = "Std. Error", t_value = "t value", Abs_t = "|t|", p_value = "p-value"
    ) |>
    gt::fmt_number(columns = c(Estimate, Std_Error, t_value, Abs_t), decimals = 4) |>
    gt::fmt_scientific(columns = c(p_value), decimals = 3) |>
    gt::tab_header(title = title_text) |>
    gt::tab_options(
      table.width = pct(80), table.font.size = px(16), column_labels.font.size = px(16),
      heading.title.font.size = px(22), data_row.padding = px(8), column_labels.padding = px(8)
    )
}

make_model_stats_gt <- function(model_stats, title_text) {
  model_stats_small <- model_stats |>
    dplyr::select(Outcome, Num_Predictors, Residual_SE, R2, Adj_R2, F_stat, F_pvalue, N_used)

  model_stats_small |>
    gt::gt() |>
    gt::cols_label(
      Outcome = "Outcome", Num_Predictors = "No. Pred", Residual_SE = "Res. SE",
      R2 = "R²", Adj_R2 = "Adj. R²", F_stat = "F Stat", F_pvalue = "Model p", N_used = "N"
    ) |>
    gt::fmt_number(columns = c(Num_Predictors, N_used), decimals = 0, use_seps = TRUE) |>
    gt::fmt_number(columns = c(Residual_SE, R2, Adj_R2, F_stat), decimals = 4) |>
    gt::fmt_scientific(columns = c(F_pvalue), decimals = 3) |>
    gt::tab_header(title = title_text) |>
    gt::tab_options(
      table.width = pct(80), table.font.size = px(16), column_labels.font.size = px(16),
      heading.title.font.size = px(22), data_row.padding = px(8), column_labels.padding = px(8)
    )
}

table5_gt <- make_model_stats_gt(all_model_stats, "Backward elimination model summary for confidence outcomes")
table6_gt <- make_predictor_stats_gt(all_predictor_stats, "Final predictors after backward elimination")

gtsave(table5_gt, "table5.png")
gtsave(table6_gt, "table6.png")

make_model_stats_gt(all_model_stats, "Backward elimination model summary for confidence outcomes")
make_predictor_stats_gt(all_predictor_stats, "Final predictors after backward elimination")

# ---- Figure 10: Model predictability after backward elimination ----
r2_plot_df <- all_model_stats %>%
  select(Outcome, R2) %>%
  arrange(R2)

ggplot(r2_plot_df, aes(x = R2, y = reorder(Outcome, R2))) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = sprintf("%.3f", R2)), hjust = -0.1, size = 4) +
  expand_limits(x = max(r2_plot_df$R2) + 0.01) +
  labs(
    title = "Model Predictability (R²) China after backward elimination",
    x = expression(R^2), y = "Confidence outcome"
  ) +
  theme_minimal()

# ---- Figure 11: Top predictors after backward elimination ----
# {r, fig.width=10, fig.height=7}
heatmap_all <- all_predictor_stats %>%
  group_by(Outcome) %>%
  mutate(Scaled_Importance = Abs_t / max(Abs_t, na.rm = TRUE)) %>%
  ungroup()

pred_order <- heatmap_all %>%
  group_by(Predictor) %>%
  summarise(Max_Importance = max(Scaled_Importance, na.rm = TRUE)) %>%
  arrange(desc(Max_Importance)) %>%
  pull(Predictor)

heatmap_all$Predictor <- factor(heatmap_all$Predictor, levels = rev(pred_order))

ggplot(heatmap_all, aes(x = Outcome, y = Predictor, fill = Scaled_Importance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Scaled_Importance)), size = 3) +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#4575b4",
    midpoint = 0.5, limits = c(0, 1)
  ) +
  labs(
    title = "Top Predictors of Confidence for China after backward elimination",
    x = "Confidence outcome", y = "Predictor", fill = "Scaled importance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ---- Figure 12: Bubble heatmap of shared predictors ----
# {r, fig.width=11, fig.height=7}
bubble_df <- all_predictor_stats %>%
  distinct(Outcome, Predictor, .keep_all = TRUE) %>%
  group_by(Predictor) %>%
  mutate(Frequency = n(), MaxAbs_t = max(Abs_t, na.rm = TRUE)) %>%
  ungroup()

pred_order <- bubble_df %>%
  distinct(Predictor, Frequency, MaxAbs_t) %>%
  arrange(Frequency, MaxAbs_t) %>%
  pull(Predictor)

bubble_df$Predictor <- factor(bubble_df$Predictor, levels = pred_order)

ggplot(bubble_df, aes(x = Outcome, y = Predictor)) +
  geom_point(
    aes(size = Abs_t, fill = Frequency),
    shape = 21, color = "black", alpha = 0.85, stroke = 0.3
  ) +
  scale_size_continuous(name = "|t|", range = c(2.5, 9)) +
  scale_fill_gradient(low = "#c6dbef", high = "#08519c", name = "Frequency") +
  labs(
    title = "Shared predictors across confidence outcomes after backward elimination in China",
    x = "Confidence outcome", y = "Predictor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )
