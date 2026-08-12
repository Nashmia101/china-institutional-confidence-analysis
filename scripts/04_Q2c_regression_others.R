# ============================================================
# Q2(c): Predictor-outcome correlations, full regression models,
# and backward elimination for the pooled "Others" group,
# plus CHN vs Others comparison
# ============================================================

outcome_vars_all <- c(
  "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
)

for (outcome in outcome_vars_all) {
  temp_df <- others_data[, c(outcome, predictor_vars), drop = FALSE]
  temp_df <- na.omit(temp_df)
  cat(outcome, ":", nrow(temp_df), "complete rows\n")
}

# ---- Figure 13: Predictor-outcome correlation heatmap (Others) ----
# {r, fig.width=10, fig.height=7}
library(reshape2)
library(ggplot2)

corr <- cor(
  others_data[, predictor_vars],
  others_data[, outcome_vars_all],
  use = "pairwise.complete.obs"
)

df <- melt(corr)

ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4", midpoint = 0) +
  labs(
    title = "Predictor–Outcome Correlations (Others)",
    x = "Confidence outcome", y = "Predictor", fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# ---- Full multiple regression models (Others) ----
full_model_results_others <- list()
for (outcome in outcome_vars_all) {
  full_formula <- as.formula(paste(outcome, "~", paste(predictor_vars, collapse = " + ")))
  full_df <- others_data[, c(outcome, predictor_vars), drop = FALSE]
  full_df <- na.omit(full_df)
  full_model <- lm(full_formula, data = full_df)
  full_model_results_others[[outcome]] <- summary(full_model)
}

full_predictor_stats_others <- data.frame()
for (outcome in names(full_model_results_others)) {
  coef_table <- as.data.frame(full_model_results_others[[outcome]]$coefficients)
  coef_table$Predictor <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[coef_table$Predictor != "(Intercept)", , drop = FALSE]
  names(coef_table) <- c("Estimate", "Std_Error", "t_value", "p_value", "Predictor")
  coef_table$Outcome <- outcome
  coef_table$Abs_t <- abs(coef_table$t_value)
  coef_table <- coef_table[, c("Outcome", "Predictor", "Estimate", "Std_Error", "t_value", "Abs_t", "p_value")]
  full_predictor_stats_others <- rbind(full_predictor_stats_others, coef_table)
}

library(dplyr)
library(ggplot2)

# NOTE: full_model_stats_others (R2/Adj_R2 per outcome) referenced below was built the
# same way as full_model_stats_b in the Q2b script — construct it analogously if re-running
# standalone (loop over full_model_results_others, extracting r.squared/fstatistic).

full_model_stats_others %>%
  arrange(R2) %>%
  ggplot(aes(x = R2, y = reorder(Outcome, R2))) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = round(R2, 3)), hjust = -0.1, size = 4) +
  expand_limits(x = max(full_model_stats_others$R2) + 0.01) +
  labs(title = "Model Predictability (R²) Others", x = expression(R^2), y = "Confidence outcome") +
  theme_minimal()

heatmap_full_others <- full_predictor_stats_others %>%
  group_by(Outcome) %>%
  mutate(Scaled_Importance = Abs_t / max(Abs_t, na.rm = TRUE)) %>%
  ungroup()

pred_order_others <- heatmap_full_others %>%
  group_by(Predictor) %>%
  summarise(Max_Importance = max(Scaled_Importance, na.rm = TRUE)) %>%
  arrange(desc(Max_Importance)) %>%
  pull(Predictor)

heatmap_full_others$Predictor <- factor(heatmap_full_others$Predictor, levels = rev(pred_order_others))
heatmap_full_others$Outcome <- factor(heatmap_full_others$Outcome, levels = outcome_vars_all)

ggplot(heatmap_full_others, aes(x = Outcome, y = Predictor, fill = Scaled_Importance)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(Scaled_Importance, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4", midpoint = 0.5, limits = c(0, 1)) +
  labs(
    title = "Predictor Importance Others",
    x = "Confidence outcome", y = "Predictor", fill = "Scaled importance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ---- Backward elimination (Others) ----
# Uses backward_elimination() defined in 03_Q2b_regression_china.R
all_results_others <- list()
for (outcome in outcome_vars_all) {
  all_results_others[[outcome]] <- backward_elimination(
    data = others_data,
    outcome = outcome,
    predictors = predictor_vars,
    alpha = 0.05
  )
}

all_model_stats_others <- do.call(rbind, lapply(all_results_others, function(x) x$model_stats))
all_predictor_stats_others <- do.call(rbind, lapply(all_results_others, function(x) x$predictor_stats))

make_model_stats_gt(all_model_stats_others, "Backward elimination model summary for confidence outcomes – Others")
make_predictor_stats_gt(all_predictor_stats_others, "Final predictors after backward elimination – Others")

table7_gt <- make_model_stats_gt(all_model_stats_others, "Backward elimination model summary for confidence outcomes (Others)")
gtsave(table7_gt, "table7.png")

# ---- Figure 14: Model predictability after backward elimination (Others) ----
r2_plot_others <- all_model_stats_others %>%
  arrange(R2)

ggplot(r2_plot_others, aes(x = R2, y = reorder(Outcome, R2))) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = round(R2, 3)), hjust = -0.1, size = 4) +
  expand_limits(x = max(r2_plot_others$R2) + 0.01) +
  labs(title = "Model Predictability (R²) Others", x = expression(R^2), y = "Confidence outcome") +
  theme_minimal()

# ---- Figure 15: Top predictors after backward elimination (Others) ----
heatmap_others <- all_predictor_stats_others %>%
  group_by(Outcome) %>%
  mutate(Scaled_Importance = Abs_t / max(Abs_t, na.rm = TRUE)) %>%
  ungroup()

pred_order_others <- heatmap_others %>%
  group_by(Predictor) %>%
  summarise(Max_Importance = max(Scaled_Importance, na.rm = TRUE)) %>%
  arrange(desc(Max_Importance)) %>%
  pull(Predictor)

heatmap_others$Predictor <- factor(heatmap_others$Predictor, levels = rev(pred_order_others))
heatmap_others$Outcome <- factor(heatmap_others$Outcome, levels = outcome_vars_all)

ggplot(heatmap_others, aes(x = Outcome, y = Predictor, fill = Scaled_Importance)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(Scaled_Importance, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4", midpoint = 0.5, limits = c(0, 1)) +
  labs(
    title = "Top Predictors of Confidence after backward elimination (Others)",
    x = "Confidence outcome", y = "Predictor", fill = "Scaled importance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ---- Figure 16: China vs Others R² comparison ----
compare_r2 <- bind_rows(
  all_model_stats %>% select(Outcome, R2) %>% mutate(Group = "CHN"),
  all_model_stats_others %>% select(Outcome, R2) %>% mutate(Group = "Others")
)

ggplot(compare_r2, aes(x = Outcome, y = R2, fill = Group)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(R2, 3)),
    position = position_dodge(width = 0.9), vjust = -0.3, size = 3
  ) +
  labs(
    title = "Model Predictability (R²) after Backward Elimination: CHN vs Others",
    x = "Confidence outcome", y = expression(R^2)
  ) +
  scale_fill_manual(values = c("CHN" = "tomato", "Others" = "dodgerblue4")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
