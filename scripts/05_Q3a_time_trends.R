# ============================================================
# Q3(a): Screening variables for association with Wave,
# mean trends over time, interaction regression (trend
# comparison), and per-wave Wilcoxon comparisons
# ============================================================

# ---- Figure 18: Absolute correlation with Wave (screening) ----
# {r, fig.width=10, fig.height=7}
time_var <- "Wave"
all_q3a_vars <- setdiff(names(VC_clean), c("Country", "Wave", "Year"))

q3a_screen_table <- data.frame(
  Variable = all_q3a_vars,
  Cor_Focus = sapply(all_q3a_vars, function(v) {
    cor(focus_data[[v]], focus_data[[time_var]], use = "pairwise.complete.obs")
  }),
  Cor_Others = sapply(all_q3a_vars, function(v) {
    cor(others_data[[v]], others_data[[time_var]], use = "pairwise.complete.obs")
  }),
  stringsAsFactors = FALSE
)

q3a_screen_table$Abs_Cor_Focus <- abs(q3a_screen_table$Cor_Focus)
q3a_screen_table$Abs_Cor_Others <- abs(q3a_screen_table$Cor_Others)

q3a_screen_table_focus <- q3a_screen_table[order(-q3a_screen_table$Abs_Cor_Focus), ]
q3a_screen_table_others <- q3a_screen_table[order(-q3a_screen_table$Abs_Cor_Others), ]

q3a_screen_plot_all <- q3a_screen_table
q3a_screen_plot_all$Max_Cor <- pmax(q3a_screen_plot_all$Abs_Cor_Focus, q3a_screen_plot_all$Abs_Cor_Others)
q3a_screen_plot_all <- q3a_screen_plot_all[order(-q3a_screen_plot_all$Max_Cor), ]
q3a_screen_plot_all$Variable <- factor(q3a_screen_plot_all$Variable, levels = rev(q3a_screen_plot_all$Variable))

ggplot(q3a_screen_plot_all, aes(y = Variable)) +
  geom_segment(
    aes(x = Abs_Cor_Focus, xend = Abs_Cor_Others, yend = Variable),
    color = "grey70", linewidth = 0.8
  ) +
  geom_point(aes(x = Abs_Cor_Focus, color = "CHN"), size = 3) +
  geom_point(aes(x = Abs_Cor_Others, color = "Others"), size = 3) +
  geom_vline(xintercept = 0.10, linetype = "dashed", color = "black") +
  scale_color_manual(name = "Group", values = c("CHN" = "dodgerblue4", "Others" = "firebrick3")) +
  labs(
    title = "Absolute correlation with Wave",
    subtitle = "Screening of all predictors for CHN vs Others",
    x = "|r| with Wave", y = "Variable"
  ) +
  theme_minimal()

# ---- Linear trend (slope) per variable, CHN vs Others ----
q3a_trend_results <- data.frame(
  Variable = all_q3a_vars,
  Slope_Focus = NA_real_,
  PValue_Focus = NA_real_,
  Slope_Others = NA_real_,
  PValue_Others = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(all_q3a_vars)) {
  v <- all_q3a_vars[i]

  focus_df <- focus_data[, c(v, time_var)]
  focus_df <- focus_df[complete.cases(focus_df), ]
  if (nrow(focus_df) >= 2) {
    focus_model <- lm(as.formula(paste(v, "~", time_var)), data = focus_df)
    focus_summary <- summary(focus_model)
    q3a_trend_results$Slope_Focus[i] <- coef(focus_model)[2]
    q3a_trend_results$PValue_Focus[i] <- coef(focus_summary)[2, 4]
  }

  others_df <- others_data[, c(v, time_var)]
  others_df <- others_df[complete.cases(others_df), ]
  if (nrow(others_df) >= 2) {
    others_model <- lm(as.formula(paste(v, "~", time_var)), data = others_df)
    others_summary <- summary(others_model)
    q3a_trend_results$Slope_Others[i] <- coef(others_model)[2]
    q3a_trend_results$PValue_Others[i] <- coef(others_summary)[2, 4]
  }
}

q3a_trend_results

# ---- Mean response by wave, CHN vs Others ----
focus_data$Group <- "CHN"
others_data$Group <- "Others"
combined_data <- rbind(focus_data, others_data)

long_data <- combined_data %>%
  select(all_of(c("Group", time_var, all_q3a_vars))) %>%
  pivot_longer(cols = all_of(all_q3a_vars), names_to = "Variable", values_to = "Value")

q3a_means <- long_data %>%
  group_by(Variable, Group, .data[[time_var]]) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

q3a_means <- q3a_means %>%
  filter(!is.na(Mean), !is.nan(Mean))

q3a_means

# ---- Figure 19: Mean response over Wave, faceted by variable ----
# {r, fig.width=12, fig.height=9}
ggplot(q3a_means, aes(x = Wave, y = Mean, color = Group, group = Group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  ggh4x::facet_wrap2(~ Variable, scales = "free_y", axes = "all") +
  labs(
    title = "Mean response over Wave for CHN vs Others",
    x = "Wave", y = "Mean response", color = "Group"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

# ---- Table 7 / Figure 20: Interaction regression (Wave x Group) ----
time_var <- "Wave"
focus_data$Group <- "CHN"
others_data$Group <- "Others"
combined_data <- rbind(focus_data, others_data)

q3a_interaction_results <- data.frame(
  Variable = all_q3a_vars,
  Interaction_Coeff = NA_real_,
  Interaction_PValue = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(all_q3a_vars)) {
  v <- all_q3a_vars[i]
  model_df <- combined_data[, c(v, time_var, "Group")]
  model_df <- model_df[complete.cases(model_df), ]

  if (nrow(model_df) >= 3 && length(unique(model_df$Group)) == 2) {
    interaction_model <- lm(as.formula(paste(v, "~", time_var, "* Group")), data = model_df)
    coef_table <- summary(interaction_model)$coefficients
    interaction_row <- grep(paste0(time_var, ":Group|Group:", time_var), rownames(coef_table))

    if (length(interaction_row) == 1) {
      q3a_interaction_results$Interaction_Coeff[i] <- coef_table[interaction_row, 1]
      q3a_interaction_results$Interaction_PValue[i] <- coef_table[interaction_row, 4]
    }
  }
}

q3a_interaction_table <- q3a_interaction_results %>%
  mutate(
    Interaction_Coeff = round(Interaction_Coeff, 4),
    Interaction_PValue = signif(Interaction_PValue, 3),
    Trend_Comparison = ifelse(Interaction_PValue < 0.05, "Different trends", "Similar trends")
  ) %>%
  arrange(Interaction_PValue)

q3a_interaction_table %>%
  gt() %>%
  tab_header(
    title = "Q3(a): Interaction regression results",
    subtitle = "Comparison of time trends between CHN and Others"
  ) %>%
  cols_label(
    Variable = "Variable", Interaction_Coeff = "Interaction Coefficient",
    Interaction_PValue = "Interaction p-value", Trend_Comparison = "Trend Comparison"
  ) %>%
  fmt_number(columns = Interaction_Coeff, decimals = 4) %>%
  fmt_scientific(columns = Interaction_PValue, decimals = 3) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

q3a_interaction_gt <- q3a_interaction_table %>%
  gt() %>%
  tab_header(
    title = "Q3(a): Interaction regression results",
    subtitle = "Comparison of time trends between CHN and Others"
  ) %>%
  cols_label(
    Variable = "Variable", Interaction_Coeff = "Interaction Coefficient",
    Interaction_PValue = "Interaction p-value", Trend_Comparison = "Trend Comparison"
  ) %>%
  fmt_number(columns = Interaction_Coeff, decimals = 4) %>%
  fmt_scientific(columns = Interaction_PValue, decimals = 3) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

q3a_interaction_gt
gtsave(q3a_interaction_gt, "table8.png")

# ---- Figure 20: Interaction effects bar chart ----
bar_data <- q3a_interaction_results %>%
  mutate(Significant = ifelse(Interaction_PValue < 0.05, "p < 0.05", "p >= 0.05")) %>%
  arrange(Interaction_Coeff)

bar_data$Variable <- factor(bar_data$Variable, levels = bar_data$Variable)

ggplot(bar_data, aes(x = Interaction_Coeff, y = Variable, fill = Significant)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("p < 0.05" = "dodgerblue4", "p >= 0.05" = "grey70")) +
  labs(
    title = "Interaction effects for time trends",
    subtitle = "CHN vs Others",
    x = "Interaction coefficient (Wave × Group)", y = "Variable", fill = "Significance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ---- Figure 21: Wilcoxon comparisons at each wave (heatmap) ----
time_var <- "Wave"
focus_data$Group <- "CHN"
others_data$Group <- "Others"
combined_data <- rbind(focus_data, others_data)

q3a_wave_compare_results <- data.frame(
  Variable = character(), Wave = numeric(), PValue = numeric(),
  Mean_CHN = numeric(), Mean_Others = numeric(),
  stringsAsFactors = FALSE
)

for (v in all_q3a_vars) {
  for (w in sort(unique(combined_data[[time_var]]))) {
    test_df <- combined_data[combined_data[[time_var]] == w, c(v, "Group")]
    test_df <- test_df[complete.cases(test_df), ]

    if (nrow(test_df) > 1 && length(unique(test_df$Group)) == 2) {
      mean_chn <- mean(test_df[test_df$Group == "CHN", v], na.rm = TRUE)
      mean_others <- mean(test_df[test_df$Group == "Others", v], na.rm = TRUE)

      test_out <- try(wilcox.test(as.formula(paste(v, "~ Group")), data = test_df), silent = TRUE)

      if (!inherits(test_out, "try-error")) {
        q3a_wave_compare_results <- rbind(
          q3a_wave_compare_results,
          data.frame(Variable = v, Wave = w, PValue = test_out$p.value, Mean_CHN = mean_chn, Mean_Others = mean_others)
        )
      }
    }
  }
}

q3a_wave_compare_plot <- q3a_wave_compare_results %>%
  mutate(
    Difference = Mean_CHN - Mean_Others,
    Result = case_when(
      PValue >= 0.05 ~ "Not significant",
      PValue < 0.05 & Difference > 0 ~ "CHN > Others",
      PValue < 0.05 & Difference < 0 ~ "CHN < Others"
    )
  )

ggplot(q3a_wave_compare_plot, aes(x = factor(Wave), y = Variable, fill = Result)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("CHN > Others" = "dodgerblue4", "CHN < Others" = "firebrick3", "Not significant" = "grey80")
  ) +
  labs(
    title = "Differences between CHN and Others across waves",
    subtitle = "Wilcoxon test showing direction and significance of differences",
    x = "Wave", y = "Variable", fill = "Comparison"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold")
  )
