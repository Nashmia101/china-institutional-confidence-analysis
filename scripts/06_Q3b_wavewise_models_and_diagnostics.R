# ============================================================
# Q3(b): Standardised regression models per wave/outcome/group,
# predictor strength over time, predictor x time interactions,
# and residual/Q-Q diagnostic plots
# ============================================================

time_var <- "Wave"
confidence_vars <- c(
  "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
)
predictor_vars <- setdiff(names(VC_clean), c("Country", "Wave", "Year", confidence_vars))
waves <- sort(unique(VC_clean[[time_var]]))

q3b_std_models <- list()

q3b_std_model_results <- data.frame(
  Outcome = character(), Wave = numeric(), Group = character(),
  N = numeric(), R2 = numeric(), Adj_R2 = numeric(),
  stringsAsFactors = FALSE
)

q3b_std_coef_results <- data.frame(
  Outcome = character(), Wave = numeric(), Group = character(),
  Predictor = character(), Std_Coefficient = numeric(),
  stringsAsFactors = FALSE
)

# ---- Fit standardised regressions per outcome x wave x group ----
for (outcome in confidence_vars) {
  for (w in waves) {

    # CHN (Focus)
    focus_wave_df <- focus_data[focus_data[[time_var]] == w, c(outcome, predictor_vars)]
    focus_wave_df <- na.omit(focus_wave_df)

    if (nrow(focus_wave_df) > 2) {
      valid_preds_focus <- predictor_vars[sapply(focus_wave_df[predictor_vars], function(x) length(unique(x)) > 1)]

      if (length(valid_preds_focus) > 0) {
        focus_model_df <- focus_wave_df[, c(outcome, valid_preds_focus), drop = FALSE]
        focus_scaled <- as.data.frame(scale(focus_model_df))
        focus_scaled <- focus_scaled[, colSums(is.na(focus_scaled)) == 0, drop = FALSE]
        valid_preds_focus2 <- setdiff(names(focus_scaled), outcome)

        if (length(valid_preds_focus2) > 0 && nrow(focus_scaled) > length(valid_preds_focus2) + 1) {
          focus_formula <- as.formula(paste(outcome, "~", paste(valid_preds_focus2, collapse = " + ")))
          focus_model <- lm(focus_formula, data = focus_scaled)
          focus_summary <- summary(focus_model)

          q3b_std_models[[paste(outcome, w, "Focus", sep = "_")]] <- focus_model

          q3b_std_model_results <- rbind(
            q3b_std_model_results,
            data.frame(
              Outcome = outcome, Wave = w, Group = "Focus", N = nrow(focus_scaled),
              R2 = focus_summary$r.squared, Adj_R2 = focus_summary$adj.r.squared
            )
          )

          focus_coef_df <- data.frame(
            Predictor = rownames(focus_summary$coefficients),
            Std_Coefficient = focus_summary$coefficients[, 1],
            stringsAsFactors = FALSE
          )
          focus_coef_df <- focus_coef_df[focus_coef_df$Predictor != "(Intercept)", ]
          focus_coef_df$Outcome <- outcome
          focus_coef_df$Wave <- w
          focus_coef_df$Group <- "Focus"

          q3b_std_coef_results <- rbind(
            q3b_std_coef_results,
            focus_coef_df[, c("Outcome", "Wave", "Group", "Predictor", "Std_Coefficient")]
          )
        }
      }
    }

    # Others
    others_wave_df <- others_data[others_data[[time_var]] == w, c(outcome, predictor_vars)]
    others_wave_df <- na.omit(others_wave_df)

    if (nrow(others_wave_df) > 2) {
      valid_preds_others <- predictor_vars[sapply(others_wave_df[predictor_vars], function(x) length(unique(x)) > 1)]

      if (length(valid_preds_others) > 0) {
        others_model_df <- others_wave_df[, c(outcome, valid_preds_others), drop = FALSE]
        others_scaled <- as.data.frame(scale(others_model_df))
        others_scaled <- others_scaled[, colSums(is.na(others_scaled)) == 0, drop = FALSE]
        valid_preds_others2 <- setdiff(names(others_scaled), outcome)

        if (length(valid_preds_others2) > 0 && nrow(others_scaled) > length(valid_preds_others2) + 1) {
          others_formula <- as.formula(paste(outcome, "~", paste(valid_preds_others2, collapse = " + ")))
          others_model <- lm(others_formula, data = others_scaled)
          others_summary <- summary(others_model)

          q3b_std_models[[paste(outcome, w, "Others", sep = "_")]] <- others_model

          q3b_std_model_results <- rbind(
            q3b_std_model_results,
            data.frame(
              Outcome = outcome, Wave = w, Group = "Others", N = nrow(others_scaled),
              R2 = others_summary$r.squared, Adj_R2 = others_summary$adj.r.squared
            )
          )

          others_coef_df <- data.frame(
            Predictor = rownames(others_summary$coefficients),
            Std_Coefficient = others_summary$coefficients[, 1],
            stringsAsFactors = FALSE
          )
          others_coef_df <- others_coef_df[others_coef_df$Predictor != "(Intercept)", ]
          others_coef_df$Outcome <- outcome
          others_coef_df$Wave <- w
          others_coef_df$Group <- "Others"

          q3b_std_coef_results <- rbind(
            q3b_std_coef_results,
            others_coef_df[, c("Outcome", "Wave", "Group", "Predictor", "Std_Coefficient")]
          )
        }
      }
    }
  }
}

q3b_std_coef_results$Abs_Std_Coefficient <- abs(q3b_std_coef_results$Std_Coefficient)

q3b_std_top_predictors <- q3b_std_coef_results %>%
  group_by(Outcome, Wave, Group) %>%
  arrange(desc(Abs_Std_Coefficient), .by_group = TRUE) %>%
  mutate(Rank = row_number()) %>%
  ungroup()

# ---- Figure 22: Predictive performance (R²) over time ----
ggplot(q3b_std_model_results, aes(x = Wave, y = R2, color = Group, group = Group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ Outcome, scales = "free_y") +
  scale_color_manual(
    values = c("Focus" = "blue", "Others" = "red"),
    labels = c("Focus" = "CHN", "Others" = "Others")
  ) +
  labs(title = "Predictive performance (R²) over time", x = "Wave", y = "R²", color = "Group") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# ---- Figure 23: Predictors over time (scaled coefficients heatmap) ----
# {r, fig.width=12, fig.height=9}
heatmap_data <- q3b_std_coef_results

ggplot(heatmap_data, aes(x = factor(Wave), y = Predictor, fill = Abs_Std_Coefficient)) +
  geom_tile(color = "grey80", linewidth = 0.2) +
  facet_grid(Group ~ Outcome, scales = "free_y", space = "free_y") +
  scale_fill_gradientn(colours = c("plum3", "lightgreen", "yellow2"), name = "|Scaled coefficient|") +
  labs(
    title = "Predictors over time (scaled coefficients)",
    subtitle = "CHN vs Others",
    x = "Wave", y = "Predictor"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "bold", size = 11)
  )

# ---- Figure 24: Strength of key predictors over time (CArmedForces, CEnvProt) ----
# {r, fig.width=12, fig.height=6}
chosen_outcomes <- c("CEnvProt", "CArmedForces")

line_data <- q3b_std_coef_results %>%
  mutate(Group = ifelse(Group == "Focus", "CHN", Group)) %>%
  filter(Outcome %in% chosen_outcomes)

top_preds <- line_data %>%
  group_by(Outcome, Predictor) %>%
  summarise(MaxAbs = max(Abs_Std_Coefficient, na.rm = TRUE), .groups = "drop") %>%
  group_by(Outcome) %>%
  arrange(desc(MaxAbs), .by_group = TRUE) %>%
  slice_head(n = 8) %>%
  ungroup()

plot_data <- line_data %>%
  semi_join(top_preds, by = c("Outcome", "Predictor"))

ggplot(
  plot_data,
  aes(x = Wave, y = Abs_Std_Coefficient, color = Predictor, linetype = Group, group = interaction(Predictor, Group))
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
  labs(
    title = "Strength of key predictors over time (|β|)",
    subtitle = "CHN vs Others",
    x = "Wave", y = "|β| (importance)", color = "Predictor", linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "bold", size = 11)
  )

# ---- Figure 25: Predictor x time interaction models (CHN vs Others) ----
q3b_time_interaction_results <- data.frame(
  Outcome = character(), Group = character(), Predictor = character(),
  Interaction_Coefficient = numeric(), Interaction_PValue = numeric(),
  stringsAsFactors = FALSE
)

for (outcome in confidence_vars) {

  # CHN
  focus_df <- focus_data[, c(outcome, time_var, predictor_vars)]
  focus_df <- na.omit(focus_df)

  if (nrow(focus_df) > 2) {
    valid_preds_focus <- predictor_vars[sapply(focus_df[predictor_vars], function(x) length(unique(x)) > 1)]

    if (length(valid_preds_focus) > 0) {
      focus_formula <- as.formula(
        paste(
          outcome, "~",
          paste(valid_preds_focus, collapse = " + "), "+",
          time_var, "+",
          paste(paste0(valid_preds_focus, ":", time_var), collapse = " + ")
        )
      )
      focus_model <- lm(focus_formula, data = focus_df)
      focus_coef <- summary(focus_model)$coefficients

      for (pred in valid_preds_focus) {
        term1 <- paste0(pred, ":", time_var)
        term2 <- paste0(time_var, ":", pred)
        row_idx <- which(rownames(focus_coef) %in% c(term1, term2))

        if (length(row_idx) == 1) {
          q3b_time_interaction_results <- rbind(
            q3b_time_interaction_results,
            data.frame(
              Outcome = outcome, Group = "CHN", Predictor = pred,
              Interaction_Coefficient = focus_coef[row_idx, 1],
              Interaction_PValue = focus_coef[row_idx, 4]
            )
          )
        }
      }
    }
  }

  # Others
  others_df <- others_data[, c(outcome, time_var, predictor_vars)]
  others_df <- na.omit(others_df)

  if (nrow(others_df) > 2) {
    valid_preds_others <- predictor_vars[sapply(others_df[predictor_vars], function(x) length(unique(x)) > 1)]

    if (length(valid_preds_others) > 0) {
      others_formula <- as.formula(
        paste(
          outcome, "~",
          paste(valid_preds_others, collapse = " + "), "+",
          time_var, "+",
          paste(paste0(valid_preds_others, ":", time_var), collapse = " + ")
        )
      )
      others_model <- lm(others_formula, data = others_df)
      others_coef <- summary(others_model)$coefficients

      for (pred in valid_preds_others) {
        term1 <- paste0(pred, ":", time_var)
        term2 <- paste0(time_var, ":", pred)
        row_idx <- which(rownames(others_coef) %in% c(term1, term2))

        if (length(row_idx) == 1) {
          q3b_time_interaction_results <- rbind(
            q3b_time_interaction_results,
            data.frame(
              Outcome = outcome, Group = "Others", Predictor = pred,
              Interaction_Coefficient = others_coef[row_idx, 1],
              Interaction_PValue = others_coef[row_idx, 4]
            )
          )
        }
      }
    }
  }
}

q3b_time_interaction_results

# ---- Figure 26: Diagnostic plots for selected Wave-6 models ----
selected_model_names <- c(
  "CArmedForces_6_Focus", "CArmedForces_6_Others",
  "CEnvProt_6_Focus", "CEnvProt_6_Others"
)
selected_model_names <- selected_model_names[selected_model_names %in% names(q3b_std_models)]

par(mfrow = c(2, 2))
for (m in selected_model_names) {
  plot(q3b_std_models[[m]], main = m)
}
