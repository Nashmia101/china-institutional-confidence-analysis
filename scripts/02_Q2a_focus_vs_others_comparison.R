# ============================================================
# Q2(a): Focus country (CHN) vs Others — summary stats,
# correlation, boxplots, Wilcoxon rank-sum tests
# ============================================================

# ---- Extract focus country data ----
focus_data <- VC_clean[VC_clean$Country == "CHN", ]
others_data <- VC_clean[VC_clean$Country != "CHN", ]

nrow(focus_data)
nrow(others_data)
table(VC_clean$Country == "CHN")

all_attrs <- names(VC_clean)[
  !(names(VC_clean) %in% c("Wave", "Country", "Year"))
]

focus_attr <- focus_data[, all_attrs]
others_attr <- others_data[, all_attrs]

length(all_attrs)
all_attrs

# ---- Table 3: Summary of attributes for focus vs others ----
# {r, fig.width=18, fig.height=10}
focus_sum <- data.frame(
  f_min = sapply(focus_attr, function(x) min(x, na.rm = TRUE)),
  f_mean = sapply(focus_attr, function(x) mean(x, na.rm = TRUE)),
  f_med = sapply(focus_attr, function(x) median(x, na.rm = TRUE)),
  f_sd = sapply(focus_attr, function(x) sd(x, na.rm = TRUE)),
  f_max = sapply(focus_attr, function(x) max(x, na.rm = TRUE))
)

others_sum <- data.frame(
  o_min = sapply(others_attr, function(x) min(x, na.rm = TRUE)),
  o_mean = sapply(others_attr, function(x) mean(x, na.rm = TRUE)),
  o_med = sapply(others_attr, function(x) median(x, na.rm = TRUE)),
  o_sd = sapply(others_attr, function(x) sd(x, na.rm = TRUE)),
  o_max = sapply(others_attr, function(x) max(x, na.rm = TRUE))
)

q2a_summary <- data.frame(
  Variable = all_attrs,
  focus_sum,
  others_sum,
  check.names = FALSE
)

q2a_summary_gt <- q2a_summary |>
  gt() |>
  fmt_number(columns = -Variable, decimals = 2) |>
  tab_spanner(
    label = "Focus (CHN)",
    columns = c(f_min, f_mean, f_med, f_sd, f_max)
  ) |>
  tab_spanner(
    label = "Others",
    columns = c(o_min, o_mean, o_med, o_sd, o_max)
  ) |>
  cols_label(
    Variable = "Variable",
    f_min = "Min", f_mean = "Mean", f_med = "Median", f_sd = "SD", f_max = "Max",
    o_min = "Min", o_mean = "Mean", o_med = "Median", o_sd = "SD", o_max = "Max"
  ) |>
  tab_header(
    title = md("**Table 3. Summary of attributes for the focus country and other countries**")
  ) |>
  cols_align(align = "center", columns = -Variable) |>
  cols_align(align = "left", columns = Variable)

q2a_summary_gt
gtsave(q2a_summary_gt, "table3.png")

# ---- Correlation matrix of all attributes ----
cor_mat <- cor(VC_clean[, all_attrs], use = "pairwise.complete.obs")
cor_df <- as.data.frame(as.table(cor_mat))

strong_cor <- cor_df %>%
  filter(Var1 != Var2, abs(Freq) >= 0.5) %>%
  arrange(desc(abs(Freq)))
strong_cor

# ---- Figure 5: Correlation matrix heatmap of attributes ----
# {r, fig.width=10, fig.height=7}
cor_df_plot <- as.data.frame(as.table(cor_mat))

ggplot(cor_df_plot, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlation matrix of attributes",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 8)
  )

# ---- Figure 6: Boxplots of all attributes by scale group ----
all_var_info <- data.frame(
  Variable = c(
    "ILFam", "ILFriends", "ILLeisure",
    "ICQHardWork", "ICQResonsibility", "ICQImagination", "ICQTolerance",
    "ICQDetermination", "ICQFaith", "ICQObedience",
    "ACTReligion", "ACTUnions", "ACTEnvOrg", "ACTHumanitarian",
    "Happy", "LifeSatis", "FutureRespect", "IncomeEquality", "HardWork",
    "PolPetition", "PolDemons", "PolLeader", "PolExperts", "PolDemoc",
    "Sex",
    "CArmedForces", "CPress", "CCivilService", "CPolParties",
    "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
  ),
  ScaleGroup = c(
    rep("1-4 scale", 3),
    rep("0-1 scale", 7),
    rep("0-2 scale", 4),
    "1-4 scale",
    "1-10 scale",
    "1-3 scale",
    "1-10 scale",
    "1-10 scale",
    "1-3 scale", "1-3 scale",
    "1-4 scale", "1-4 scale", "1-4 scale",
    "1-2 scale",
    rep("1-4 scale", 8)
  ),
  stringsAsFactors = FALSE
)

focus_plot <- focus_data[, all_var_info$Variable]
focus_plot$Group <- "CHN"

others_plot <- others_data[, all_var_info$Variable]
others_plot$Group <- "Others"

plot_df <- rbind(focus_plot, others_plot) %>%
  pivot_longer(
    cols = -Group,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value), is.finite(Value)) %>%
  left_join(all_var_info, by = "Variable")

make_scale_boxplot <- function(scale_name) {
  ggplot(
    subset(plot_df, ScaleGroup == scale_name),
    aes(x = Variable, y = Value, fill = Group)
  ) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      outlier.alpha = 0.25
    ) +
    labs(
      title = paste("CHN vs Others:", scale_name),
      x = "Variable",
      y = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 8),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(face = "bold", size = 9)
    )
}

p01 <- make_scale_boxplot("0-1 scale")
p02 <- make_scale_boxplot("0-2 scale")
p12 <- make_scale_boxplot("1-2 scale")
p13 <- make_scale_boxplot("1-3 scale")
p14 <- make_scale_boxplot("1-4 scale")
p110 <- make_scale_boxplot("1-10 scale")

p01
p02
p12
p13
p14
p110

# ---- Table 4: Wilcoxon rank-sum test for all attributes ----
all_wilcox_results <- data.frame(
  Variable = character(),
  W = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (v in all_attrs) {
  x <- na.omit(focus_attr[[v]])
  y <- na.omit(others_attr[[v]])
  test <- wilcox.test(x, y, exact = FALSE)
  all_wilcox_results <- rbind(
    all_wilcox_results,
    data.frame(
      Variable = v,
      W = as.numeric(test$statistic),
      P_Value = test$p.value
    )
  )
}

all_wilcox_results$P_Value <- signif(all_wilcox_results$P_Value, 4)
all_wilcox_results$Significant <- ifelse(all_wilcox_results$P_Value < 0.05, "Yes", "No")

all_wilcox_results_gt <- all_wilcox_results |>
  gt() |>
  cols_label(
    Variable = "Variable",
    W = "W Statistic",
    P_Value = "p-value",
    Significant = "Significant"
  ) |>
  fmt_number(columns = W, decimals = 2) |>
  tab_header(
    title = "Wilcoxon rank-sum test results for all attributes"
  )

all_wilcox_results_gt
