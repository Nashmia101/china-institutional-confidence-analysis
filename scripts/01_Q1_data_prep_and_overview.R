# ============================================================
# Q1: Data preprocessing, cleaning, and dataset overview
# ============================================================

# ---- Data preprocessing and cleaning ----
set.seed(34091904)
VCData <- read.csv("WVSExtract.csv")
VC <- VCData[sample(1:nrow(VCData), 100000, replace = FALSE), ]
VC <- VC[, c(1:3, sort(sample(4:50, 25, replace = FALSE)),
             sort(sample(51:65, 8, replace = FALSE)))]

VC_clean <- VC
num_cols <- sapply(VC_clean, is.numeric)

VC_clean[num_cols] <- lapply(
  VC_clean[num_cols],
  function(x) {
    x[x %in% c(-1, -2, -3, -4, -5)] <- NA
    x
  }
)

colSums(is.na(VC_clean))
round(colSums(is.na(VC_clean)) / nrow(VC_clean) * 100, 2)

lapply(VC_clean[, names(VC_clean) != "Country"], function(x) sort(unique(na.omit(x))))
sapply(VC_clean[, names(VC_clean) != "Country"], function(x) length(unique(na.omit(x))))

# ---- Table 1: Dataset overview ----
table1 <- data.frame(
  Feature = c(
    "Dataset",
    "Observations",
    "Variables",
    "Focus country",
    "Countries represented",
    "Wave coverage",
    "Year coverage",
    "Character variables",
    "Numeric variables",
    "Identifier variables",
    "Attribute predictor variables",
    "Confidence outcome variables",
    "Main predictor variables",
    "Main outcome variables"
  ),
  Value = c(
    "VC",
    format(nrow(VC), big.mark = ","),
    ncol(VC),
    "CHN",
    length(unique(VC$Country)),
    paste(min(VC$Wave, na.rm = TRUE), "to", max(VC$Wave, na.rm = TRUE)),
    paste(min(VC$Year, na.rm = TRUE), "to", max(VC$Year, na.rm = TRUE)),
    paste(sum(sapply(VC, is.character)), "(Country)"),
    sum(sapply(VC, is.numeric)),
    "3 (Wave, Country, Year)",
    25,
    sum(grepl("^C", names(VC)) & names(VC) != "Country"),
    "Important-in-life, child qualities, activities, wellbeing, political attitudes",
    "Confidence in institutions/organisations"
  ),
  stringsAsFactors = FALSE
)

table1 |>
  gt() |>
  tab_header(
    title = "Table 1. Dataset overview"
  )

write.csv(table1, "table1_dataset_overview.csv", row.names = FALSE)

# ---- Figure 1: Missing values by variable ----
missing_counts <- colSums(is.na(VC_clean))
missing_pct <- round(missing_counts / nrow(VC_clean) * 100, 2)

missing_df <- data.frame(
  Variable = names(missing_counts),
  Missing = as.integer(missing_counts),
  MissingPct = missing_pct,
  stringsAsFactors = FALSE
)

missing_df <- missing_df[!missing_df$Variable %in% c("Wave", "Country", "Year"), ]
missing_df <- missing_df[order(-missing_df$MissingPct), ]
missing_df$Variable <- factor(
  missing_df$Variable,
  levels = rev(missing_df$Variable)
)

missing_df$Label <- ""
missing_df$Label[which.max(missing_df$MissingPct)] <- paste0(
  missing_df$Variable[which.max(missing_df$MissingPct)],
  " (", missing_df$MissingPct[which.max(missing_df$MissingPct)], "%)"
)
missing_df$Label[which.min(missing_df$MissingPct)] <- paste0(
  missing_df$Variable[which.min(missing_df$MissingPct)],
  " (", missing_df$MissingPct[which.min(missing_df$MissingPct)], "%)"
)

ggplot(missing_df, aes(x = Variable, y = MissingPct)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(
    aes(label = Label),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Figure 1: Percentage of missing values by variable",
    x = "Variable",
    y = "Missing values (%)"
  ) +
  theme_minimal() +
  expand_limits(y = max(missing_df$MissingPct) + 10)

# ---- Table 2: Summary statistics by theme ----
vars_for_summary <- c(
  "ILFam", "ILFriends", "ILLeisure", "ICQHardWork", "ICQResonsibility",
  "ICQImagination", "ICQTolerance", "ICQDetermination", "ICQFaith", "ICQObedience",
  "ACTReligion", "ACTUnions", "ACTEnvOrg", "ACTHumanitarian", "Happy", "LifeSatis",
  "FutureRespect", "IncomeEquality", "HardWork", "PolPetition", "PolDemons", "PolLeader",
  "PolExperts", "PolDemoc", "Sex", "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
)

theme_group <- c(
  rep("Important in life", 3), rep("Child qualities", 7), rep("Activities", 4),
  rep("Wellbeing/attitudes", 5), rep("Political", 5), "Demographics", rep("Confidence", 8)
)

summary_table <- data.frame(
  Theme = theme_group,
  Variable = vars_for_summary,
  Min = sapply(VC_clean[vars_for_summary], function(x) min(x, na.rm = TRUE)),
  Q1 = sapply(VC_clean[vars_for_summary], function(x) quantile(x, 0.25, na.rm = TRUE)),
  Median = sapply(VC_clean[vars_for_summary], function(x) median(x, na.rm = TRUE)),
  Q3 = sapply(VC_clean[vars_for_summary], function(x) quantile(x, 0.75, na.rm = TRUE)),
  Mean = round(sapply(VC_clean[vars_for_summary], function(x) mean(x, na.rm = TRUE)), 2),
  SD = round(sapply(VC_clean[vars_for_summary], function(x) sd(x, na.rm = TRUE)), 2),
  Max = sapply(VC_clean[vars_for_summary], function(x) max(x, na.rm = TRUE)),
  stringsAsFactors = FALSE
)

summary_table |>
  gt(groupname_col = "Theme") |>
  tab_header(
    title = md("**Table 2. Summary statistics of predictor and outcome variables**")
  ) |>
  cols_label(
    Variable = "Variable", Min = "Min", Q1 = "Q1", Median = "Median",
    Q3 = "Q3", Mean = "Mean", SD = "SD", Max = "Max"
  ) |>
  fmt_number(
    columns = c(Min, Q1, Median, Q3, Mean, SD, Max), decimals = 2
  ) |>
  tab_options(
    table.width = pct(100), heading.align = "left", data_row.padding = px(4)
  )

# ---- Figure 2: Faceted bar plot of response distributions ----
# NOTE: `plot_vars` should be defined as the set of variables to plot
# (this was not explicitly re-declared in the original appendix before this block).
plot_long <- VC_clean[, plot_vars] %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  mutate(Variable = factor(Variable, levels = plot_vars))

final_plot <- ggplot(plot_long, aes(x = factor(Response))) +
  geom_bar(fill = "dodgerblue4") +
  facet_wrap(~Variable, scales = "free", ncol = 8) +
  labs(
    title = "Distribution of responses across all variables",
    x = "Response category",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 28),
    plot.title = element_text(face = "bold", size = 36),
    axis.text.x = element_text(face = "bold", size = 22),
    axis.text.y = element_text(face = "bold", size = 22),
    axis.title.x = element_text(face = "bold", size = 30),
    axis.title.y = element_text(face = "bold", size = 30)
  )

ggsave("distribution_plot.png", final_plot, width = 40, height = 35, dpi = 300)

# ---- Figure 3: Correlation matrix of confidence variables ----
conf_vars <- c(
  "CArmedForces", "CPress", "CCivilService", "CPolParties",
  "CMajComp", "CEnvProt", "CWomensMvt", "CEU"
)

conf_df <- VC_clean[, conf_vars]
conf_corr <- cor(conf_df, use = "pairwise.complete.obs")
round(conf_corr, 3)

corrplot(
  conf_corr,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,
  diag = TRUE,
  mar = c(0, 0, 2, 0),
  title = "Pairwise correlation matrix of confidence variables"
)

# ---- Figure 4: Sample distribution by country and year ----
# {r, fig.width=18, fig.height=10}
library(dplyr)
library(ggplot2)

country_year_counts <- VC %>%
  count(Country, Year)

country_order <- VC %>%
  count(Country, sort = TRUE) %>%
  arrange(n) %>%
  pull(Country)

country_year_counts$Country <- factor(country_year_counts$Country, levels = country_order)

ggplot(country_year_counts, aes(x = Country, y = n, fill = factor(Year))) +
  geom_col() +
  labs(
    title = "Sample distribution by country and year",
    x = "Country",
    y = "Count",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5, hjust = 1, size = 11),
    axis.text.y = element_text(face = "bold", size = 11),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )
