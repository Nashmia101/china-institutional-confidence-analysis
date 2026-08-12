# Social Organisation Predictability: China vs Global Trends

Statistical analysis of institutional confidence patterns in China compared to 57 other countries, using World Values Survey data spanning 1981–2023. Built in R using hypothesis testing, multiple regression, and time-trend modelling to examine how demographic, attitudinal, and civic-participation factors predict trust in institutions — and how those relationships shift over time.

## Overview

Using a 100,000-observation sample from the World Values Survey (36 variables, Waves 1–7, 58 countries), this project investigates:

1. **How China differs from the rest of the world** across values, civic participation, and institutional confidence (Wilcoxon rank-sum tests across 33 variables)
2. **What predicts institutional confidence** in China vs. other countries (multiple regression with backward elimination across 7–8 confidence outcomes)
3. **How these relationships evolve over time** (interaction regression and wave-by-wave modelling across Waves 3–7)

## Key findings

- China differs significantly from the global sample on 31 of 33 variables tested (Wilcoxon, α = 0.05) — spanning child-rearing values, civic participation, and institutional trust
- Institutional confidence in China is most strongly predicted by political attitudes (e.g. belief in democracy) and work ethic, while the global sample is more shaped by religious faith and gender
- Predictive power is modest across the board (R² = 0.06–0.10), indicating confidence is jointly driven by many weak predictors rather than one dominant factor
- Time-trend analysis shows 19 of 33 variables have significantly different trajectories in China vs. other countries, with the widest divergence in income equality attitudes and civic engagement

## Methodology

- **Data cleaning:** negative codes recoded as NA; binary/ordinal variables retained; missingness ranged from 0.64% to 75.39% by variable
- **Group comparison:** Wilcoxon rank-sum tests (non-parametric, due to skewed/compressed ordinal distributions)
- **Prediction:** multiple linear regression with backward elimination (p > 0.05 removal threshold), fit separately for China and the pooled "Others" group
- **Time trends:** Wave × Group interaction regression to test whether trends differ significantly between groups; standardised coefficients compared across individual waves
- **Diagnostics:** residuals-vs-fitted and Q-Q plots checked for representative models; tail departures from normality noted but expected given ordinal response data

## Tech stack

R · ggplot2 · dplyr/tidyr · gt (tables) · corrplot · ggh4x (faceting) · reshape2

## Repo structure

├── README.md
└── scripts/
    ├── 00_setup.R
    ├── 01_Q1_data_prep_and_overview.R
    ├── 02_Q2a_focus_vs_others_comparison.R
    ├── 03_Q2b_regression_china.R
    ├── 04_Q2c_regression_others.R
    ├── 05_Q3a_time_trends.R
    └── 06_Q3b_wavewise_models_and_diagnostics.R

## Running this

Scripts are numbered to run in sequence — later scripts depend on objects created earlier (focus_data, others_data, backward_elimination(), all_model_stats, etc.). Requires WVSExtract.csv in the working directory (not included; source: World Values Survey, worldvaluessurvey.org).
