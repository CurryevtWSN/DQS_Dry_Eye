# Diquafosol Sodium Dry Eye Treatment Analysis

This repository script organizes the original dry-eye treatment analysis workflow into a GitHub-ready R script.

## Main workflow

The script performs:

1. Baseline descriptive analyses
2. Three-group and pairwise treatment comparisons
3. 1:2 propensity score matching
4. Post-PSM baseline balance tables
5. Extraction of matched longitudinal datasets
6. Linear mixed-effects / MMRM-style analyses for:
   - FBUT
   - Schirmer I test
   - DEQS
   - CFS
   - TMH
   - LLT
7. Least-squares mean plots, forest plots, ANOVA tables, and nested adjusted model tables

## Before running

Edit the following line in `dqs_dry_eye_analysis_github_clean.R`:

```r
PROJECT_DIR <- "/media1T/data/wsn/data_analysis/倩姐/DQS"
```

Set it to your local project root that contains:

```text
data/
res/
PSM/
PSM12data/
psm12res/
```

## Required R packages

The script checks required packages at startup. Install missing packages with:

```r
install.packages(c(
  "readxl", "dplyr", "data.table", "tidyr", "stringr", "purrr", "rlang",
  "MatchIt", "tableone", "openxlsx", "lme4", "lmerTest", "emmeans",
  "broom.mixed", "ggplot2", "patchwork", "sjPlot", "performance"
))
```


## Run

```bash
Rscript dqs_dry_eye_analysis_github_clean.R
```
