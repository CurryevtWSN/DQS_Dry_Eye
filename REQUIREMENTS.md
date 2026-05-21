# Requirements

This project is an R-based clinical data analysis workflow.

## R version

| Software | Minimum version |
|---|---:|
| R | >= 4.3.0 |

## Required R packages

| Package | Minimum recommended version | Source | Main use |
|---|---:|---|---|
| readxl | >= 1.4.3 | CRAN | Read Excel workbooks |
| dplyr | >= 1.1.4 | CRAN | Data manipulation |
| data.table | >= 1.15.4 | CRAN | Fast CSV import/export |
| tidyr | >= 1.3.1 | CRAN | Missing data handling and data reshaping |
| stringr | >= 1.5.1 | CRAN | String handling |
| purrr | >= 1.0.2 | CRAN | Functional programming utilities |
| rlang | >= 1.1.3 | CRAN | Tidy evaluation support |
| MatchIt | >= 4.5.5 | CRAN | Propensity score matching |
| tableone | >= 0.13.2 | CRAN | Baseline table fallback |
| openxlsx | >= 4.2.5 | CRAN | Excel output |
| lme4 | >= 1.1-35 | CRAN | Linear mixed-effects models |
| lmerTest | >= 3.1-3 | CRAN | P values for mixed-effects models |
| emmeans | >= 1.10.0 | CRAN | Estimated marginal means / LSMeans |
| broom.mixed | >= 0.2.9.5 | CRAN | Tidy model output |
| ggplot2 | >= 3.5.0 | CRAN | Plotting |
| patchwork | >= 1.2.0 | CRAN | Figure composition |
| sjPlot | >= 2.8.15 | CRAN | Regression model summary tables |
| performance | >= 0.12.3 | CRAN | Model diagnostics |

## Optional packages

| Package | Minimum version | Source | Note |
|---|---:|---|---|
| easytable.WSN | Not fixed | Internal/custom | Used by the original script for Table 1; the cleaned script falls back to `tableone` if unavailable. |
| renv | >= 1.0.0 | CRAN | Recommended for exact reproducibility and lockfile generation. |

## Install/check dependencies

Run:

```r
source("requirements.R")
check_project_requirements(install_missing = TRUE, update_outdated = TRUE)
```

To export the actual package versions used in your local environment:

```r
source("requirements.R")
write_session_info("session_info.txt")
```

## Exact reproducibility with renv

For a GitHub project, the most reproducible approach is to generate an `renv.lock` file after confirming that the script runs successfully:

```r
install.packages("renv")
renv::init()
source("requirements.R")
check_project_requirements(install_missing = TRUE, update_outdated = TRUE)
renv::snapshot()
```

Then upload both `renv.lock` and the analysis script to GitHub.
