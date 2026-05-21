# Project dependency installer/checker
#
# This file checks whether the required R packages are installed and whether
# their versions meet the minimum recommended versions for this analysis.
#
# Usage:
#   source("requirements.R")
#   check_project_requirements()
#
# To install missing or outdated packages automatically:
#   check_project_requirements(install_missing = TRUE, update_outdated = TRUE)

required_packages <- data.frame(
  package = c(
    "readxl",
    "dplyr",
    "data.table",
    "tidyr",
    "stringr",
    "purrr",
    "rlang",
    "MatchIt",
    "tableone",
    "openxlsx",
    "lme4",
    "lmerTest",
    "emmeans",
    "broom.mixed",
    "ggplot2",
    "patchwork",
    "sjPlot",
    "performance"
  ),
  min_version = c(
    "1.4.3",
    "1.1.4",
    "1.15.4",
    "1.3.1",
    "1.5.1",
    "1.0.2",
    "1.1.3",
    "4.5.5",
    "0.13.2",
    "4.2.5",
    "1.1-35",
    "3.1-3",
    "1.10.0",
    "0.2.9.5",
    "3.5.0",
    "1.2.0",
    "2.8.15",
    "0.12.3"
  ),
  source = "CRAN",
  stringsAsFactors = FALSE
)

optional_packages <- data.frame(
  package = c(
    "easytable.WSN",
    "renv"
  ),
  min_version = c(
    NA,
    "1.0.0"
  ),
  source = c(
    "Internal/custom package used by the original script; optional fallback is provided by tableone.",
    "CRAN; recommended for reproducible package locking."
  ),
  stringsAsFactors = FALSE
)

check_one_package <- function(package, min_version = NA_character_) {
  installed <- requireNamespace(package, quietly = TRUE)

  if (!installed) {
    return(data.frame(
      package = package,
      required_version = min_version,
      installed_version = NA_character_,
      status = "not installed",
      stringsAsFactors = FALSE
    ))
  }

  installed_version <- as.character(utils::packageVersion(package))

  if (is.na(min_version) || is.null(min_version) || min_version == "") {
    status <- "installed"
  } else {
    status <- if (
      utils::compareVersion(installed_version, min_version) >= 0
    ) {
      "ok"
    } else {
      "outdated"
    }
  }

  data.frame(
    package = package,
    required_version = min_version,
    installed_version = installed_version,
    status = status,
    stringsAsFactors = FALSE
  )
}

check_project_requirements <- function(install_missing = FALSE,
                                       update_outdated = FALSE,
                                       include_optional = TRUE,
                                       repos = "https://cloud.r-project.org") {
  required_status <- do.call(
    rbind,
    Map(
      check_one_package,
      required_packages$package,
      required_packages$min_version
    )
  )

  optional_status <- do.call(
    rbind,
    Map(
      check_one_package,
      optional_packages$package,
      optional_packages$min_version
    )
  )

  if (install_missing || update_outdated) {
    to_install <- required_status$package[
      required_status$status == "not installed" |
        (update_outdated & required_status$status == "outdated")
    ]

    if (length(to_install) > 0) {
      message("Installing/updating required packages: ", paste(to_install, collapse = ", "))
      utils::install.packages(to_install, repos = repos)
    }
  }

  message("\nRequired packages:")
  print(required_status, row.names = FALSE)

  if (include_optional) {
    message("\nOptional packages:")
    print(optional_status, row.names = FALSE)
  }

  invisible(list(
    required = required_status,
    optional = optional_status
  ))
}

write_session_info <- function(output_file = "session_info.txt") {
  sink(output_file)
  on.exit(sink(), add = TRUE)

  cat("R session information\n")
  cat("=====================\n\n")
  print(sessionInfo())

  cat("\n\nRequired package versions\n")
  cat("=========================\n\n")
  status <- check_project_requirements(
    install_missing = FALSE,
    update_outdated = FALSE,
    include_optional = TRUE
  )
  print(status$required, row.names = FALSE)
  print(status$optional, row.names = FALSE)

  invisible(output_file)
}

# Run a passive check when this file is sourced interactively.
if (interactive()) {
  check_project_requirements(
    install_missing = FALSE,
    update_outdated = FALSE,
    include_optional = TRUE
  )
}
