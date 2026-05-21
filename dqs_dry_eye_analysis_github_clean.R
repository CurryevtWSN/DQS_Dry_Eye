# Diquafosol Sodium Dry Eye Treatment Analysis
# GitHub-ready R script
#
# Purpose:
#   1. Generate baseline descriptive tables.
#   2. Compare DQS monotherapy, DQS + artificial tears, and DQS + anti-inflammatory therapy.
#   3. Perform 1:2 propensity score matching.
#   4. Extract matched longitudinal datasets.
#   5. Fit linear mixed-effects models / MMRM-style models for FBUT, Schirmer, DEQS, CFS, TMH, and LLT.
#   6. Export publication-ready figures and model summary tables.
#
# Notes:
#   - All comments, function names, and output file names are written in English for GitHub.
#   - Some source column names remain in Chinese because they must match the original Excel files.
#   - Set PROJECT_DIR before running the script.
#   - The original script used easytable.WSN::table_1(). This script keeps compatibility with
#     easytable.WSN and provides a fallback based on tableone + openxlsx.

# =============================================================================
# 0. Package loading
# =============================================================================

required_packages <- c(
  "readxl", "dplyr", "data.table", "tidyr", "stringr", "purrr", "rlang",
  "MatchIt", "tableone", "openxlsx", "lme4", "lmerTest", "emmeans",
  "broom.mixed", "ggplot2", "patchwork", "sjPlot", "performance"
)

load_or_stop <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Please install the following packages before running this script:\n",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

load_or_stop(required_packages)

has_easytable <- requireNamespace("easytable.WSN", quietly = TRUE)

# =============================================================================
# 1. Project configuration
# =============================================================================

PROJECT_DIR <- "/media1T/data/wsn/data_analysis/倩姐/DQS"
setwd(PROJECT_DIR)

PATHS <- list(
  raw_workbook = file.path("data", "副本地夸磷索钠数据原始数据-精简分析用-全集.xlsx"),
  cleaned_workbook = file.path("data", "LJY数据清洗.xlsx"),
  baseline_medication_review = file.path("res", "基线状态是否有合并用药01.xlsx"),
  specified_center_workbook = file.path("data", "指定中心数据 0407.xlsx"),
  overall_model_workbook = file.path("data", "总体混合效应模型数据.xlsx"),
  result_dir = "res",
  psm_dir = "PSM",
  psm_longitudinal_dir = "PSM12data",
  psm_result_dir = "psm12res",
  data_dir = "data"
)

dir.create(PATHS$result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(PATHS$psm_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(PATHS$psm_longitudinal_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(PATHS$psm_result_dir, recursive = TRUE, showWarnings = FALSE)

SOURCE_COLUMNS <- list(
  subject_id = "USUBJID",
  site_id = "SITEID",
  visit = "VISIT",
  visit_number = "访视编号",
  group = "分组",
  baseline_visit = "筛选/基线期（第0天）",
  severity_code = "严重程度 (研究者判断)(Code)",
  severity = "严重程度",
  artificial_tears_flag = "DQS联和角膜修复剂/玻璃酸钠",
  anti_inflammatory_flag = "DQS联和免疫相关药物/抗炎药物",
  specified_center_flag = "是否为指定中心病例",
  yes = "是",
  monotherapy_source = "单独用药",
  monotherapy = "单独",
  artificial_tears = "人工泪液",
  anti_inflammatory = "免疫"
)

GROUP_LABELS <- list(
  monotherapy_vs_artificial_tears = c(
    "单独" = "DQS Monotherapy",
    "人工泪液" = "DQS + Artificial Tears"
  ),
  monotherapy_vs_anti_inflammatory = c(
    "单独" = "DQS Monotherapy",
    "免疫" = "DQS + Anti-inflammatory"
  ),
  artificial_tears_vs_anti_inflammatory = c(
    "人工泪液" = "DQS + Artificial Tears",
    "免疫" = "DQS + Anti-inflammatory"
  )
)

COMPARISON_SPECS <- list(
  monotherapy_vs_artificial_tears = list(
    short_id = "A",
    treatment_group = SOURCE_COLUMNS$artificial_tears,
    control_group = SOURCE_COLUMNS$monotherapy,
    file_name = "matched_longitudinal_monotherapy_vs_artificial_tears.csv",
    psm_file_name = "psm_monotherapy_vs_artificial_tears.csv",
    figure_title = "Monotherapy vs. Artificial Tears",
    forest_title = "Difference: Monotherapy - Artificial Tears",
    labels = GROUP_LABELS$monotherapy_vs_artificial_tears
  ),
  monotherapy_vs_anti_inflammatory = list(
    short_id = "B",
    treatment_group = SOURCE_COLUMNS$anti_inflammatory,
    control_group = SOURCE_COLUMNS$monotherapy,
    file_name = "matched_longitudinal_monotherapy_vs_anti_inflammatory.csv",
    psm_file_name = "psm_monotherapy_vs_anti_inflammatory.csv",
    figure_title = "Monotherapy vs. Anti-inflammatory",
    forest_title = "Difference: Monotherapy - Anti-inflammatory",
    labels = GROUP_LABELS$monotherapy_vs_anti_inflammatory
  ),
  artificial_tears_vs_anti_inflammatory = list(
    short_id = "C",
    treatment_group = SOURCE_COLUMNS$anti_inflammatory,
    control_group = SOURCE_COLUMNS$artificial_tears,
    file_name = "matched_longitudinal_artificial_tears_vs_anti_inflammatory.csv",
    psm_file_name = "psm_artificial_tears_vs_anti_inflammatory.csv",
    figure_title = "Artificial Tears vs. Anti-inflammatory",
    forest_title = "Difference: Artificial Tears - Anti-inflammatory",
    labels = GROUP_LABELS$artificial_tears_vs_anti_inflammatory
  )
)

CLINICAL_RENAME_MAP <- c(
  SubjectID = "USUBJID",
  SiteID = "SITEID",
  Gender = "性别",
  Age = "年龄",
  BMI = "BMI",
  MedicalHistory = "受试者是否有其他疾病史",
  EyeSurgeryHistory = "受试者是否有眼部手术史",
  MGD_DryEye = "脂质异常型（MGD）干眼",
  VDT_DryEye = "视频显示终端（VDT）干眼",
  SurgeryRelated_DryEye = "手术相关性干眼",
  SjogrenRelated_DryEye = "干燥综合征相关干眼",
  DiabetesRelated_DryEye = "糖尿病相关干眼",
  DrugRelated_DryEye = "药物相关性干眼",
  ContactLensRelated_DryEye = "角膜接触镜相关干眼",
  OtherRiskFactor_DryEye = "其他危险因素类型干眼",
  AqueousDeficient_DryEye = "水液缺乏型干眼",
  LipidAbnormal_DryEye = "脂质异常型干眼",
  MucinAbnormal_DryEye = "黏蛋白异常型干眼",
  TearDynamics_DryEye = "泪液动力学异常型干眼",
  Mixed_DryEye = "混合型干眼",
  Mixed_DryEye_Full = "混合型干眼(如勾选此项，请同步勾选上述致病因素)",
  Severity = "严重程度",
  SeverityCode = "严重程度 (研究者判断)(Code)",
  CFS_Code = "CFS(Code)",
  IsSpecifiedCase = "是否为指定病例",
  Baseline_FBUT_5S = "基线FBUT时间_5S",
  Baseline_FBUT = "基线FBUT时间",
  Baseline_Schirmer_I = "基线SchirmerⅠ试验（无麻醉）",
  Baseline_Schirmer_I_5mm = "基线SchirmerⅠ试验（无麻醉）≤5mm与＞5",
  Baseline_Schirmer_I_10mm = "基线SchirmerⅠ试验（无麻醉）≤10与＞10",
  Fluorescein_TotalScore = "荧光素染色泪膜破裂总分",
  DEQS_TotalScore = "DEQS问卷评分总分",
  PRK_History = "是否有屈光性角膜切削术",
  Keratomileusis_History = "是否有角膜磨削术",
  Keratectomy_History = "是否有角膜切削术",
  RK_History = "是否有放射状角膜切开术"
)

DROP_COLUMNS <- c(
  "SITEID", "是，研究眼为", "仅DQS",
  "DQS联和角膜修复剂/玻璃酸钠",
  "DQS联和免疫相关药物/抗炎药物",
  "分组", "VISIT", "是否继续使用研究药物",
  "有无治疗方式", "是否为指定中心病例", "用药频率组",
  "受试者是否完成整个研究",
  "是否使用\"\"玻璃酸钠\"\" \"\"聚乙二醇\"\" \"\"卡波姆\"\" \"\"聚乙烯醇\"\" \"\"右旋糖酐\"\" \"\"羟丙甲纤维素\"\"",
  "是否使用氟米龙",
  "是否使用\"\"普拉洛芬\"\" \"\"溴芬酸钠\"\" \"\"双氯芬酸钠\"\"",
  "是否使用\"\"牛血清碱性成纤维细胞生长因子\"\" \"\"人表皮生长因子, 重组\"\" \"\"血，牛，去蛋白、低分子量组分\"\"",
  "是否使用\"\"热敷\"\" \"\"雾化\"\" \"\"熏蒸\"\"",
  "是否使用\"\"睑板腺按摩\"\"", "是否使用强脉冲光", "是否使用环孢素"
)

FONT_FAMILY <- "Times New Roman"

# =============================================================================
# 2. General helper functions
# =============================================================================

safe_cols_by_index <- function(data, idx) {
  idx <- idx[idx >= 1 & idx <= ncol(data)]
  names(data)[idx]
}

safe_rename <- function(data, mapping) {
  mapping <- mapping[unname(mapping) %in% names(data)]
  if (length(mapping) == 0) return(data)

  old_names <- unname(mapping)
  new_names <- names(mapping)
  dplyr::rename(data, !!!rlang::set_names(rlang::syms(old_names), new_names))
}

write_csv_safe <- function(data, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(data, file = file)
}

write_xlsx_safe <- function(data, file, row_names = FALSE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(data, file = file, rowNames = row_names)
}

mutate_id_columns_as_character <- function(data, include_site = TRUE) {
  id_cols <- c(SOURCE_COLUMNS$subject_id, SOURCE_COLUMNS$visit)
  if (include_site) id_cols <- c(id_cols, SOURCE_COLUMNS$site_id)
  id_cols <- intersect(id_cols, names(data))

  data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(id_cols), as.character))
}

format_p_value <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

get_ci_columns <- function(data) {
  lower_col <- names(data)[grepl("lower|LCL", names(data), ignore.case = TRUE)][1]
  upper_col <- names(data)[grepl("upper|UCL", names(data), ignore.case = TRUE)][1]

  if (is.na(lower_col) || is.na(upper_col)) {
    stop("Confidence interval columns were not found.", call. = FALSE)
  }

  list(lower = lower_col, upper = upper_col)
}

filter_existing_model_vars <- function(data, vars) {
  vars <- intersect(vars, names(data))
  vars[vapply(vars, function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    if (length(x) == 0) return(FALSE)
    length(unique(x)) >= 2
  }, logical(1))]
}

# =============================================================================
# 3. Table 1 helper
# =============================================================================

run_table1 <- function(data,
                       continuous_vars,
                       categorical_vars,
                       group_var,
                       output_file,
                       digits = 2,
                       direction = "v") {
  continuous_vars <- intersect(continuous_vars, names(data))
  categorical_vars <- intersect(categorical_vars, names(data))

  if (!group_var %in% names(data)) {
    warning("The group variable was not found: ", group_var)
    return(invisible(NULL))
  }

  if (length(continuous_vars) == 0 && length(categorical_vars) == 0) {
    warning("No valid variables were found for Table 1: ", output_file)
    return(invisible(NULL))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  if (has_easytable) {
    easytable.WSN::table_1(
      data = data,
      lx.z = continuous_vars,
      lx.fz = NULL,
      fl = categorical_vars,
      direction = direction,
      by = group_var,
      showOrder = NULL,
      time = NULL,
      y = NULL,
      adjust = NULL,
      xlsx = output_file,
      round = digits
    )
  } else {
    factor_vars <- categorical_vars
    vars <- c(continuous_vars, categorical_vars)

    table_one <- tableone::CreateTableOne(
      vars = vars,
      strata = group_var,
      data = data,
      factorVars = factor_vars,
      test = TRUE
    )

    table_df <- as.data.frame(
      print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
    )

    write_xlsx_safe(table_df, output_file, row_names = TRUE)
  }

  invisible(output_file)
}

run_table1_two_digits <- function(data,
                                  continuous_vars,
                                  categorical_vars,
                                  group_var,
                                  output_prefix) {
  run_table1(
    data = data,
    continuous_vars = continuous_vars,
    categorical_vars = categorical_vars,
    group_var = group_var,
    output_file = paste0(output_prefix, "_2_digits.xlsx"),
    digits = 2
  )

  run_table1(
    data = data,
    continuous_vars = continuous_vars,
    categorical_vars = categorical_vars,
    group_var = group_var,
    output_file = paste0(output_prefix, "_3_digits.xlsx"),
    digits = 3
  )
}

# =============================================================================
# 4. Baseline descriptive analyses
# =============================================================================

prepare_baseline_medication_data <- function() {
  prescription_data <- readxl::read_excel(PATHS$raw_workbook, sheet = "医生处方")

  baseline_data <- prescription_data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$visit]] %in% SOURCE_COLUMNS$baseline_visit) %>%
    dplyr::mutate(CombinedMedication = "No")

  output_file <- file.path(PATHS$result_dir, "baseline_combination_medication.csv")
  write_csv_safe(baseline_data, output_file)

  baseline_data
}

run_baseline_medication_tables <- function() {
  if (file.exists(PATHS$baseline_medication_review)) {
    data <- readxl::read_excel(PATHS$baseline_medication_review)
  } else {
    data <- prepare_baseline_medication_data()
  }

  continuous_vars <- safe_cols_by_index(data, c(5:9, 31))
  categorical_vars <- safe_cols_by_index(data, c(3:4, 10:30, 33:49))

  run_table1_two_digits(
    data = data,
    continuous_vars = continuous_vars,
    categorical_vars = categorical_vars,
    group_var = SOURCE_COLUMNS$group,
    output_prefix = file.path(PATHS$result_dir, "baseline_combination_medication")
  )

  severity_col <- SOURCE_COLUMNS$severity_code
  severity_levels <- list(mild = 1, moderate = 2, severe = 3)

  for (severity_name in names(severity_levels)) {
    if (!severity_col %in% names(data)) next

    severity_data <- data %>%
      dplyr::filter(.data[[severity_col]] == severity_levels[[severity_name]])

    run_table1_two_digits(
      data = severity_data,
      continuous_vars = continuous_vars,
      categorical_vars = safe_cols_by_index(data, c(3:4, 10:24, 27:30, 33:49)),
      group_var = SOURCE_COLUMNS$group,
      output_prefix = file.path(PATHS$result_dir, paste0("baseline_", severity_name, "_dry_eye"))
    )
  }

  invisible(data)
}

load_cleaned_baseline_data <- function() {
  readxl::read_excel(PATHS$cleaned_workbook) %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$visit]] %in% SOURCE_COLUMNS$baseline_visit)
}

make_three_group_baseline_data <- function(data) {
  monotherapy <- data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$group]] %in% SOURCE_COLUMNS$monotherapy_source) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$monotherapy)

  artificial_tears <- data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$artificial_tears_flag]] %in% SOURCE_COLUMNS$yes) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$artificial_tears)

  anti_inflammatory <- data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$anti_inflammatory_flag]] %in% SOURCE_COLUMNS$yes) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$anti_inflammatory)

  dplyr::bind_rows(monotherapy, artificial_tears, anti_inflammatory)
}

make_pairwise_baseline_data <- function(three_group_data) {
  list(
    monotherapy_vs_artificial_tears = three_group_data %>%
      dplyr::filter(Group %in% c(SOURCE_COLUMNS$monotherapy, SOURCE_COLUMNS$artificial_tears)),
    monotherapy_vs_anti_inflammatory = three_group_data %>%
      dplyr::filter(Group %in% c(SOURCE_COLUMNS$monotherapy, SOURCE_COLUMNS$anti_inflammatory)),
    artificial_tears_vs_anti_inflammatory = three_group_data %>%
      dplyr::filter(Group %in% c(SOURCE_COLUMNS$artificial_tears, SOURCE_COLUMNS$anti_inflammatory))
  )
}

run_baseline_treatment_group_tables <- function() {
  data <- load_cleaned_baseline_data()

  run_table1_two_digits(
    data = data,
    continuous_vars = safe_cols_by_index(data, 3:8),
    categorical_vars = safe_cols_by_index(data, c(9:47, 51:54)),
    group_var = SOURCE_COLUMNS$group,
    output_prefix = file.path(PATHS$result_dir, "monotherapy_vs_combination_baseline")
  )

  three_group_data <- make_three_group_baseline_data(data)

  run_table1_two_digits(
    data = three_group_data,
    continuous_vars = safe_cols_by_index(three_group_data, 4:8),
    categorical_vars = safe_cols_by_index(three_group_data, c(9:47, 51:54)),
    group_var = "Group",
    output_prefix = file.path(PATHS$result_dir, "three_group_baseline")
  )

  pairwise_data <- make_pairwise_baseline_data(three_group_data)

  for (comparison_name in names(pairwise_data)) {
    comparison_data <- pairwise_data[[comparison_name]]

    run_table1_two_digits(
      data = comparison_data,
      continuous_vars = safe_cols_by_index(comparison_data, 3:8),
      categorical_vars = safe_cols_by_index(comparison_data, c(9:47, 51:54)),
      group_var = "Group",
      output_prefix = file.path(PATHS$result_dir, paste0(comparison_name, "_baseline"))
    )
  }

  invisible(pairwise_data)
}

# =============================================================================
# 5. Propensity score matching
# =============================================================================

run_psm_for_pair <- function(data,
                             comparison_name,
                             treatment_group,
                             covariates,
                             selected_columns = NULL,
                             ratio = 2,
                             caliper = 0.1,
                             run_matching = TRUE) {
  if (!"Group" %in% names(data)) {
    stop("The input data must contain a Group column.", call. = FALSE)
  }

  if (!is.null(selected_columns)) {
    selected_columns <- intersect(selected_columns, names(data))
    data <- data %>% dplyr::select(dplyr::all_of(selected_columns))
  }

  data <- data %>%
    dplyr::mutate(treat = ifelse(Group == treatment_group, 1, 0))

  complete_vars <- intersect(c("treat", covariates), names(data))
  data_clean <- data %>% tidyr::drop_na(dplyr::all_of(complete_vars))

  covariates <- intersect(covariates, names(data_clean))
  if (length(covariates) == 0) {
    stop("No valid covariates were available for PSM: ", comparison_name, call. = FALSE)
  }

  if (!run_matching) {
    matched_data <- data_clean
    matched_data$distance <- NA_real_
    matched_data$weights <- 1
    matched_data$subclass <- NA
  } else {
    psm_formula <- stats::reformulate(covariates, response = "treat")

    matched_model <- MatchIt::matchit(
      formula = psm_formula,
      data = data_clean,
      method = "nearest",
      ratio = ratio,
      caliper = caliper
    )

    print(summary(matched_model))
    matched_data <- MatchIt::match.data(matched_model)
  }

  psm_file <- file.path(PATHS$psm_dir, COMPARISON_SPECS[[comparison_name]]$psm_file_name)
  write_csv_safe(matched_data, psm_file)

  matched_data
}

run_main_psm <- function(pairwise_data) {
  psm_configs <- list(
    monotherapy_vs_artificial_tears = list(
      covariates = c("DEQS问卷评分总分", "基线FBUT时间", "荧光素染色泪膜破裂总分"),
      selected_indices = c(1:47, 51:54, 56)
    ),
    monotherapy_vs_anti_inflammatory = list(
      covariates = c("BMI", "年龄", "DEQS问卷评分总分", "基线FBUT时间", "荧光素染色泪膜破裂总分", "性别"),
      selected_indices = c(1:47, 51:54, 56)
    ),
    artificial_tears_vs_anti_inflammatory = list(
      covariates = c("年龄", "基线FBUT时间", "性别"),
      selected_indices = c(1:47, 52:54, 56)
    )
  )

  matched_list <- list()

  for (comparison_name in names(psm_configs)) {
    data <- pairwise_data[[comparison_name]]
    spec <- COMPARISON_SPECS[[comparison_name]]
    cfg <- psm_configs[[comparison_name]]

    selected_columns <- safe_cols_by_index(data, cfg$selected_indices)

    matched_list[[comparison_name]] <- run_psm_for_pair(
      data = data,
      comparison_name = comparison_name,
      treatment_group = spec$treatment_group,
      covariates = cfg$covariates,
      selected_columns = selected_columns,
      ratio = 2,
      caliper = 0.1,
      run_matching = TRUE
    )
  }

  invisible(matched_list)
}

run_post_psm_baseline_tables <- function(matched_list) {
  for (comparison_name in names(matched_list)) {
    data <- matched_list[[comparison_name]]
    spec <- COMPARISON_SPECS[[comparison_name]]

    data$treat <- ifelse(data$treat == 1, spec$treatment_group, spec$control_group)

    run_table1_two_digits(
      data = data,
      continuous_vars = safe_cols_by_index(data, 3:8),
      categorical_vars = safe_cols_by_index(data, 9:51),
      group_var = "treat",
      output_prefix = file.path(PATHS$result_dir, paste0(comparison_name, "_post_psm_baseline"))
    )
  }

  invisible(NULL)
}

# =============================================================================
# 6. Extract longitudinal datasets after PSM
# =============================================================================

extract_matched_longitudinal_data <- function(matched_list) {
  full_data <- readxl::read_excel(PATHS$cleaned_workbook)

  matched_ids <- lapply(matched_list, function(x) unique(x[[SOURCE_COLUMNS$subject_id]]))

  data_A <- full_data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$subject_id]] %in% matched_ids$monotherapy_vs_artificial_tears)

  data_A_1 <- data_A %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$group]] %in% SOURCE_COLUMNS$monotherapy_source) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$monotherapy)

  data_A_2 <- data_A %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$artificial_tears_flag]] %in% SOURCE_COLUMNS$yes) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$artificial_tears)

  longitudinal_A <- dplyr::bind_rows(data_A_1, data_A_2)

  data_B <- full_data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$subject_id]] %in% matched_ids$monotherapy_vs_anti_inflammatory)

  data_B_1 <- data_B %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$group]] %in% SOURCE_COLUMNS$monotherapy_source) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$monotherapy)

  data_B_2 <- data_B %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$anti_inflammatory_flag]] %in% SOURCE_COLUMNS$yes) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$anti_inflammatory)

  longitudinal_B <- dplyr::bind_rows(data_B_1, data_B_2)

  data_C <- full_data %>%
    dplyr::filter(.data[[SOURCE_COLUMNS$subject_id]] %in% matched_ids$artificial_tears_vs_anti_inflammatory) %>%
    dplyr::mutate(
      Group = dplyr::case_when(
        .data[[SOURCE_COLUMNS$anti_inflammatory_flag]] == SOURCE_COLUMNS$yes ~ SOURCE_COLUMNS$anti_inflammatory,
        .data[[SOURCE_COLUMNS$artificial_tears_flag]] == SOURCE_COLUMNS$yes ~ SOURCE_COLUMNS$artificial_tears,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(Group))

  output_list <- list(
    monotherapy_vs_artificial_tears = longitudinal_A,
    monotherapy_vs_anti_inflammatory = longitudinal_B,
    artificial_tears_vs_anti_inflammatory = data_C
  )

  for (comparison_name in names(output_list)) {
    output_file <- file.path(PATHS$psm_longitudinal_dir, COMPARISON_SPECS[[comparison_name]]$file_name)
    write_csv_safe(output_list[[comparison_name]], output_file)
  }

  invisible(output_list)
}

load_longitudinal_pair_data <- function(comparison_name, source_dir = PATHS$psm_longitudinal_dir) {
  file <- file.path(source_dir, COMPARISON_SPECS[[comparison_name]]$file_name)
  data.table::fread(file, data.table = FALSE)
}

# =============================================================================
# 7. Outcome model preparation
# =============================================================================

filter_mgd_and_aqueous_deficient <- function(data) {
  mgd_col <- "脂质异常型（MGD）干眼"
  aqueous_col <- "水液缺乏型干眼"

  if (all(c(mgd_col, aqueous_col) %in% names(data))) {
    data <- data %>%
      dplyr::filter(.data[[mgd_col]] > 0, .data[[aqueous_col]] > 0)
  }

  data
}

filter_schirmer_baseline_le_10 <- function(data) {
  schirmer_col <- "基线SchirmerⅠ试验（无麻醉）"

  if (schirmer_col %in% names(data)) {
    data <- data %>%
      dplyr::mutate("{schirmer_col}" := as.numeric(.data[[schirmer_col]])) %>%
      dplyr::filter(.data[[schirmer_col]] <= 10)
  }

  data
}

prepare_outcome_data <- function(longitudinal_data,
                                 sheet,
                                 value_col,
                                 baseline_col,
                                 include_site = TRUE,
                                 filter_function = NULL) {
  join_keys <- c(SOURCE_COLUMNS$subject_id, SOURCE_COLUMNS$visit)
  if (include_site) join_keys <- c(SOURCE_COLUMNS$subject_id, SOURCE_COLUMNS$site_id, SOURCE_COLUMNS$visit)

  longitudinal_data <- mutate_id_columns_as_character(longitudinal_data, include_site = include_site)

  raw_data <- readxl::read_excel(PATHS$raw_workbook, sheet = sheet) %>%
    dplyr::select(dplyr::any_of(c(join_keys, value_col, baseline_col))) %>%
    mutate_id_columns_as_character(include_site = include_site)

  join_keys <- intersect(join_keys, intersect(names(longitudinal_data), names(raw_data)))

  merged_data <- longitudinal_data %>%
    dplyr::left_join(raw_data, by = join_keys) %>%
    dplyr::select(-dplyr::any_of(DROP_COLUMNS)) %>%
    dplyr::rename(score = dplyr::all_of(value_col), baseline = dplyr::all_of(baseline_col))

  if (!is.null(filter_function)) {
    merged_data <- filter_function(merged_data)
  }

  merged_data
}

prepare_model_data <- function(data, exclude_baseline_visit = FALSE) {
  data <- data %>%
    safe_rename(CLINICAL_RENAME_MAP)

  if ("基线FBUT时间...24" %in% names(data)) {
    data <- data %>% dplyr::rename(Baseline_FBUT_5S = `基线FBUT时间...24`)
  }

  if ("基线FBUT时间...25" %in% names(data)) {
    data <- data %>% dplyr::rename(Baseline_FBUT = `基线FBUT时间...25`)
  }

  if (SOURCE_COLUMNS$visit_number %in% names(data)) {
    data <- data %>% dplyr::mutate(VISIT = as.factor(.data[[SOURCE_COLUMNS$visit_number]]))
  } else if ("Visit" %in% names(data)) {
    data <- data %>% dplyr::mutate(VISIT = as.factor(Visit))
  } else if (SOURCE_COLUMNS$visit %in% names(data)) {
    data <- data %>% dplyr::mutate(VISIT = as.factor(.data[[SOURCE_COLUMNS$visit]]))
  }

  if (!"SubjectID" %in% names(data) && SOURCE_COLUMNS$subject_id %in% names(data)) {
    data <- data %>% dplyr::rename(SubjectID = dplyr::all_of(SOURCE_COLUMNS$subject_id))
  }

  data <- data %>%
    dplyr::mutate(
      Group = as.factor(Group),
      SubjectID = as.factor(SubjectID),
      score = as.numeric(score),
      baseline = as.numeric(baseline)
    )

  factor_candidates <- c(
    "Gender", "MedicalHistory", "EyeSurgeryHistory", "MGD_DryEye", "VDT_DryEye",
    "SurgeryRelated_DryEye", "SjogrenRelated_DryEye", "DiabetesRelated_DryEye",
    "DrugRelated_DryEye", "ContactLensRelated_DryEye", "OtherRiskFactor_DryEye",
    "AqueousDeficient_DryEye", "LipidAbnormal_DryEye", "MucinAbnormal_DryEye",
    "TearDynamics_DryEye", "Mixed_DryEye", "Mixed_DryEye_Full",
    "IsSpecifiedCase", "PRK_History", "Keratomileusis_History",
    "Keratectomy_History", "RK_History", "Severity", "SeverityCode", "CFS_Code"
  )

  factor_candidates <- intersect(factor_candidates, names(data))
  data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(factor_candidates), as.factor))

  if (exclude_baseline_visit && "VISIT" %in% names(data)) {
    data <- data %>% dplyr::filter(VISIT != "1")
  }

  data
}

fit_mmrm <- function(data, adjustment_vars = NULL, reml = FALSE) {
  adjustment_vars <- filter_existing_model_vars(data, adjustment_vars)

  fixed_terms <- c("baseline", "Group * VISIT", adjustment_vars)
  formula_text <- paste0("score ~ ", paste(fixed_terms, collapse = " + "), " + (1 | SubjectID)")

  lmerTest::lmer(
    formula = stats::as.formula(formula_text),
    data = data,
    REML = reml
  )
}

extract_anova <- function(model, comparison_label) {
  as.data.frame(anova(model)) %>%
    tibble::rownames_to_column("Source") %>%
    dplyr::mutate(Comparison = comparison_label, .before = 1) %>%
    dplyr::rename(F_value = `F value`, P_value = `Pr(>F)`) %>%
    dplyr::mutate(P_value = format_p_value(P_value))
}

extract_emmeans <- function(model, comparison_label) {
  emm <- emmeans::emmeans(model, ~ Group | VISIT)

  as.data.frame(emm) %>%
    dplyr::mutate(Comparison = comparison_label, .before = 1)
}

extract_pairwise_contrasts <- function(model, comparison_label) {
  emm <- emmeans::emmeans(model, ~ Group | VISIT)
  contrast_df <- as.data.frame(confint(pairs(emm, reverse = TRUE)))
  contrast_df %>% dplyr::mutate(Comparison = comparison_label, .before = 1)
}

# =============================================================================
# 8. Outcome plotting
# =============================================================================

theme_publication <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size, base_family = FONT_FAMILY) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = base_size - 2, hjust = 0)
    )
}

plot_lsmeans <- function(model, title, y_label, group_labels, adjustment_note = NULL) {
  emm <- emmeans::emmeans(model, ~ Group | VISIT)
  df <- as.data.frame(emm)
  ci_cols <- get_ci_columns(df)

  df$GroupLabel <- factor(df$Group, levels = names(group_labels), labels = group_labels)
  df$VisitLabel <- factor(
    paste("Visit", df$VISIT),
    levels = paste("Visit", sort(unique(as.numeric(as.character(df$VISIT)))))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = VisitLabel,
    y = emmean,
    color = GroupLabel,
    group = GroupLabel
  )) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[[ci_cols$lower]], ymax = .data[[ci_cols$upper]]),
      width = 0.1,
      linewidth = 0.7
    ) +
    ggplot2::scale_color_manual(values = c("#2E5A87", "#A93226", "#229954")) +
    ggplot2::labs(
      subtitle = title,
      x = NULL,
      y = y_label,
      color = "Treatment Group",
      caption = adjustment_note
    ) +
    theme_publication()

  p
}

plot_contrast_forest <- function(model, title, x_label, adjustment_note = NULL) {
  emm <- emmeans::emmeans(model, ~ Group | VISIT)
  df <- as.data.frame(confint(pairs(emm, reverse = TRUE)))

  ci_cols <- get_ci_columns(df)
  estimate_col <- if ("estimate" %in% names(df)) "estimate" else names(df)[grepl("estimate", names(df), ignore.case = TRUE)][1]

  df$VisitLabel <- factor(
    paste("Visit", df$VISIT),
    levels = paste("Visit", sort(unique(as.numeric(as.character(df$VISIT)))))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[estimate_col]], y = VisitLabel)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#B22222", linewidth = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[ci_cols$lower]], xmax = .data[[ci_cols$upper]]),
      width = 0.2,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 3.0, color = "#2E5A87") +
    ggplot2::labs(
      subtitle = title,
      x = x_label,
      y = NULL,
      caption = adjustment_note
    ) +
    theme_publication() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"))
}

save_outcome_figures <- function(profile_plots,
                                 forest_plots,
                                 outcome_id,
                                 outcome_title,
                                 output_dir,
                                 width_per_panel = 5,
                                 height_profile = 5,
                                 height_forest = 6,
                                 height_combined = 12) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  profile_combined <- patchwork::wrap_plots(profile_plots, nrow = 1, guides = "collect") +
    patchwork::plot_annotation(
      title = paste0("Least Squares Means of ", outcome_title),
      subtitle = "Analysis based on linear mixed-effects models",
      tag_levels = "A"
    ) &
    ggplot2::theme(legend.position = "bottom")

  forest_combined <- patchwork::wrap_plots(forest_plots, nrow = 1) +
    patchwork::plot_annotation(
      title = paste0("Treatment Differences in ", outcome_title),
      subtitle = "Positive estimates indicate greater improvement in the first group",
      tag_levels = "A"
    )

  combined_plot <- (profile_combined / forest_combined) +
    patchwork::plot_layout(heights = c(1, 1.2)) +
    patchwork::plot_annotation(
      title = paste0("Comprehensive Analysis of ", outcome_title),
      caption = "Data are presented as least squares means or treatment differences with 95% confidence intervals."
    ) &
    theme_publication(base_size = 12)

  n_panels <- length(profile_plots)
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(outcome_id, "_lsmeans.pdf")),
    plot = profile_combined,
    width = width_per_panel * n_panels,
    height = height_profile,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(outcome_id, "_forest.pdf")),
    plot = forest_combined,
    width = width_per_panel * n_panels,
    height = height_forest,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(outcome_id, "_combined.pdf")),
    plot = combined_plot,
    width = width_per_panel * n_panels,
    height = height_combined,
    dpi = 300
  )

  invisible(list(
    profile = profile_combined,
    forest = forest_combined,
    combined = combined_plot
  ))
}

# =============================================================================
# 9. Repeated-measures outcome analysis
# =============================================================================

run_outcome_analysis <- function(outcome_spec,
                                 comparison_names,
                                 source_dir = PATHS$psm_longitudinal_dir,
                                 output_dir = PATHS$psm_result_dir) {
  models <- list()
  profile_plots <- list()
  forest_plots <- list()
  anova_tables <- list()
  emmeans_tables <- list()
  contrast_tables <- list()

  for (comparison_name in comparison_names) {
    spec <- COMPARISON_SPECS[[comparison_name]]

    longitudinal_data <- load_longitudinal_pair_data(comparison_name, source_dir = source_dir)

    outcome_data <- prepare_outcome_data(
      longitudinal_data = longitudinal_data,
      sheet = outcome_spec$sheet,
      value_col = outcome_spec$value_col,
      baseline_col = outcome_spec$baseline_col,
      include_site = outcome_spec$include_site,
      filter_function = outcome_spec$filter_function
    )

    model_data <- prepare_model_data(
      outcome_data,
      exclude_baseline_visit = outcome_spec$exclude_baseline_visit
    )

    model <- fit_mmrm(
      data = model_data,
      adjustment_vars = outcome_spec$adjustment_vars,
      reml = outcome_spec$reml
    )

    models[[comparison_name]] <- model

    adjustment_note <- if (length(outcome_spec$adjustment_vars) > 0) {
      paste("Adjusted for:", paste(outcome_spec$adjustment_vars, collapse = ", "))
    } else {
      "Adjusted for baseline value and repeated measures."
    }

    profile_plots[[comparison_name]] <- plot_lsmeans(
      model = model,
      title = spec$figure_title,
      y_label = outcome_spec$y_label,
      group_labels = spec$labels,
      adjustment_note = adjustment_note
    )

    forest_plots[[comparison_name]] <- plot_contrast_forest(
      model = model,
      title = spec$forest_title,
      x_label = outcome_spec$contrast_x_label,
      adjustment_note = adjustment_note
    )

    anova_tables[[comparison_name]] <- extract_anova(model, spec$figure_title)
    emmeans_tables[[comparison_name]] <- extract_emmeans(model, spec$figure_title)
    contrast_tables[[comparison_name]] <- extract_pairwise_contrasts(model, spec$figure_title)
  }

  save_outcome_figures(
    profile_plots = profile_plots,
    forest_plots = forest_plots,
    outcome_id = outcome_spec$id,
    outcome_title = outcome_spec$title,
    output_dir = output_dir,
    width_per_panel = ifelse(length(comparison_names) == 3, 5, 5),
    height_combined = ifelse(length(comparison_names) == 3, 14, 12)
  )

  write_csv_safe(
    dplyr::bind_rows(anova_tables),
    file.path(output_dir, paste0(outcome_spec$id, "_anova.csv"))
  )

  write_csv_safe(
    dplyr::bind_rows(emmeans_tables),
    file.path(output_dir, paste0(outcome_spec$id, "_emmeans.csv"))
  )

  write_csv_safe(
    dplyr::bind_rows(contrast_tables),
    file.path(output_dir, paste0(outcome_spec$id, "_contrasts.csv"))
  )

  invisible(models)
}

common_adjustment_vars_A <- c(
  "MedicalHistory", "EyeSurgeryHistory", "MGD_DryEye",
  "SurgeryRelated_DryEye", "PRK_History", "Keratomileusis_History"
)

common_adjustment_vars_B <- c(
  "MedicalHistory", "EyeSurgeryHistory", "MGD_DryEye", "VDT_DryEye",
  "SjogrenRelated_DryEye", "AqueousDeficient_DryEye",
  "TearDynamics_DryEye", "Mixed_DryEye"
)

common_adjustment_vars_C <- c(
  "EyeSurgeryHistory", "MGD_DryEye", "VDT_DryEye", "TearDynamics_DryEye"
)

default_adjustment_vars <- unique(c(
  common_adjustment_vars_A,
  common_adjustment_vars_B,
  common_adjustment_vars_C
))

OUTCOME_SPECS <- list(
  fbut_change = list(
    id = "fbut_change",
    title = "Change in FBUT from Baseline",
    sheet = "BUT",
    value_col = "较基线变化-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean Change in FBUT (s)",
    contrast_x_label = "Difference in FBUT (s) [95% CI]",
    include_site = TRUE,
    exclude_baseline_visit = TRUE,
    filter_function = NULL,
    adjustment_vars = default_adjustment_vars,
    reml = FALSE
  ),
  fbut_value_mgd_aqueous = list(
    id = "fbut_value_mgd_aqueous",
    title = "FBUT in MGD and Aqueous-deficient Dry Eye",
    sheet = "BUT",
    value_col = "分析值-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean FBUT (s)",
    contrast_x_label = "Difference in FBUT (s) [95% CI]",
    include_site = TRUE,
    exclude_baseline_visit = FALSE,
    filter_function = filter_mgd_and_aqueous_deficient,
    adjustment_vars = NULL,
    reml = FALSE
  ),
  schirmer_value_mgd_aqueous = list(
    id = "schirmer_value_mgd_aqueous",
    title = "Schirmer I Test in MGD and Aqueous-deficient Dry Eye",
    sheet = "Schirmer",
    value_col = "分析值-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean Schirmer I Test (mm)",
    contrast_x_label = "Difference in Schirmer I Test (mm) [95% CI]",
    include_site = TRUE,
    exclude_baseline_visit = FALSE,
    filter_function = function(x) filter_mgd_and_aqueous_deficient(filter_schirmer_baseline_le_10(x)),
    adjustment_vars = NULL,
    reml = FALSE
  ),
  deqs_value_mgd_aqueous = list(
    id = "deqs_value_mgd_aqueous",
    title = "DEQS Score in MGD and Aqueous-deficient Dry Eye",
    sheet = "DEQS",
    value_col = "分析值-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean DEQS Score",
    contrast_x_label = "Difference in DEQS Score [95% CI]",
    include_site = TRUE,
    exclude_baseline_visit = FALSE,
    filter_function = filter_mgd_and_aqueous_deficient,
    adjustment_vars = NULL,
    reml = FALSE
  ),
  cfs_value_mgd_aqueous = list(
    id = "cfs_value_mgd_aqueous",
    title = "CFS Score in MGD and Aqueous-deficient Dry Eye",
    sheet = "CFS_5区",
    value_col = "分析值-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean CFS Score",
    contrast_x_label = "Difference in CFS Score [95% CI]",
    include_site = TRUE,
    exclude_baseline_visit = FALSE,
    filter_function = filter_mgd_and_aqueous_deficient,
    adjustment_vars = NULL,
    reml = FALSE
  ),
  tmh_change = list(
    id = "tmh_change",
    title = "Change in Tear Meniscus Height from Baseline",
    sheet = "TMH",
    value_col = "较基线变化-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean Change in TMH",
    contrast_x_label = "Difference in TMH [95% CI]",
    include_site = FALSE,
    exclude_baseline_visit = TRUE,
    filter_function = NULL,
    adjustment_vars = default_adjustment_vars,
    reml = FALSE
  ),
  llt_change = list(
    id = "llt_change",
    title = "Change in Lipid Layer Thickness from Baseline",
    sheet = "LLT",
    value_col = "较基线变化-眼部",
    baseline_col = "基线-眼部",
    y_label = "Mean Change in LLT",
    contrast_x_label = "Difference in LLT [95% CI]",
    include_site = FALSE,
    exclude_baseline_visit = TRUE,
    filter_function = NULL,
    adjustment_vars = default_adjustment_vars,
    reml = FALSE
  )
)

# =============================================================================
# 10. Specified-center data and PSM
# =============================================================================

load_specified_center_data <- function() {
  readxl::read_excel(PATHS$specified_center_workbook) %>%
    dplyr::rename(
      ArtificialTearsFlag = 联合人工泪液,
      AntiInflammatoryFlag = 联合免疫抑制剂
    )
}

make_specified_center_three_group_data <- function(data) {
  artificial_tears <- data %>%
    dplyr::filter(ArtificialTearsFlag == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$artificial_tears)

  anti_inflammatory <- data %>%
    dplyr::filter(AntiInflammatoryFlag == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$anti_inflammatory)

  monotherapy <- data %>%
    dplyr::filter(单独用药 == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$monotherapy)

  dplyr::bind_rows(monotherapy, artificial_tears, anti_inflammatory)
}

run_specified_center_psm <- function() {
  data <- load_specified_center_data()
  three_group_data <- make_specified_center_three_group_data(data)

  pairwise_data <- make_pairwise_baseline_data(three_group_data)

  specified_configs <- list(
    monotherapy_vs_artificial_tears = list(
      covariates = c("DEQS问卷评分总分", "基线FBUT时间", "荧光素染色泪膜破裂总分"),
      selected_indices = c(1:8, 9:32, 34:41, 44:47, 49, 52:53, 55),
      run_matching = FALSE
    ),
    monotherapy_vs_anti_inflammatory = list(
      covariates = c("性别", "年龄", "DEQS问卷评分总分"),
      selected_indices = c(1:8, 9:32, 34:41, 44:47, 49, 52, 55),
      run_matching = TRUE
    ),
    artificial_tears_vs_anti_inflammatory = list(
      covariates = c("年龄"),
      selected_indices = c(1:8, 9:15, 17:32, 34:41, 44:47, 49, 53:55),
      run_matching = TRUE
    )
  )

  matched_list <- list()

  for (comparison_name in names(specified_configs)) {
    comparison_data <- pairwise_data[[comparison_name]]
    cfg <- specified_configs[[comparison_name]]
    spec <- COMPARISON_SPECS[[comparison_name]]

    selected_columns <- safe_cols_by_index(comparison_data, cfg$selected_indices)

    matched_list[[comparison_name]] <- run_psm_for_pair(
      data = comparison_data,
      comparison_name = comparison_name,
      treatment_group = spec$treatment_group,
      covariates = cfg$covariates,
      selected_columns = selected_columns,
      ratio = 2,
      caliper = 0.1,
      run_matching = cfg$run_matching
    )
  }

  specified_dir <- file.path(PATHS$psm_longitudinal_dir, "specified_center")
  dir.create(specified_dir, recursive = TRUE, showWarnings = FALSE)

  for (comparison_name in names(matched_list)) {
    output_file <- file.path(specified_dir, COMPARISON_SPECS[[comparison_name]]$file_name)
    write_csv_safe(matched_list[[comparison_name]], output_file)
  }

  invisible(matched_list)
}

# =============================================================================
# 11. Nested adjusted model tables for matched data
# =============================================================================

prepare_nested_model_data <- function(longitudinal_data,
                                      sheet,
                                      value_col,
                                      baseline_col,
                                      filter_function = NULL,
                                      include_site = TRUE) {
  data <- prepare_outcome_data(
    longitudinal_data = longitudinal_data,
    sheet = sheet,
    value_col = value_col,
    baseline_col = baseline_col,
    include_site = include_site,
    filter_function = filter_function
  )

  data <- prepare_model_data(data, exclude_baseline_visit = FALSE)

  data <- data %>%
    dplyr::rename(
      Score = score,
      Baseline = baseline
    ) %>%
    dplyr::mutate(
      Severity = if ("Severity" %in% names(.)) as.factor(Severity) else NA
    )

  yes_no_vars <- c(
    "MedicalHistory", "EyeSurgeryHistory", "PRK_History", "Keratomileusis_History",
    "MGD_DryEye", "VDT_DryEye", "SurgeryRelated_DryEye", "SjogrenRelated_DryEye",
    "DiabetesRelated_DryEye", "DrugRelated_DryEye", "ContactLensRelated_DryEye",
    "AqueousDeficient_DryEye", "LipidAbnormal_DryEye", "MucinAbnormal_DryEye",
    "TearDynamics_DryEye", "Mixed_DryEye"
  )

  yes_no_vars <- intersect(yes_no_vars, names(data))
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(yes_no_vars),
        ~ factor(.x, levels = c("否", "是"))
      )
    )

  data
}

fit_nested_models <- function(data) {
  core_vars <- c("Baseline", "Group", "VISIT", "SubjectID")
  if (!all(core_vars %in% names(data))) {
    stop("The nested model data do not contain the required model variables.", call. = FALSE)
  }

  model1_vars <- filter_existing_model_vars(data, c("Age", "Gender", "BMI"))

  model2_vars <- filter_existing_model_vars(
    data,
    c(
      "Age", "Gender", "BMI", "Severity", "MGD_DryEye", "VDT_DryEye",
      "SurgeryRelated_DryEye", "SjogrenRelated_DryEye", "DiabetesRelated_DryEye",
      "DrugRelated_DryEye", "ContactLensRelated_DryEye"
    )
  )

  model3_vars <- filter_existing_model_vars(
    data,
    c(
      "Age", "Gender", "BMI", "Severity", "AqueousDeficient_DryEye",
      "LipidAbnormal_DryEye", "MucinAbnormal_DryEye",
      "TearDynamics_DryEye", "Mixed_DryEye"
    )
  )

  make_formula <- function(extra_vars = NULL) {
    fixed_terms <- c("Baseline", "Group * VISIT", extra_vars)
    stats::as.formula(paste0("Score ~ ", paste(fixed_terms, collapse = " + "), " + (1 | SubjectID)"))
  }

  list(
    crude = lmerTest::lmer(make_formula(NULL), data = data, REML = FALSE),
    model1 = lmerTest::lmer(make_formula(model1_vars), data = data, REML = FALSE),
    model2 = lmerTest::lmer(make_formula(model2_vars), data = data, REML = FALSE),
    model3 = lmerTest::lmer(make_formula(model3_vars), data = data, REML = FALSE)
  )
}

save_nested_model_table <- function(models, output_file) {
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  sjPlot::tab_model(
    models$crude,
    models$model1,
    models$model2,
    models$model3,
    show.ci = 0.95,
    show.se = TRUE,
    p.style = "numeric",
    dv.labels = c("Crude Model", "Model 1", "Model 2", "Model 3"),
    CSS = list(css.table = "font-family: 'Times New Roman', serif;"),
    file = output_file
  )
}

run_nested_model_tables_for_outcome <- function(outcome_spec,
                                                comparison_names = c(
                                                  "monotherapy_vs_artificial_tears",
                                                  "monotherapy_vs_anti_inflammatory"
                                                ),
                                                source_dir = PATHS$psm_longitudinal_dir,
                                                output_dir = PATHS$psm_result_dir) {
  for (comparison_name in comparison_names) {
    longitudinal_data <- load_longitudinal_pair_data(comparison_name, source_dir = source_dir)

    model_data <- prepare_nested_model_data(
      longitudinal_data = longitudinal_data,
      sheet = outcome_spec$sheet,
      value_col = outcome_spec$value_col,
      baseline_col = outcome_spec$baseline_col,
      filter_function = outcome_spec$filter_function,
      include_site = outcome_spec$include_site
    )

    models <- fit_nested_models(model_data)

    output_file <- file.path(
      output_dir,
      paste0(outcome_spec$id, "_", comparison_name, "_nested_lmm_models.html")
    )

    save_nested_model_table(models, output_file)
  }

  invisible(NULL)
}

# =============================================================================
# 12. Overall cohort mixed-effects models and predictor screening
# =============================================================================

prepare_overall_pair_data <- function() {
  data <- readxl::read_excel(PATHS$overall_model_workbook)

  monotherapy <- data %>%
    dplyr::filter(单独用药 == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$monotherapy)

  artificial_tears <- data %>%
    dplyr::filter(联合人工泪液 == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$artificial_tears)

  anti_inflammatory <- data %>%
    dplyr::filter(联合免疫抑制剂 == 1) %>%
    dplyr::mutate(Group = SOURCE_COLUMNS$anti_inflammatory)

  list(
    monotherapy_vs_artificial_tears = dplyr::bind_rows(monotherapy, artificial_tears),
    monotherapy_vs_anti_inflammatory = dplyr::bind_rows(monotherapy, anti_inflammatory),
    artificial_tears_vs_anti_inflammatory = dplyr::bind_rows(artificial_tears, anti_inflammatory)
  )
}

run_overall_fbut_models <- function() {
  pair_data <- prepare_overall_pair_data()

  raw_data <- readxl::read_excel(PATHS$raw_workbook, sheet = "BUT") %>%
    dplyr::select(
      dplyr::any_of(c(
        SOURCE_COLUMNS$subject_id, SOURCE_COLUMNS$site_id, SOURCE_COLUMNS$visit,
        "较基线变化-眼部", "基线-眼部"
      ))
    ) %>%
    mutate_id_columns_as_character(include_site = TRUE)

  for (comparison_name in names(pair_data)) {
    data <- pair_data[[comparison_name]] %>%
      mutate_id_columns_as_character(include_site = TRUE) %>%
      dplyr::left_join(
        raw_data,
        by = c(SOURCE_COLUMNS$subject_id, SOURCE_COLUMNS$site_id, SOURCE_COLUMNS$visit)
      ) %>%
      dplyr::select(-dplyr::any_of(DROP_COLUMNS)) %>%
      dplyr::rename(score = `较基线变化-眼部`, baseline = `基线-眼部`) %>%
      dplyr::select(-dplyr::any_of(c("单独用药", "联合人工泪液", "联合免疫抑制剂")))

    model_data <- prepare_model_data(data, exclude_baseline_visit = TRUE) %>%
      dplyr::rename(Score = score, Baseline = baseline)

    models <- fit_nested_models(model_data)

    output_file <- file.path(
      PATHS$psm_result_dir,
      paste0("overall_fbut_", comparison_name, "_nested_lmm_models.html")
    )

    save_nested_model_table(models, output_file)
  }

  invisible(NULL)
}

run_predictor_screening <- function(data,
                                    outcome_col = "Score",
                                    subject_col = "SubjectID",
                                    visit_col = "VISIT",
                                    candidate_vars,
                                    output_prefix) {
  candidate_vars <- filter_existing_model_vars(data, candidate_vars)

  univariable_results <- purrr::map_dfr(candidate_vars, function(var) {
    formula_text <- paste0(outcome_col, " ~ ", var, " + (1 | ", subject_col, ")")

    fit <- tryCatch(
      lmerTest::lmer(stats::as.formula(formula_text), data = data, REML = FALSE),
      error = function(e) NULL
    )

    if (is.null(fit)) return(NULL)

    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(Predictor = var, .before = 1)
  })

  write_csv_safe(univariable_results, paste0(output_prefix, "_univariable_lmm.csv"))

  significant_vars <- univariable_results %>%
    dplyr::filter(p.value < 0.05) %>%
    dplyr::pull(Predictor) %>%
    unique()

  if (length(significant_vars) > 0) {
    formula_text <- paste0(
      outcome_col, " ~ ",
      paste(significant_vars, collapse = " + "),
      " + (1 | ", subject_col, ")"
    )

    multivariable_fit <- lmerTest::lmer(
      stats::as.formula(formula_text),
      data = data,
      REML = FALSE
    )

    multivariable_results <- broom.mixed::tidy(
      multivariable_fit,
      effects = "fixed",
      conf.int = TRUE
    )

    write_csv_safe(multivariable_results, paste0(output_prefix, "_multivariable_lmm.csv"))
  }

  invisible(univariable_results)
}

# =============================================================================
# 13. Main workflow
# =============================================================================

main <- function() {
  message("Step 1: Running baseline descriptive analyses...")
  run_baseline_medication_tables()
  pairwise_data <- run_baseline_treatment_group_tables()

  message("Step 2: Running main 1:2 propensity score matching...")
  matched_list <- run_main_psm(pairwise_data)

  message("Step 3: Exporting post-PSM baseline tables...")
  run_post_psm_baseline_tables(matched_list)

  message("Step 4: Extracting matched longitudinal datasets...")
  extract_matched_longitudinal_data(matched_list)

  message("Step 5: Running repeated-measures outcome analyses...")

  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$fbut_change,
    comparison_names = names(COMPARISON_SPECS),
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  two_group_comparisons <- c(
    "monotherapy_vs_artificial_tears",
    "monotherapy_vs_anti_inflammatory"
  )

  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$fbut_value_mgd_aqueous,
    comparison_names = two_group_comparisons,
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$schirmer_value_mgd_aqueous,
    comparison_names = two_group_comparisons,
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$deqs_value_mgd_aqueous,
    comparison_names = two_group_comparisons,
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$cfs_value_mgd_aqueous,
    comparison_names = two_group_comparisons,
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  message("Step 6: Running nested adjusted model tables for PSM datasets...")

  nested_specs <- list(
    OUTCOME_SPECS$fbut_value_mgd_aqueous,
    OUTCOME_SPECS$deqs_value_mgd_aqueous,
    OUTCOME_SPECS$schirmer_value_mgd_aqueous,
    OUTCOME_SPECS$cfs_value_mgd_aqueous
  )

  for (spec in nested_specs) {
    run_nested_model_tables_for_outcome(
      outcome_spec = spec,
      comparison_names = two_group_comparisons,
      source_dir = PATHS$psm_longitudinal_dir,
      output_dir = PATHS$psm_result_dir
    )
  }

  message("Step 7: Running specified-center analyses if the specified-center file exists...")
  if (file.exists(PATHS$specified_center_workbook)) {
    run_specified_center_psm()

    specified_dir <- file.path(PATHS$psm_longitudinal_dir, "specified_center")
    specified_output_dir <- file.path(PATHS$psm_result_dir, "specified_center")

    run_outcome_analysis(
      outcome_spec = OUTCOME_SPECS$tmh_change,
      comparison_names = names(COMPARISON_SPECS),
      source_dir = specified_dir,
      output_dir = specified_output_dir
    )
  }

  message("Step 8: Running LLT analysis on matched longitudinal datasets...")
  run_outcome_analysis(
    outcome_spec = OUTCOME_SPECS$llt_change,
    comparison_names = names(COMPARISON_SPECS),
    source_dir = PATHS$psm_longitudinal_dir,
    output_dir = PATHS$psm_result_dir
  )

  message("Step 9: Running overall FBUT nested models if the overall-model workbook exists...")
  if (file.exists(PATHS$overall_model_workbook)) {
    run_overall_fbut_models()
  }

  message("Analysis completed.")
}

if (sys.nframe() == 0) {
  main()
}
