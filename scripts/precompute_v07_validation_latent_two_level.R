#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the debiasR package root.", call. = FALSE)
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(debiasR)
}

if (!requireNamespace("rstan", quietly = TRUE)) {
  stop("The `rstan` package is required for the latent two-level backend.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

formula_text <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = " ")
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  as.integer(value)
}

env_num <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  as.numeric(value)
}

env_chr <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  value
}

stable_fingerprint <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path))
}

valid_area_code <- function(x) {
  grepl("^[EW][0-9]+$", x)
}

read_mapp1_lad <- function(path, source, time_label) {
  if (!file.exists(path)) {
    stop("Missing Mapp1 LAD flow file: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    transmute(
      origin = .data$LAD22CD_home,
      destination = .data$LAD22CD_work,
      flow = as.numeric(.data$count),
      mpd_source = source,
      mpd_time = time_label
    )
}

read_mapp2_lad <- function(path, source, time_label) {
  if (!file.exists(path)) {
    stop("Missing Mapp2 LAD flow file: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    transmute(
      origin = .data$home_LAD22CD,
      destination = .data$work_LAD22CD,
      flow = as.numeric(.data$trips),
      mpd_source = source,
      mpd_time = time_label
    )
}

read_census_lad <- function(path) {
  if (!file.exists(path)) {
    stop("Missing Census LTLA OD flow file: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) |>
    tibble::as_tibble() |>
    transmute(
      origin = .data[["Lower tier local authorities code"]],
      destination = .data[["LTLA of workplace code"]],
      flow = as.numeric(.data[["Count"]])
    ) |>
    filter(
      valid_area_code(.data$origin),
      valid_area_code(.data$destination),
      is.finite(.data$flow),
      .data$flow >= 0
    ) |>
    group_by(.data$origin, .data$destination) |>
    summarise(flow = sum(.data$flow, na.rm = TRUE), .groups = "drop")
}

flows_root <- env_chr(
  "DEBIAS_V07_LATENT_FLOW_ROOT",
  "/Volumes/DEBIAS/data/outputs/flows"
)
scenario <- env_chr("DEBIAS_V07_LATENT_SCENARIO", "s3")
if (!scenario %in% c("s3", "s4")) {
  stop("`DEBIAS_V07_LATENT_SCENARIO` must be `s3` or `s4`.", call. = FALSE)
}

iter <- env_int("DEBIAS_V07_LATENT_ITER", 1000L)
chains <- env_int("DEBIAS_V07_LATENT_CHAINS", 4L)
seed <- env_int("DEBIAS_V07_LATENT_SEED", 20260702L)
max_od <- env_int("DEBIAS_V07_LATENT_MAX_OD", Inf)
out_dir <- env_chr("DEBIAS_V07_LATENT_OUT_DIR", file.path("inst", "extdata"))
latent_adapt_delta <- env_num("DEBIAS_V07_LATENT_ADAPT_DELTA", 0.95)
latent_max_treedepth <- env_int("DEBIAS_V07_LATENT_MAX_TREEDEPTH", 12L)

example_data <- debiasR::debiasR_example_data(
  n_areas = Inf,
  complete_grid = TRUE,
  geography = "lad"
)
area_set <- sort(unique(example_data$mpd_od$origin))

mapp1_month_path <- file.path(flows_root, "htw", "mapp1", "mapp1_lad_month.csv")
mapp1_week_path <- file.path(flows_root, "htw", "mapp1", "mapp1_lad_week.csv")
mapp2_month_path <- file.path(flows_root, "htw", "mapp2", "mapp2_lad_month.csv")
census_path <- file.path(flows_root, "htw", "census", "ODWP01EW_LTLA.csv")

mpd_rows <- bind_rows(
  read_mapp1_lad(mapp1_month_path, source = "mapp1", time_label = "month"),
  read_mapp2_lad(mapp2_month_path, source = "mapp2", time_label = "month"),
  if (identical(scenario, "s4")) {
    read_mapp1_lad(mapp1_week_path, source = "mapp1", time_label = "week")
  }
) |>
  filter(
    .data$origin %in% area_set,
    .data$destination %in% area_set,
    is.finite(.data$flow),
    .data$flow >= 0
  ) |>
  group_by(.data$origin, .data$destination, .data$mpd_source, .data$mpd_time) |>
  summarise(flow = sum(.data$flow, na.rm = TRUE), .groups = "drop")

benchmark_od <- read_census_lad(census_path) |>
  filter(.data$origin %in% area_set, .data$destination %in% area_set)
benchmark_keys <- benchmark_od |>
  distinct(.data$origin, .data$destination)

mpd_rows <- mpd_rows |>
  semi_join(benchmark_keys, by = c("origin", "destination"))

latent_counts <- mpd_rows |>
  distinct(.data$origin, .data$destination, .data$mpd_source, .data$mpd_time) |>
  count(.data$origin, .data$destination, name = "n_source_time_rows") |>
  filter(.data$n_source_time_rows >= 2L)

if (is.finite(max_od)) {
  latent_counts <- latent_counts |>
    arrange(.data$origin, .data$destination) |>
    slice_head(n = max_od)
}

mpd_rows <- mpd_rows |>
  semi_join(latent_counts, by = c("origin", "destination"))

if (nrow(mpd_rows) == 0L) {
  stop("No repeated-source OD rows remain after filtering.", call. = FALSE)
}

coverage <- example_data$coverage |>
  filter(.data$origin %in% area_set) |>
  select(-any_of(c("mpd_source", "mpd_time")))
covariates <- example_data$covariates |>
  filter(.data$area %in% area_set)
distance <- example_data$distance |>
  filter(.data$origin %in% area_set, .data$destination %in% area_set)

method_id <- if (identical(scenario, "s3")) {
  "bayes_latent_s3_od"
} else {
  "bayes_latent_s4_od"
}
method_label <- if (identical(scenario, "s3")) {
  "Bayesian latent S3 OD"
} else {
  "Bayesian latent S4 OD"
}

mobility_formula <- ~ rural_pct_o + rural_pct_d + log_distance
bias_formula <- ~ bias_e_origin

message(
  "Fitting ", method_id, " with ", nrow(mpd_rows), " source rows and ",
  nrow(latent_counts), " OD latent states..."
)
start <- Sys.time()
fit <- suppressWarnings(
  debiasR::adjust_multilevel_bayes(
    mpd_od_df = mpd_rows,
    coverage_df = coverage,
    covariates_df = covariates,
    distance_df = distance,
    source_col = "mpd_source",
    time_col = if (identical(scenario, "s4")) "mpd_time" else NULL,
    scenario = scenario,
    repeated_observation = if (identical(scenario, "s3")) "source" else "source_time",
    target_scale = "true_flow",
    observation_model = "latent_two_level",
    coverage_scale = "origin",
    latent_flow_unit = "od",
    mobility_formula = mobility_formula,
    bias_formula = bias_formula,
    model_engine = "bayesian",
    model_family = "poisson",
    flow_adj_summary = "median",
    latent_adapt_delta = latent_adapt_delta,
    latent_max_treedepth = latent_max_treedepth,
    iter = iter,
    chains = chains,
    seed = seed,
    refresh = 0
  )
)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

source_rows <- fit |>
  as_tibble() |>
  mutate(method = method_id, .before = 1)

adjusted_od <- source_rows |>
  group_by(.data$method, .data$origin, .data$destination) |>
  summarise(
    observation_model = "latent_two_level",
    target_scale = "true_flow",
    n_source_rows = dplyr::n(),
    flow = mean(.data$flow, na.rm = TRUE),
    flow_adj = mean(.data$flow_adj, na.rm = TRUE),
    flow_true_pred = mean(.data$flow_true_pred, na.rm = TRUE),
    flow_mpd_pred = mean(.data$flow_mpd_pred, na.rm = TRUE),
    flow_adj_mean = mean(.data$flow_adj_mean, na.rm = TRUE),
    flow_adj_median = mean(.data$flow_adj_median, na.rm = TRUE),
    flow_adj_q2.5 = mean(.data$flow_adj_q2.5, na.rm = TRUE),
    flow_adj_q97.5 = mean(.data$flow_adj_q97.5, na.rm = TRUE),
    .groups = "drop"
  )

overall <- debiasR::validate_flow_overall(
  adj_df = adjusted_od,
  benchmark_od_df = benchmark_od,
  comparisons = "adjusted_vs_benchmark",
  drop_zeros = FALSE,
  return_joined = FALSE,
  method_name = method_id
)

diagnostics <- attr(fit, "diagnostics")
convergence <- diagnostics$convergence
result_metadata <- attr(fit, "result_metadata")
latent_identifiability <- result_metadata$latent_identifiability

metadata <- tibble(
  method = method_id,
  method_label = method_label,
  specification_role = if (identical(scenario, "s3")) {
    "Repeated-source latent OD validation"
  } else {
    "Unbalanced repeated-source/time latent OD validation"
  },
  benchmark_used_in_fit = "No benchmark OD cells",
  coverage_scale = "origin",
  random_intercept = "latent_flow_id",
  mobility_formula = formula_text(mobility_formula),
  bias_formula = formula_text(bias_formula),
  resolved_formula = paste(attr(fit, "formula"), collapse = " "),
  backend = attr(fit, "backend"),
  model_engine = attr(fit, "model_engine"),
  model_family = attr(fit, "model_family"),
  target_scale = attr(fit, "target_scale"),
  observation_model = attr(fit, "observation_model"),
  prediction_scope = attr(fit, "prediction_scope"),
  scenario = result_metadata$scenario,
  repeated_observation = result_metadata$repeated_observation,
  n_sources = result_metadata$n_sources,
  n_time_periods = result_metadata$n_time_periods,
  source_col = result_metadata$source_col,
  time_col = result_metadata$time_col %||% NA_character_,
  flow_adj_summary = result_metadata$flow_adj_summary,
  offset_column = result_metadata$offset_column,
  distance_source = result_metadata$distance_source,
  iter = iter,
  chains = chains,
  seed = seed,
  input_fingerprint = stable_fingerprint(list(
    mpd_od = arrange(mpd_rows, .data$origin, .data$destination, .data$mpd_source, .data$mpd_time),
    benchmark_od = arrange(benchmark_od, .data$origin, .data$destination),
    coverage = arrange(coverage, .data$origin),
    covariates = arrange(covariates, .data$area),
    distance = arrange(distance, .data$origin, .data$destination)
  )),
  area_set = paste(area_set, collapse = ";"),
  geography = "lad",
  n_areas_loaded = length(area_set),
  n_validation_rows = nrow(adjusted_od),
  n_source_rows = nrow(source_rows),
  n_input_rows = result_metadata$n_input_rows,
  n_fit_rows = result_metadata$n_fit_rows,
  n_prediction_rows = result_metadata$n_prediction_rows,
  n_zero_filled_prediction_rows = result_metadata$n_zero_filled_prediction_rows,
  latent_flow_unit = result_metadata$latent_flow_unit,
  n_latent_flows = result_metadata$n_latent_flows,
  latent_weak_identification_warning = latent_identifiability$weak_identification_warning %||% NA,
  min_observations_per_latent_flow = latent_identifiability$min_observations_per_latent_flow %||% NA,
  max_observations_per_latent_flow = latent_identifiability$max_observations_per_latent_flow %||% NA,
  n_unobserved_latent_flows = latent_identifiability$n_unobserved_latent_flows %||% NA,
  latent_source_effect_layer = result_metadata$latent_source_effect_layer,
  latent_time_effect_layer = result_metadata$latent_time_effect_layer,
  latent_backend_contract = result_metadata$latent_backend_contract,
  runtime_seconds = attr(fit, "runtime_seconds"),
  elapsed_sec = elapsed,
  convergence_status = convergence$status,
  rhat_max = convergence$rhat_max,
  n_eff_min = convergence$n_eff_min,
  divergences = convergence$divergences %||% NA,
  divergence_rate = convergence$divergence_rate %||% NA,
  treedepth_hits = convergence$treedepth_hits %||% NA,
  treedepth_hit_rate = convergence$treedepth_hit_rate %||% NA,
  ebfmi_min = convergence$ebfmi_min %||% NA,
  adapt_delta = convergence$adapt_delta %||% NA,
  max_treedepth = convergence$max_treedepth %||% NA,
  diagnostic_note = dplyr::case_when(
    is.finite(convergence$rhat_max) && convergence$rhat_max > 1.05 ~ "Inspect: max R-hat above 1.05",
    is.finite(convergence$n_eff_min) && convergence$n_eff_min < 100 ~ "Inspect: low minimum ESS",
    is.finite(convergence$divergences) && convergence$divergences > 0 ~ "Inspect: divergent transitions",
    is.finite(convergence$treedepth_hits) && convergence$treedepth_hits > 0 ~ "Inspect: tree-depth hits",
    is.finite(convergence$ebfmi_min) && convergence$ebfmi_min < 0.3 ~ "Inspect: low E-BFMI",
    TRUE ~ "No headline warning"
  ),
  mae = overall$mae,
  rmse = overall$rmse,
  pearson_r = overall$pearson_r,
  spearman_rho = overall$spearman_rho
)

selection <- metadata |>
  mutate(
    full_lad_support = .data$n_areas_loaded == length(area_set),
    repeated_source_support = .data$min_observations_per_latent_flow >= 2,
    diagnostics_ok = .data$convergence_status == "available" &
      (is.na(.data$rhat_max) | .data$rhat_max <= 1.05) &
      (is.na(.data$n_eff_min) | .data$n_eff_min >= 100) &
      (is.na(.data$divergences) | .data$divergences == 0) &
      (is.na(.data$treedepth_hits) | .data$treedepth_hits == 0) &
      (is.na(.data$ebfmi_min) | .data$ebfmi_min >= 0.3),
    include_in_primary = FALSE,
    include_in_sensitivity = .data$full_lad_support & .data$repeated_source_support,
    reporting_role = "latent repeated-source evidence",
    selection_rule = paste(
      "Latent rows are reported separately from complete-grid v07 methods because",
      "they use observed repeated-source OD support rather than the 97,969-row complete grid."
    )
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  adjusted_od,
  file.path(out_dir, "v07-validation-bayesian-latent-adjusted.csv"),
  row.names = FALSE
)
utils::write.csv(
  source_rows |>
    slice_head(n = 10),
  file.path(out_dir, "v07-validation-bayesian-latent-display.csv"),
  row.names = FALSE
)
utils::write.csv(
  metadata,
  file.path(out_dir, "v07-validation-bayesian-latent-metadata.csv"),
  row.names = FALSE
)
utils::write.csv(
  selection,
  file.path(out_dir, "v07-validation-bayesian-latent-selection.csv"),
  row.names = FALSE
)

message("Wrote v07 latent Bayesian validation output files to ", out_dir, ".")
