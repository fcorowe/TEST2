#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the debiasR package root.")
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(debiasR)
}

if (!requireNamespace("rstanarm", quietly = TRUE)) {
  stop("The `rstanarm` package is required to fit the v07 Bayesian example.")
}

formula_text <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = " ")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  as.integer(value)
}

env_n_areas <- function(name, default = Inf) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  if (tolower(value) %in% c("inf", "infinity", "all", "full")) {
    return(Inf)
  }
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 1L) {
    stop("`", name, "` must be a positive number or `Inf`.")
  }
  if (is.infinite(parsed)) {
    return(Inf)
  }
  as.integer(parsed)
}

env_methods <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  methods <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0L) {
    stop("`", name, "` must name at least one Bayesian method.")
  }
  methods
}

env_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  tolower(value) %in% c("1", "true", "yes", "y")
}

stable_fingerprint <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path))
}

input_fingerprint <- function(mpd_df,
                              coverage_df,
                              covariates_df,
                              distance_df) {
  stable_fingerprint(list(
    mpd_od = tibble::as_tibble(mpd_df) |>
      dplyr::arrange(.data$origin, .data$destination),
    coverage = tibble::as_tibble(coverage_df) |>
      dplyr::arrange(.data$origin, .data$destination),
    covariates = tibble::as_tibble(covariates_df) |>
      dplyr::arrange(.data$area),
    distance = tibble::as_tibble(distance_df) |>
      dplyr::arrange(.data$origin, .data$destination)
  ))
}

iter <- env_int("DEBIAS_V07_BAYES_ITER", 1000L)
chains <- env_int("DEBIAS_V07_BAYES_CHAINS", 4L)
seed_base <- env_int("DEBIAS_V07_BAYES_SEED", 20260626L)
n_areas <- env_n_areas("DEBIAS_V07_BAYES_N_AREAS", Inf)
out_dir <- Sys.getenv(
  "DEBIAS_V07_BAYES_OUT_DIR",
  unset = file.path("inst", "extdata")
)
refit_existing <- env_bool("DEBIAS_V07_BAYES_REFIT_EXISTING", FALSE)

example_data <- debiasR::debiasR_example_data(
  n_areas = n_areas,
  complete_grid = TRUE,
  geography = "lad"
)

mpd_od <- example_data$mpd_od
benchmark_od <- example_data$benchmark_od
coverage <- example_data$coverage
covariates <- example_data$covariates
distance <- example_data$distance
fingerprint <- input_fingerprint(
  mpd_df = mpd_od,
  coverage_df = coverage,
  covariates_df = covariates,
  distance_df = distance
)

expected_keys <- mpd_od |>
  select(origin, destination) |>
  distinct()
benchmark_keys <- benchmark_od |>
  select(origin, destination) |>
  distinct()

if (nrow(anti_join(expected_keys, benchmark_keys, by = c("origin", "destination"))) > 0L) {
  stop("Benchmark data do not cover all MPD validation rows.")
}
if (!isTRUE(example_data$od_audit$strict_square_support[1])) {
  stop("The v07 example requires a strict square OD grid.")
}

bayesian_specs_all <- list(
  bayes_gravity = list(
    label = "Bayesian gravity",
    role = "Gravity baseline",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "origin",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance
  ),
  bayes_gravity_rural = list(
    label = "Bayesian gravity + rural",
    role = "Gravity plus one interpretable area characteristic",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "origin",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d
  ),
  bayes_gravity_education = list(
    label = "Bayesian gravity + education",
    role = "Gravity plus two interpretable area characteristics",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "origin",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d
  ),
  bayes_origin_pool = list(
    label = "Bayesian origin pooling",
    role = "Flexible origin pooling",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "origin",
    random_intercept = "origin",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d +
      (1 | origin)
  ),
  bayes_gravity_both_cov = list(
    label = "Bayesian gravity, origin-destination coverage",
    role = "Coverage-scale sensitivity",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "both",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d
  ),
  bayes_gravity_dest_cov = list(
    label = "Bayesian gravity, destination coverage",
    role = "Coverage-scale sensitivity",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "destination",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d
  ),
  bayes_rf_gravity = list(
    label = "Bayesian reduced-form gravity",
    role = "MPD-scale reduced-form sensitivity",
    observation_model = "reduced_form",
    target_scale = "mpd_counterfactual",
    coverage_scale = "origin",
    random_intercept = "none",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d,
    bias_formula = ~ bias_e_origin
  ),
  bayes_corridor_pool = list(
    label = "Bayesian corridor pooling",
    role = "Corridor sensitivity",
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = "origin",
    random_intercept = "od",
    model_family = "poisson",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + (1 | od_id)
  )
)

selected_method_ids <- env_methods(
  "DEBIAS_V07_BAYES_METHODS",
  c(
    "bayes_gravity",
    "bayes_gravity_rural",
    "bayes_gravity_education",
    "bayes_origin_pool",
    "bayes_gravity_both_cov",
    "bayes_gravity_dest_cov",
    "bayes_rf_gravity"
  )
)
unknown_methods <- setdiff(selected_method_ids, names(bayesian_specs_all))
if (length(unknown_methods) > 0L) {
  stop(
    "Unknown `DEBIAS_V07_BAYES_METHODS` value(s): ",
    paste(unknown_methods, collapse = ", ")
  )
}
bayesian_specs <- bayesian_specs_all[selected_method_ids]

fit_one_spec <- function(method_id, spec, spec_index) {
  message("Fitting ", method_id, "...")
  start <- Sys.time()
  fit <- debiasR::adjust_multilevel_bayes(
    mpd_od_df = mpd_od,
    coverage_df = coverage,
    covariates_df = covariates,
    distance_df = distance,
    observation_model = spec$observation_model,
    target_scale = spec$target_scale,
    coverage_scale = spec$coverage_scale,
    mobility_formula = spec$mobility_formula,
    bias_formula = spec$bias_formula %||% ~ 0,
    model_family = spec$model_family,
    model_engine = "bayesian",
    random_intercept = spec$random_intercept,
    prediction_scope = "complete_grid",
    iter = iter,
    chains = chains,
    seed = seed_base + spec_index,
    refresh = 0
  )
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  out_keys <- fit |>
    select(origin, destination) |>
    distinct()
  if (nrow(anti_join(expected_keys, out_keys, by = c("origin", "destination"))) > 0L ||
      nrow(anti_join(out_keys, expected_keys, by = c("origin", "destination"))) > 0L) {
    stop("Output rows do not match validation rows for ", method_id, ".")
  }
  if (any(!is.finite(fit$flow_adj)) || any(fit$flow_adj < 0)) {
    stop("Non-finite or negative adjusted flows for ", method_id, ".")
  }

  diagnostics <- attr(fit, "diagnostics")
  convergence <- diagnostics$convergence
  result_metadata <- attr(fit, "result_metadata")
  overall <- debiasR::validate_flow_overall(
    adj_df = fit,
    benchmark_od_df = benchmark_od,
    comparisons = "adjusted_vs_benchmark",
    drop_zeros = FALSE,
    return_joined = FALSE,
    method_name = method_id
  )

  adjusted <- fit |>
    as_tibble() |>
    select(
      any_of(c(
        "origin",
        "destination",
        "mpd_source",
        "mpd_time",
        "mpd_observed",
        "mpd_zero_filled",
        "mpd_row_status",
        "prediction_scope",
        "model_fit_status",
        "flow",
        "flow_adj",
        "flow_mpd_pred",
        "flow_true_pred",
        "flow_adj_mean",
        "flow_adj_median",
        "flow_adj_q2.5",
        "flow_adj_q97.5",
        "flow_mpd_pred_mean",
        "flow_mpd_pred_median",
        "flow_mpd_pred_q2.5",
        "flow_mpd_pred_q97.5",
        "observation_probability",
        "latent_flow_id",
        "latent_flow_unit",
        "coverage_rate_o",
        "coverage_rate_d",
        "log_observation_probability",
        "distance_km",
        "log_distance",
        "bias_e_origin"
      ))
    ) |>
    mutate(
      method = method_id,
      .before = 1
    ) |>
    mutate(
      observation_model = spec$observation_model,
      target_scale = spec$target_scale,
      .after = method
    )

  display <- adjusted |>
    slice_head(n = 5)

  adjusted_validation <- adjusted |>
    select(
      method,
      observation_model,
      target_scale,
      origin,
      destination,
      flow,
      flow_adj
    )

  latent_identifiability <- result_metadata$latent_identifiability

  metadata <- tibble(
    method = method_id,
    method_label = spec$label,
    specification_role = spec$role,
    benchmark_used_in_fit = "No benchmark OD cells",
    coverage_scale = spec$coverage_scale,
    random_intercept = spec$random_intercept,
    mobility_formula = formula_text(spec$mobility_formula),
    bias_formula = formula_text(spec$bias_formula %||% ~ 0),
    resolved_formula = paste(attr(fit, "formula"), collapse = " "),
    backend = attr(fit, "backend"),
    model_engine = attr(fit, "model_engine"),
    model_family = attr(fit, "model_family"),
    target_scale = attr(fit, "target_scale"),
    observation_model = attr(fit, "observation_model"),
    prediction_scope = attr(fit, "prediction_scope"),
    iter = iter,
    chains = chains,
    seed = seed_base + spec_index,
    input_fingerprint = fingerprint,
    area_set = paste(sort(unique(mpd_od$origin)), collapse = ";"),
    geography = example_data$metadata$geography[1],
    n_areas_requested = n_areas,
    n_areas_loaded = example_data$metadata$n_areas[1],
    n_validation_rows = nrow(fit),
    n_fit_rows = result_metadata$n_fit_rows,
    n_prediction_rows = result_metadata$n_prediction_rows,
    n_mpd_zero_filled = example_data$metadata$n_mpd_zero_filled[1],
    n_benchmark_zero_filled = example_data$metadata$n_benchmark_zero_filled[1],
    scenario = result_metadata$scenario,
    repeated_observation = result_metadata$repeated_observation,
    n_sources = result_metadata$n_sources,
    n_time_periods = result_metadata$n_time_periods,
    source_col = result_metadata$source_col,
    time_col = result_metadata$time_col,
    flow_adj_summary = result_metadata$flow_adj_summary,
    offset_column = result_metadata$offset_column,
    distance_source = result_metadata$distance_source,
    n_input_rows = result_metadata$n_input_rows,
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
      is.finite(convergence$rhat_max) && convergence$rhat_max > 1.1 ~ "Inspect: max R-hat above 1.1",
      is.finite(convergence$n_eff_min) && convergence$n_eff_min < 50 ~ "Inspect: low minimum ESS",
      TRUE ~ "No headline warning"
    ),
    mae = overall$mae,
    rmse = overall$rmse,
    pearson_r = overall$pearson_r,
    spearman_rho = overall$spearman_rho
  )

  list(
    adjusted = adjusted_validation,
    display = display,
    metadata = metadata
  )
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

adjusted_path <- file.path(out_dir, "v07-validation-bayesian-adjusted.csv")
display_path <- file.path(out_dir, "v07-validation-bayesian-display.csv")
metadata_path <- file.path(out_dir, "v07-validation-bayesian-metadata.csv")
selection_path <- file.path(out_dir, "v07-validation-bayesian-selection.csv")

existing_adjusted <- tibble()
existing_display <- tibble()
existing_metadata <- tibble()

if (!refit_existing &&
    file.exists(adjusted_path) &&
    file.exists(display_path) &&
    file.exists(metadata_path)) {
  existing_adjusted <- tibble::as_tibble(
    utils::read.csv(adjusted_path, stringsAsFactors = FALSE)
  ) |>
    filter(.data$method %in% selected_method_ids)
  existing_display <- tibble::as_tibble(
    utils::read.csv(display_path, stringsAsFactors = FALSE)
  ) |>
    filter(.data$method %in% selected_method_ids)
  existing_metadata <- tibble::as_tibble(
    utils::read.csv(metadata_path, stringsAsFactors = FALSE)
  ) |>
    filter(.data$method %in% selected_method_ids)
}

existing_methods <- intersect(selected_method_ids, unique(existing_metadata$method))
fit_method_ids <- if (refit_existing) {
  selected_method_ids
} else {
  setdiff(selected_method_ids, existing_methods)
}

outputs <- Map(
  fit_one_spec,
  fit_method_ids,
  bayesian_specs[fit_method_ids],
  match(fit_method_ids, selected_method_ids)
)

adjusted_all <- bind_rows(
  existing_adjusted,
  bind_rows(lapply(outputs, `[[`, "adjusted"))
)
display_all <- bind_rows(
  existing_display,
  bind_rows(lapply(outputs, `[[`, "display"))
)
metadata_all <- bind_rows(
  existing_metadata,
  bind_rows(lapply(outputs, `[[`, "metadata"))
)

if (nrow(adjusted_all) == 0L || nrow(metadata_all) == 0L) {
  stop("No Bayesian validation outputs were available or generated.")
}

backfill_model_columns <- function(data, metadata) {
  if (!"observation_model" %in% names(data)) {
    data$observation_model <- NA_character_
  }
  if (!"target_scale" %in% names(data)) {
    data$target_scale <- NA_character_
  }
  data |>
    left_join(
      metadata |>
        select(
          method,
          metadata_observation_model = observation_model,
          metadata_target_scale = target_scale
        ),
      by = "method"
    ) |>
    mutate(
      observation_model = coalesce(.data$observation_model, .data$metadata_observation_model),
      target_scale = coalesce(.data$target_scale, .data$metadata_target_scale)
    ) |>
    select(-"metadata_observation_model", -"metadata_target_scale")
}

adjusted_all <- backfill_model_columns(adjusted_all, metadata_all)
display_all <- backfill_model_columns(display_all, metadata_all)

metric_rows <- bind_rows(lapply(split(adjusted_all, adjusted_all$method), function(adj) {
  method_id <- unique(adj$method)
  overall <- debiasR::validate_flow_overall(
    adj_df = adj,
    benchmark_od_df = benchmark_od,
    comparisons = "adjusted_vs_benchmark",
    drop_zeros = FALSE,
    return_joined = FALSE,
    method_name = method_id
  )
  tibble(
    method = method_id,
    mae = overall$mae,
    rmse = overall$rmse,
    pearson_r = overall$pearson_r,
    spearman_rho = overall$spearman_rho
  )
}))

metadata_all <- metadata_all |>
  select(-any_of(c("mae", "rmse", "pearson_r", "spearman_rho"))) |>
  left_join(metric_rows, by = "method")

selection_all <- metadata_all |>
  mutate(
    full_lad_support = .data$n_validation_rows == nrow(mpd_od) &
      .data$n_prediction_rows == nrow(mpd_od),
    diagnostics_ok = .data$convergence_status == "available" &
      (is.na(.data$rhat_max) | .data$rhat_max <= 1.05) &
      (is.na(.data$n_eff_min) | .data$n_eff_min >= 100),
    comparable_true_flow_scale = .data$target_scale == "true_flow" &
      .data$observation_model %in% c("coverage_offset", "latent_two_level"),
    selected_for_broad_comparison = .data$full_lad_support &
      .data$diagnostics_ok &
      .data$comparable_true_flow_scale,
    include_in_primary = .data$selected_for_broad_comparison,
    include_in_sensitivity = .data$observation_model == "reduced_form",
    reporting_role = case_when(
      .data$include_in_primary ~ "main true-flow comparison",
      .data$include_in_sensitivity ~ "MPD-scale sensitivity only",
      TRUE ~ "diagnostic record only"
    ),
    selection_rule = paste(
      "Main v07 Bayesian comparison requires full LAD row support, acceptable R-hat/ESS,",
      "and a true-flow-scale target. Reduced-form rows are reported as MPD-scale sensitivity checks."
    )
  )

order_methods <- function(data) {
  data |>
    mutate(.method_order = match(.data$method, selected_method_ids)) |>
    arrange(.data$.method_order, .data$origin, .data$destination) |>
    select(-.method_order)
}

adjusted_all <- order_methods(adjusted_all)
display_all <- order_methods(display_all)
metadata_all <- metadata_all |>
  mutate(.method_order = match(.data$method, selected_method_ids)) |>
  arrange(.data$.method_order) |>
  select(-.method_order)
selection_all <- selection_all |>
  mutate(.method_order = match(.data$method, selected_method_ids)) |>
  arrange(.data$.method_order) |>
  select(-.method_order)

utils::write.csv(
  adjusted_all,
  adjusted_path,
  row.names = FALSE
)
utils::write.csv(
  display_all,
  display_path,
  row.names = FALSE
)
utils::write.csv(
  metadata_all,
  metadata_path,
  row.names = FALSE
)
utils::write.csv(
  selection_all,
  selection_path,
  row.names = FALSE
)

message("Wrote v07 Bayesian validation output files to ", out_dir, ".")
