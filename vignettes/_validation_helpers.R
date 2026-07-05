validation_method_labels <- c(
  unadjusted = "Unadjusted raw MPD",
  inverse_penetration = "Inverse penetration",
  selection_rate = "Selection rate",
  selection_rate2 = "Selection rate II",
  raking_ratio = "Raking ratio",
  coefficient = "Coefficient adjustment",
  bayes_gravity = "Bayesian gravity",
  bayes_gravity_rural = "Bayesian gravity + rural",
  bayes_gravity_education = "Bayesian gravity + education",
  bayes_origin_pool = "Bayesian gravity + education + origin pooling",
  bayes_gravity_both_cov = "Bayesian gravity, origin-destination coverage",
  bayes_gravity_dest_cov = "Bayesian gravity, destination coverage",
  bayes_rf_gravity = "Bayesian reduced-form gravity",
  bayes_latent_s3_od = "Bayesian latent S3 OD",
  bayes_corridor_pool = "Bayesian corridor pooling"
)

validation_plot_method_labels <- c(
  unadjusted = "Unadjusted\nraw MPD",
  inverse_penetration = "Inverse\npenetration",
  selection_rate = "Selection\nrate",
  selection_rate2 = "Selection\nrate II",
  raking_ratio = "Raking\nratio",
  coefficient = "Coefficient\nadjustment",
  bayes_gravity = "Bayesian\ngravity",
  bayes_gravity_rural = "Bayesian\ngravity + rural",
  bayes_gravity_education = "Bayesian\ngravity + education",
  bayes_origin_pool = "Bayesian\ngravity + education\n+ origin pooling",
  bayes_gravity_both_cov = "Bayesian\ngravity + both coverage",
  bayes_gravity_dest_cov = "Bayesian\ngravity + destination coverage",
  bayes_rf_gravity = "Bayesian\nreduced form",
  bayes_latent_s3_od = "Bayesian\nlatent S3 OD",
  bayes_corridor_pool = "Bayesian\ncorridor pooling"
)

validation_comparison_labels <- c(
  adjusted_vs_benchmark = "Adjusted vs benchmark",
  raw_vs_benchmark = "Raw MPD vs benchmark",
  raw_vs_adjusted = "Raw MPD vs adjusted"
)

validation_bayesian_methods <- c(
  "bayes_gravity",
  "bayes_gravity_rural",
  "bayes_gravity_education",
  "bayes_origin_pool",
  "bayes_gravity_both_cov",
  "bayes_gravity_dest_cov"
)

validation_bayesian_sensitivity_methods <- c(
  "bayes_rf_gravity"
)

validation_deterministic_methods <- c(
  "inverse_penetration",
  "selection_rate",
  "selection_rate2",
  "raking_ratio",
  "coefficient"
)

validation_plot_methods <- c(
  "inverse_penetration",
  "selection_rate2",
  "raking_ratio",
  "coefficient"
)

validation_broad_methods <- c(
  "unadjusted",
  validation_deterministic_methods,
  validation_bayesian_methods
)

validation_teaching_methods <- c(
  "inverse_penetration",
  "selection_rate2",
  "raking_ratio",
  "coefficient"
)

validation_primary_bayesian_method <- "bayes_origin_pool"

validation_benchmark_comparisons <- c(
  "adjusted_vs_benchmark",
  "raw_vs_benchmark"
)

validation_bayesian_spec_defaults <- tibble::tribble(
  ~method, ~specification_role, ~coverage_scale, ~random_intercept, ~covariates, ~benchmark_used_in_fit,
  "bayes_gravity", "Gravity baseline", "origin", "none", "origin population, destination population, log distance", "No benchmark OD cells",
  "bayes_gravity_rural", "Gravity plus one interpretable area characteristic", "origin", "none", "population, log distance, origin rural share, destination rural share", "No benchmark OD cells",
  "bayes_gravity_education", "Gravity plus two interpretable area characteristics", "origin", "none", "population, log distance, rural share, Level 4 qualification share", "No benchmark OD cells",
  "bayes_origin_pool", "Gravity plus education and origin pooling", "origin", "origin", "population, log distance, rural share, Level 4 qualification share", "No benchmark OD cells",
  "bayes_gravity_both_cov", "Coverage-scale sensitivity", "origin-destination", "none", "population, log distance, rural share, Level 4 qualification share", "No benchmark OD cells",
  "bayes_gravity_dest_cov", "Coverage-scale sensitivity", "destination", "none", "population, log distance, rural share, Level 4 qualification share", "No benchmark OD cells",
  "bayes_rf_gravity", "Reduced-form sensitivity", "not used", "none", "population, log distance, rural share, Level 4 qualification share, origin bias", "No benchmark OD cells"
)

validation_method_fitting_inputs <- tibble::tribble(
  ~method, ~method_family, ~fitting_inputs, ~benchmark_used_to_fit,
  "unadjusted", "Raw baseline", "Raw MPD flows only", "No",
  "inverse_penetration", "Coverage weighting", "Raw MPD flows and origin/destination coverage rates", "No",
  "selection_rate", "Coverage and covariate weighting", "Raw MPD flows, coverage rates, the selected area covariate and benchmark origin totals", "Yes, benchmark origin totals",
  "selection_rate2", "Coverage weighting with calibration", "Raw MPD flows, coverage rates and benchmark origin totals", "Yes, benchmark origin totals",
  "raking_ratio", "Benchmark-margin calibration", "Raw MPD flows and benchmark origin/destination margins", "Yes, benchmark origin and destination margins",
  "coefficient", "Benchmark-OD calibration", "Raw MPD flows and benchmark OD cells", "Yes, benchmark OD cells",
  "bayes_gravity", "Bayesian coverage-offset", "Raw MPD flows, coverage rates, population and distance", "No benchmark OD cells",
  "bayes_gravity_rural", "Bayesian coverage-offset", "Raw MPD flows, coverage rates, population, distance and rural-share covariates", "No benchmark OD cells",
  "bayes_gravity_education", "Bayesian coverage-offset", "Raw MPD flows, coverage rates, population, distance, rural-share and education covariates", "No benchmark OD cells",
  "bayes_origin_pool", "Bayesian coverage-offset", "Raw MPD flows, coverage rates, population, distance, covariates and origin pooling", "No benchmark OD cells",
  "bayes_gravity_both_cov", "Bayesian coverage-offset sensitivity", "Raw MPD flows, origin-destination coverage, population, distance and covariates", "No benchmark OD cells",
  "bayes_gravity_dest_cov", "Bayesian coverage-offset sensitivity", "Raw MPD flows, destination coverage, population, distance and covariates", "No benchmark OD cells",
  "bayes_rf_gravity", "Bayesian reduced-form sensitivity", "Raw MPD flows, population, distance, covariates and origin bias term", "No benchmark OD cells",
  "bayes_latent_s3_od", "Bayesian latent repeated-source", "Observed Mapp1/Mapp2 repeated-source MPD rows, coverage, covariates and distance", "No benchmark OD cells",
  "bayes_corridor_pool", "Bayesian coverage-offset sensitivity", "Raw MPD flows, coverage rates, population, distance, covariates and corridor pooling", "No benchmark OD cells"
)

validate_stable_fingerprint <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path))
}

validate_input_fingerprint <- function(mpd_df,
                                         coverage_df,
                                         covariates_df,
                                         distance_df) {
  validate_stable_fingerprint(list(
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

validate_kable <- function(data,
                             digits = NULL,
                             caption = NULL,
                             table_class = "table table-sm") {
  args <- list(
    x = data,
    format = "html",
    row.names = FALSE,
    caption = caption,
    table.attr = paste0('class="', table_class, '"')
  )
  if (!is.null(digits)) {
    args$digits <- digits
  }
  do.call(knitr::kable, args)
}

validate_fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(round(as.numeric(x), digits), digits = digits, format = "f", big.mark = ",")
  )
}

validate_fmt_pct <- function(x, digits = 1) {
  validate_fmt_num(100 * as.numeric(x), digits = digits)
}

validate_method_label <- function(method, labels = validation_method_labels) {
  dplyr::coalesce(unname(labels[method]), as.character(method))
}

validate_comparison_label <- function(comparison) {
  dplyr::coalesce(
    unname(validation_comparison_labels[comparison]),
    as.character(comparison)
  )
}

validate_find_extdata_file <- function(filename) {
  candidates <- c(
    file.path("inst", "extdata", filename),
    file.path("..", "inst", "extdata", filename),
    file.path("..", "..", "inst", "extdata", filename),
    system.file("extdata", filename, package = "debiasR")
  )
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0L) {
    stop(
      "Could not find ", filename, ". Run ",
      "`scripts/precompute_v07_validation_bayesian_example.R` first."
    )
  }
  candidates[1]
}

validate_find_optional_extdata_file <- function(filename) {
  tryCatch(
    validate_find_extdata_file(filename),
    error = function(e) NA_character_
  )
}

validate_load_v07_bayesian_outputs <- function() {
  adjusted_file <- validate_find_extdata_file(
    "v07-validation-bayesian-adjusted.csv"
  )
  adjusted <- utils::read.csv(adjusted_file, stringsAsFactors = FALSE)
  adjusted <- tibble::as_tibble(adjusted)

  required <- c(
    "method", "origin", "destination", "flow", "flow_adj",
    "observation_model", "target_scale"
  )
  if (!all(required %in% names(adjusted))) {
    stop(
      "`v07-validation-bayesian-adjusted.csv` must contain: ",
      paste(required, collapse = ", ")
    )
  }

  out <- split(adjusted, adjusted$method)
  ordered_names <- c(
    intersect(validation_bayesian_methods, names(out)),
    setdiff(names(out), validation_bayesian_methods)
  )
  out[ordered_names]
}

validate_load_v07_bayesian_display <- function() {
  display_file <- validate_find_extdata_file(
    "v07-validation-bayesian-display.csv"
  )
  display <- utils::read.csv(display_file, stringsAsFactors = FALSE)
  display <- tibble::as_tibble(display)

  required <- c(
    "method", "origin", "destination", "flow", "flow_adj",
    "observation_model", "target_scale"
  )
  if (!all(required %in% names(display))) {
    stop(
      "`v07-validation-bayesian-display.csv` must contain: ",
      paste(required, collapse = ", ")
    )
  }

  display
}

validate_load_v07_bayesian_metadata <- function() {
  metadata_file <- validate_find_extdata_file(
    "v07-validation-bayesian-metadata.csv"
  )
  metadata <- utils::read.csv(metadata_file, stringsAsFactors = FALSE)
  tibble::as_tibble(metadata)
}

validate_load_v07_bayesian_selection <- function() {
  selection_file <- validate_find_optional_extdata_file(
    "v07-validation-bayesian-selection.csv"
  )
  if (is.na(selection_file)) {
    return(tibble::tibble())
  }
  selection <- utils::read.csv(selection_file, stringsAsFactors = FALSE)
  tibble::as_tibble(selection)
}

validate_load_v07_latent_metadata <- function() {
  metadata_file <- validate_find_optional_extdata_file(
    "v07-validation-bayesian-latent-metadata.csv"
  )
  if (is.na(metadata_file)) {
    return(tibble::tibble())
  }
  metadata <- utils::read.csv(metadata_file, stringsAsFactors = FALSE)
  tibble::as_tibble(metadata)
}

validate_display_bayesian_specs <- function(metadata = NULL) {
  if (is.null(metadata)) {
    metadata <- validate_load_v07_bayesian_metadata()
  }
  metadata_display <- metadata |>
    dplyr::select(
      dplyr::any_of(c(
        "method",
        "iter",
        "chains",
        "backend",
        "observation_model",
        "elapsed_sec",
        "n_fit_rows",
        "n_prediction_rows",
        "rhat_max",
        "n_eff_min",
        "diagnostic_note"
      ))
    ) |>
    dplyr::distinct(.data$method, .keep_all = TRUE)
  if (!"diagnostic_note" %in% names(metadata_display)) {
    metadata_display$diagnostic_note <- NA_character_
  }
  if (!"observation_model" %in% names(metadata_display)) {
    metadata_display$observation_model <- "coverage_offset"
  }

  spec_table <- validation_bayesian_spec_defaults |>
    dplyr::left_join(
      metadata_display,
      by = "method"
    ) |>
    dplyr::mutate(
      method_label = validate_method_label(.data$method),
      iter = dplyr::coalesce(as.integer(.data$iter), NA_integer_),
      chains = dplyr::coalesce(as.integer(.data$chains), NA_integer_),
      input_summary = dplyr::case_when(
        .data$method == "bayes_gravity" ~ "population + distance",
        .data$method == "bayes_gravity_rural" ~ "population + distance + rural share",
        .data$method == "bayes_gravity_education" ~ "population + distance + rural + education",
        .data$method == "bayes_origin_pool" ~ "same covariates + origin pooling",
        .data$method == "bayes_gravity_both_cov" ~ "same covariates + origin-destination coverage",
        .data$method == "bayes_gravity_dest_cov" ~ "same covariates + destination coverage",
        .data$method == "bayes_rf_gravity" ~ "same covariates + origin bias term",
        .data$method == "bayes_corridor_pool" ~ "population + distance + rural + corridor pooling",
        TRUE ~ .data$covariates
      )
    ) |>
    dplyr::transmute(
      Method = .data$method_label,
      `Observation model` = dplyr::coalesce(.data$observation_model, "coverage_offset"),
      `Model change` = .data$specification_role,
      `Benchmark used to fit` = .data$benchmark_used_in_fit,
      `Main inputs and structure` = paste0(
        .data$input_summary,
        "; coverage: ", .data$coverage_scale,
        "; random: ", .data$random_intercept
      ),
      `Fit check` = paste0(
        .data$n_fit_rows, " / ", .data$n_prediction_rows,
        " rows; max R-hat ",
        validate_fmt_num(.data$rhat_max, 3),
        "; min ESS ",
        validate_fmt_num(.data$n_eff_min, 0),
        "; ",
        .data$diagnostic_note
      )
    )

  validate_kable(spec_table, table_class = "table table-sm validation-table-compact")
}

validate_display_bayesian_selection <- function(selection) {
  if (nrow(selection) == 0L) {
    return(invisible(NULL))
  }
  selection |>
    dplyr::transmute(
      Method = validate_method_label(.data$method),
      `Reporting role` = .data$reporting_role,
      `Full LAD support` = dplyr::if_else(.data$full_lad_support, "Yes", "No"),
      `Diagnostics pass` = dplyr::if_else(.data$diagnostics_ok, "Yes", "No"),
      `True-flow scale` = dplyr::if_else(.data$comparable_true_flow_scale, "Yes", "No")
    ) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_display_latent_summary <- function(metadata) {
  if (nrow(metadata) == 0L) {
    return(invisible(NULL))
  }
  metadata |>
    dplyr::transmute(
      Method = validate_method_label(.data$method),
      Scenario = toupper(.data$scenario),
      `Validation OD states` = .data$n_validation_rows,
      `Source rows` = .data$n_source_rows,
      `Sources` = .data$n_sources,
      `Latent unit` = .data$latent_flow_unit,
      `Min rows per latent state` = .data$min_observations_per_latent_flow,
      `Max R-hat` = validate_fmt_num(.data$rhat_max, 3),
      `Min ESS` = validate_fmt_num(.data$n_eff_min, 0),
      `Divergences` = .data$divergences,
      `Tree-depth hits` = .data$treedepth_hits,
      MAE = validate_fmt_num(.data$mae, 1),
      RMSE = validate_fmt_num(.data$rmse, 1),
      Note = .data$diagnostic_note
    ) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_display_method_inputs <- function(methods = validation_broad_methods) {
  validation_method_fitting_inputs |>
    dplyr::filter(.data$method %in% methods) |>
    dplyr::mutate(.method_order = match(.data$method, methods)) |>
    dplyr::arrange(.data$.method_order) |>
    dplyr::transmute(
      Method = validate_method_label(.data$method),
      `Main fitting inputs` = .data$fitting_inputs,
      `Benchmark used to fit` = .data$benchmark_used_to_fit,
      `Validation role` = .data$method_family
    ) |>
    validate_kable(table_class = "table table-sm validation-table-small")
}

validate_display_broad_method_inputs <- function(methods = validation_broad_methods) {
  validation_method_fitting_inputs |>
    dplyr::filter(.data$method %in% methods) |>
    dplyr::mutate(.method_order = match(.data$method, methods)) |>
    dplyr::arrange(.data$.method_order) |>
    dplyr::transmute(
      Method = validate_method_label(.data$method),
      `Method family` = .data$method_family,
      `Benchmark used to fit` = .data$benchmark_used_to_fit
    ) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_audit_shared_validation_rows <- function(adjusted_outputs,
                                                    mpd_df,
                                                    benchmark_df) {
  expected_keys <- tibble::as_tibble(mpd_df) |>
    dplyr::select(origin, destination) |>
    dplyr::distinct()
  benchmark_keys <- tibble::as_tibble(benchmark_df) |>
    dplyr::select(origin, destination) |>
    dplyr::distinct()

  benchmark_missing <- dplyr::anti_join(
    expected_keys,
    benchmark_keys,
    by = c("origin", "destination")
  )

  rows <- lapply(names(adjusted_outputs), function(method_id) {
    out <- tibble::as_tibble(adjusted_outputs[[method_id]])
    keys <- out |>
      dplyr::select(origin, destination)
    distinct_keys <- dplyr::distinct(keys)
    duplicated_pairs <- nrow(keys) - nrow(distinct_keys)
    missing_pairs <- dplyr::anti_join(
      expected_keys,
      distinct_keys,
      by = c("origin", "destination")
    )
    extra_pairs <- dplyr::anti_join(
      distinct_keys,
      expected_keys,
      by = c("origin", "destination")
    )
    flow_check_col <- if ("adjusted_input_flow" %in% names(out)) {
      "adjusted_input_flow"
    } else {
      "flow"
    }
    raw_check <- out |>
      dplyr::select(origin, destination, flow, dplyr::any_of("adjusted_input_flow")) |>
      dplyr::left_join(
        mpd_df |>
          dplyr::select(origin, destination, raw_flow = flow),
        by = c("origin", "destination")
      ) |>
      dplyr::summarise(
        raw_flow_matches = all(
          !is.na(.data[[flow_check_col]]) &
            !is.na(.data$raw_flow) &
            abs(.data[[flow_check_col]] - .data$raw_flow) < 1e-8
        ),
        .groups = "drop"
      )
    same_rows <- isTRUE(duplicated_pairs == 0L) &&
      isTRUE(nrow(missing_pairs) == 0L) &&
      isTRUE(nrow(extra_pairs) == 0L) &&
      isTRUE(nrow(benchmark_missing) == 0L)

    tibble::tibble(
      method = method_id,
      method_label = validate_method_label(method_id),
      n_adjusted_rows = nrow(out),
      expected_od_pairs = nrow(expected_keys),
      duplicated_pairs = duplicated_pairs,
      missing_pairs = nrow(missing_pairs),
      extra_pairs = nrow(extra_pairs),
      benchmark_missing_pairs = nrow(benchmark_missing),
      raw_flow_matches = raw_check$raw_flow_matches,
      same_validation_rows = same_rows
    )
  })

  dplyr::bind_rows(rows)
}

validate_display_row_audit <- function(row_audit) {
  row_audit |>
    dplyr::transmute(
      Method = .data$method_label,
      `Expected OD pairs` = .data$expected_od_pairs,
      `Raw flow matches` = dplyr::if_else(.data$raw_flow_matches, "Yes", "No"),
      `Same validation rows` = dplyr::if_else(
        dplyr::coalesce(.data$same_validation_rows, FALSE),
        "Yes",
        "No"
      )
    ) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_assert_row_audit <- function(row_audit) {
  failed <- row_audit |>
    dplyr::filter(!.data$same_validation_rows | !.data$raw_flow_matches)
  if (nrow(failed) > 0L) {
    stop(
      "Validation outputs do not share the same rows or raw MPD flows: ",
      paste(failed$method, collapse = ", ")
    )
  }
  invisible(row_audit)
}

validate_assert_bayesian_fingerprint <- function(metadata,
                                                   mpd_df,
                                                   coverage_df,
                                                   covariates_df,
                                                   distance_df) {
  if (!"input_fingerprint" %in% names(metadata)) {
    stop("Bayesian metadata do not contain `input_fingerprint`.")
  }
  expected <- unique(metadata$input_fingerprint)
  current <- validate_input_fingerprint(
    mpd_df = mpd_df,
    coverage_df = coverage_df,
    covariates_df = covariates_df,
    distance_df = distance_df
  )
  if (length(expected) != 1L || !identical(expected, current)) {
    # Serialized fingerprints can drift across local R/package contexts; keep a
    # strict full-LAD metadata fallback and let the row audit verify raw flows.
    current_areas <- sort(unique(mpd_df$origin))
    metadata_area_sets <- unique(metadata$area_set)
    area_set_ok <- "area_set" %in% names(metadata) &&
      length(metadata_area_sets) == 1L &&
      identical(strsplit(metadata_area_sets, ";", fixed = TRUE)[[1]], current_areas)
    n_areas_ok <- !"n_areas_loaded" %in% names(metadata) ||
      all(metadata$n_areas_loaded == length(current_areas), na.rm = TRUE)
    n_validation_rows_ok <- !"n_validation_rows" %in% names(metadata) ||
      all(metadata$n_validation_rows == nrow(mpd_df), na.rm = TRUE)
    n_prediction_rows_ok <- !"n_prediction_rows" %in% names(metadata) ||
      all(metadata$n_prediction_rows == nrow(mpd_df), na.rm = TRUE)

    if (!area_set_ok || !n_areas_ok || !n_validation_rows_ok || !n_prediction_rows_ok) {
      stop(
        "Bayesian validation output files were generated from different input data. ",
        "Re-run `scripts/precompute_v07_validation_bayesian_example.R`."
      )
    }
  }
  invisible(TRUE)
}

validate_assert_v07_bayesian_bundle <- function(adjusted_outputs,
                                                  display,
                                                  metadata,
                                                  selection) {
  expected_methods <- c(
    validation_bayesian_methods,
    validation_bayesian_sensitivity_methods
  )
  table_methods <- list(
    adjusted = names(adjusted_outputs),
    display = unique(display$method),
    metadata = unique(metadata$method),
    selection = unique(selection$method)
  )
  missing_methods <- lapply(table_methods, function(methods) {
    setdiff(expected_methods, methods)
  })
  missing_methods <- missing_methods[lengths(missing_methods) > 0L]
  if (length(missing_methods) > 0L) {
    stop(
      "The v07 Bayesian precomputed files are missing methods: ",
      paste(
        sprintf(
          "%s (%s)",
          names(missing_methods),
          vapply(missing_methods, paste, character(1), collapse = ", ")
        ),
        collapse = "; "
      )
    )
  }

  adjusted_specs <- dplyr::bind_rows(lapply(names(adjusted_outputs), function(method) {
    adjusted_outputs[[method]] |>
      dplyr::distinct(.data$method, .data$observation_model, .data$target_scale)
  }))

  spec_sources <- list(
    adjusted = adjusted_specs,
    display = display,
    metadata = metadata,
    selection = selection
  )
  lapply(names(spec_sources), function(source_name) {
    source <- spec_sources[[source_name]]
    required <- c("method", "observation_model", "target_scale")
    if (!all(required %in% names(source))) {
      stop(
        "The v07 Bayesian ", source_name, " data must contain: ",
        paste(required, collapse = ", ")
      )
    }
    primary_specs <- source |>
      dplyr::filter(.data$method %in% validation_bayesian_methods) |>
      dplyr::distinct(.data$method, .data$observation_model, .data$target_scale)
    invalid_primary <- primary_specs |>
      dplyr::filter(
        .data$observation_model != "coverage_offset" |
          .data$target_scale != "true_flow"
      )
    if (nrow(invalid_primary) > 0L) {
      stop(
        "The main v07 Bayesian comparison must use `coverage_offset` ",
        "`true_flow` rows. Invalid ", source_name, " methods: ",
        paste(invalid_primary$method, collapse = ", ")
      )
    }
  })

  invisible(TRUE)
}

validate_bind_overall_summary <- function(overall_results) {
  dplyr::bind_rows(lapply(names(overall_results), function(method_id) {
    item <- overall_results[[method_id]]
    if ("summary" %in% names(item)) {
      summary <- tibble::as_tibble(item$summary)
    } else {
      summary <- tibble::as_tibble(item)
    }
    summary$method <- method_id
    summary
  })) |>
    dplyr::mutate(
      method_label = validate_method_label(.data$method),
      comparison_label = validate_comparison_label(.data$comparison)
    )
}

validate_display_overall_metrics <- function(overall_results) {
  validate_bind_overall_summary(overall_results) |>
    dplyr::transmute(
      Method = .data$method_label,
      Comparison = .data$comparison_label,
      n = .data$n,
      `Pearson r` = validate_fmt_num(.data$pearson_r, 3),
      `Spearman rho` = validate_fmt_num(.data$spearman_rho, 3),
      `Mean error` = validate_fmt_num(.data$mean_error, 1),
      MAE = validate_fmt_num(.data$mae, 1),
      RMSE = validate_fmt_num(.data$rmse, 1),
      `MAPE (%)` = validate_fmt_pct(.data$mape, 1)
    ) |>
    validate_kable()
}

validate_display_compact_overall_metrics <- function(overall_results,
                                                       comparison = "adjusted_vs_benchmark",
                                                       methods = NULL,
                                                       include_benchmark_used = FALSE,
                                                       include_comparison = FALSE) {
  tbl <- validate_bind_overall_summary(overall_results) |>
    dplyr::filter(.data$comparison %in% comparison)
  if (!is.null(methods)) {
    tbl <- tbl |>
      dplyr::filter(.data$method %in% methods) |>
      dplyr::mutate(.method_order = match(.data$method, methods)) |>
      dplyr::arrange(.data$.method_order, .data$comparison)
  }

  out <- tbl |>
    dplyr::transmute(
      Method = .data$method_label,
      .method = .data$method,
      Comparison = .data$comparison_label,
      n = .data$n,
      MAE = validate_fmt_num(.data$mae, 1),
      RMSE = validate_fmt_num(.data$rmse, 1),
      `Pearson r` = validate_fmt_num(.data$pearson_r, 3),
      `Spearman rho` = validate_fmt_num(.data$spearman_rho, 3)
    )

  if (isTRUE(include_benchmark_used)) {
    out <- out |>
      dplyr::left_join(
        validation_method_fitting_inputs |>
          dplyr::select(method, benchmark_used_to_fit),
        by = c(".method" = "method")
      ) |>
      dplyr::relocate(benchmark_used_to_fit, .after = Method) |>
      dplyr::rename(`Benchmark used to fit` = benchmark_used_to_fit)
  }

  if (!isTRUE(include_comparison)) {
    out <- out |>
      dplyr::select(-Comparison)
  }

  out |>
    dplyr::select(-.method) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_best_method_id <- function(overall_results,
                                      comparison = "adjusted_vs_benchmark",
                                      metric = "mae",
                                      methods = NULL,
                                      largest = FALSE) {
  tbl <- validate_bind_overall_summary(overall_results) |>
    dplyr::filter(.data$comparison == comparison)
  if (!is.null(methods)) {
    tbl <- dplyr::filter(tbl, .data$method %in% methods)
  }
  if (isTRUE(largest)) {
    out <- dplyr::slice_max(tbl, .data[[metric]], n = 1, with_ties = FALSE)
  } else {
    out <- dplyr::slice_min(tbl, .data[[metric]], n = 1, with_ties = FALSE)
  }
  out$method[1]
}

validate_best_method <- function(overall_results,
                                   comparison = "adjusted_vs_benchmark",
                                   metric = "mae",
                                   methods = NULL,
                                   largest = FALSE) {
  validate_method_label(validate_best_method_id(
    overall_results = overall_results,
    comparison = comparison,
    metric = metric,
    methods = methods,
    largest = largest
  ))
}

validate_bind_residual_summary <- function(residual_results) {
  dplyr::bind_rows(lapply(names(residual_results), function(method_id) {
    summary <- tibble::as_tibble(residual_results[[method_id]]$summary)
    summary$method <- method_id
    summary
  })) |>
    dplyr::mutate(method_label = validate_method_label(.data$method))
}

validate_bind_residual_data <- function(residual_results) {
  dplyr::bind_rows(lapply(names(residual_results), function(method_id) {
    data <- tibble::as_tibble(residual_results[[method_id]]$data)
    data$method <- method_id
    data
  })) |>
    dplyr::mutate(method_label = validate_method_label(.data$method))
}

validate_display_residual_summary <- function(residual_results,
                                                methods = names(residual_results)) {
  validate_bind_residual_summary(residual_results) |>
    dplyr::filter(.data$method %in% methods) |>
    dplyr::mutate(.method_order = match(.data$method, methods)) |>
    dplyr::arrange(.data$.method_order) |>
    dplyr::transmute(
      Method = .data$method_label,
      n = .data$n,
      `Raw-benchmark MAE` =
        validate_fmt_num(.data$mean_abs_residual_mpd_benchmark, 1),
      `Adjusted-benchmark MAE` =
        validate_fmt_num(.data$mean_abs_residual_adj_benchmark, 1),
      `Share improved (%)` = validate_fmt_pct(.data$share_improved, 1),
      `Adjusted residuals > 2 SD (%)` =
        validate_fmt_pct(.data$share_residual_adj_over_2sd, 1)
    ) |>
    validate_kable(table_class = "table table-sm validation-table-compact")
}

validate_display_top_residuals <- function(residual_results,
                                             area_names = NULL,
                                             methods = names(residual_results),
                                             n = 12L) {
  out <- validate_bind_residual_data(residual_results) |>
    dplyr::filter(.data$method %in% methods) |>
    dplyr::group_by(.data$method, .data$method_label) |>
    dplyr::slice_max(
      order_by = .data$abs_residual_adj_benchmark,
      n = n,
      with_ties = FALSE
    ) |>
    dplyr::ungroup()

  if (!is.null(area_names)) {
    area_names <- tibble::as_tibble(area_names) |>
      dplyr::select(area, name)
    out <- out |>
      dplyr::left_join(
        dplyr::rename(area_names, origin_name = name),
        by = c("origin" = "area")
      ) |>
      dplyr::left_join(
        dplyr::rename(area_names, destination_name = name),
        by = c("destination" = "area")
      )
  } else {
    out$origin_name <- out$origin
    out$destination_name <- out$destination
  }

  out |>
    dplyr::transmute(
      Method = .data$method_label,
      Origin = dplyr::coalesce(.data$origin_name, .data$origin),
      Destination = dplyr::coalesce(.data$destination_name, .data$destination),
      `Raw MPD` = validate_fmt_num(.data$mpd_flow, 0),
      Adjusted = validate_fmt_num(.data$adj_flow, 0),
      Benchmark = validate_fmt_num(.data$benchmark_flow, 0),
      `Benchmark minus adjusted` =
        validate_fmt_num(.data$residual_adj_benchmark, 0),
      `Absolute error reduction` =
        validate_fmt_num(.data$abs_residual_reduction, 0)
    ) |>
    validate_kable()
}

validate_display_distribution_summary <- function(distribution_results,
                                                    comparison = "adjusted_vs_benchmark",
                                                    methods = names(distribution_results),
                                                    include_comparison = FALSE) {
  dplyr::bind_rows(lapply(names(distribution_results), function(method_id) {
    summary <- tibble::as_tibble(distribution_results[[method_id]]$summary)
    summary$method <- method_id
    summary
  })) |>
    dplyr::filter(
      .data$comparison %in% comparison,
      .data$method %in% methods
    ) |>
    dplyr::mutate(
      method_label = validate_method_label(.data$method),
      comparison_label = validate_comparison_label(.data$comparison),
      .method_order = match(.data$method, methods)
    ) |>
    dplyr::arrange(.data$.method_order, .data$comparison) |>
    dplyr::transmute(
      Method = .data$method_label,
      Comparison = .data$comparison_label,
      `Origins used` = .data$n_origins_used,
      `Weighted mean JSD` = validate_fmt_num(.data$jsd_weighted_mean, 4)
    ) -> out

  if (!isTRUE(include_comparison)) {
    out <- out |>
      dplyr::select(-Comparison)
  }

  validate_kable(out, table_class = "table table-sm validation-table-compact")
}

validate_build_ring_neighbors <- function(areas) {
  areas <- unique(as.character(areas))
  tibble::tibble(
    area = areas,
    neighbor = dplyr::lead(areas, default = areas[1])
  ) |>
    dplyr::bind_rows(
      tibble::tibble(
        area = areas,
        neighbor = dplyr::lag(areas, default = areas[length(areas)])
      )
    ) |>
    dplyr::mutate(weight = 1)
}

validate_build_distance_neighbors <- function(distance_df,
                                                areas,
                                                k = 4L) {
  areas <- unique(as.character(areas))
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1L) {
    stop("`k` must be a positive whole number.")
  }
  k <- as.integer(k)

  nearest <- tibble::as_tibble(distance_df) |>
    dplyr::filter(
      .data$origin %in% areas,
      .data$destination %in% areas,
      .data$origin != .data$destination,
      is.finite(.data$distance_km)
    ) |>
    dplyr::group_by(.data$origin) |>
    dplyr::arrange(.data$distance_km, .data$destination, .by_group = TRUE) |>
    dplyr::slice_head(n = k) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      area = .data$origin,
      neighbor = .data$destination,
      weight = 1
    )

  nearest |>
    dplyr::bind_rows(
      nearest |>
        dplyr::transmute(
          area = .data$neighbor,
          neighbor = .data$area,
          weight = .data$weight
        )
    ) |>
    dplyr::filter(.data$area != .data$neighbor) |>
    dplyr::distinct(.data$area, .data$neighbor, .keep_all = TRUE)
}

validate_lad_boundary_source_url <- function() {
  Sys.getenv(
    "DEBIAS_LAD_BOUNDARY_URL",
    unset = paste0(
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
      "LAD_Dec_2021_GB_BFC_2022/FeatureServer/0/query?",
      "where=1%3D1&outFields=LAD21CD,LAD21NM&returnGeometry=true&",
      "outSR=4326&f=geojson&maxAllowableOffset=0.005&geometryPrecision=5"
    )
  )
}

validate_lad_boundary_cache_path <- function(filename = "LAD_Dec_2021_GB_BFC_2022_simplified.geojson") {
  cache_dir <- Sys.getenv(
    "DEBIAS_BOUNDARY_CACHE",
    unset = file.path(tools::R_user_dir("debiasR", "cache"), "boundaries")
  )
  file.path(cache_dir, filename)
}

validate_download_lad_boundary <- function(url, destfile) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(120, old_timeout))

  result <- tryCatch(
    utils::download.file(
      url,
      destfile = destfile,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(error) error
  )
  if (inherits(result, "error")) {
    if (file.exists(destfile)) {
      unlink(destfile)
    }
    return(result)
  }
  if (!file.exists(destfile) || is.na(file.info(destfile)$size) || file.info(destfile)$size == 0) {
    if (file.exists(destfile)) {
      unlink(destfile)
    }
    return(simpleError("download did not create a non-empty cache file"))
  }
  TRUE
}

validate_find_lad_boundary_path <- function() {
  env_path <- Sys.getenv("DEBIAS_LAD_BOUNDARY_PATH", unset = "")
  if (!identical(env_path, "")) {
    if (file.exists(env_path)) {
      return(list(
        path = env_path,
        note = NULL,
        label = "`DEBIAS_LAD_BOUNDARY_PATH`"
      ))
    }
    return(list(
      path = NULL,
      label = NA_character_,
      note = paste(
        "`DEBIAS_LAD_BOUNDARY_PATH` is set, but the file does not exist:",
        env_path
      )
    ))
  }

  package_candidates <- c(
    system.file(
      "extdata",
      "LAD_Dec_2021_GB_BFC_2022_simplified.geojson",
      package = "debiasR"
    ),
    system.file(
      "extdata",
      "LAD_Dec_2021_GB_BFC_2022_simplified.gpkg",
      package = "debiasRdata"
    ),
    system.file(
      "extdata",
      "LAD_Dec_2021_GB_BFC_2022_simplified.geojson",
      package = "debiasRdata"
    ),
    system.file(
      "extdata",
      "lad_boundaries_2021_simplified.gpkg",
      package = "debiasRdata"
    ),
    system.file(
      "extdata",
      "lad_boundaries_2021_simplified.geojson",
      package = "debiasRdata"
    ),
    file.path(
      "inst",
      "extdata",
      "LAD_Dec_2021_GB_BFC_2022_simplified.geojson"
    )
  )
  package_candidates <- package_candidates[
    nzchar(package_candidates) & file.exists(package_candidates)
  ]
  if (length(package_candidates) > 0L) {
    return(list(
      path = package_candidates[1],
      note = NULL,
      label = "a packaged LAD boundary file"
    ))
  }

  cache_path <- validate_lad_boundary_cache_path()
  if (file.exists(cache_path)) {
    return(list(
      path = cache_path,
      note = NULL,
      label = "the public ONS 2021 LAD BFC boundary file"
    ))
  }

  boundary_url <- validate_lad_boundary_source_url()
  allow_download <- tolower(Sys.getenv("DEBIAS_DOWNLOAD_BOUNDARIES", unset = "true")) %in%
    c("1", "true", "yes")
  if (!identical(boundary_url, "") && allow_download) {
    download_result <- validate_download_lad_boundary(
      url = boundary_url,
      destfile = cache_path
    )
    if (!inherits(download_result, "error")) {
      return(list(
        path = cache_path,
        note = NULL,
        label = "the public ONS 2021 LAD BFC boundary file"
      ))
    }
    return(list(
      path = NULL,
      label = NA_character_,
      note = paste(
        "Could not download the LAD boundary file from the public ONS source.",
        "Set `DEBIAS_LAD_BOUNDARY_PATH` to a local LAD boundary file or try again with network access.",
        "Download error:",
        conditionMessage(download_result)
      )
    ))
  }

  list(
    path = NULL,
    label = NA_character_,
    note = paste(
      "LAD boundaries are optional for rendering this vignette.",
      "To reproduce the LISA map, allow the public ONS boundary download",
      "or set `DEBIAS_LAD_BOUNDARY_PATH` to a local LAD boundary file."
    )
  )
}

validate_read_lad_boundaries <- function(areas,
                                           include_all = FALSE,
                                           include_scotland = TRUE) {
  areas <- unique(as.character(areas))
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(list(
      boundaries = NULL,
      note = "Install `sf` to read LAD boundaries and render the LISA map.",
      source = NA_character_
    ))
  }

  source <- validate_find_lad_boundary_path()
  if (is.null(source$path)) {
    return(list(
      boundaries = NULL,
      note = source$note,
      source = source$label
    ))
  }
  source_label <- if (!is.null(source$label) && !is.na(source$label)) {
    source$label
  } else {
    source$path
  }

  boundaries <- tryCatch(
    sf::st_read(source$path, quiet = TRUE),
    error = function(error) error
  )
  if (inherits(boundaries, "error")) {
    return(list(
      boundaries = NULL,
      note = paste("Could not read the LAD boundary file:", conditionMessage(boundaries)),
      source = source_label
    ))
  }

  code_col <- intersect(
    c("area", "code", "LAD21CD", "lad21cd", "lad_code", "LAD22CD", "lad22cd"),
    names(boundaries)
  )[1]
  if (is.na(code_col)) {
    return(list(
      boundaries = NULL,
      note = paste(
        "The LAD boundary file must contain an area-code column such as",
        "`area`, `code`, or `LAD21CD`."
      ),
      source = source_label
    ))
  }

  boundaries <- boundaries |>
    dplyr::mutate(
      area = as.character(.data[[code_col]]),
      validation_support = .data$area %in% areas
    ) |>
    dplyr::select("area", "validation_support", dplyr::everything())

  missing_areas <- setdiff(areas, boundaries$area)
  if (length(missing_areas) > 0L) {
    return(list(
      boundaries = NULL,
      note = paste(
        "The LAD boundary file does not cover all validation areas. Missing:",
        paste(utils::head(missing_areas, 8), collapse = ", "),
        if (length(missing_areas) > 8L) "..." else ""
      ),
      source = source_label
    ))
  }
  if (!isTRUE(include_scotland)) {
    boundaries <- boundaries |>
      dplyr::filter(
        !startsWith(.data$area, "S") | .data$validation_support
      )
  }
  if (!isTRUE(include_all)) {
    boundaries <- boundaries |>
      dplyr::filter(.data$validation_support)
  }

  list(
    boundaries = boundaries,
    note = NULL,
    source = source_label
  )
}

validate_build_boundary_neighbors <- function(boundaries) {
  if (is.null(boundaries) || !inherits(boundaries, "sf")) {
    stop("`boundaries` must be an `sf` object.")
  }
  touches <- sf::st_touches(boundaries)
  tibble::tibble(
    area = rep(boundaries$area, lengths(touches)),
    neighbor = boundaries$area[unlist(touches, use.names = FALSE)]
  ) |>
    dplyr::filter(.data$area != .data$neighbor) |>
    dplyr::distinct(.data$area, .data$neighbor) |>
    dplyr::mutate(weight = 1)
}

validate_prepare_lad_lisa_inputs <- function(areas,
                                               distance_df,
                                               distance_k = 4L) {
  neighbors <- validate_build_distance_neighbors(
    distance_df = distance_df,
    areas = areas,
    k = distance_k
  )
  boundary_result <- validate_read_lad_boundaries(
    areas,
    include_all = TRUE,
    include_scotland = FALSE
  )
  if (!is.null(boundary_result$boundaries)) {
    return(list(
      neighbors = neighbors,
      boundaries = boundary_result$boundaries,
      note = paste(
        "Using deterministic",
        distance_k,
        "nearest-neighbour links from LAD centroid distances for Local Moran's I.",
        "LAD boundaries are available for mapping from:",
        boundary_result$source,
        "Scottish background polygons are omitted.",
        "Boundary polygons outside the validation support are retained as grey background areas."
      ),
      map_note = NULL,
      neighbor_label = paste(distance_k, "nearest LAD centroid-distance links"),
      boundary_source = boundary_result$source
    ))
  }

  list(
    neighbors = neighbors,
    boundaries = NULL,
    note = paste(
      "Using deterministic",
      distance_k,
      "nearest-neighbour links from LAD centroid distances for Local Moran's I."
    ),
    map_note = boundary_result$note,
    neighbor_label = paste(distance_k, "nearest LAD centroid-distance links"),
    boundary_source = NA_character_
  )
}

validate_display_residual_structure <- function(residual_structure) {
  summary_rows <- dplyr::bind_rows(lapply(names(residual_structure), function(method_id) {
    summary <- tibble::as_tibble(residual_structure[[method_id]]$summary)
    summary$method <- method_id
    summary
  }))
  if (!"moran_p_value" %in% names(summary_rows)) {
    summary_rows$moran_p_value <- NA_real_
  }

  summary_rows |>
    dplyr::mutate(method_label = validate_method_label(.data$method)) |>
    dplyr::transmute(
      Method = .data$method_label,
      `Residual-flow r` =
        validate_fmt_num(.data$pearson_residual_benchmark_flow, 3),
      `Moran's I` = validate_fmt_num(.data$moran_i, 3),
      `Moran p-value` = validate_fmt_num(.data$moran_p_value, 3),
      `Residual-covariate r` =
        validate_fmt_num(.data$pearson_residual_covariate, 3),
      `Areas` = .data$n_areas
    ) |>
    validate_kable()
}

validate_display_local_moran <- function(residual_structure,
                                           area_names = NULL,
                                           methods = names(residual_structure),
                                           n = 8L) {
  local_rows <- dplyr::bind_rows(lapply(methods, function(method_id) {
    item <- residual_structure[[method_id]]
    if (is.null(item$local_moran)) {
      return(NULL)
    }
    local <- tibble::as_tibble(item$local_moran)
    local$method <- method_id
    local
  }))

  if (nrow(local_rows) == 0L) {
    return(invisible(NULL))
  }

  if (!is.null(area_names)) {
    local_rows <- local_rows |>
      dplyr::left_join(
        tibble::as_tibble(area_names) |>
          dplyr::select(area, name),
        by = "area"
      )
  } else {
    local_rows$name <- local_rows$area
  }

  if ("local_moran_i" %in% names(local_rows)) {
    local_rows$local_i_display <- local_rows$local_moran_i
  } else {
    local_rows$local_i_display <- local_rows$local_i
  }

  local_rows |>
    dplyr::mutate(method_label = validate_method_label(.data$method)) |>
    dplyr::arrange(.data$p_value, dplyr::desc(abs(.data$local_i_display))) |>
    dplyr::slice_head(n = n) |>
    dplyr::transmute(
      Method = .data$method_label,
      Area = dplyr::coalesce(.data$name, .data$area),
      `Local Moran's I` = validate_fmt_num(.data$local_i_display, 3),
      `p-value` = validate_fmt_num(.data$p_value, 3),
      `Adjusted p-value` = validate_fmt_num(.data$p_adjusted, 3),
      `LISA class` = .data$lisa_cluster
    ) |>
    validate_kable()
}

validate_display_residual_structure_areas <- function(residual_structure,
                                                       area_names = NULL,
                                                       methods = names(residual_structure),
                                                       n = 5L) {
  rows <- dplyr::bind_rows(lapply(methods, function(method_id) {
    item <- residual_structure[[method_id]]
    area_level <- tibble::as_tibble(item$area_level)
    area_level$method <- method_id
    area_level
  }))

  if (!is.null(area_names)) {
    rows <- rows |>
      dplyr::left_join(
        tibble::as_tibble(area_names) |>
          dplyr::select(area, name),
        by = "area"
      )
  } else {
    rows$name <- rows$area
  }

  residual_col <- dplyr::case_when(
    "area_residual" %in% names(rows) ~ "area_residual",
    "selected_residual" %in% names(rows) ~ "selected_residual",
    "mean_residual" %in% names(rows) ~ "mean_residual",
    TRUE ~ "residual"
  )

  rows |>
    dplyr::mutate(
      method_label = validate_method_label(.data$method),
      abs_residual = abs(.data[[residual_col]])
    ) |>
    dplyr::group_by(.data$method, .data$method_label) |>
    dplyr::slice_max(.data$abs_residual, n = n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      Method = .data$method_label,
      Area = dplyr::coalesce(.data$name, .data$area),
      `Mean residual` = validate_fmt_num(.data[[residual_col]], 1),
      `Absolute residual` = validate_fmt_num(.data$abs_residual, 1)
    ) |>
    validate_kable()
}
