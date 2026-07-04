.adjust_multilevel_inla <- function(mpd_od_df,
                                    coverage_df,
                                    covariates_df = NULL,
                                    distance_df = NULL,
                                    flow_col = "flow",
                                    income_col = NULL,
                                    pop_col = "population",
                                    distance_col = "distance_km",
                                    source_col = NULL,
                                    time_col = NULL,
                                    scenario = c("auto", "s1", "s2", "s3", "s4"),
                                    repeated_observation = c("auto", "none", "time", "source", "source_time"),
                                    formula_info = list(formula = NULL, source = "default"),
                                    fixed_effects = c("gravity", "gravity_rural", "gravity_education"),
                                    model_family = c("poisson", "negbin"),
                                    coverage_scale = c("origin", "destination", "both"),
                                    spatial_effect = c(
                                      "none",
                                      "origin_iid",
                                      "destination_iid",
                                      "origin_besag",
                                      "destination_besag",
                                      "origin_bym2",
                                      "destination_bym2"
                                    ),
                                    area_neighbors = NULL,
                                    area_col = "area",
                                    neighbor_col = "neighbor",
                                    prediction_scope = c("observed", "complete_grid"),
                                    flow_adj_summary = c("mean", "median"),
                                    control_compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                                    keep_cols = character()) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "The experimental INLA backend requires the optional 'INLA' package. ",
      "Install it from the R-INLA repository before calling ",
      "`adjust_multilevel_bayes(backend = 'inla')`."
    )
  }

  scenario <- match.arg(scenario)
  repeated_observation <- match.arg(repeated_observation)
  fixed_effects <- match.arg(fixed_effects)
  model_family <- match.arg(model_family)
  coverage_scale <- match.arg(coverage_scale)
  spatial_effect <- match.arg(spatial_effect)
  prediction_scope <- match.arg(prediction_scope)
  flow_adj_summary <- match.arg(flow_adj_summary)
  start_time <- Sys.time()

  scenario_info <- .resolve_multilevel_scenario(
    mpd_od_df = mpd_od_df,
    coverage_df = coverage_df,
    source_col = source_col,
    time_col = time_col,
    scenario = scenario,
    repeated_observation = repeated_observation
  )

  od_audit <- NULL
  if (prediction_scope == "complete_grid") {
    od_audit <- .audit_multilevel_complete_grid(
      mpd_od_df,
      flow_col = flow_col,
      scenario_cols = scenario_info$audit_cols
    )
    if (!isTRUE(od_audit$strict_square_support[[1]])) {
      stop(
        "`prediction_scope = 'complete_grid'` requires a strict square OD grid: ",
        "same origin and destination area set, expected OD row count, no duplicate pairs, ",
        "and finite non-negative flows."
      )
    }
  }

  prep <- .prepare_multilevel_bayes_data(
    mpd_od_df = mpd_od_df,
    coverage_df = coverage_df,
    covariates_df = covariates_df,
    distance_df = distance_df,
    flow_col = flow_col,
    income_col = income_col,
    pop_col = pop_col,
    distance_col = distance_col,
    source_col = source_col,
    time_col = time_col,
    scenario_info = scenario_info
  )

  model_df <- .set_multilevel_observation_probability(
    data = prep$model_df,
    coverage_scale = coverage_scale
  )
  .validate_multilevel_coverage_offset_data(
    data = model_df,
    coverage_scale = coverage_scale
  )

  if (prediction_scope == "observed" && "mpd_observed" %in% names(model_df)) {
    model_df <- dplyr::filter(model_df, .data$mpd_observed)
  }

  model_df$flow_inla <- if ("mpd_observed" %in% names(model_df)) {
    ifelse(model_df$mpd_observed %in% TRUE, model_df$flow, NA_real_)
  } else {
    model_df$flow
  }

  fixed_terms <- .inla_fixed_terms(
    data = model_df,
    fixed_effects = fixed_effects,
    include_pop_terms = prep$has_pop_terms,
    formula_info = formula_info
  )
  spatial_info <- .inla_spatial_effect_info(
    data = model_df,
    spatial_effect = spatial_effect,
    area_neighbors = area_neighbors,
    area_col = area_col,
    neighbor_col = neighbor_col
  )
  model_df <- spatial_info$data

  fit_formula <- .build_inla_formula(
    fixed_terms = fixed_terms,
    spatial_term = spatial_info$formula_term,
    graph_path = spatial_info$graph_path
  )

  fit_vars <- setdiff(unique(all.vars(fit_formula)), "flow_inla")
  fit_keep <- stats::complete.cases(model_df[, fit_vars, drop = FALSE])
  if (!all(fit_keep)) {
    warning(
      sum(!fit_keep),
      " row(s) were excluded from the INLA fit because predictor variables were missing.",
      call. = FALSE
    )
  }
  inla_df <- model_df[fit_keep, , drop = FALSE]
  if (sum(is.finite(inla_df$flow_inla)) < 2L) {
    stop("The INLA fit requires at least two observed MPD rows after preprocessing.")
  }

  fit <- INLA::inla(
    formula = fit_formula,
    data = inla_df,
    family = .inla_family(model_family),
    control.predictor = list(compute = TRUE),
    control.compute = control_compute
  )

  fitted_summary <- .extract_inla_fitted_summary(fit, n = nrow(inla_df))
  obs_prob <- inla_df$observation_probability
  flow_mpd_pred <- .inla_summary_value(fitted_summary, flow_adj_summary)
  flow_adj_mean <- fitted_summary$mean / obs_prob
  flow_adj_median <- fitted_summary$median / obs_prob
  flow_adj_q2.5 <- fitted_summary$q2.5 / obs_prob
  flow_adj_q97.5 <- fitted_summary$q97.5 / obs_prob
  flow_adj <- if (flow_adj_summary == "median") flow_adj_median else flow_adj_mean

  coef_tbl <- .coef_summary_inla(fit)
  runtime_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  result_metadata <- list(
    stage = "stage_1_inla_experimental",
    backend = "inla",
    model_engine = "bayesian",
    model_family = model_family,
    target_scale = "true_flow",
    observation_model = "coverage_offset",
    coverage_scale = coverage_scale,
    fixed_effects = fixed_effects,
    spatial_effect = spatial_effect,
    prediction_scope = prediction_scope,
    flow_adj_summary = flow_adj_summary,
    distance_source = prep$distance_source,
    runtime_seconds = runtime_seconds,
    n_input_rows = nrow(prep$base_df),
    n_fit_rows = sum(is.finite(inla_df$flow_inla)),
    n_prediction_rows = nrow(inla_df),
    n_zero_filled_prediction_rows = if ("mpd_zero_filled" %in% names(inla_df)) {
      sum(inla_df$mpd_zero_filled %in% TRUE, na.rm = TRUE)
    } else {
      0L
    },
    od_audit = od_audit
  )
  diagnostics <- .collect_inla_diagnostics(fit)

  keep <- unique(c(
    "origin",
    "destination",
    "flow",
    "mpd_observed",
    "mpd_zero_filled",
    "mpd_row_status",
    "observation_probability",
    "coverage_rate_o",
    "coverage_rate_d",
    "log_observation_probability",
    "distance_km",
    "log_distance",
    "log_pop_o",
    "log_pop_d",
    keep_cols
  ))
  keep <- keep[keep %in% names(inla_df)]

  out <- inla_df |>
    dplyr::mutate(
      flow_mpd_pred = as.numeric(flow_mpd_pred),
      flow_true_pred = as.numeric(flow_adj),
      flow_adj = as.numeric(flow_adj),
      flow_adj_mean = as.numeric(flow_adj_mean),
      flow_adj_median = as.numeric(flow_adj_median),
      flow_adj_q2.5 = as.numeric(flow_adj_q2.5),
      flow_adj_q97.5 = as.numeric(flow_adj_q97.5)
    ) |>
    dplyr::select(
      dplyr::all_of(keep),
      flow_mpd_pred,
      flow_true_pred,
      flow_adj,
      flow_adj_mean,
      flow_adj_median,
      flow_adj_q2.5,
      flow_adj_q97.5
    ) |>
    tibble::as_tibble()

  attr(out, "model") <- fit
  attr(out, "formula") <- deparse(fit_formula)
  attr(out, "coefficients") <- coef_tbl
  attr(out, "backend") <- "inla"
  attr(out, "model_engine") <- "bayesian"
  attr(out, "model_family") <- model_family
  attr(out, "target_scale") <- "true_flow"
  attr(out, "observation_model") <- "coverage_offset"
  attr(out, "coverage_scale") <- coverage_scale
  attr(out, "fixed_effects") <- fixed_effects
  attr(out, "spatial_effect") <- spatial_effect
  attr(out, "stage") <- "stage_1_inla_experimental"
  attr(out, "result_metadata") <- result_metadata
  attr(out, "flow_adj_summary") <- flow_adj_summary
  attr(out, "prediction_scope") <- prediction_scope
  attr(out, "runtime_seconds") <- runtime_seconds
  attr(out, "diagnostics") <- diagnostics
  attr(out, "od_audit") <- od_audit
  attr(out, "prototype_notes") <- paste(
    "Experimental INLA coverage-offset true-flow model.",
    "Use this path for fast specification and spatial-dependency experiments;",
    "validate against held-out benchmark rows before treating it as a recommended adjustment."
  )

  out
}

.inla_fixed_terms <- function(data,
                              fixed_effects,
                              include_pop_terms,
                              formula_info = list(formula = NULL, source = "default")) {
  if (!is.null(formula_info$formula)) {
    terms <- .formula_fixed_effect_vars(formula_info$formula)
    terms <- setdiff(terms, "log_observation_probability")
  } else if (!is.null(formula_info$mobility_formula)) {
    mobility_rhs <- paste(deparse(formula_info$mobility_formula[[2]]), collapse = " ")
    mobility_formula <- stats::as.formula(paste("flow ~", mobility_rhs))
    terms <- .formula_fixed_effect_vars(mobility_formula)
  } else {
    terms <- "log_distance"
    if (isTRUE(include_pop_terms)) {
      terms <- c(terms, "log_pop_o", "log_pop_d")
    }
    if (fixed_effects %in% c("gravity_rural", "gravity_education")) {
      terms <- c(terms, "rural_pct_o", "rural_pct_d")
    }
    if (fixed_effects == "gravity_education") {
      terms <- c(terms, "per_level4_o", "per_level4_d")
    }
  }

  terms <- setdiff(unique(terms), "flow")
  missing_terms <- setdiff(unique(terms), names(data))
  if (length(missing_terms) > 0L) {
    stop(
      "`fixed_effects = '", fixed_effects,
      "' requires prepared variable(s): ",
      paste(missing_terms, collapse = ", "),
      ". Supply matching covariates or choose a simpler fixed-effect set."
    )
  }
  unique(terms)
}

.inla_family <- function(model_family) {
  switch(
    model_family,
    poisson = "poisson",
    negbin = "nbinomial"
  )
}

.inla_spatial_effect_info <- function(data,
                                      spatial_effect,
                                      area_neighbors = NULL,
                                      area_col = "area",
                                      neighbor_col = "neighbor") {
  if (identical(spatial_effect, "none")) {
    return(list(data = data, formula_term = NULL, graph_path = NULL))
  }

  area_role <- if (grepl("^origin", spatial_effect)) "origin" else "destination"
  index_col <- paste0(area_role, "_inla_idx")
  area_levels <- sort(unique(as.character(c(data$origin, data$destination))))
  data[[index_col]] <- match(as.character(data[[area_role]]), area_levels)

  effect_model <- sub("^[^_]+_", "", spatial_effect)
  if (identical(effect_model, "iid")) {
    return(list(
      data = data,
      formula_term = paste0("f(", index_col, ", model = 'iid')"),
      graph_path = NULL
    ))
  }

  if (is.null(area_neighbors)) {
    stop(
      "`spatial_effect = '", spatial_effect,
      "' requires `area_neighbors` with focal and neighbouring area columns."
    )
  }

  graph_path <- tempfile("debiasr-inla-graph-", fileext = ".adj")
  .write_inla_graph(
    area_neighbors = area_neighbors,
    areas = area_levels,
    path = graph_path,
    area_col = area_col,
    neighbor_col = neighbor_col
  )

  list(
    data = data,
    formula_term = paste0(
      "f(",
      index_col,
      ", model = '",
      effect_model,
      "', graph = graph_path, scale.model = TRUE)"
    ),
    graph_path = graph_path
  )
}

.build_inla_formula <- function(fixed_terms, spatial_term = NULL, graph_path = NULL) {
  rhs <- c(fixed_terms, "offset(log_observation_probability)", spatial_term)
  formula_env <- list2env(list(graph_path = graph_path), parent = parent.frame())
  stats::as.formula(
    paste("flow_inla ~", paste(rhs, collapse = " + ")),
    env = formula_env
  )
}

.write_inla_graph <- function(area_neighbors,
                              areas,
                              path,
                              area_col = "area",
                              neighbor_col = "neighbor") {
  req <- c(area_col, neighbor_col)
  if (!all(req %in% names(area_neighbors))) {
    stop("`area_neighbors` must contain: ", paste(req, collapse = ", "))
  }

  area_levels <- as.character(areas)
  idx <- stats::setNames(seq_along(area_levels), area_levels)
  links <- area_neighbors |>
    dplyr::transmute(
      area = as.character(.data[[area_col]]),
      neighbor = as.character(.data[[neighbor_col]])
    ) |>
    dplyr::filter(
      .data$area %in% area_levels,
      .data$neighbor %in% area_levels,
      .data$area != .data$neighbor
    ) |>
    dplyr::distinct()
  reverse_links <- tibble::tibble(
    area = links$neighbor,
    neighbor = links$area
  )
  links <- dplyr::bind_rows(links, reverse_links) |>
    dplyr::distinct()

  adjacency <- split(idx[links$neighbor], idx[links$area])
  lines <- c(as.character(length(area_levels)))
  for (i in seq_along(area_levels)) {
    neighbors <- sort(unique(as.integer(adjacency[[as.character(i)]])))
    neighbors <- neighbors[is.finite(neighbors)]
    lines <- c(
      lines,
      paste(c(i, length(neighbors), neighbors), collapse = " ")
    )
  }

  writeLines(lines, con = path)
  invisible(path)
}

.extract_inla_fitted_summary <- function(fit, n) {
  summary_tbl <- fit$summary.fitted.values
  if (is.null(summary_tbl) || nrow(summary_tbl) != n) {
    stop("INLA did not return fitted-value summaries for all prediction rows.")
  }

  out <- data.frame(
    mean = as.numeric(summary_tbl$mean),
    q2.5 = .inla_summary_col(summary_tbl, c("0.025quant", "X0.025quant")),
    median = .inla_summary_col(summary_tbl, c("0.5quant", "X0.5quant", "median")),
    q97.5 = .inla_summary_col(summary_tbl, c("0.975quant", "X0.975quant")),
    stringsAsFactors = FALSE
  )
  out$median[!is.finite(out$median)] <- out$mean[!is.finite(out$median)]
  out$q2.5[!is.finite(out$q2.5)] <- out$mean[!is.finite(out$q2.5)]
  out$q97.5[!is.finite(out$q97.5)] <- out$mean[!is.finite(out$q97.5)]
  out
}

.inla_summary_col <- function(summary_tbl, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(summary_tbl)) {
      return(as.numeric(summary_tbl[[candidate]]))
    }
  }
  rep(NA_real_, nrow(summary_tbl))
}

.inla_summary_value <- function(summary_tbl, flow_adj_summary) {
  if (identical(flow_adj_summary, "median")) {
    return(summary_tbl$median)
  }
  summary_tbl$mean
}

.coef_summary_inla <- function(fit) {
  fixed <- fit$summary.fixed
  if (is.null(fixed) || nrow(fixed) == 0L) {
    return(tibble::tibble())
  }
  out <- as.data.frame(fixed)
  out$term <- rownames(out)
  rownames(out) <- NULL
  if ("0.025quant" %in% names(out)) names(out)[names(out) == "0.025quant"] <- "q2.5"
  if ("0.975quant" %in% names(out)) names(out)[names(out) == "0.975quant"] <- "q97.5"
  tibble::as_tibble(out)
}

.collect_inla_diagnostics <- function(fit) {
  list(
    backend = "inla",
    has_bias_term = FALSE,
    convergence = list(status = "inla_approximation"),
    dic = if (!is.null(fit$dic$dic)) fit$dic$dic else NA_real_,
    waic = if (!is.null(fit$waic$waic)) fit$waic$waic else NA_real_,
    cpo_failures = if (!is.null(fit$cpo$failure)) {
      sum(fit$cpo$failure > 0, na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
}
