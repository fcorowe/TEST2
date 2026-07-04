test_that("INLA graph writer symmetrises neighbour links", {
  graph_path <- tempfile(fileext = ".adj")
  debiasR:::.write_inla_graph(
    area_neighbors = data.frame(
      area = c("A", "B"),
      neighbor = c("B", "C")
    ),
    areas = c("A", "B", "C"),
    path = graph_path
  )

  expect_equal(
    readLines(graph_path),
    c(
      "3",
      "1 1 2",
      "2 2 1 3",
      "3 1 2"
    )
  )
})

test_that("INLA fixed-effect sets validate required prepared covariates", {
  base <- data.frame(
    log_distance = 1,
    log_pop_o = 2,
    log_pop_d = 3,
    rural_pct_o = 0.2,
    rural_pct_d = 0.3,
    per_level4_o = 0.4,
    per_level4_d = 0.5
  )

  expect_equal(
    debiasR:::.inla_fixed_terms(
      data = base,
      fixed_effects = "gravity_education",
      include_pop_terms = TRUE
    ),
    c(
      "log_distance",
      "log_pop_o",
      "log_pop_d",
      "rural_pct_o",
      "rural_pct_d",
      "per_level4_o",
      "per_level4_d"
    )
  )

  expect_error(
    debiasR:::.inla_fixed_terms(
      data = base[setdiff(names(base), "per_level4_d")],
      fixed_effects = "gravity_education",
      include_pop_terms = TRUE
    ),
    "requires prepared variable"
  )
})

test_that("INLA fixed terms respect existing formula interfaces", {
  base <- data.frame(
    flow = 1,
    log_distance = 1,
    log_pop_o = 2,
    log_pop_d = 3,
    rural_pct_o = 0.2,
    bias_e_origin = 0.1,
    log_observation_probability = log(0.2),
    origin = "A"
  )

  expect_equal(
    debiasR:::.inla_fixed_terms(
      data = base,
      fixed_effects = "gravity",
      include_pop_terms = TRUE,
      formula_info = list(
        formula = flow ~ log_distance + rural_pct_o +
          offset(log_observation_probability) + (1 | origin)
      )
    ),
    c("log_distance", "rural_pct_o")
  )

  expect_equal(
    debiasR:::.inla_fixed_terms(
      data = base,
      fixed_effects = "gravity",
      include_pop_terms = TRUE,
      formula_info = list(
        mobility_formula = ~ log_distance + log_pop_o + log_pop_d +
          (1 | origin)
      )
    ),
    c("log_distance", "log_pop_o", "log_pop_d")
  )
})

test_that("adjust_multilevel_bayes reports missing optional INLA dependency clearly", {
  if (requireNamespace("INLA", quietly = TRUE)) {
    skip("INLA is installed; unavailable-backend path is not active in this environment.")
  }

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = data.frame(origin = "A", destination = "A", flow = 1),
      coverage_df = data.frame(origin = "A", population = 10, user_count = 1),
      backend = "inla"
    ),
    "requires the optional 'INLA' package"
  )
})

test_that("spatial effects route through the integrated INLA backend", {
  expect_equal(
    debiasR:::.resolve_multilevel_backend(
      model_family = "poisson",
      backend = "auto",
      spatial_effect = "origin_bym2"
    ),
    "inla"
  )

  expect_error(
    debiasR:::.resolve_multilevel_backend(
      model_family = "poisson",
      backend = "rstanarm",
      spatial_effect = "origin_bym2"
    ),
    "requires `backend = 'inla'`"
  )

  expect_error(
    debiasR:::.resolve_multilevel_backend(
      model_family = "zinb",
      backend = "auto",
      spatial_effect = "origin_bym2"
    ),
    "supports Poisson and negative-binomial"
  )
})
