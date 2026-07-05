make_validate_flow_methods_fixture <- function() {
  mpd_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(100, 50, 25, 75)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(90, 65, 35, 70)
  )

  adjusted_outputs <- list(
    method_a = data.frame(
      origin = mpd_df$origin,
      destination = mpd_df$destination,
      flow = mpd_df$flow,
      flow_adj = c(92, 62, 33, 72)
    ),
    method_b = data.frame(
      origin = mpd_df$origin,
      destination = mpd_df$destination,
      flow = mpd_df$flow,
      flow_adj = c(96, 58, 30, 78)
    )
  )

  list(
    mpd_df = mpd_df,
    benchmark_od_df = benchmark_od_df,
    adjusted_outputs = adjusted_outputs
  )
}

test_that("validate_flow_prepare_outputs standardizes adjusted outputs", {
  fixture <- make_validate_flow_methods_fixture()

  prepared <- validate_flow_prepare_outputs(
    fixture$adjusted_outputs,
    mpd_df = fixture$mpd_df
  )

  expect_equal(names(prepared), c("method_a", "method_b"))
  expect_true(all(c("flow", "flow_adj", "adjusted_input_flow") %in% names(prepared$method_a)))
  expect_equal(prepared$method_a$flow, fixture$mpd_df$flow)
  expect_equal(prepared$method_a$adjusted_input_flow, fixture$adjusted_outputs$method_a$flow)
  expect_equal(prepared$method_a$flow_adj, fixture$adjusted_outputs$method_a$flow_adj)

  expect_error(
    validate_flow_prepare_outputs(list(fixture$adjusted_outputs$method_a), fixture$mpd_df),
    "`adjusted_outputs` must be a named list"
  )
})

test_that("validate_flow_*_methods apply validators across named outputs", {
  fixture <- make_validate_flow_methods_fixture()
  adjusted_outputs <- validate_flow_prepare_outputs(
    fixture$adjusted_outputs,
    mpd_df = fixture$mpd_df
  )

  overall <- validate_flow_overall_methods(
    adjusted_outputs,
    fixture$benchmark_od_df,
    comparisons = "adjusted_vs_benchmark"
  )
  residuals <- validate_flow_residual_methods(
    adjusted_outputs,
    fixture$benchmark_od_df,
    top_n = 2
  )
  distributions <- validate_flow_distribution_methods(
    adjusted_outputs,
    fixture$benchmark_od_df,
    comparisons = "adjusted_vs_benchmark"
  )

  area_neighbors <- data.frame(
    area = c("A", "B"),
    neighbor = c("B", "A")
  )
  structure <- validate_flow_residual_structure_methods(
    adjusted_outputs,
    fixture$benchmark_od_df,
    methods = "method_a",
    area_neighbors = area_neighbors,
    spatial_role = "origin"
  )

  expect_equal(names(overall), c("method_a", "method_b"))
  expect_equal(overall$method_a$method, "method_a")
  expect_equal(names(residuals), c("method_a", "method_b"))
  expect_equal(residuals$method_b$summary$method, "method_b")
  expect_equal(nrow(residuals$method_b$top_worst), 2)
  expect_equal(names(distributions), c("method_a", "method_b"))
  expect_equal(distributions$method_a$summary$method, "method_a")
  expect_equal(names(structure), "method_a")
  expect_equal(structure$method_a$summary$method, "method_a")

  expect_error(
    validate_flow_residual_structure_methods(
      adjusted_outputs,
      fixture$benchmark_od_df,
      methods = "missing"
    ),
    "`methods` must name elements in `adjusted_outputs`"
  )
})

test_that("validate_flow_margins prepares origin and destination totals", {
  fixture <- make_validate_flow_methods_fixture()
  adjusted_outputs <- validate_flow_prepare_outputs(
    fixture$adjusted_outputs,
    mpd_df = fixture$mpd_df
  )

  origin_margins <- validate_flow_margins(
    adjusted_outputs,
    fixture$benchmark_od_df,
    role = "origin"
  )
  destination_margins <- validate_flow_margins(
    adjusted_outputs,
    fixture$benchmark_od_df,
    role = "destination"
  )

  expect_equal(origin_margins$adjusted_outputs$method_a$origin, c("A", "B"))
  expect_equal(origin_margins$adjusted_outputs$method_a$destination, rep("origin_total", 2))
  expect_equal(origin_margins$adjusted_outputs$method_a$flow, c(150, 100))
  expect_equal(origin_margins$adjusted_outputs$method_a$flow_adj, c(154, 105))
  expect_equal(origin_margins$benchmark$flow, c(155, 105))

  expect_equal(destination_margins$adjusted_outputs$method_b$origin, rep("destination_total", 2))
  expect_equal(destination_margins$adjusted_outputs$method_b$destination, c("X", "Y"))
  expect_equal(destination_margins$adjusted_outputs$method_b$flow, c(125, 125))
  expect_equal(destination_margins$adjusted_outputs$method_b$flow_adj, c(126, 136))
  expect_equal(destination_margins$benchmark$flow, c(125, 135))
})
