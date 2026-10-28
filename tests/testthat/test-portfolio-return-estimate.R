portfolio_return_fixture <- function(fund, date) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  )
}


test_that("consolidates Long Term contributions", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_portfolio_return(previous, current)

  expect_s3_class(
    result,
    "portfolio_return_estimate"
  )

  expect_identical(
    result$components$component,
    c("funds", "fixed_income", "equities", "futures")
  )

  expect_equal(
    result$summary$estimated_result,
    10194.59,
    tolerance = 0.02
  )

  expect_equal(
    result$summary$reported_return_percent,
    100 * (1.32034965 / 1.31494548 - 1)
  )

  expect_equal(
    result$summary$estimated_return_percent,
    100 * result$summary$estimated_result / 2409809.35
  )

  expect_identical(
    result$summary$comparison_status,
    "within_tolerance"
  )

  expect_identical(
    result$summary$coverage_status,
    "partial_model"
  )

  expect_equal(result$summary$n_unestimated, 0L)
  expect_equal(result$summary$n_pending_operations, 1L)
})


test_that("consolidates Linear contributions", {
  previous <- portfolio_return_fixture(
    "linear",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "linear",
    "2026-10-19"
  )

  result <- estimate_portfolio_return(previous, current)

  expect_equal(
    result$summary$estimated_result,
    8931.56,
    tolerance = 0.02
  )

  expect_equal(
    result$summary$reported_return_percent,
    100 * (1.19800106 / 1.19767464 - 1)
  )

  expect_identical(
    result$summary$comparison_status,
    "within_tolerance"
  )

  expect_equal(result$summary$n_unestimated, 0L)

  expect_equal(
    result$summary$n_pending_operations,
    1L
  )
})


test_that("allows a configurable return tolerance", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_portfolio_return(
    previous,
    current,
    tolerance_pp = 0.01
  )

  expect_identical(
    result$summary$comparison_status,
    "outside_tolerance"
  )

  expect_equal(result$summary$tolerance_pp, 0.01)
})


test_that("uses the reported futures adjustment unchanged", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_portfolio_return(previous, current)

  futures <- result$components |>
    dplyr::filter(.data$component == "futures")

  expect_equal(futures$estimated_result, -320.56)

  expect_equal(
    result$details$futures$summary$estimated_result,
    -320.56
  )
})


test_that("flags futures quantity changes without changing P&L", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$futures$qtd[[1L]] <- 7

  result <- estimate_portfolio_return(previous, current)

  expect_equal(
    result$details$futures$summary$estimated_result,
    -320.56
  )

  expect_equal(
    result$details$futures_quantities$summary$n_pending,
    1L
  )

  expect_true(any(
    result$issues$issue_type ==
      "quantity_change_requires_operations"
  ))

  expect_equal(
    result$summary$n_pending_operations,
    2L
  )
})


test_that("does not approve incomplete return estimates", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$equities$pu_atual_bruto[[1L]] <-
    NA_real_

  result <- estimate_portfolio_return(previous, current)

  expect_equal(result$summary$n_unestimated, 1L)

  expect_identical(
    result$summary$comparison_status,
    "not_evaluated"
  )
})


test_that("preserves underlying attribution details", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_portfolio_return(previous, current)

  expect_named(
    result$details,
    c(
      "funds",
      "fixed_income",
      "equities",
      "futures",
      "futures_quantities"
    )
  )

  expect_s3_class(
    result$details$funds,
    "fund_return_attribution"
  )

  expect_s3_class(
    result$details$futures_quantities,
    "futures_quantity_check"
  )
})


test_that("validates return tolerances", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  expect_error(
    estimate_portfolio_return(
      previous,
      current,
      tolerance_pp = -0.01
    )
  )

  expect_error(
    estimate_portfolio_return(
      previous,
      current,
      tolerance_pp = NA_real_
    )
  )
})


test_that("does not mutate imported positions", {
  previous <- portfolio_return_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- portfolio_return_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  estimate_portfolio_return(previous, current)

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
