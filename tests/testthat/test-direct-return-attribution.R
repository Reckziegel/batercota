direct_attribution_fixture <- function(fund, date) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  )
}


test_that("reconstructs real fixed income results", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  expect_s3_class(
    result,
    "fixed_income_return_attribution"
  )

  expect_equal(nrow(result$contributions), 2L)

  expect_equal(
    result$summary$n_quantity_changed,
    0L
  )

  expect_equal(result$summary$n_unestimated, 0L)

  expect_equal(
    result$summary$opening_value_coverage,
    1
  )

  expect_equal(
    result$summary$estimated_result,
    -218.03,
    tolerance = 0.01
  )
})


test_that("calculates LTN and NTN-B results separately", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  ltn <- result$contributions |>
    dplyr::filter(.data$title == "LTN00003E8D3")

  ntnb <- result$contributions |>
    dplyr::filter(.data$title == "NTN-B002C3BC")

  expect_equal(nrow(ltn), 1L)
  expect_equal(nrow(ntnb), 1L)

  expect_equal(ltn$estimated_result, 109.09)
  expect_equal(ntnb$estimated_result, -327.12)

  expect_identical(
    ltn$status,
    "unchanged_quantity"
  )

  expect_identical(
    ntnb$status,
    "unchanged_quantity"
  )

  expect_equal(
    result$summary$estimated_contribution_pp,
    100 * -218.03 / 2409809.35
  )
})


test_that("separates fixed income quantity changes", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$fixed_income$qtd[[1L]] <- 200

  current$positions$fixed_income$vl_mrc[[1L]] <- round(
    200 * current$positions$fixed_income$pu_bruto[[1L]],
    2
  )

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  ltn <- result$contributions |>
    dplyr::filter(.data$title == "LTN00003E8D3")

  expected_result <- 213 *
    (768.533401 - 768.021224)

  expect_identical(
    ltn$status,
    "changed_quantity"
  )

  expect_equal(
    ltn$estimated_result,
    expected_result
  )

  expect_equal(
    ltn$quantity_change,
    -13
  )

  expect_true(
    is.finite(ltn$non_price_change_approx)
  )
})


test_that("does not assume zero for exited securities", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$fixed_income <-
    current$positions$fixed_income[-1L, ]

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  exited <- result$contributions |>
    dplyr::filter(.data$status == "exited")

  expect_equal(nrow(exited), 1L)
  expect_true(is.na(exited$estimated_result))

  expect_equal(
    result$summary$n_unestimated,
    1L
  )
})


test_that("does not estimate invalid fixed income prices", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$fixed_income$pu_bruto[[1L]] <-
    NA_real_

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  expect_equal(result$summary$n_unestimated, 1L)

  expect_true(any(
    result$contributions$status == "invalid_price"
  ))
})


test_that("does not associate missing ISINs", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  previous$positions$fixed_income$isin[[1L]] <-
    NA_character_

  current$positions$fixed_income$isin[[1L]] <-
    NA_character_

  result <- estimate_fixed_income_contributions(
    previous,
    current
  )

  expect_equal(
    sum(result$contributions$status == "missing_isin"),
    2L
  )
})


test_that("rejects duplicate fixed income identifiers", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$fixed_income$isin[[2L]] <-
    current$positions$fixed_income$isin[[1L]]

  expect_error(
    estimate_fixed_income_contributions(previous, current)
  )
})


test_that("reads the October 16 futures adjustment directly", {
  position <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  result <- estimate_futures_adjustments(position)

  expect_s3_class(
    result,
    "futures_adjustment_attribution"
  )

  expect_equal(result$summary$n_positions, 1L)

  expect_equal(
    result$summary$estimated_result,
    -1796.80
  )

  expect_true(
    is.na(result$summary$estimated_contribution_pp)
  )
})


test_that("does not subtract previous futures adjustments", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_futures_adjustments(
    position = current,
    previous_nav = previous$metadata$patrimonio_liquido
  )

  expect_equal(
    result$summary$estimated_result,
    -320.56
  )

  expect_equal(
    result$summary$estimated_contribution_pp,
    100 * -320.56 / 2409809.35
  )

  expect_identical(
    result$summary$status,
    "reported"
  )
})


test_that("does not treat futures notional as result", {
  position <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_futures_adjustments(position)

  future <- result$contributions

  expect_equal(future$notional, 378406.56)
  expect_equal(future$adjustment, -320.56)
  expect_equal(future$estimated_result, -320.56)

  expect_false(
    isTRUE(all.equal(
      future$estimated_result,
      future$notional
    ))
  )
})


test_that("supports portfolios without open futures", {
  position <- direct_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  result <- estimate_futures_adjustments(position)

  expect_equal(result$summary$n_positions, 0L)
  expect_equal(result$summary$estimated_result, 0)

  expect_identical(
    result$summary$status,
    "no_open_positions"
  )
})


test_that("flags missing futures adjustments", {
  position <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  position$positions$futures$ajuste[[1L]] <-
    NA_real_

  result <- estimate_futures_adjustments(position)

  expect_equal(result$summary$n_unestimated, 1L)
  expect_true(is.na(result$summary$estimated_result))

  expect_identical(
    result$summary$status,
    "partial"
  )
})


test_that("validates previous NAV for futures", {
  position <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  expect_error(
    estimate_futures_adjustments(
      position,
      previous_nav = 0
    )
  )

  expect_error(
    estimate_futures_adjustments(
      position,
      previous_nav = NA_real_
    )
  )
})


test_that("rejects incompatible fixed income dates", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  expect_error(
    estimate_fixed_income_contributions(previous, current)
  )
})


test_that("preserves original position objects", {
  previous <- direct_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- direct_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  estimate_fixed_income_contributions(previous, current)

  estimate_futures_adjustments(
    current,
    previous_nav = previous$metadata$patrimonio_liquido
  )

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
