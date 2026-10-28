equity_attribution_fixture <- function(fund, date) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  )
}


test_that("reconstructs Long Term equity results", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_equity_contributions(previous, current)

  expect_s3_class(
    result,
    "equity_return_attribution"
  )

  expect_equal(nrow(result$contributions), 7L)
  expect_equal(result$summary$n_estimated, 7L)
  expect_equal(result$summary$n_quantity_changed, 0L)
  expect_equal(result$summary$n_unestimated, 0L)

  expect_equal(
    result$summary$estimated_result,
    5441
  )

  expect_equal(
    result$summary$estimated_contribution_pp,
    100 * 5441 / 2409809.35
  )

  expect_equal(
    result$summary$opening_value_coverage,
    1
  )

  expect_identical(
    result$summary$status,
    "complete"
  )
})


test_that("calculates individual equity contributions", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_equity_contributions(previous, current)

  expected <- tibble::tibble(
    title = c(
      "XINA11", "B3SA3", "BPAC11", "BRBI11",
      "SIMH3", "XPBR31", "ACWX39"
    ),
    expected_result = c(
      -385, 2145, 1395, -72, 1080, 1404, -126
    )
  )

  actual <- result$contributions |>
    dplyr::select(
      "title",
      estimated_result = "estimated_result"
    ) |>
    dplyr::left_join(expected, by = "title")

  expect_false(anyNA(actual$expected_result))

  expect_equal(
    actual$estimated_result,
    actual$expected_result
  )
})


test_that("separates price and quantity effects", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  index <- which(
    stringr::str_trim(
      current$positions$equities$cd_tit
    ) == "B3SA3"
  )

  current$positions$equities$qtd[index] <- 5600

  current$positions$equities$vl_mrc[index] <-
    5600 * 22.50

  result <- estimate_equity_contributions(previous, current)

  b3 <- result$contributions |>
    dplyr::filter(.data$title == "B3SA3")

  expect_identical(b3$status, "changed_quantity")
  expect_equal(b3$quantity_change, 100)
  expect_equal(b3$estimated_result, 2145)

  expect_equal(
    b3$non_price_change_approx,
    2250
  )

  expect_equal(
    result$summary$n_quantity_changed,
    1L
  )
})


test_that("does not estimate missing closing prices", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$equities$pu_atual_bruto[[1L]] <-
    NA_real_

  result <- estimate_equity_contributions(previous, current)

  expect_equal(result$summary$n_unestimated, 1L)

  expect_identical(
    result$summary$status,
    "partial"
  )

  expect_true(any(
    result$contributions$status == "invalid_price"
  ))
})


test_that("does not assume zero for exited equities", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$equities <-
    current$positions$equities[-1L, ]

  result <- estimate_equity_contributions(previous, current)

  exited <- result$contributions |>
    dplyr::filter(.data$status == "exited")

  expect_equal(nrow(exited), 1L)
  expect_true(is.na(exited$estimated_result))
  expect_true(is.na(exited$financial_change))
  expect_equal(result$summary$n_unestimated, 1L)
})


test_that("preserves missing ISIN records", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  previous$positions$equities$isin[[1L]] <- NA_character_
  current$positions$equities$isin[[1L]] <- NA_character_

  result <- estimate_equity_contributions(previous, current)

  expect_equal(
    sum(result$contributions$status == "missing_isin"),
    2L
  )

  expect_equal(result$summary$n_unestimated, 2L)
})


test_that("supports funds without direct equities", {
  previous <- equity_attribution_fixture(
    "linear",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  result <- estimate_equity_contributions(previous, current)

  expect_equal(nrow(result$contributions), 0L)
  expect_equal(result$summary$estimated_result, 0)

  expect_identical(
    result$summary$status,
    "no_positions"
  )
})


test_that("rejects duplicate equity ISINs", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$equities$isin[[2L]] <-
    current$positions$equities$isin[[1L]]

  expect_error(
    estimate_equity_contributions(previous, current)
  )
})


test_that("preserves original equity positions", {
  previous <- equity_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- equity_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  estimate_equity_contributions(previous, current)

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
