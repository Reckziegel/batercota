fund_attribution_fixture <- function(fund, date) {
  path <- testthat::test_path(
    "fixtures",
    "daycoval-position",
    paste0(fund, "_", date, ".csv")
  )

  read_daycoval_position(path)
}


test_that("estimates real fund contributions", {
  cases <- tibble::tribble(
    ~fund, ~n_positions,
    "long_term", 9L,
    "linear", 17L
  )

  purrr::pwalk(
    cases,
    function(fund, n_positions) {
      previous <- fund_attribution_fixture(
        fund,
        "2026-10-16"
      )

      current <- fund_attribution_fixture(
        fund,
        "2026-10-19"
      )

      result <- estimate_fund_contributions(
        previous,
        current
      )

      expect_s3_class(
        result,
        "fund_return_attribution"
      )

      expect_equal(
        nrow(result$contributions),
        n_positions
      )

      expect_equal(
        result$summary$n_estimated,
        n_positions
      )

      expect_equal(
        result$summary$n_quantity_changed,
        1L
      )

      expect_equal(
        result$summary$n_unestimated,
        0L
      )

      expect_equal(
        result$summary$opening_value_coverage,
        1
      )

      expect_true(
        is.finite(result$summary$estimated_result)
      )
    }
  )
})


test_that("uses financial changes for unchanged positions", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_fund_contributions(
    previous,
    current
  )

  az_quest <- result$contributions |>
    dplyr::filter(
      .data$title == "CF 11392165"
    )

  expect_equal(nrow(az_quest), 1L)

  expect_identical(
    az_quest$status,
    "unchanged_quantity"
  )

  expect_equal(
    az_quest$estimated_result,
    1295.42
  )

  expect_equal(
    az_quest$contribution_pp,
    100 * 1295.42 / 2409809.35
  )
})


test_that("separates Compass redemption from price return", {
  previous <- fund_attribution_fixture(
    "linear",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  result <- estimate_fund_contributions(
    previous,
    current
  )

  compass <- result$contributions |>
    dplyr::filter(
      .data$title == "CF 47936719"
    )

  expect_equal(nrow(compass), 1L)

  expect_identical(
    compass$status,
    "changed_quantity"
  )

  expect_equal(
    compass$quantity_change,
    -57990.724616,
    tolerance = 1e-6
  )

  expect_equal(
    compass$financial_change,
    -99444.73,
    tolerance = 0.01
  )

  expected_result <- 840036.447744 *
    (1.7244137 - 1.7237527)

  expect_equal(
    compass$estimated_result,
    expected_result,
    tolerance = 0.01
  )

  expect_true(
    compass$estimated_result > 0
  )

  expect_true(
    compass$quantity_effect_approx < 0
  )
})


test_that("recognizes Daycoval cash sweep movements", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- estimate_fund_contributions(
    previous,
    current
  )

  sweep <- result$contributions |>
    dplyr::filter(
      .data$title == "TPUBI97062PQ"
    )

  expect_equal(nrow(sweep), 1L)

  expect_identical(
    sweep$status,
    "changed_quantity"
  )

  expect_true(
    sweep$quantity_change < 0
  )

  expect_true(
    sweep$estimated_result > 0
  )

  expect_true(
    sweep$financial_change < 0
  )
})


test_that("matches positions by ISIN after a name change", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$funds$titulo[[1L]] <- "NOVO NOME"

  result <- estimate_fund_contributions(
    previous,
    current
  )

  expect_equal(
    nrow(result$contributions),
    9L
  )

  expect_equal(
    result$summary$n_unestimated,
    0L
  )
})


test_that("does not estimate missing closing prices", {
  previous <- fund_attribution_fixture(
    "linear",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  current$positions$funds$pu[[1L]] <- NA_real_

  result <- estimate_fund_contributions(
    previous,
    current
  )

  expect_equal(
    result$summary$n_unestimated,
    1L
  )

  expect_true(any(
    result$contributions$status == "invalid_price"
  ))

  expect_true(
    result$summary$opening_value_coverage < 1
  )
})


test_that("does not assume zero for missing positions", {
  previous <- fund_attribution_fixture(
    "linear",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  current$positions$funds <-
    current$positions$funds[-1L, ]

  result <- estimate_fund_contributions(
    previous,
    current
  )

  exited <- result$contributions |>
    dplyr::filter(.data$status == "exited")

  expect_equal(nrow(exited), 1L)

  expect_true(
    is.na(exited$estimated_result)
  )

  expect_true(
    is.na(exited$financial_change)
  )

  expect_equal(
    result$summary$n_unestimated,
    1L
  )
})


test_that("preserves missing ISINs as separate records", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  previous$positions$funds$isin[[1L]] <- NA_character_
  current$positions$funds$isin[[1L]] <- NA_character_

  result <- estimate_fund_contributions(
    previous,
    current
  )

  expect_equal(
    sum(result$contributions$status == "missing_isin"),
    2L
  )

  expect_equal(
    result$summary$n_unestimated,
    2L
  )
})


test_that("rejects duplicate ISINs", {
  previous <- fund_attribution_fixture(
    "linear",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  current$positions$funds$isin[[2L]] <-
    current$positions$funds$isin[[1L]]

  expect_error(
    estimate_fund_contributions(previous, current)
  )
})


test_that("rejects different portfolios and invalid dates", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "linear",
    "2026-10-19"
  )

  expect_error(
    estimate_fund_contributions(previous, current)
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  expect_error(
    estimate_fund_contributions(previous, current)
  )
})


test_that("does not mutate imported positions", {
  previous <- fund_attribution_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- fund_attribution_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  estimate_fund_contributions(previous, current)

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
