
shareholder_liability_fixture <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      cached <<- list(
        plan = read_fund_cash_plan(
          testthat::test_path(
            "..", "..", "plataforma",
            "fundos - Movimentações Previstas LINEAR.xlsx"
          )
        ),
        movement = read_daycoval_daily_movement(
          testthat::test_path(
            "fixtures", "daycoval-financial",
            "linear_movement_2026-10-16_2026-10-22.csv"
          )
        ),
        cash = read_daycoval_cash_statement(
          testthat::test_path(
            "fixtures", "daycoval-financial",
            "linear_cash_2026-10-16_2026-10-22.csv"
          )
        ),
        opening = read_daycoval_position(
          testthat::test_path(
            "fixtures", "daycoval-position",
            "linear_2026-10-16.csv"
          )
        ),
        checkpoint = read_daycoval_position(
          testthat::test_path(
            "fixtures", "daycoval-position",
            "linear_2026-10-19.csv"
          )
        )
      )
    }

    cached
  }
})


shareholder_liability_case <- function(case = shareholder_liability_fixture()) {
  build_shareholder_liability_timeline(
    cash_plan = case$plan,
    daily_movement = case$movement,
    cash_statement = case$cash,
    opening_position = case$opening,
    positions = list(case$checkpoint)
  )
}


test_that("reconstructs four daily closing liabilities", {
  result <- shareholder_liability_case()
  timeline <- result$timeline

  expect_s3_class(result, "shareholder_liability_timeline")
  expect_equal(
    timeline$date,
    as.Date(c(
      "2026-10-19", "2026-10-20", "2026-10-21", "2026-10-22"
    ))
  )
  expect_equal(
    timeline$daycoval_opening,
    c(65969.19, 156457.82, 185636.67, 128050.60)
  )
  expect_equal(
    timeline$daycoval_conversions,
    c(90488.63, 29178.85, 32902.56, 0)
  )
  expect_equal(
    timeline$daycoval_settlements,
    c(0, 0, 90488.63, 29178.85)
  )
  expect_equal(
    timeline$daycoval_closing,
    c(156457.82, 185636.67, 128050.60, 98871.75)
  )
})


test_that("includes the manager's planned conversions and payments", {
  result <- shareholder_liability_case()
  timeline <- result$timeline

  expect_equal(result$metadata$manager_opening_liability, 0)
  expect_equal(
    timeline$manager_conversions,
    c(88955.76, 29169.07, 26839.68, 0),
    tolerance = 0.01
  )
  expect_equal(
    timeline$manager_scheduled_payments,
    c(0, 0, 88955.76, 29169.07),
    tolerance = 0.01
  )
  expect_equal(
    timeline$manager_anchored_closing,
    c(154924.95, 184094.02, 121977.94, 92808.87),
    tolerance = 0.01
  )
  expect_equal(
    timeline$closing_difference,
    c(1532.87, 1542.65, 6072.66, 6062.88),
    tolerance = 0.01
  )
})


test_that("verifies payments using cash statement identifiers", {
  result <- shareholder_liability_case()
  links <- result$settlement_links
  timeline <- result$timeline

  expect_equal(nrow(links), 3L)
  expect_true(all(links$verification == "verified"))
  expect_equal(
    links$reg_cp,
    c("1936158011", "1938143403", "1939079629")
  )
  expect_equal(
    timeline$verified_cash_settlements,
    c(0, 0, 90488.63, 29178.85)
  )
  expect_equal(timeline$unverified_cash_settlements, c(0, 0, 0, 0))
  expect_identical(
    timeline$payment_status,
    c("no_settlement", "no_settlement", "verified", "verified")
  )
})


test_that("matches independent position checkpoint", {
  result <- shareholder_liability_case()
  timeline <- result$timeline

  expect_equal(timeline$reported_closing[[1L]], 156457.82)
  expect_equal(timeline$checkpoint_difference[[1L]], 0)
  expect_identical(timeline$checkpoint_status[[1L]], "consistent")
  expect_true(all(timeline$checkpoint_status[-1L] == "not_checked"))
})


test_that("flags material daily conversion differences", {
  result <- shareholder_liability_case()
  timeline <- result$timeline

  expect_equal(timeline$conversion_difference[[3L]], 6062.88,
               tolerance = 0.01)
  expect_identical(timeline$manager_status[[3L]], "requires_review")
  expect_true(any(result$issues$issue_type == "manager_difference"))
})


test_that("does not claim settlement when cash evidence is missing", {
  case <- shareholder_liability_fixture()
  case$cash$movements <- case$cash$movements |>
    dplyr::filter(.data$reg != "1938143403")

  result <- shareholder_liability_case(case)
  links <- result$settlement_links

  expect_true(any(links$verification == "missing_cash_movement"))
  expect_identical(
    result$timeline$payment_status[[3L]],
    "requires_review"
  )
  expect_equal(
    result$timeline$unverified_cash_settlements[[3L]],
    90488.63
  )
})


test_that("never replaces a missing manager estimate with zero", {
  case <- shareholder_liability_fixture()
  index <- which(
    case$plan$shareholder_operations$conversion_date ==
      as.Date("2026-10-19")
  )[[1L]]

  case$plan$shareholder_operations$gross_value[[index]] <- NA_real_
  result <- shareholder_liability_case(case)

  expect_true(all(is.na(result$timeline$manager_conversions)))
  expect_true(all(is.na(result$timeline$manager_anchored_closing)))
  expect_true(any(
    result$issues$issue_type == "invalid_internal_redemption"
  ))
})


test_that("rejects reports from different funds", {
  case <- shareholder_liability_fixture()
  case$cash$metadata$codigo_carteira <- "99999999"

  expect_error(
    shareholder_liability_case(case),
    "carteiras diferentes"
  )
})


test_that("validates estimated-value tolerances", {
  case <- shareholder_liability_fixture()

  expect_error(
    build_shareholder_liability_timeline(
      case$plan, case$movement, case$cash,
      case$opening, tolerance_pct = -0.01
    ),
    "tolerâncias"
  )
})
