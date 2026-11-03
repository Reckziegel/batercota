
shareholder_payables_fixture <- function(fund) {
  filenames <- c(
    linear = "fundos - Movimentações Previstas LINEAR.xlsx",
    long_term = "fundos - Movimentações Previstas Long Term LB.xlsx"
  )

  plan <- read_fund_cash_plan(
    testthat::test_path(
      "..",
      "..",
      "plataforma",
      filenames[[fund]]
    )
  )

  position_path <- function(date) {
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  }

  list(
    plan = plan,
    previous = read_daycoval_position(
      position_path("2026-10-16")
    ),
    current = read_daycoval_position(
      position_path("2026-10-19")
    )
  )
}


test_that("identifies real Linear redemption liabilities", {
  case <- shareholder_payables_fixture("linear")

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  expect_s3_class(
    result,
    "shareholder_payables_reconciliation"
  )

  summary <- result$summary

  expect_equal(summary$reported_opening, 65969.19)
  expect_equal(summary$reported_closing, 156457.82)
  expect_equal(summary$reported_change, 90488.63)

  expect_equal(nrow(result$previous_payables), 1L)
  expect_equal(nrow(result$current_payables), 1L)

  expect_identical(
    as.character(result$current_payables$grupo),
    "21010"
  )
})


test_that("bridges Linear liabilities using internal flows", {
  case <- shareholder_payables_fixture("linear")

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  summary <- result$summary

  expect_equal(summary$planned_opening, 0)
  expect_equal(summary$planned_closing, 88955.76)

  expect_equal(
    summary$planned_new_conversions,
    88955.76
  )

  expect_equal(summary$planned_payments, 0)

  expect_equal(
    summary$anchored_closing,
    154924.95
  )

  expect_equal(
    summary$difference_brl,
    1532.87
  )

  expect_identical(
    summary$stock_status,
    "opening_unmatched"
  )

  expect_identical(
    summary$bridge_status,
    "within_tolerance_conditional"
  )

  expect_false(result$metadata$settlement_verified)
})


test_that("preserves individual pending redemptions", {
  case <- shareholder_payables_fixture("linear")

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  expect_equal(nrow(result$closing_operations), 5L)

  expect_true(all(
    result$closing_operations$conversion_date ==
      as.Date("2026-10-19")
  ))

  expect_true(all(
    result$closing_operations$liquidation_date ==
      as.Date("2026-10-21")
  ))

  expect_true(all(
    result$closing_operations$source_row %in%
      case$plan$shareholder_operations$source_row
  ))
})


test_that("flags a real Long Term financial discrepancy", {
  case <- shareholder_payables_fixture("long_term")

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  summary <- result$summary

  expect_equal(summary$reported_opening, 0)
  expect_equal(summary$reported_closing, 1396.03)

  expect_equal(
    summary$planned_new_conversions,
    600
  )

  expect_equal(
    summary$difference_brl,
    796.03
  )

  expect_identical(
    summary$bridge_status,
    "requires_review"
  )
})


test_that("does not mistake a missing report for zero", {
  case <- shareholder_payables_fixture("linear")

  case$current$payables <- NULL

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  expect_true(is.na(
    result$summary$reported_closing
  ))

  expect_true(is.na(
    result$summary$difference_brl
  ))

  expect_identical(
    result$summary$bridge_status,
    "missing_daycoval"
  )
})


test_that("retains operations with missing financial values", {
  case <- shareholder_payables_fixture("linear")

  index <- which(
    case$plan$shareholder_operations$conversion_date ==
      as.Date("2026-10-19")
  )

  case$plan$shareholder_operations$gross_value[
    index[[1L]]
  ] <- NA_real_

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  expect_equal(nrow(result$invalid_operations), 1L)

  expect_identical(
    result$summary$bridge_status,
    "incomplete_internal"
  )

  expect_true(any(
    result$issues$issue_type == "invalid_internal_operation"
  ))
})


test_that("scheduled payment changes outstanding balance", {
  case <- shareholder_payables_fixture("linear")

  index <- which(
    case$plan$shareholder_operations$conversion_date ==
      as.Date("2026-10-19")
  )

  case$plan$shareholder_operations$liquidation_date[
    index[[1L]]
  ] <- as.Date("2026-10-19")

  result <- reconcile_shareholder_payables(
    case$plan,
    case$previous,
    case$current
  )

  expect_equal(nrow(result$closing_operations), 4L)
  expect_equal(nrow(result$scheduled_payments), 1L)

  expect_gt(
    result$summary$planned_payments,
    0
  )
})


test_that("rejects different fund positions", {
  linear <- shareholder_payables_fixture("linear")
  long_term <- shareholder_payables_fixture("long_term")

  expect_error(
    reconcile_shareholder_payables(
      linear$plan,
      linear$previous,
      long_term$current
    ),
    "fundos diferentes"
  )
})


test_that("validates financial tolerances", {
  case <- shareholder_payables_fixture("linear")

  expect_error(
    reconcile_shareholder_payables(
      case$plan,
      case$previous,
      case$current,
      tolerance_brl = -1
    )
  )

  expect_error(
    reconcile_shareholder_payables(
      case$plan,
      case$previous,
      case$current,
      tolerance_pct = NA_real_
    )
  )
})
