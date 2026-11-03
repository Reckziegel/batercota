
shareholder_conversion_case <- function() {
  plan_path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  position_path <- testthat::test_path(
    "fixtures",
    "daycoval-position",
    "linear_2026-10-19.csv"
  )

  plan <- read_fund_cash_plan(plan_path)
  position <- read_daycoval_position(position_path)

  diagnostics <- structure(
    list(
      metadata = list(
        codigo_carteira =
          position$metadata$codigo_carteira,
        previous_date = as.Date("2026-10-16"),
        current_date = position$metadata$data_posicao
      ),
      summary = tibble::tibble(
        reported_applications =
          position$shareholder_flows$tot_apl[[1L]],
        reported_redemptions =
          position$shareholder_flows$tot_rgt[[1L]]
      )
    ),
    class = "shareholder_flow_diagnostics"
  )

  list(
    plan = plan,
    position = position,
    diagnostics = diagnostics
  )
}


test_that("reconciles real Linear shareholder conversions", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  expect_s3_class(
    result,
    "shareholder_conversion_reconciliation"
  )

  expect_named(
    result,
    c(
      "metadata",
      "summary",
      "operations",
      "intermediate_operations",
      "issues",
      "shareholder_diagnostics"
    )
  )

  expect_equal(nrow(result$operations), 5L)

  redemptions <- result$summary |>
    dplyr::filter(.data$flow_type == "redemptions")

  expect_equal(redemptions$n_operations, 5L)

  expect_equal(
    redemptions$planned_amount,
    88955.76,
    tolerance = 0.01
  )

  expect_equal(
    redemptions$reported_amount,
    90488.63
  )

  expect_equal(
    redemptions$difference_brl,
    1532.87,
    tolerance = 0.01
  )

  expect_identical(
    redemptions$status,
    "within_tolerance"
  )
})


test_that("preserves operation source references", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  expect_true(all(
    c("source_file", "source_sheet", "source_row") %in%
      names(result$operations)
  ))

  expect_true(all(
    result$operations$source_row %in%
      case$plan$shareholder_operations$source_row
  ))

  expect_true(all(
    result$operations$conversion_date ==
      as.Date("2026-10-19")
  ))
})


test_that("flags materially different reported redemptions", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  case$diagnostics$summary$reported_redemptions <- 1000000

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  redemptions <- result$summary |>
    dplyr::filter(.data$flow_type == "redemptions")

  expect_identical(
    redemptions$status,
    "requires_review"
  )

  expect_true(any(
    result$issues$issue_type == "requires_review"
  ))
})


test_that("does not replace unknown amounts with zero", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  current_rows <- which(
    case$plan$shareholder_operations$conversion_date ==
      as.Date("2026-10-19")
  )

  case$plan$shareholder_operations$gross_value[
    current_rows[[1L]]
  ] <- NA_real_

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  redemptions <- result$summary |>
    dplyr::filter(.data$flow_type == "redemptions")

  expect_true(is.na(redemptions$planned_amount))

  expect_identical(
    redemptions$status,
    "incomplete_internal"
  )
})


test_that("flags movements missing from internal controls", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  case$plan$shareholder_operations <-
    case$plan$shareholder_operations |>
    dplyr::filter(
      .data$conversion_date != as.Date("2026-10-19") |
        is.na(.data$conversion_date)
    )

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  redemptions <- result$summary |>
    dplyr::filter(.data$flow_type == "redemptions")

  expect_equal(redemptions$planned_amount, 0)

  expect_identical(
    redemptions$status,
    "missing_internal_operation"
  )
})


test_that("does not treat missing Daycoval reports as zero", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  case$diagnostics$summary$reported_redemptions <- NA_real_

  result <- reconcile_shareholder_conversions(
    case$plan,
    case$diagnostics,
    case$position
  )

  redemptions <- result$summary |>
    dplyr::filter(.data$flow_type == "redemptions")

  expect_true(is.na(redemptions$difference_brl))

  expect_identical(
    redemptions$status,
    "missing_daycoval_report"
  )
})


test_that("rejects reconciliation between different funds", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  case$plan$shareholder_operations$fund_code <- "12345678"

  expect_error(
    reconcile_shareholder_conversions(
      case$plan,
      case$diagnostics,
      case$position
    ),
    "não corresponde ao fundo Daycoval"
  )
})


test_that("validates tolerance parameters", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  skip_if_not(file.exists(path))

  case <- shareholder_conversion_case()

  expect_error(
    reconcile_shareholder_conversions(
      case$plan,
      case$diagnostics,
      case$position,
      tolerance_pct = -0.1
    )
  )

  expect_error(
    reconcile_shareholder_conversions(
      case$plan,
      case$diagnostics,
      case$position,
      tolerance_brl = NA_real_
    )
  )
})
