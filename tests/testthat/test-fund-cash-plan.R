
cash_plan_fixture <- function(fund) {
  filenames <- c(
    linear = "fundos - Movimentações Previstas LINEAR.xlsx",
    reta_alocacao = "fundos - Movimentações Previstas Reta Alocação.xlsx",
    long_term = "fundos - Movimentações Previstas Long Term LB.xlsx"
  )

  testthat::test_path(
    "..",
    "..",
    "plataforma",
    filenames[[fund]]
  )
}


test_that("imports the three Daycoval fund cash plans", {
  for (fund in c("linear", "reta_alocacao", "long_term")) {
    path <- cash_plan_fixture(fund)

    skip_if_not(file.exists(path))

    result <- read_fund_cash_plan(path)

    expect_s3_class(result, "fund_cash_plan")

    expect_named(
      result,
      c(
        "metadata",
        "shareholder_operations",
        "investment_operations",
        "cash_flows",
        "issues",
        "raw"
      )
    )

    expect_s3_class(result$shareholder_operations, "tbl_df")
    expect_s3_class(result$investment_operations, "tbl_df")
    expect_s3_class(result$cash_flows, "tbl_df")
    expect_s3_class(result$issues, "tbl_df")

    expect_gt(nrow(result$shareholder_operations), 0L)
    expect_gt(nrow(result$investment_operations), 0L)
    expect_gt(nrow(result$cash_flows), 0L)

    expect_equal(
      result$metadata$n_shareholder_operations,
      nrow(result$shareholder_operations)
    )

    expect_equal(
      result$metadata$n_investment_operations,
      nrow(result$investment_operations)
    )

    expect_equal(
      result$metadata$n_cash_flow_dates,
      nrow(result$cash_flows)
    )
  }
})


test_that("preserves operation identities and missing identifiers", {
  path <- cash_plan_fixture("linear")
  skip_if_not(file.exists(path))

  result <- read_fund_cash_plan(path)

  shareholder <- result$shareholder_operations
  investments <- result$investment_operations

  expect_type(shareholder$operation_id, "character")
  expect_type(investments$operation_id, "character")

  expect_true(anyNA(shareholder$operation_id))
  expect_true(anyNA(investments$operation_id))

  expect_true(all(shareholder$source_row >= 2L))
  expect_true(all(investments$source_row >= 2L))

  expect_false(anyDuplicated(investments$source_row) > 0L)

  expect_true(all(
    investments$source_row %in%
      result$raw$investment_operations$source_row
  ))
})


test_that("preserves dates and reported financial values", {
  path <- cash_plan_fixture("reta_alocacao")
  skip_if_not(file.exists(path))

  result <- read_fund_cash_plan(path)

  shareholder <- result$shareholder_operations
  investments <- result$investment_operations
  flows <- result$cash_flows

  expect_s3_class(shareholder$operation_date, "Date")
  expect_s3_class(shareholder$conversion_date, "Date")
  expect_s3_class(shareholder$liquidation_date, "Date")

  expect_s3_class(investments$conversion_date, "Date")
  expect_s3_class(investments$liquidation_date, "Date")

  expect_s3_class(flows$date, "Date")
  expect_s3_class(flows$shareholder_conversion_date, "Date")

  expect_type(shareholder$gross_value, "double")
  expect_type(investments$gross_value, "double")
  expect_type(flows$cumulative_cash_balance, "double")

  expect_false(anyNA(flows$date))
})


test_that("imports Brazilian numbers stored as text", {
  expect_equal(
    cash_plan_parse_cell("\u00a090.955,98", "number"),
    90955.98
  )

  expect_equal(
    cash_plan_parse_cell("R$ 1.234,56", "number"),
    1234.56
  )

  expect_true(is.na(
    cash_plan_parse_cell("valor inválido", "number")
  ))

  path <- cash_plan_fixture("long_term")
  skip_if_not(file.exists(path))

  result <- read_fund_cash_plan(path)

  operation <- result$investment_operations[
    result$investment_operations$source_row == 54L,
    ,
    drop = FALSE
  ]

  expect_equal(nrow(operation), 1L)
  expect_equal(operation$gross_value[[1L]], 90955.98)
})


test_that("distinguishes formulas from manual cash balances", {
  path <- cash_plan_fixture("long_term")
  skip_if_not(file.exists(path))

  result <- read_fund_cash_plan(path)
  flows <- result$cash_flows

  expect_true(any(flows$cash_balance_is_formula))
  expect_true(any(flows$cash_balance_is_manual))

  expect_false(any(
    flows$cash_balance_is_formula &
      flows$cash_balance_is_manual
  ))

  expect_equal(
    result$metadata$n_formula_cash_balances,
    sum(flows$cash_balance_is_formula)
  )

  expect_equal(
    result$metadata$n_manual_cash_balances,
    sum(flows$cash_balance_is_manual)
  )
})


test_that("preserves zero and missing values separately", {
  path <- cash_plan_fixture("long_term")
  skip_if_not(file.exists(path))

  result <- read_fund_cash_plan(path)
  flows <- result$cash_flows

  expect_true(any(flows$estimated_shareholder_redemptions == 0))
  expect_true(any(is.na(flows$other_movements)))

  expect_true(all(
    is.finite(flows$cumulative_cash_balance[
      !is.na(flows$cumulative_cash_balance)
    ])
  ))
})


test_that("parses valid dates without silently fixing invalid ones", {
  expect_identical(
    cash_plan_parse_cell("19/10/2026", "date"),
    as.Date("2026-10-19")
  )

  expect_identical(
    cash_plan_parse_cell("2026-10-19", "date"),
    as.Date("2026-10-19")
  )

  expect_true(is.na(
    cash_plan_parse_cell("31/02/2026", "date")
  ))

  expect_true(is.na(
    cash_plan_parse_cell("data inválida", "date")
  ))
})


test_that("rejects invalid workbook paths", {
  expect_error(
    read_fund_cash_plan("arquivo_inexistente.xlsx"),
    "arquivo XLSX existente"
  )
})
