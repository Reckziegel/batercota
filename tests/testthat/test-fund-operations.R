
fund_operations_fixture <- function() {
  testthat::test_path(
    "fixtures",
    "operations",
    "fund_operations.xlsx"
  )
}


read_fund_operations_fixture <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      cached <<- read_fund_operations(
        fund_operations_fixture()
      )
    }

    cached
  }
})


test_that("imports the real operations workbook", {
  result <- read_fund_operations_fixture()

  expect_s3_class(result, "fund_operations")

  expect_named(
    result,
    c("metadata", "operations", "issues", "raw")
  )

  expect_s3_class(result$operations, "tbl_df")
  expect_s3_class(result$issues, "tbl_df")
  expect_s3_class(result$raw, "tbl_df")

  expect_equal(nrow(result$operations), 9734L)
  expect_equal(nrow(result$raw), 9734L)

  expect_identical(result$metadata$n_operations, 9734L)
  expect_equal(result$metadata$n_portfolios, 4L)

  expect_identical(
    result$metadata$operation_start,
    as.Date("2024-04-30")
  )

  expect_identical(
    result$metadata$operation_end,
    as.Date("2026-10-26")
  )
})


test_that("preserves all four operation types", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  counts <- table(operations$operation_type)

  expect_equal(as.integer(counts[["Aplicação"]]), 5250L)
  expect_equal(as.integer(counts[["Resgate Cotas"]]), 3814L)
  expect_equal(as.integer(counts[["Resgate Bruto"]]), 560L)
  expect_equal(as.integer(counts[["Resgate Total"]]), 110L)

  expect_equal(sum(counts), 9734L)

  expect_equal(
    sum(operations$is_total_redemption),
    110L
  )

  expect_true(all(
    operations$is_total_redemption ==
      (operations$operation_type == "Resgate Total")
  ))
})


test_that("keeps missing redemption amounts distinct from zero", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  missing_value <- is.na(operations$value)

  expect_equal(sum(missing_value), 18L)

  expect_true(all(
    operations$operation_type[missing_value] == "Resgate Total"
  ))

  full_redemptions <- operations |>
    dplyr::filter(.data$is_total_redemption)

  expect_equal(nrow(full_redemptions), 110L)
  expect_equal(sum(is.na(full_redemptions$value)), 18L)
  expect_equal(sum(!is.na(full_redemptions$value)), 92L)
  expect_equal(sum(full_redemptions$value == 0, na.rm = TRUE), 22L)

  expect_equal(
    sum(operations$value == 0, na.rm = TRUE),
    28L
  )

  first_operation <- operations |>
    dplyr::filter(.data$source_row == 2L)

  expect_equal(nrow(first_operation), 1L)
  expect_identical(
    first_operation$operation_type[[1L]],
    "Resgate Total"
  )

  expect_true(is.na(first_operation$value[[1L]]))
  expect_true(is.na(first_operation$quantity[[1L]]))
  expect_true(is.na(first_operation$net_value[[1L]]))
})


test_that("preserves numerical information and missing quantities", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  expect_type(operations$value, "double")
  expect_type(operations$quantity, "double")
  expect_type(operations$net_value, "double")

  expect_equal(sum(is.na(operations$quantity)), 38L)
  expect_equal(sum(is.na(operations$net_value)), 37L)

  expect_true(all(
    is.finite(operations$value[!is.na(operations$value)])
  ))

  expect_true(all(
    is.finite(operations$quantity[!is.na(operations$quantity)])
  ))
})


test_that("converts portfolio CNPJs into character identifiers", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  expect_type(
    operations$portfolio_fund_cnpj,
    "character"
  )

  expect_true(all(
    grepl(
      "^[0-9]{14}$",
      operations$portfolio_fund_cnpj
    )
  ))

  expect_setequal(
    unique(operations$portfolio_fund_cnpj),
    c(
      "54236091000198",
      "57593831000131",
      "56365598000177",
      "60629171000106"
    )
  )

  expect_identical(
    fund_operations_cnpj(54236091000198, 2L),
    "54236091000198"
  )

  expect_error(
    fund_operations_cnpj("CNPJ_INVALIDO", 2L),
    "CNPJ inválido"
  )
})


test_that("accepts missing historical ISINs", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  expect_equal(
    sum(is.na(operations$asset_isin)),
    4772L
  )

  expect_equal(result$metadata$n_missing_isin, 4772L)

  expect_true(all(
    !is.na(operations$asset_identifier)
  ))

  expect_identical(
    operations$asset_identifier[[1L]],
    "TREND INFLACAO LONGA FI RF"
  )
})


test_that("preserves operation, conversion and settlement dates", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  for (field in c(
    "operation_date",
    "conversion_date",
    "liquidation_date"
  )) {
    expect_s3_class(operations[[field]], "Date")
  }

  first_operation <- operations |>
    dplyr::filter(.data$source_row == 2L)

  expect_identical(
    first_operation$operation_date[[1L]],
    as.Date("2026-10-26")
  )

  expect_identical(
    first_operation$conversion_date[[1L]],
    as.Date("2026-10-26")
  )

  expect_identical(
    first_operation$liquidation_date[[1L]],
    as.Date("2026-10-27")
  )
})


test_that("imports corrected dates without reporting issues", {
  result <- read_fund_operations_fixture()

  expect_equal(nrow(result$issues), 0L)
  expect_equal(result$metadata$n_issues, 0L)

  operation <- result$operations |>
    dplyr::filter(.data$source_row == 35L)

  expect_equal(nrow(operation), 1L)

  expect_false(
    is.na(operation$liquidation_date[[1L]])
  )
})


test_that("detects invalid dates without modifying the source", {
  result <- read_fund_operations_fixture()

  raw <- result$raw
  operations <- result$operations

  i <- which(raw$source_row == 35L)

  expect_length(i, 1L)

  # Simulate the original data-entry error.
  raw$liquidation_date[[i]] <- "24/07/206"
  operations$liquidation_date[i] <- as.Date(NA)

  issues <- fund_operations_date_issues(
    raw = raw,
    operations = operations
  )

  expect_equal(nrow(issues), 1L)

  expect_identical(
    issues$source_row[[1L]],
    35L
  )

  expect_identical(
    issues$field[[1L]],
    "liquidation_date"
  )

  expect_identical(
    issues$issue_type[[1L]],
    "invalid_date"
  )

  expect_identical(
    issues$raw_value[[1L]],
    "24/07/206"
  )

  expect_identical(
    raw$liquidation_date[[i]],
    "24/07/206"
  )
})

test_that("does not silently correct malformed dates", {
  expect_identical(
    fund_operations_date("24/07/206"),
    as.Date(NA)
  )

  expect_identical(
    fund_operations_date("29/02/2024"),
    as.Date("2024-02-29")
  )

  expect_true(is.na(
    fund_operations_date("31/02/2026")
  ))

  expect_true(is.na(
    fund_operations_date("")
  ))
})


test_that("keeps the original Excel row references", {
  result <- read_fund_operations_fixture()

  expect_identical(
    result$operations$source_row,
    seq.int(2L, 9735L)
  )

  expect_identical(
    result$raw$source_row,
    seq.int(2L, 9735L)
  )

  expect_true(all(
    result$operations$source_file ==
      "fund_operations.xlsx"
  ))

  expect_named(
    result$raw,
    c(
      "source_file",
      "source_row",
      "portfolio_fund_cnpj",
      "portfolio_fund_name",
      "asset_identifier",
      "asset_isin",
      "conversion_date",
      "liquidation_date",
      "operation_date",
      "operation_type",
      "value",
      "Quantidade",
      "Vl Líquido"
    )
  )
})


test_that("rejects missing values in operations other than total redemptions", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  operations$operation_type[[1L]] <- "Aplicação"
  operations$is_total_redemption[[1L]] <- FALSE

  expect_error(
    fund_operations_validate_semantics(operations),
    "não é Resgate Total"
  )
})


test_that("rejects unknown operation types", {
  result <- read_fund_operations_fixture()
  operations <- result$operations

  operations$operation_type[[1L]] <- "Operação Desconhecida"

  expect_error(
    fund_operations_validate_semantics(operations),
    "Operação desconhecida"
  )
})


test_that("validates workbook paths and source names", {
  expect_error(
    read_fund_operations(
      file.path(tempdir(), "operacoes_inexistentes.xlsx")
    ),
    "path"
  )

  expect_error(
    read_fund_operations(
      fund_operations_fixture(),
      source_name = ""
    ),
    "source_name"
  )
})
