
daycoval_financial_fixture <- function(report) {
  filenames <- c(
    movement = "linear_movement_2026-10-16_2026-10-22.csv",
    cash = "linear_cash_2026-10-16_2026-10-22.csv"
  )

  testthat::test_path(
    "fixtures",
    "daycoval-financial",
    filenames[[report]]
  )
}


daycoval_financial_case <- function() {
  list(
    movement = read_daycoval_daily_movement(
      daycoval_financial_fixture("movement")
    ),
    cash = read_daycoval_cash_statement(
      daycoval_financial_fixture("cash")
    )
  )
}


test_that("imports five days of real accounting movements", {
  result <- read_daycoval_daily_movement(
    daycoval_financial_fixture("movement")
  )

  expect_s3_class(result, "daycoval_daily_movement")

  expect_named(
    result,
    c("metadata", "entries", "layouts", "raw")
  )

  expect_equal(nrow(result$entries), 340L)
  expect_equal(result$metadata$n_days, 5L)

  expect_identical(
    result$metadata$codigo_carteira,
    "13711539"
  )

  expect_identical(
    result$metadata$data_inicial,
    as.Date("2026-10-16")
  )

  expect_identical(
    result$metadata$data_final,
    as.Date("2026-10-22")
  )

  expect_equal(
    result$metadata$dates,
    as.Date(c(
      "2026-10-16",
      "2026-10-19",
      "2026-10-20",
      "2026-10-21",
      "2026-10-22"
    ))
  )
})


test_that("preserves accounting source references", {
  result <- read_daycoval_daily_movement(
    daycoval_financial_fixture("movement")
  )

  entries <- result$entries

  expect_type(entries$reg, "character")
  expect_type(entries$reg_cp, "character")

  expect_false(anyNA(entries$reg))
  expect_false(anyDuplicated(entries$reg) > 0L)

  expect_identical(
    entries$source_line,
    seq_len(340L)
  )

  expect_true(all(
    entries$record_number ==
      as.character(seq_len(340L))
  ))

  expect_true(
    "P0115_QuebraPag" %in% result$layouts$record_type
  )
})


test_that("imports cash movements without daily totalizers", {
  result <- read_daycoval_cash_statement(
    daycoval_financial_fixture("cash")
  )

  expect_s3_class(result, "daycoval_cash_statement")

  expect_equal(nrow(result$movements), 18L)
  expect_equal(nrow(result$daily_totals), 4L)

  expect_true(all(result$movements$tp_det == "2"))
  expect_true(all(result$daily_totals$tp_det == "3"))

  expect_identical(
    result$metadata$codigo_carteira,
    "13711539"
  )

  expect_identical(
    result$metadata$opening_date,
    as.Date("2026-10-15")
  )

  expect_identical(
    result$metadata$closing_date,
    as.Date("2026-10-22")
  )

  expect_equal(result$metadata$opening_balance, 1000)
  expect_equal(result$metadata$closing_balance, 1000)

  expect_equal(result$metadata$net_cash_flow, 0)
  expect_equal(result$metadata$balance_difference, 0)

  expect_equal(nrow(result$issues), 0L)
})


test_that("does not invent cash movements on inactive days", {
  result <- read_daycoval_cash_statement(
    daycoval_financial_fixture("cash")
  )

  expected_dates <- as.Date(c(
    "2026-10-16",
    "2026-10-20",
    "2026-10-21",
    "2026-10-22"
  ))

  expect_equal(
    sort(unique(result$movements$dt)),
    expected_dates
  )

  expect_false(any(
    result$movements$dt == as.Date("2026-10-19")
  ))
})


test_that("matches every cash movement with an accounting entry", {
  case <- daycoval_financial_case()

  accounting_cash <- case$movement$entries |>
    dplyr::filter(.data$grupo == "1.01.08")

  cash <- case$cash$movements

  expect_equal(nrow(accounting_cash), 18L)
  expect_equal(nrow(cash), 18L)

  index <- match(
    cash$reg,
    accounting_cash$reg
  )

  expect_false(anyNA(index))
  expect_false(anyDuplicated(index) > 0L)

  expect_equal(
    cash$dt,
    accounting_cash$data_mov[index]
  )

  expect_equal(
    dplyr::coalesce(cash$vl_deb, 0),
    accounting_cash$debito[index],
    tolerance = 0.01
  )

  expect_equal(
    dplyr::coalesce(cash$vl_cred, 0),
    accounting_cash$credito[index],
    tolerance = 0.01
  )
})


test_that("identifies actual shareholder redemption payments", {
  case <- daycoval_financial_case()

  payments <- case$cash$movements |>
    dplyr::filter(.data$tit_cp == "RESGA") |>
    dplyr::arrange(.data$dt)

  expect_equal(nrow(payments), 3L)

  expect_equal(
    payments$dt,
    as.Date(c(
      "2026-10-16",
      "2026-10-21",
      "2026-10-22"
    ))
  )

  expect_equal(
    payments$vl_cred,
    c(177401.31, 90488.63, 29178.85)
  )

  expect_equal(
    payments$reg,
    c("1936158011", "1938143403", "1939079629")
  )
})


test_that("reconstructs real shareholder liability movements", {
  result <- read_daycoval_daily_movement(
    daycoval_financial_fixture("movement")
  )

  liabilities <- result$entries |>
    dplyr::filter(.data$grupo == "2.10.10") |>
    dplyr::group_by(.data$data_mov) |>
    dplyr::summarise(
      conversions = sum(.data$credito),
      settlements = sum(.data$debito),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$data_mov)

  expect_equal(
    liabilities$data_mov,
    as.Date(c(
      "2026-10-16",
      "2026-10-19",
      "2026-10-20",
      "2026-10-21",
      "2026-10-22"
    ))
  )

  expect_equal(
    liabilities$conversions,
    c(0, 90488.63, 29178.85, 32902.56, 0)
  )

  expect_equal(
    liabilities$settlements,
    c(177401.31, 0, 0, 90488.63, 29178.85)
  )
})


test_that("keeps nonstandard redemption expenses separate", {
  result <- read_daycoval_cash_statement(
    daycoval_financial_fixture("cash")
  )

  pco <- result$movements |>
    dplyr::filter(.data$tit_cp == "@VARSG")

  overpayment <- result$movements |>
    dplyr::filter(.data$tit_cp == "VLR A REC")

  expect_equal(nrow(pco), 1L)
  expect_equal(nrow(overpayment), 1L)

  expect_equal(pco$vl_cred, 7916.35)
  expect_equal(overpayment$vl_cred, 308.57)

  expect_false(any(
    c(pco$reg, overpayment$reg) %in%
      result$movements$reg[
        result$movements$tit_cp == "RESGA"
      ]
  ))
})


test_that("preserves the complete raw reports", {
  case <- daycoval_financial_case()

  expect_equal(nrow(case$movement$raw), 342L)
  expect_equal(nrow(case$cash$raw), 30L)

  expect_true(any(
    case$movement$raw$record_flag == "C",
    na.rm = TRUE
  ))

  expect_true(any(
    case$cash$raw$record_flag == "C",
    na.rm = TRUE
  ))

  expect_true(all(
    case$cash$movements$source_line %in%
      case$cash$raw$source_line
  ))
})


test_that("rejects incompatible report types", {
  expect_error(
    read_daycoval_daily_movement(
      daycoval_financial_fixture("cash")
    ),
    "layouts obrigatórios"
  )

  expect_error(
    read_daycoval_cash_statement(
      daycoval_financial_fixture("movement")
    ),
    "layouts obrigatórios"
  )
})


test_that("parses Brazilian thousands separators correctly", {
  data <- tibble::tibble(
    source_line = 1:4,
    quantidade = c(
      "57.990,724616",
      "1.250.000,50",
      "-156.457,82",
      NA_character_
    )
  )

  result <- daycoval_financial_parse_fields(
    data,
    numeric_columns = "quantidade"
  )

  expect_equal(
    result$quantidade[1:3],
    c(57990.724616, 1250000.50, -156457.82)
  )

  expect_true(is.na(result$quantidade[[4L]]))
})


test_that("rejects malformed Brazilian numbers", {
  data <- tibble::tibble(
    source_line = 67L,
    quantidade = "57.99,724616"
  )

  expect_error(
    daycoval_financial_parse_fields(
      data,
      numeric_columns = "quantidade"
    ),
    "Valor numérico inválido"
  )
})

