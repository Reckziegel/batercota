
make_position_changes_case <- function() {
  start_date <- as.Date("2026-10-16")
  end_date <- as.Date("2026-10-19")

  make_reconciliation <- function(
    date,
    titles,
    quantities,
    values
  ) {
    matches <- tibble::tibble(
      daycoval_title = titles,
      xp_asset_name = paste("Fundo", titles),
      xp_quantity = quantities,
      daycoval_quantity = quantities,
      xp_market_value = values,
      daycoval_market_value = values
    )

    issues <- tibble::tibble(
      issue_type = character(),
      asset_identifier = character(),
      source_file = character(),
      source_row = integer()
    )

    structure(
      list(
        metadata = list(
          position_date = date,
          daycoval_portfolio_code = "13711539",
          xp_account_number = "10850753"
        ),
        matches = matches,
        issues = issues
      ),
      class = "positions_reconciliation"
    )
  }

  make_position <- function(date, titles, isins) {
    funds <- tibble::tibble(
      titulo = titles,
      isin = isins,
      source_file = "position.csv",
      source_line = seq_along(titles)
    )

    structure(
      list(
        metadata = list(
          data_posicao = date,
          codigo_carteira = "13711539",
          cnpj = "60.629.171/0001-06"
        ),
        positions = list(funds = funds)
      ),
      class = "daycoval_position"
    )
  }

  previous <- make_reconciliation(
    start_date,
    c("CF A", "CF B", "CF C"),
    c(100, 15, 30),
    c(1000, 300, 600)
  )

  current <- make_reconciliation(
    end_date,
    c("CF A", "CF B", "CF D"),
    c(90, 20, 50),
    c(925, 405, 1000)
  )

  previous_position <- make_position(
    start_date,
    c("CF A", "CF B", "CF C"),
    c("ISIN_A", "ISIN_B", "ISIN_C")
  )

  current_position <- make_position(
    end_date,
    c("CF A", "CF B", "CF D"),
    c("ISIN_A", "ISIN_B", "ISIN_D")
  )

  records <- tibble::tibble(
    source_file = "operations.xlsx",
    source_row = 2:5,
    portfolio_fund_cnpj = "60629171000106",
    asset_identifier = c(
      "Fundo A", "Fundo B", "Fundo C", "Fundo A"
    ),
    asset_isin = c(
      "ISIN_A", "ISIN_B", "ISIN_C", "ISIN_A"
    ),
    operation_type = c(
      "Resgate Bruto",
      "Aplicação",
      "Resgate Total",
      "Resgate Bruto"
    ),
    operation_date = as.Date(c(
      "2026-04-20",
      "2026-10-17",
      "2026-05-27",
      "2026-04-27"
    )),
    conversion_date = as.Date(c(
      "2026-10-19",
      "2026-10-18",
      "2026-10-19",
      "2026-10-26"
    )),
    quantity = c(0, 5, NA_real_, 0),
    value = c(1000, 100, NA_real_, 2000)
  )

  operations <- structure(
    list(operations = records),
    class = "fund_operations"
  )

  list(
    previous = previous,
    current = current,
    operations = operations,
    previous_position = previous_position,
    current_position = current_position
  )
}


run_position_changes_case <- function(case) {
  reconcile_position_changes(
    previous = case$previous,
    current = case$current,
    operations = case$operations,
    previous_position = case$previous_position,
    current_position = case$current_position
  )
}


test_that("identifies holdings, entries and exits", {
  result <- run_position_changes_case(
    make_position_changes_case()
  )

  expect_s3_class(result, "position_changes")

  changes <- result$changes

  expect_setequal(
    changes$daycoval_title,
    c("CF A", "CF B", "CF C", "CF D")
  )

  expect_identical(
    changes$change_type,
    c("held", "held", "exited", "entered")
  )

  exited <- changes |>
    dplyr::filter(.data$daycoval_title == "CF C")

  entered <- changes |>
    dplyr::filter(.data$daycoval_title == "CF D")

  expect_true(is.na(exited$xp_quantity_change))
  expect_true(is.na(entered$xp_quantity_change))
})


test_that("preserves quantity and market value changes", {
  result <- run_position_changes_case(
    make_position_changes_case()
  )

  fund_a <- result$changes |>
    dplyr::filter(.data$daycoval_title == "CF A")

  fund_b <- result$changes |>
    dplyr::filter(.data$daycoval_title == "CF B")

  expect_equal(fund_a$xp_quantity_change, -10)
  expect_equal(fund_a$daycoval_quantity_change, -10)

  expect_equal(fund_a$xp_market_value_change, -75)
  expect_equal(fund_b$xp_quantity_change, 5)
  expect_equal(fund_b$xp_market_value_change, 105)
})


test_that("uses the conversion window without inferring quantities", {
  result <- run_position_changes_case(
    make_position_changes_case()
  )

  expect_equal(nrow(result$links), 3L)

  expect_true(all(
    result$links$conversion_date >
      as.Date("2026-10-16")
  ))

  expect_true(all(
    result$links$conversion_date <=
      as.Date("2026-10-19")
  ))

  fund_a <- result$changes |>
    dplyr::filter(.data$daycoval_title == "CF A")

  fund_b <- result$changes |>
    dplyr::filter(.data$daycoval_title == "CF B")

  expect_equal(fund_a$n_conversions, 1L)
  expect_true(is.na(fund_a$signed_operation_quantity))

  expect_identical(
    fund_a$operation_status,
    "quantity_unavailable"
  )

  expect_equal(fund_b$signed_operation_quantity, 5)

  expect_identical(
    fund_b$operation_status,
    "quantity_reported_unconfirmed"
  )
})


test_that("rejects reversed position dates", {
  case <- make_position_changes_case()

  expect_error(
    reconcile_position_changes(
      previous = case$current,
      current = case$previous,
      operations = case$operations,
      previous_position = case$current_position,
      current_position = case$previous_position
    ),
    "datas válidas e crescentes"
  )
})


test_that("does not associate operations with conflicting ISINs", {
  case <- make_position_changes_case()

  case$current_position$positions$funds$isin[
    case$current_position$positions$funds$titulo == "CF A"
  ] <- "ISIN_DIFFERENT"

  result <- run_position_changes_case(case)

  fund_a <- result$changes |>
    dplyr::filter(.data$daycoval_title == "CF A")

  expect_true(is.na(fund_a$asset_isin))
  expect_equal(fund_a$n_conversions, 0L)

  expect_true(any(
    result$issues$issue_type == "isin_conflict"
  ))
})
