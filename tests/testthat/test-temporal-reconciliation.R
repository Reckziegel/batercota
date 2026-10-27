
temporal_reconciliation_cases <- function() {
  list(
    long_term_16 = list(
      fund = "long_term",
      date = as.Date("2026-10-16"),
      portfolio_code = "13386735",
      xp_account = "10850552",
      xp_file = "Extrato_10850552_26_10_2026 18_50_28.pdf",
      position_file = "long_term_2026-10-16.csv",
      n_funds = 9L,
      n_xp = 8L,
      daycoval_only = "TPUBI97062PQ"
    ),
    long_term_19 = list(
      fund = "long_term",
      date = as.Date("2026-10-19"),
      portfolio_code = "13386735",
      xp_account = "10850552",
      xp_file = "Extrato_10850552_23_10_2026 18_01_12.pdf",
      position_file = "long_term_2026-10-19.csv",
      n_funds = 9L,
      n_xp = 8L,
      daycoval_only = "TPUBI97062PQ"
    ),
    linear_16 = list(
      fund = "linear",
      date = as.Date("2026-10-16"),
      portfolio_code = "13711539",
      xp_account = "10850753",
      xp_file = "Extrato_10850753_26_10_2026 18_55_08.pdf",
      position_file = "linear_2026-10-16.csv",
      n_funds = 17L,
      n_xp = 16L,
      daycoval_only = "TPUBI97062PT"
    ),
    linear_19 = list(
      fund = "linear",
      date = as.Date("2026-10-19"),
      portfolio_code = "13711539",
      xp_account = "10850753",
      xp_file = "Extrato_10850753_26_10_2026 18_54_45.pdf",
      position_file = "linear_2026-10-19.csv",
      n_funds = 17L,
      n_xp = 16L,
      daycoval_only = "TPUBI97062PT"
    )
  )
}


temporal_reconciliation_mapping <- function(fund) {
  if (fund == "long_term") {
    return(tibble::tribble(
      ~xp_asset_name, ~daycoval_title,
      "Absolute Pace LB Advisory FIF em Cotas de FIA - RL", "CF 32892827",
      "AZ Quest Small Mid Caps FIF CIC em Ações RL", "CF 11392165",
      "BNP Paribas Small Caps FIF em Ações RL", "CF 11108013",
      "Genoa Capital Arpa Advisory CIC CIA RL", "CF 39317019",
      "Giant DAO Multifactor LB FIF em Cotas de FIM", "CF 40920066",
      "Ibiuna Long Biased FIF CIC Multi RL", "FIM LONG_BIA",
      "Trend Bolsas Emergentes FIM RL", "CF 40212899",
      "Trend Bolsas Europeias FIA RL", "CF 39227204"
    ))
  }

  if (fund == "linear") {
    return(tibble::tibble(
      xp_asset_name = c(
        "Arx Vinson Advisory FIC de FIF RF CP RL",
        "Compass High Yield 180 Advisory FIF em Cotas de FIM",
        "Compass Yield 30 FIF RF CP LP RL",
        "Ibiuna Credit FIF CIC Multimercado CP RL",
        "JGP Crédito Ecossistema 360 Advisory FIC de FIF Multimercado CP RL",
        "Legacy Capital Credit Master CIM CP RL - Subclasse Legacy Credit Advisory",
        "Pátria Crédito Estruturado 365 FIDC",
        "Porto Seguro Ipê RF FIC FI CP LP",
        "Root Capital High Yield Advisory FIC de FIF Multimercado CP RL",
        "SPX Seahawk Advisory FIC FIRF CP LP",
        "SulAmérica Crédito Ativo FIRF CP LP",
        "Tivio Alt Credito High Yield 180 FIDC RL",
        "Trend Pós-Fixado FIC FIRF Simples RL",
        "Valora Vanguard FIC de FIDC RL",
        "Vinland Crédito Estruturado FIDC RL",
        "XP Crédito Estruturado 360 FIDC RL"
      ),
      daycoval_title = c(
        "CF 41888492",
        "CF 47936719",
        "CF 36318479",
        "CF 37310657",
        "CF 45683300",
        "CF 208056707",
        "CF 49983891",
        "CF 35378376",
        "CF 34431610",
        "CF 35648999",
        "CF 13823084",
        "CF 58038938",
        "CF 26559284",
        "CF 59657690",
        "CF 47640954",
        "CF 27227796"
      )
    ))
  }

  cli::cli_abort("Fundo desconhecido: {.val {fund}}.")
}


temporal_reconciliation_portfolio <- local({
  portfolios <- NULL

  function(code, date) {
    if (is.null(portfolios)) {
      files <- list.files(
        testthat::test_path("fixtures", "daycoval"),
        pattern = "\\.csv$",
        full.names = TRUE
      )

      portfolios <<- purrr::map(
        files,
        read_daycoval_portfolio
      )
    }

    selected <- purrr::keep(
      portfolios,
      function(portfolio) {
        identical(
          portfolio$metadata$codigo_carteira,
          code
        ) &&
          identical(
            portfolio$metadata$data_posicao,
            date
          )
      }
    )

    if (length(selected) != 1L) {
      cli::cli_abort(
        "Esperada uma Carteira Diária para {code} em {date}."
      )
    }

    selected[[1L]]
  }
})


temporal_reconciliation_position <- function(case) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      case$position_file
    )
  )
}


temporal_reconciliation_xp <- function(case) {
  read_xp_statement(
    testthat::test_path(
      "fixtures",
      "xp",
      case$xp_file
    )
  )
}


temporal_reconciliation_result <- function(case) {
  reconcile_positions(
    daycoval = temporal_reconciliation_portfolio(
      case$portfolio_code,
      case$date
    ),
    xp = temporal_reconciliation_xp(case),
    mapping = temporal_reconciliation_mapping(case$fund),
    daycoval_only = case$daycoval_only
  )
}


test_that("Daycoval reconciles internally on both dates", {
  cases <- temporal_reconciliation_cases()

  purrr::walk(cases, function(case) {
    portfolio <- temporal_reconciliation_portfolio(
      case$portfolio_code,
      case$date
    )

    position <- temporal_reconciliation_position(case)

    expect_identical(
      position$metadata$codigo_carteira,
      case$portfolio_code
    )

    expect_identical(
      position$metadata$data_posicao,
      case$date
    )

    expect_equal(
      nrow(position$positions$funds),
      case$n_funds
    )

    result <- reconcile_daycoval(
      portfolio = portfolio,
      position = position
    )

    expect_s3_class(
      result,
      "daycoval_reconciliation"
    )

    expect_equal(
      nrow(result$matches),
      case$n_funds
    )

    expect_equal(
      result$summary$matched_equal,
      case$n_funds
    )

    expect_equal(nrow(result$issues), 0L)

    expect_true(all(
      result$checks$status == "equal"
    ))
  })
})


test_that("XP and Daycoval reconcile positions across funds and dates", {
  cases <- temporal_reconciliation_cases()

  purrr::walk(cases, function(case) {
    xp <- temporal_reconciliation_xp(case)

    expect_identical(
      xp$metadata$account_number,
      case$xp_account
    )

    expect_identical(
      xp$metadata$position_date,
      case$date
    )

    result <- temporal_reconciliation_result(case)

    expect_s3_class(
      result,
      "positions_reconciliation"
    )

    expect_identical(
      result$metadata$position_date,
      case$date
    )

    expect_equal(
      result$summary$matched,
      case$n_xp
    )

    expect_equal(
      nrow(result$matches),
      case$n_xp
    )

    expect_equal(
      nrow(result$checks),
      2L * case$n_xp
    )

    expect_equal(
      result$summary$unmatched_xp,
      0L
    )

    expect_equal(
      result$summary$unmatched_daycoval,
      0L
    )

    expect_equal(
      result$summary$expected_daycoval_only,
      1L
    )

    expect_equal(nrow(result$issues), 1L)

    expect_identical(
      result$issues$issue_type,
      "expected_daycoval_only"
    )

    expect_identical(
      result$issues$asset_identifier,
      case$daycoval_only
    )

    expect_false(
      anyDuplicated(result$matches$match_id) > 0L
    )

    expect_true(all(
      result$matches$match_type == "explicit"
    ))
  })
})


test_that("Absolute Pace discrepancy persists across dates", {
  cases <- temporal_reconciliation_cases()

  result_16 <- temporal_reconciliation_result(
    cases$long_term_16
  )

  result_19 <- temporal_reconciliation_result(
    cases$long_term_19
  )

  absolute_16 <- result_16$matches |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  absolute_19 <- result_19$matches |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_equal(nrow(absolute_16), 1L)
  expect_equal(nrow(absolute_19), 1L)

  difference_16 <- (
    absolute_16$daycoval_quantity -
      absolute_16$xp_quantity
  )

  difference_19 <- (
    absolute_19$daycoval_quantity -
      absolute_19$xp_quantity
  )

  expect_equal(difference_16, 173.72767049)
  expect_equal(difference_19, 173.72767049)

  financial_cents <- function(position) {
    round(position$daycoval_market_value * 100) -
      round(position$xp_market_value * 100)
  }

  expect_equal(financial_cents(absolute_16), 62865)
  expect_equal(financial_cents(absolute_19), 63339)
})


test_that("real files with different dates cannot be reconciled", {
  cases <- temporal_reconciliation_cases()

  portfolio <- temporal_reconciliation_portfolio(
    cases$long_term_19$portfolio_code,
    cases$long_term_19$date
  )

  xp <- temporal_reconciliation_xp(
    cases$long_term_16
  )

  expect_error(
    reconcile_positions(
      daycoval = portfolio,
      xp = xp,
      mapping = temporal_reconciliation_mapping("long_term"),
      daycoval_only = "TPUBI97062PQ"
    ),
    "datas de posição diferentes"
  )
})


test_that("identifies the unmatched Linear position on October 16", {
  case <- temporal_reconciliation_cases()$linear_16

  portfolio <- temporal_reconciliation_portfolio(
    case$portfolio_code,
    case$date
  )

  xp <- temporal_reconciliation_xp(case)
  position <- temporal_reconciliation_position(case)
  mapping <- temporal_reconciliation_mapping(case$fund)

  records <- position_reconciliation_prepare(
    daycoval = portfolio,
    xp = xp
  )

  xp_names <- records$xp$xp_asset_name
  daycoval_names <- records$daycoval$daycoval_title

  position_names <- position$positions$funds$titulo |>
    stringr::str_trim()

  diagnostics <- dplyr::bind_rows(
    tibble::tibble(
      issue = "xp_without_mapping",
      identifier = setdiff(
        xp_names,
        mapping$xp_asset_name
      )
    ),
    tibble::tibble(
      issue = "mapping_without_xp",
      identifier = setdiff(
        mapping$xp_asset_name,
        xp_names
      )
    ),
    tibble::tibble(
      issue = "mapping_missing_daily_portfolio",
      identifier = setdiff(
        mapping$daycoval_title,
        daycoval_names
      )
    ),
    tibble::tibble(
      issue = "mapping_missing_position_c",
      identifier = setdiff(
        mapping$daycoval_title,
        position_names
      )
    )
  )

  if (nrow(diagnostics) > 0L) {
    details <- paste0(
      diagnostics$issue,
      ": ",
      diagnostics$identifier,
      collapse = "\n"
    )

    testthat::fail(
      paste("Associações pendentes:\n", details)
    )
  }

  result <- temporal_reconciliation_result(case)

  expect_equal(
    result$summary$matched,
    case$n_xp
  )
})


temporal_test_operations <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      cached <<- read_fund_operations(
        testthat::test_path(
          "fixtures",
          "operations",
          "fund_operations.xlsx"
        )
      )
    }

    cached
  }
})


test_that("operations integrate with all four real position scenarios", {
  cases <- temporal_reconciliation_cases()
  operations <- temporal_test_operations()

  purrr::walk(cases, function(case) {
    reconciliation <- temporal_reconciliation_result(case)
    position <- temporal_reconciliation_position(case)

    result <- explain_position_differences(
      reconciliation = reconciliation,
      operations = operations,
      daycoval_position = position
    )

    expect_s3_class(result, "position_explanations")

    expect_identical(
      result$metadata$position_date,
      case$date
    )

    expect_setequal(
      result$assessments$match_id,
      reconciliation$matches$match_id
    )

    expect_equal(
      result$summary$matched_positions,
      nrow(reconciliation$matches)
    )

    expect_equal(
      result$summary$linked_operations,
      nrow(result$links)
    )

    portfolio_cnpj <- gsub(
      "[^0-9]",
      "",
      position$metadata$cnpj
    )

    expect_true(all(
      result$links$portfolio_fund_cnpj ==
        portfolio_cnpj
    ))

    expect_false(anyNA(
      result$links[c("source_file", "source_row")]
    ))

    expect_equal(
      nrow(
        dplyr::anti_join(
          result$candidates,
          result$links,
          by = c("source_file", "source_row")
        )
      ),
      0L
    )
  })
})


test_that("Compass redemption reduces positions in both sources", {
  cases <- temporal_reconciliation_cases()

  earlier <- temporal_reconciliation_result(
    cases$linear_16
  )

  later <- temporal_reconciliation_result(
    cases$linear_19
  )

  compass <- function(reconciliation) {
    reconciliation$matches |>
      dplyr::filter(
        .data$daycoval_title == "CF 47936719"
      )
  }

  before <- compass(earlier)
  after <- compass(later)

  expect_equal(nrow(before), 1L)
  expect_equal(nrow(after), 1L)

  expect_gt(
    before$xp_quantity,
    after$xp_quantity
  )

  expect_gt(
    before$daycoval_quantity,
    after$daycoval_quantity
  )

  expect_true(all(is.finite(c(
    before$xp_quantity,
    after$xp_quantity,
    before$daycoval_quantity,
    after$daycoval_quantity
  ))))
})


test_that("Compass conversion is classified relative to position date", {
  cases <- temporal_reconciliation_cases()

  operations <- temporal_test_operations()

  xp_16 <- temporal_reconciliation_xp(
    cases$linear_16
  )

  xp_19 <- temporal_reconciliation_xp(
    cases$linear_19
  )

  compass_name <- paste(
    "Compass High Yield 180 Advisory",
    "FIF em Cotas de FIM"
  )

  quantity_16 <- xp_16$positions |>
    dplyr::filter(.data$asset_name == .env$compass_name) |>
    dplyr::pull(.data$quantity)

  quantity_19 <- xp_19$positions |>
    dplyr::filter(.data$asset_name == .env$compass_name) |>
    dplyr::pull(.data$quantity)

  expect_length(quantity_16, 1L)
  expect_length(quantity_19, 1L)

  redeemed_quantity <- quantity_16 - quantity_19

  expect_gt(redeemed_quantity, 0)

  expected_phases <- c(
    linear_16 = "pending_conversion",
    linear_19 = "conversion_on_position_date"
  )

  for (case_name in names(expected_phases)) {
    case <- cases[[case_name]]

    reconciliation <- temporal_reconciliation_result(case)
    position <- temporal_reconciliation_position(case)

    compass_position <- position$positions$funds |>
      dplyr::filter(
        stringr::str_trim(.data$titulo) == "CF 47936719"
      )

    expect_equal(nrow(compass_position), 1L)

    compass_isin <- compass_position$isin[[1L]]

    expect_false(is.na(compass_isin))
    expect_true(nzchar(compass_isin))

    # Synthetic operation based on the observed XP event.
    # Settlement date deliberately remains unknown.
    case_operations <- operations
    new_operation <- operations$operations[1L, ]

    new_row <- as.integer(
      max(operations$operations$source_row) + 1L
    )

    new_operation$source_row <- new_row

    new_operation$portfolio_fund_cnpj <- gsub(
      "[^0-9]",
      "",
      position$metadata$cnpj
    )

    new_operation$asset_identifier <- compass_name
    new_operation$asset_isin <- compass_isin
    new_operation$operation_type <- "Resgate Bruto"
    new_operation$operation_date <- as.Date("2026-04-20")
    new_operation$conversion_date <- as.Date("2026-10-19")
    new_operation$liquidation_date <- as.Date(NA)
    new_operation$value <- 100000
    new_operation$quantity <- redeemed_quantity
    new_operation$net_value <- NA_real_
    new_operation$is_total_redemption <- FALSE

    case_operations$operations <- dplyr::bind_rows(
      operations$operations,
      new_operation
    )

    result <- explain_position_differences(
      reconciliation = reconciliation,
      operations = case_operations,
      daycoval_position = position
    )

    candidate <- result$candidates |>
      dplyr::filter(.data$source_row == .env$new_row)

    expect_equal(nrow(candidate), 1L)

    expect_identical(
      candidate$phase[[1L]],
      expected_phases[[case_name]]
    )

    expect_true(
      candidate$may_affect_quantity[[1L]]
    )

    expect_equal(
      candidate$signed_quantity[[1L]],
      -redeemed_quantity
    )

    expect_identical(
      candidate$asset_isin[[1L]],
      compass_isin
    )
  }
})


test_that("real Compass redemption follows its conversion date", {
  cases <- temporal_reconciliation_cases()
  operations <- temporal_test_operations()

  expected_phases <- c(
    linear_16 = "pending_conversion",
    linear_19 = "conversion_on_position_date"
  )

  for (case_name in names(expected_phases)) {
    case <- cases[[case_name]]

    reconciliation <- temporal_reconciliation_result(case)
    position <- temporal_reconciliation_position(case)

    result <- explain_position_differences(
      reconciliation = reconciliation,
      operations = operations,
      daycoval_position = position
    )

    compass <- result$candidates |>
      dplyr::filter(
        .data$daycoval_title == "CF 47936719",
        .data$operation_type == "Resgate Bruto",
        .data$operation_date == as.Date("2026-04-20"),
        .data$conversion_date == as.Date("2026-10-19")
      )

    expect_equal(nrow(compass), 1L)

    expect_equal(compass$value, 100000)

    expect_identical(
      compass$phase[[1L]],
      expected_phases[[case_name]]
    )

    expect_true(
      compass$may_affect_quantity[[1L]]
    )

    # The workbook has no usable share quantity for
    # this redemption. Do not infer one.
    expect_true(
      is.na(compass$signed_quantity[[1L]])
    )

    expect_false(is.na(compass$asset_isin[[1L]]))
    expect_false(is.na(compass$source_row[[1L]]))
  }
})



temporal_real_position_changes <- function(fund) {
  cases <- temporal_reconciliation_cases()

  previous_case <- cases[[paste0(fund, "_16")]]
  current_case <- cases[[paste0(fund, "_19")]]

  previous <- temporal_reconciliation_result(previous_case)
  current <- temporal_reconciliation_result(current_case)

  result <- reconcile_position_changes(
    previous = previous,
    current = current,
    operations = temporal_test_operations(),
    previous_position = temporal_reconciliation_position(
      previous_case
    ),
    current_position = temporal_reconciliation_position(
      current_case
    )
  )

  list(
    previous = previous,
    current = current,
    result = result
  )
}


test_that("real position changes preserve asset identity", {
  for (fund in c("long_term", "linear")) {
    case <- temporal_real_position_changes(fund)
    changes <- case$result$changes

    expect_s3_class(
      case$result,
      "position_changes"
    )

    expect_identical(
      case$result$metadata$previous_date,
      as.Date("2026-10-16")
    )

    expect_identical(
      case$result$metadata$current_date,
      as.Date("2026-10-19")
    )

    expect_setequal(
      changes$daycoval_title,
      union(
        case$previous$matches$daycoval_title,
        case$current$matches$daycoval_title
      )
    )

    expect_false(
      anyDuplicated(changes$daycoval_title) > 0L
    )

    # Sweep funds are Daycoval-only exceptions,
    # not matched XP investment positions.
    expect_false(any(
      stringr::str_detect(
        changes$daycoval_title,
        "^TPUBI"
      )
    ))

    expect_true(all(
      case$result$links$conversion_date >
        as.Date("2026-10-16")
    ))

    expect_true(all(
      case$result$links$conversion_date <=
        as.Date("2026-10-19")
    ))
  }
})


test_that("real Compass redemption is linked without imputing shares", {
  case <- temporal_real_position_changes("linear")
  result <- case$result

  compass <- result$changes |>
    dplyr::filter(
      .data$daycoval_title == "CF 47936719"
    )

  expect_equal(nrow(compass), 1L)

  expect_identical(
    compass$change_type[[1L]],
    "held"
  )

  # Both sources report fewer shares on October 19.
  expect_lt(
    compass$xp_quantity_change[[1L]],
    0
  )

  expect_lt(
    compass$daycoval_quantity_change[[1L]],
    0
  )

  # The actual redemption is identified using its
  # instrument identity and conversion date.
  redemption <- result$links |>
    dplyr::filter(
      .data$daycoval_title == "CF 47936719",
      .data$operation_type == "Resgate Bruto",
      .data$operation_date == as.Date("2026-04-20"),
      .data$conversion_date == as.Date("2026-10-19")
    )

  expect_equal(nrow(redemption), 1L)

  expect_equal(
    redemption$value,
    100000
  )

  expect_true(
    is.na(redemption$signed_quantity[[1L]])
  )

  expect_true(
    compass$n_conversions[[1L]] >= 1L
  )

  expect_identical(
    compass$operation_status[[1L]],
    "quantity_unavailable"
  )

  # A linked redemption is evidence of an event,
  # not automatic proof of quantity reconciliation.
  expect_true(is.na(
    compass$signed_operation_quantity[[1L]]
  ))
})



