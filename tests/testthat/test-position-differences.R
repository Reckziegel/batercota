
position_explanation_mapping <- function() {
  tibble::tribble(
    ~xp_asset_name, ~daycoval_title,
    "Absolute Pace LB Advisory FIF em Cotas de FIA - RL",
    "CF 32892827",
    "AZ Quest Small Mid Caps FIF CIC em Ações RL",
    "CF 11392165",
    "BNP Paribas Small Caps FIF em Ações RL",
    "CF 11108013",
    "Genoa Capital Arpa Advisory CIC CIA RL",
    "CF 39317019",
    "Giant DAO Multifactor LB FIF em Cotas de FIM",
    "CF 40920066",
    "Ibiuna Long Biased FIF CIC Multi RL",
    "FIM LONG_BIA",
    "Trend Bolsas Emergentes FIM RL",
    "CF 40212899",
    "Trend Bolsas Europeias FIA RL",
    "CF 39227204"
  )
}


position_explanation_case <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      portfolio <- read_daycoval_portfolio(
        testthat::test_path(
          "fixtures",
          "daycoval",
          "Carteira Diária - 261022_155515.csv"
        )
      )

      position <- read_daycoval_position(
        testthat::test_path(
          "fixtures",
          "daycoval-position",
          "long_term_2026-10-19.csv"
        )
      )

      xp <- read_xp_statement(
        testthat::test_path(
          "fixtures",
          "xp",
          "Extrato_10850552_23_10_2026 18_01_12.pdf"
        )
      )

      operations <- read_fund_operations(
        testthat::test_path(
          "fixtures",
          "operations",
          "fund_operations.xlsx"
        )
      )

      reconciliation <- reconcile_positions(
        daycoval = portfolio,
        xp = xp,
        mapping = position_explanation_mapping(),
        daycoval_only = "TPUBI97062PQ"
      )

      cached <<- list(
        reconciliation = reconciliation,
        operations = operations,
        position = position
      )
    }

    unserialize(serialize(cached, NULL))
  }
})


explain_test_case <- function(case) {
  explain_position_differences(
    reconciliation = case$reconciliation,
    operations = case$operations,
    daycoval_position = case$position
  )
}


add_test_operation <- function(
    case,
    operation_type = "Aplicação",
    quantity = 10,
    value = 100,
    operation_date = as.Date("2026-10-18"),
    conversion_date = as.Date("2026-10-20"),
    liquidation_date = as.Date("2026-10-21"),
    portfolio_cnpj = "56365598000177",
    isin = "BR01VHCTF009"
) {
  records <- case$operations$operations

  new <- records[1L, ]

  new$source_row <- as.integer(
    max(records$source_row) + 1L
  )

  new$portfolio_fund_cnpj <- portfolio_cnpj
  new$asset_identifier <- "ABSOLUTE PACE LB ADV FC FIA"
  new$asset_isin <- isin
  new$operation_type <- operation_type
  new$operation_date <- operation_date
  new$conversion_date <- conversion_date
  new$liquidation_date <- liquidation_date
  new$value <- value
  new$quantity <- quantity
  new$net_value <- NA_real_
  new$is_total_redemption <-
    operation_type == "Resgate Total"

  case$operations$operations <- dplyr::bind_rows(
    records,
    new
  )

  case
}


test_that("links real Long Term operations through portfolio CNPJ and ISIN", {
  case <- position_explanation_case()
  result <- explain_test_case(case)

  expect_s3_class(result, "position_explanations")

  expect_named(
    result,
    c(
      "metadata",
      "links",
      "candidates",
      "assessments",
      "issues",
      "summary"
    )
  )

  expect_identical(
    result$metadata$position_date,
    as.Date("2026-10-19")
  )

  expect_identical(
    result$metadata$portfolio_cnpj,
    "56365598000177"
  )

  expect_equal(result$summary$matched_positions, 8L)
  expect_equal(result$summary$portfolio_operations, 889L)
  expect_equal(result$summary$linked_operations, 70L)

  expect_equal(nrow(result$assessments), 8L)

  expect_true(all(
    result$links$portfolio_fund_cnpj ==
      "56365598000177"
  ))

  expect_setequal(
    unique(result$links$match_id),
    result$assessments$match_id
  )

  expect_false(anyNA(result$links$asset_isin))
})


test_that("finds no pending Long Term operations on the position date", {
  case <- position_explanation_case()
  result <- explain_test_case(case)

  expect_equal(nrow(result$candidates), 0L)
  expect_equal(result$summary$relevant_operations, 0L)
  expect_equal(result$summary$quantity_candidates, 0L)
  expect_equal(nrow(result$issues), 0L)

  expect_true(all(
    result$links$phase == "completed"
  ))

  expect_true(all(
    result$assessments$n_relevant_operations == 0L
  ))

  expect_true(all(
    result$assessments$n_quantity_candidates == 0L
  ))
})


test_that("does not incorrectly explain the Absolute Pace difference", {
  case <- position_explanation_case()
  result <- explain_test_case(case)

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_equal(nrow(absolute), 1L)

  expect_equal(
    absolute$quantity_difference,
    173.72767049
  )

  expect_equal(
    absolute$market_value_difference,
    633.39
  )

  expect_gt(
    absolute$n_historical_operations,
    0L
  )

  expect_equal(
    absolute$n_relevant_operations,
    0L
  )

  expect_identical(
    absolute$status,
    "no_relevant_operations"
  )

  expect_true(is.na(
    absolute$signed_candidate_quantity[[1L]]
  ))
})


test_that("preserves source references for linked operations", {
  case <- position_explanation_case()
  result <- explain_test_case(case)

  expect_false(anyNA(result$links$source_file))
  expect_false(anyNA(result$links$source_row))

  expect_false(anyDuplicated(
    result$links[c("source_file", "source_row")]
  ) > 0L)

  expect_true(all(
    result$links$source_row %in%
      case$operations$operations$source_row
  ))

  expect_true(all(
    result$links$match_id %in%
      case$reconciliation$matches$match_id
  ))
})


test_that("identifies an application pending conversion", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    quantity = 10,
    operation_date = as.Date("2026-10-18"),
    conversion_date = as.Date("2026-10-20"),
    liquidation_date = as.Date("2026-10-21")
  )

  result <- explain_test_case(case)

  candidate <- result$candidates |>
    dplyr::filter(
      .data$source_row ==
        max(case$operations$operations$source_row)
    )

  expect_equal(nrow(candidate), 1L)

  expect_identical(
    candidate$phase,
    "pending_conversion"
  )

  expect_true(candidate$may_affect_quantity[[1L]])
  expect_equal(candidate$signed_quantity, 10)

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_equal(absolute$n_relevant_operations, 1L)
  expect_equal(absolute$n_quantity_candidates, 1L)
  expect_equal(absolute$signed_candidate_quantity, 10)

  expect_identical(
    absolute$status,
    "operations_do_not_reconcile"
  )
})


test_that("identifies quantitative consistency without claiming proof", {
  case <- position_explanation_case()

  i <- which(
    case$reconciliation$matches$daycoval_title ==
      "CF 32892827"
  )

  expect_length(i, 1L)

  daycoval_quantity <-
    case$reconciliation$matches$daycoval_quantity[[i]]

  case$reconciliation$matches$xp_quantity[[i]] <-
    daycoval_quantity - 10

  case <- add_test_operation(
    case,
    quantity = 10
  )

  result <- explain_test_case(case)

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_equal(absolute$quantity_difference, 10)

  expect_identical(
    absolute$status,
    "quantity_consistent_unconfirmed"
  )

  expect_equal(
    result$summary$quantitatively_consistent_unconfirmed,
    1L
  )
})


test_that("does not infer quantities for total redemptions", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    operation_type = "Resgate Total",
    quantity = NA_real_,
    value = NA_real_
  )

  result <- explain_test_case(case)

  candidate <- result$candidates |>
    dplyr::filter(
      .data$source_row ==
        max(case$operations$operations$source_row)
    )

  expect_equal(nrow(candidate), 1L)
  expect_true(is.na(candidate$value[[1L]]))
  expect_true(is.na(candidate$quantity[[1L]]))
  expect_true(is.na(candidate$signed_quantity[[1L]]))

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_identical(
    absolute$status,
    "quantity_not_determined"
  )
})


test_that("does not treat pending settlement as a quantity change", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    operation_type = "Resgate Cotas",
    quantity = 10,
    operation_date = as.Date("2026-10-17"),
    conversion_date = as.Date("2026-10-18"),
    liquidation_date = as.Date("2026-10-21")
  )

  result <- explain_test_case(case)

  candidate <- result$candidates |>
    dplyr::filter(
      .data$source_row ==
        max(case$operations$operations$source_row)
    )

  expect_equal(nrow(candidate), 1L)
  expect_identical(candidate$phase, "pending_settlement")

  expect_false(candidate$may_affect_quantity[[1L]])
  expect_true(is.na(candidate$signed_quantity[[1L]]))

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_identical(
    absolute$status,
    "settlement_only"
  )
})


test_that("keeps orders after the position date outside candidates", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    operation_date = as.Date("2026-10-20"),
    conversion_date = as.Date("2026-10-21"),
    liquidation_date = as.Date("2026-10-22")
  )

  result <- explain_test_case(case)

  new_row <- max(case$operations$operations$source_row)

  operation <- result$links |>
    dplyr::filter(.data$source_row == .env$new_row)

  expect_equal(nrow(operation), 1L)
  expect_identical(operation$phase, "future_order")

  expect_false(new_row %in% result$candidates$source_row)
  expect_equal(nrow(result$candidates), 0L)
})


test_that("matches conversion on the position date as a candidate", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    operation_date = as.Date("2026-10-17"),
    conversion_date = as.Date("2026-10-19"),
    liquidation_date = as.Date("2026-10-20")
  )

  result <- explain_test_case(case)

  expect_equal(nrow(result$candidates), 1L)

  expect_identical(
    result$candidates$phase,
    "conversion_on_position_date"
  )

  expect_true(
    result$candidates$may_affect_quantity[[1L]]
  )
})


test_that("never associates operations from a different portfolio", {
  case <- position_explanation_case()

  case <- add_test_operation(
    case,
    portfolio_cnpj = "54236091000198"
  )

  result <- explain_test_case(case)

  expect_equal(nrow(result$candidates), 0L)
  expect_equal(result$summary$portfolio_operations, 889L)
  expect_equal(result$summary$linked_operations, 70L)
})


test_that("reports missing ISINs without forcing a match", {
  case <- position_explanation_case()

  index <- which(
    stringr::str_trim(
      case$position$positions$funds$titulo
    ) == "CF 32892827"
  )

  expect_length(index, 1L)

  case$position$positions$funds$isin[[index]] <- NA_character_

  result <- explain_test_case(case)

  absolute <- result$assessments |>
    dplyr::filter(
      .data$daycoval_title == "CF 32892827"
    )

  expect_identical(
    absolute$status,
    "identifier_unavailable"
  )

  expect_true(any(
    result$issues$issue_type == "missing_position_isin"
  ))
})


test_that("rejects ambiguous ISIN associations", {
  case <- position_explanation_case()

  index <- which(
    stringr::str_trim(
      case$position$positions$funds$titulo
    ) == "CF 39317019"
  )

  expect_length(index, 1L)

  case$position$positions$funds$isin[[index]] <-
    "BR01VHCTF009"

  expect_error(
    explain_test_case(case),
    "múltiplas posições"
  )
})


test_that("rejects different position dates", {
  case <- position_explanation_case()

  case$position$metadata$data_posicao <-
    as.Date("2026-10-20")

  expect_error(
    explain_test_case(case),
    "datas diferentes"
  )
})


test_that("rejects different Daycoval portfolios", {
  case <- position_explanation_case()

  case$position$metadata$codigo_carteira <- "99999999"

  expect_error(
    explain_test_case(case),
    "carteiras diferentes"
  )
})


test_that("validates input classes", {
  case <- position_explanation_case()

  expect_error(
    explain_position_differences(
      reconciliation = list(),
      operations = case$operations,
      daycoval_position = case$position
    ),
    "positions_reconciliation"
  )

  expect_error(
    explain_position_differences(
      reconciliation = case$reconciliation,
      operations = list(),
      daycoval_position = case$position
    ),
    "fund_operations"
  )

  expect_error(
    explain_position_differences(
      reconciliation = case$reconciliation,
      operations = case$operations,
      daycoval_position = list()
    ),
    "daycoval_position"
  )
})
