
long_term_position_mapping <- function() {
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


read_long_term_position_case <- function(time = "12") {
  daycoval_path <- testthat::test_path(
    "fixtures",
    "daycoval",
    "Carteira Diária - 261022_155515.csv"
  )

  xp_path <- testthat::test_path(
    "fixtures",
    "xp",
    paste0("Extrato_10850552_23_10_2026 18_01_", time, ".pdf")
  )

  list(
    daycoval = read_daycoval_portfolio(daycoval_path),
    xp = read_xp_statement(xp_path),
    mapping = long_term_position_mapping()
  )
}


reconcile_long_term_case <- function(case) {
  reconcile_positions(
    daycoval = case$daycoval,
    xp = case$xp,
    mapping = case$mapping,
    daycoval_only = "TPUBI97062PQ"
  )
}


test_that("reconciles real Long Term investment fund positions", {
  case <- read_long_term_position_case()
  result <- reconcile_long_term_case(case)

  expect_s3_class(result, "positions_reconciliation")

  expect_named(
    result,
    c("metadata", "matches", "checks", "issues", "summary")
  )

  expect_equal(nrow(result$matches), 8L)
  expect_equal(nrow(result$checks), 16L)

  expect_equal(result$summary$daycoval_records, 9L)
  expect_equal(result$summary$xp_records, 8L)
  expect_equal(result$summary$matched, 8L)

  expect_equal(result$summary$unmatched_daycoval, 0L)
  expect_equal(result$summary$unmatched_xp, 0L)
  expect_equal(result$summary$expected_daycoval_only, 1L)

  expect_identical(
    result$metadata$position_date,
    as.Date("2026-10-19")
  )

  expect_true(all(result$matches$match_type == "explicit"))
  expect_false(anyDuplicated(result$matches$match_id) > 0L)
})


test_that("identifies the Daycoval sweep fund as an expected exception", {
  case <- read_long_term_position_case()
  result <- reconcile_long_term_case(case)

  expect_equal(nrow(result$issues), 1L)

  expect_identical(
    result$issues$issue_type,
    "expected_daycoval_only"
  )

  expect_identical(
    result$issues$asset_identifier,
    "TPUBI97062PQ"
  )

  expect_identical(
    result$issues$source_report,
    "daycoval"
  )

  expect_true(all(!is.na(result$issues$source_line)))
})


test_that("preserves the known Absolute Pace discrepancy", {
  case <- read_long_term_position_case()
  result <- reconcile_long_term_case(case)

  absolute_match <- result$matches |>
    dplyr::filter(.data$daycoval_title == "CF 32892827")

  expect_equal(nrow(absolute_match), 1L)

  expect_equal(
    absolute_match$daycoval_quantity,
    28920.520988
  )

  expect_equal(
    absolute_match$xp_quantity,
    28746.79331751
  )

  expect_equal(
    absolute_match$daycoval_market_value,
    105440.03
  )

  expect_equal(
    absolute_match$xp_market_value,
    104806.64
  )

  absolute_checks <- result$checks |>
    dplyr::filter(
      .data$match_id == absolute_match$match_id[[1L]]
    )

  quantity <- absolute_checks |>
    dplyr::filter(.data$field == "quantity")

  market_value <- absolute_checks |>
    dplyr::filter(.data$field == "market_value")

  expect_equal(quantity$difference, 173.72767049)
  expect_equal(market_value$difference, 633.39)

  expect_identical(quantity$status, "different")
  expect_identical(market_value$status, "different")
})


test_that("preserves small differences without arbitrary tolerances", {
  case <- read_long_term_position_case()
  result <- reconcile_long_term_case(case)

  bnp <- result$matches |>
    dplyr::filter(.data$daycoval_title == "CF 11108013")

  expect_equal(nrow(bnp), 1L)

  checks <- result$checks |>
    dplyr::filter(.data$match_id == bnp$match_id[[1L]])

  quantity <- checks |>
    dplyr::filter(.data$field == "quantity")

  market_value <- checks |>
    dplyr::filter(.data$field == "market_value")

  expect_equal(quantity$difference, 0.00000050)
  expect_equal(market_value$difference, -0.01)

  expect_identical(quantity$status, "different")
  expect_identical(market_value$status, "different")
})


test_that("reconciles both XP movement windows identically", {
  one_day <- read_long_term_position_case("12")
  four_days <- read_long_term_position_case("36")

  result_one <- reconcile_long_term_case(one_day)
  result_four <- reconcile_long_term_case(four_days)

  comparison_columns <- c(
    "xp_asset_name",
    "daycoval_title",
    "daycoval_quantity",
    "xp_quantity",
    "daycoval_market_value",
    "xp_market_value"
  )

  expect_identical(
    result_one$matches[comparison_columns],
    result_four$matches[comparison_columns]
  )

  expect_identical(
    result_one$checks,
    result_four$checks
  )

  expect_identical(
    result_one$summary,
    result_four$summary
  )
})


test_that("matching does not depend on quantities or financial values", {
  case <- read_long_term_position_case()

  index <- which(
    stringr::str_trim(case$daycoval$entries$titulo) ==
      "CF 32892827"
  )

  expect_length(index, 1L)

  case$daycoval$entries$qt[index] <-
    case$daycoval$entries$qt[index] + 100

  case$daycoval$entries$vl_mrc[index] <-
    case$daycoval$entries$vl_mrc[index] + 1000

  result <- reconcile_long_term_case(case)

  expect_equal(nrow(result$matches), 8L)

  absolute <- result$matches |>
    dplyr::filter(.data$daycoval_title == "CF 32892827")

  expect_equal(nrow(absolute), 1L)

  checks <- result$checks |>
    dplyr::filter(.data$match_id == absolute$match_id[[1L]])

  expect_true(all(checks$status == "different"))
})


test_that("does not force matches without an explicit mapping", {
  case <- read_long_term_position_case()

  case$mapping <- case$mapping |>
    dplyr::filter(.data$daycoval_title != "CF 39317019")

  result <- reconcile_long_term_case(case)

  expect_equal(nrow(result$matches), 7L)
  expect_equal(result$summary$unmatched_daycoval, 1L)
  expect_equal(result$summary$unmatched_xp, 1L)
  expect_equal(result$summary$expected_daycoval_only, 1L)

  expect_true(
    "CF 39317019" %in% result$issues$asset_identifier
  )

  expect_true(
    "Genoa Capital Arpa Advisory CIC CIA RL" %in%
      result$issues$asset_identifier
  )
})


test_that("preserves source references on both sides", {
  case <- read_long_term_position_case()
  result <- reconcile_long_term_case(case)

  matches <- result$matches

  expect_false(anyNA(matches$daycoval_source_file))
  expect_false(anyNA(matches$daycoval_source_line))
  expect_false(anyNA(matches$daycoval_record_number))

  expect_false(anyNA(matches$xp_source_file))
  expect_false(anyNA(matches$xp_source_page))
  expect_false(anyNA(matches$xp_source_row))

  expect_true(all(lengths(matches$xp_source_tokens) > 0L))

  expect_true(all(
    matches$daycoval_source_line %in%
      case$daycoval$entries$source_line
  ))

  expect_true(all(
    matches$xp_source_page %in%
      case$xp$positions$source_page
  ))
})


test_that("rejects duplicated operational mappings", {
  case <- read_long_term_position_case()

  duplicate <- case$mapping[1L, ]

  case$mapping <- dplyr::bind_rows(
    case$mapping,
    duplicate
  )

  expect_error(
    reconcile_long_term_case(case),
    "duplicados"
  )
})


test_that("rejects ambiguous source identifiers", {
  case <- read_long_term_position_case()

  index <- which(
    stringr::str_trim(case$daycoval$entries$titulo) ==
      "CF 32892827"
  )

  duplicate <- case$daycoval$entries[index, ]
  duplicate$source_line <- 999L
  duplicate$record_number <- "999"

  case$daycoval$entries <- dplyr::bind_rows(
    case$daycoval$entries,
    duplicate
  )

  expect_error(
    reconcile_long_term_case(case),
    "duplicados"
  )
})


test_that("rejects conflicting exception definitions", {
  case <- read_long_term_position_case()

  expect_error(
    reconcile_positions(
      daycoval = case$daycoval,
      xp = case$xp,
      mapping = case$mapping,
      daycoval_only = "CF 32892827"
    ),
    "mapping"
  )

  expect_error(
    reconcile_positions(
      daycoval = case$daycoval,
      xp = case$xp,
      mapping = case$mapping,
      daycoval_only = "INEXISTENTE"
    ),
    "não encontradas"
  )
})


test_that("rejects different position dates", {
  case <- read_long_term_position_case()

  case$xp$metadata$position_date <- as.Date("2026-10-20")

  expect_error(
    reconcile_long_term_case(case),
    "datas de posição diferentes"
  )
})


test_that("validates input classes", {
  case <- read_long_term_position_case()

  expect_error(
    reconcile_positions(
      daycoval = list(),
      xp = case$xp,
      mapping = case$mapping
    ),
    "daycoval_portfolio"
  )

  expect_error(
    reconcile_positions(
      daycoval = case$daycoval,
      xp = list(),
      mapping = case$mapping
    ),
    "xp_statement"
  )
})

