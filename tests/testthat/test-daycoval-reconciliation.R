
read_daycoval_reconciliation_case <- function(
    fund = c("linear", "long_term")
) {
  fund <- match.arg(fund)

  cases <- list(
    linear = list(
      portfolio = "Carteira Diária - 261022_155515 (1).csv",
      position = "linear_2026-10-19.csv",
      n_funds = 17L
    ),
    long_term = list(
      portfolio = "Carteira Diária - 261022_155515.csv",
      position = "long_term_2026-10-19.csv",
      n_funds = 9L
    )
  )

  case <- cases[[fund]]

  portfolio_path <- testthat::test_path(
    "fixtures",
    "daycoval",
    case$portfolio
  )

  position_path <- testthat::test_path(
    "fixtures",
    "daycoval-position",
    case$position
  )

  list(
    portfolio = read_daycoval_portfolio(portfolio_path),
    position = read_daycoval_position(position_path),
    n_funds = case$n_funds
  )
}


test_that("reconciles all real investment fund positions", {
  for (fund in c("linear", "long_term")) {
    case <- read_daycoval_reconciliation_case(fund)

    result <- reconcile_daycoval(
      portfolio = case$portfolio,
      position = case$position
    )

    expect_s3_class(
      result,
      "daycoval_reconciliation"
    )

    expect_named(
      result,
      c(
        "metadata",
        "matches",
        "checks",
        "issues",
        "summary"
      )
    )

    expect_equal(
      nrow(result$matches),
      case$n_funds
    )

    expect_equal(
      nrow(result$checks),
      2L * case$n_funds
    )

    expect_equal(
      nrow(result$issues),
      0L
    )

    expect_true(all(
      result$checks$status == "equal"
    ))

    expect_true(all(
      result$checks$difference == 0
    ))

    expect_equal(
      result$summary$matched_equal,
      case$n_funds
    )

    expect_equal(
      result$summary$matched_different,
      0L
    )
  }
})


test_that("preserves source identifiers in every match", {
  case <- read_daycoval_reconciliation_case("linear")

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  matches <- result$matches

  expect_false(anyNA(matches$portfolio_source_file))
  expect_false(anyNA(matches$position_source_file))

  expect_false(anyNA(matches$portfolio_source_line))
  expect_false(anyNA(matches$position_source_line))

  expect_false(anyNA(matches$portfolio_record_number))
  expect_false(anyNA(matches$position_record_number))

  expect_false(anyDuplicated(matches$match_id) > 0L)

  expect_true(all(
    matches$portfolio_source_line %in%
      case$portfolio$entries$source_line
  ))

  expect_true(all(
    matches$position_source_line %in%
      case$position$positions$funds$source_line
  ))
})


test_that("matches funds independently of market values", {
  case <- read_daycoval_reconciliation_case("linear")

  original <- case$position$positions$funds$vl_mrc[[1L]]

  case$position$positions$funds$vl_mrc[[1L]] <- original + 1

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(nrow(result$matches), 17L)

  differences <- result$checks |>
    dplyr::filter(.data$status == "different")

  expect_equal(nrow(differences), 1L)

  expect_identical(
    differences$field,
    "market_value"
  )

  expect_equal(
    differences$difference,
    -1
  )

  expect_equal(
    result$summary$matched_different,
    1L
  )

  expect_equal(
    nrow(result$issues),
    0L
  )
})


test_that("detects quantity differences without losing identity", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$positions$funds$qtd[[1L]] <-
    case$position$positions$funds$qtd[[1L]] + 1

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  differences <- result$checks |>
    dplyr::filter(.data$status == "different")

  expect_equal(
    nrow(result$matches),
    17L
  )

  expect_equal(
    nrow(differences),
    1L
  )

  expect_identical(
    differences$field,
    "quantity"
  )

  expect_equal(
    differences$difference,
    -1
  )

  expect_equal(
    result$summary$matched_different,
    1L
  )
})


test_that("normalizes only external title whitespace", {
  case <- read_daycoval_reconciliation_case("linear")

  original_titles <- case$position$positions$funds$titulo

  case$position$positions$funds$titulo <- paste0(
    "  ",
    original_titles,
    "  "
  )

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    17L
  )

  expect_equal(
    nrow(result$issues),
    0L
  )

  expect_true(all(
    result$matches$match_type == "direct"
  ))
})


test_that("does not force matches with duplicated identifiers", {
  case <- read_daycoval_reconciliation_case("linear")

  funds <- case$position$positions$funds

  duplicate <- funds[1L, ]

  duplicate$source_line <- 999L
  duplicate$record_number <- "999"

  case$position$positions$funds <- dplyr::bind_rows(
    funds,
    duplicate
  )

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    16L
  )

  expect_equal(
    result$summary$ambiguous_records,
    3L
  )

  expect_equal(
    sum(result$issues$issue_type == "ambiguous"),
    3L
  )

  expect_false(
    stringr::str_trim(funds$titulo[[1L]]) %in%
      result$matches$key
  )
})


test_that("detects unmatched records on both sides", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$positions$funds$titulo[[1L]] <- "CF INEXISTENTE"

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    16L
  )

  expect_equal(
    result$summary$unmatched_portfolio,
    1L
  )

  expect_equal(
    result$summary$unmatched_position,
    1L
  )

  expect_equal(
    nrow(result$issues),
    2L
  )
})


test_that("identifies unmapped accounting groups", {
  case <- read_daycoval_reconciliation_case("linear")

  fund_title <- stringr::str_trim(
    case$position$positions$funds$titulo[[1L]]
  )

  index <- which(
    stringr::str_trim(
      case$portfolio$entries$titulo
    ) == fund_title
  )

  expect_length(index, 1L)

  case$portfolio$entries$grp_n3[index] <- "99999"

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    17L
  )

  expect_equal(
    result$summary$unmapped_accounting_groups,
    1L
  )

  classification_issues <- result$issues |>
    dplyr::filter(
      .data$issue_type == "unmapped_accounting_group"
    )

  expect_equal(
    nrow(classification_issues),
    1L
  )

  expect_identical(
    classification_issues$accounting_group,
    "99999"
  )
})


test_that("does not match missing operational identifiers", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$positions$funds$titulo[[1L]] <- ""

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    16L
  )

  expect_equal(
    result$summary$unmatched_position,
    1L
  )

  expect_equal(
    result$summary$unmatched_portfolio,
    1L
  )

  expect_false(anyNA(result$matches$key))
})


test_that("supports positions without a fund layout", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$positions$funds <- NULL

  result <- reconcile_daycoval(
    case$portfolio,
    case$position
  )

  expect_equal(
    nrow(result$matches),
    0L
  )

  expect_equal(
    nrow(result$checks),
    0L
  )

  expect_equal(
    result$summary$position_records,
    0L
  )

  expect_equal(
    result$summary$unmatched_portfolio,
    17L
  )
})


test_that("rejects different portfolios", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$metadata$codigo_carteira <- "99999999"

  expect_error(
    reconcile_daycoval(
      case$portfolio,
      case$position
    ),
    "carteiras diferentes"
  )
})


test_that("rejects different accounting dates", {
  case <- read_daycoval_reconciliation_case("linear")

  case$position$metadata$data_posicao <- as.Date(
    "2026-10-20"
  )

  expect_error(
    reconcile_daycoval(
      case$portfolio,
      case$position
    ),
    "datas diferentes"
  )
})


test_that("validates reconciliation input classes", {
  case <- read_daycoval_reconciliation_case("linear")

  expect_error(
    reconcile_daycoval(
      list(),
      case$position
    ),
    "daycoval_portfolio"
  )

  expect_error(
    reconcile_daycoval(
      case$portfolio,
      list()
    ),
    "daycoval_position"
  )
})


test_that("rejects unsupported reconciliation components", {
  case <- read_daycoval_reconciliation_case("linear")

  expect_error(
    reconcile_daycoval(
      case$portfolio,
      case$position,
      components = "cash"
    ),
    "components"
  )

  expect_error(
    reconcile_daycoval(
      case$portfolio,
      case$position,
      components = c("funds", "cash")
    ),
    "components"
  )
})
