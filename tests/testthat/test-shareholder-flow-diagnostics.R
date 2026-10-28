shareholder_diagnostics_fixture <- local({
  portfolios <- NULL

  function(fund, date) {
    codes <- c(
      long_term = "13386735",
      linear = "13711539"
    )

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

    portfolio <- purrr::keep(
      portfolios,
      function(x) {
        identical(
          x$metadata$codigo_carteira,
          unname(codes[[fund]])
        ) &&
          identical(
            x$metadata$data_posicao,
            as.Date(date)
          )
      }
    )

    if (length(portfolio) != 1L) {
      stop("Carteira Diária não encontrada.")
    }

    position <- read_daycoval_position(
      testthat::test_path(
        "fixtures",
        "daycoval-position",
        paste0(fund, "_", date, ".csv")
      )
    )

    list(
      portfolio = portfolio[[1L]],
      position = position
    )
  }
})


shareholder_diagnostics_case <- function(fund) {
  previous <- shareholder_diagnostics_fixture(
    fund,
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    fund,
    "2026-10-19"
  )

  diagnose_shareholder_flows(
    previous_position = previous$position,
    current_position = current$position,
    previous_portfolio = previous$portfolio,
    current_portfolio = current$portfolio
  )
}


test_that("diagnoses Long Term shareholder flows", {
  result <- shareholder_diagnostics_case("long_term")

  expect_s3_class(
    result,
    "shareholder_flow_diagnostics"
  )

  expect_equal(
    result$summary$reported_redemptions,
    1396.03
  )

  expect_equal(
    result$summary$implied_net_flow,
    -1458.40
  )

  expect_equal(
    result$summary$flow_difference,
    -62.37
  )

  expect_equal(
    result$summary$quota_adjustment_effect,
    -62.36
  )

  expect_equal(
    result$summary$difference_vs_adjustment,
    -0.01
  )

  expect_identical(
    result$summary$status,
    "consistent_with_quota_adjustment"
  )
})


test_that("diagnoses Linear shareholder flows", {
  result <- shareholder_diagnostics_case("linear")

  expect_equal(
    result$summary$reported_redemptions,
    90488.63
  )

  expect_equal(
    result$summary$implied_net_flow,
    -90488.62
  )

  expect_equal(
    result$summary$flow_difference,
    0.01
  )

  expect_equal(
    result$summary$quota_adjustment_effect,
    247.08
  )

  expect_identical(
    result$summary$status,
    "within_tolerance"
  )
})


test_that("preserves quota adjustment source records", {
  result <- shareholder_diagnostics_case("linear")

  expect_equal(nrow(result$adjustments), 1L)

  expect_true(
    "source_records_previous" %in%
      names(result$adjustments)
  )

  expect_true(
    "source_records_current" %in%
      names(result$adjustments)
  )

  expect_equal(
    result$adjustments$change,
    247.08
  )
})


test_that("does not treat missing flow reports as zero", {
  previous <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-19"
  )

  current$position$shareholder_flows <- NULL

  result <- diagnose_shareholder_flows(
    previous$position,
    current$position,
    previous$portfolio,
    current$portfolio
  )

  expect_true(
    is.na(result$summary$reported_net_flow)
  )

  expect_true(
    is.na(result$summary$flow_difference)
  )

  expect_identical(
    result$summary$status,
    "missing_flow_report"
  )
})


test_that("flags unexplained flow differences", {
  previous <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-19"
  )

  current$position$shareholder_flows$tot_rgt <- 80000

  result <- diagnose_shareholder_flows(
    previous$position,
    current$position,
    previous$portfolio,
    current$portfolio
  )

  expect_identical(
    result$summary$status,
    "requires_review"
  )

  expect_equal(nrow(result$issues), 1L)
})


test_that("rejects invalid reported flows", {
  previous <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-19"
  )

  current$position$shareholder_flows$tot_rgt <- -100

  expect_error(
    diagnose_shareholder_flows(
      previous$position,
      current$position,
      previous$portfolio,
      current$portfolio
    )
  )
})


test_that("validates monetary tolerance", {
  previous <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    "linear",
    "2026-10-19"
  )

  expect_error(
    diagnose_shareholder_flows(
      previous$position,
      current$position,
      previous$portfolio,
      current$portfolio,
      tolerance_brl = -0.01
    )
  )
})


test_that("does not mutate imported reports", {
  previous <- shareholder_diagnostics_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- shareholder_diagnostics_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  diagnose_shareholder_flows(
    previous$position,
    current$position,
    previous$portfolio,
    current$portfolio
  )

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
