
# tests/testthat/test-partial-portfolio-return.R

partial_return_portfolio <- local({
  portfolios <- NULL

  function(code, date) {
    if (is.null(portfolios)) {
      paths <- list.files(
        testthat::test_path("fixtures", "daycoval"),
        pattern = "\\.csv$",
        full.names = TRUE
      )

      portfolios <<- purrr::map(
        paths,
        read_daycoval_portfolio
      )
    }

    matches <- purrr::keep(
      portfolios,
      function(portfolio) {
        identical(
          portfolio$metadata$codigo_carteira,
          code
        ) &&
          identical(
            portfolio$metadata$data_posicao,
            as.Date(date)
          )
      }
    )

    if (length(matches) != 1L) {
      stop("Fixture Daycoval não encontrada.")
    }

    matches[[1L]]
  }
})


partial_return_case <- function() {
  list(
    previous_position = read_daycoval_position(
      testthat::test_path(
        "fixtures",
        "daycoval-position",
        "linear_2026-10-16.csv"
      )
    ),
    current_position = read_daycoval_position(
      testthat::test_path(
        "fixtures",
        "daycoval-position",
        "linear_2026-10-19.csv"
      )
    ),
    previous_portfolio = partial_return_portfolio(
      "13711539",
      "2026-10-16"
    ),
    current_portfolio = partial_return_portfolio(
      "13711539",
      "2026-10-19"
    )
  )
}


partial_return_reconcile <- function(case) {
  reconcile_portfolio_return(
    previous_position = case$previous_position,
    current_position = case$current_position,
    previous_portfolio = case$previous_portfolio,
    current_portfolio = case$current_portfolio
  )
}


test_that("complete estimates preserve existing results", {
  result <- partial_return_reconcile(
    partial_return_case()
  )

  summary <- result$summary

  expect_s3_class(
    result,
    "portfolio_return_reconciliation"
  )

  expect_identical(
    summary$estimate_completeness,
    "complete"
  )

  expect_equal(summary$n_unestimated, 0L)

  expect_equal(
    summary$partial_estimated_result,
    summary$estimated_result
  )

  expect_equal(
    summary$partial_estimated_return_percent,
    summary$estimated_return_percent
  )

  expect_equal(
    summary$partial_difference_pp,
    summary$difference_pp
  )

  expect_equal(
    summary$partial_residual_brl,
    summary$residual_brl
  )

  expect_identical(
    summary$comparison_status,
    "within_tolerance"
  )
})


test_that("missing fund price produces a partial estimate", {
  case <- partial_return_case()

  case$current_position$positions$funds$pu[[1L]] <-
    NA_real_

  result <- partial_return_reconcile(case)
  summary <- result$summary

  expect_gt(summary$n_unestimated, 0L)

  expect_true(is.na(summary$estimated_result))
  expect_true(is.na(summary$estimated_return_percent))

  expect_true(
    is.finite(summary$partial_estimated_result)
  )

  expect_true(
    is.finite(summary$partial_estimated_return_percent)
  )

  expect_identical(
    summary$comparison_status,
    "not_evaluated"
  )

  expect_identical(
    summary$estimate_completeness,
    "incomplete"
  )

  expect_lt(
    summary$position_coverage_percent,
    100
  )
})


test_that("partial result sums only known contributions", {
  case <- partial_return_case()

  case$current_position$positions$funds$pu[[1L]] <-
    NA_real_

  result <- partial_return_reconcile(case)
  components <- result$components

  known <- components$partial_estimated_result[
    is.finite(components$partial_estimated_result)
  ]

  expect_equal(
    result$summary$partial_estimated_result,
    sum(known)
  )

  expect_true(any(
    components$estimate_status == "partial"
  ))

  expect_true(any(
    result$issues$issue_type == "invalid_price"
  ))
})


test_that("opening investment coverage detects missing returns", {
  case <- partial_return_case()

  complete <- partial_return_reconcile(case)

  case$current_position$positions$funds$pu[[1L]] <-
    NA_real_

  partial <- partial_return_reconcile(case)

  expect_equal(
    complete$summary$opening_investment_coverage_percent,
    100,
    tolerance = 1e-8
  )

  expect_lt(
    partial$summary$opening_investment_coverage_percent,
    100
  )

  expect_gt(
    partial$summary$covered_opening_investment_value,
    0
  )

  expect_lte(
    partial$summary$covered_opening_investment_value,
    partial$summary$opening_investment_value
  )
})


test_that("missing estimates are not converted to zero", {
  result <- partial_return_reconcile(
    partial_return_case()
  )

  result$components$n_estimated[] <- 0L
  result$components$n_unestimated <-
    result$components$n_positions

  result$components$estimated_result[] <- NA_real_

  result$details$accounting_expenses$summary$n_estimated <-
    0L

  result$details$accounting_expenses$summary$n_unestimated <-
    result$details$accounting_expenses$summary$n_accounts

  updated <- return_reconciliation_add_partial(result)

  expect_true(is.na(
    updated$summary$partial_estimated_result
  ))

  expect_true(is.na(
    updated$summary$partial_estimated_return_percent
  ))

  expect_true(is.na(
    updated$summary$partial_difference_pp
  ))
})


test_that("partial attribution preserves source objects", {
  case <- partial_return_case()

  original_previous <- case$previous_position
  original_current <- case$current_position

  result <- partial_return_reconcile(case)

  expect_identical(
    case$previous_position,
    original_previous
  )

  expect_identical(
    case$current_position,
    original_current
  )

  expect_named(
    result,
    c(
      "metadata",
      "components",
      "summary",
      "issues",
      "details"
    )
  )

  expect_true(all(c(
    "estimated_result",
    "partial_estimated_result",
    "partial_contribution_pp",
    "estimate_status"
  ) %in% names(result$components)))
})
