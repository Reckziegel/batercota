accounting_expense_fixture <- local({
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
      stop("Fixture contábil não encontrada.")
    }

    matches[[1L]]
  }
})


accounting_expense_position <- function(fund, date) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  )
}


test_that("estimates Long Term expense effects", {
  previous <- accounting_expense_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_expense_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- estimate_accounting_expenses(
    previous,
    current
  )

  expect_s3_class(
    result,
    "accounting_expense_attribution"
  )

  expect_equal(
    result$summary$estimated_effect,
    -290.77,
    tolerance = 0.01
  )

  expect_equal(result$summary$n_unestimated, 0L)

  expect_identical(
    result$summary$status,
    "estimated"
  )

  expect_equal(
    sum(result$effects$change[
      result$effects$component == "deferred_expenses"
    ]),
    -20.83,
    tolerance = 0.01
  )

  expect_equal(
    sum(result$effects$change[
      result$effects$component == "accrued_expenses"
    ]),
    -269.94,
    tolerance = 0.01
  )
})


test_that("estimates Linear expense effects", {
  previous <- accounting_expense_fixture(
    "13711539",
    "2026-10-16"
  )

  current <- accounting_expense_fixture(
    "13711539",
    "2026-10-19"
  )

  result <- estimate_accounting_expenses(
    previous,
    current
  )

  expect_equal(
    result$summary$estimated_effect,
    -778.23,
    tolerance = 0.01
  )

  expect_equal(result$summary$n_unestimated, 0L)
})


test_that("excludes shareholder flows and futures", {
  previous <- accounting_expense_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_expense_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- estimate_accounting_expenses(
    previous,
    current
  )

  expect_false(any(
    result$effects$grp_n2 %in% c(
      "21000",
      "24000",
      "19100"
    )
  ))

  expect_false(any(
    result$effects$grp_n3 == "21010"
  ))

  expect_false(any(
    result$effects$grp_n3 == "24005"
  ))
})


test_that("flags positive expense balance effects", {
  changes <- tibble::tibble(
    grp_n1 = c("20000", "10000"),
    grp_n2 = c("23700", "19700"),
    grp_n3 = c("23711", "19701"),
    group_name_current = c(
      "TAXA DE GESTAO",
      "TAXA CVM DIFERIDA"
    ),
    group_name_previous = c(
      "TAXA DE GESTAO",
      "TAXA CVM DIFERIDA"
    ),
    previous_amount = c(-100, 100),
    current_amount = c(-50, 150),
    change = c(50, 50)
  )

  effects <- accounting_expense_effects(changes)

  expect_equal(nrow(effects), 2L)

  expect_true(all(
    effects$status == "requires_cash_flow_review"
  ))

  expect_true(all(is.na(effects$estimated_effect)))
})


test_that("reconciles Long Term after expenses", {
  previous_position <- accounting_expense_position(
    "long_term",
    "2026-10-16"
  )

  current_position <- accounting_expense_position(
    "long_term",
    "2026-10-19"
  )

  previous_portfolio <- accounting_expense_fixture(
    "13386735",
    "2026-10-16"
  )

  current_portfolio <- accounting_expense_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- reconcile_portfolio_return(
    previous_position,
    current_position,
    previous_portfolio,
    current_portfolio
  )

  expect_s3_class(
    result,
    "portfolio_return_reconciliation"
  )

  expect_equal(
    result$summary$accounting_expense_effect,
    -290.77,
    tolerance = 0.01
  )

  expect_equal(
    result$summary$estimated_result,
    9903.82,
    tolerance = 0.03
  )

  expect_lt(
    abs(result$summary$residual_brl),
    0.10
  )

  expect_identical(
    result$summary$comparison_status,
    "within_tolerance"
  )

  expect_identical(
    result$summary$coverage_status,
    "partial_model"
  )
})


test_that("reconciles Linear with residual disclosure", {
  previous_position <- accounting_expense_position(
    "linear",
    "2026-10-16"
  )

  current_position <- accounting_expense_position(
    "linear",
    "2026-10-19"
  )

  previous_portfolio <- accounting_expense_fixture(
    "13711539",
    "2026-10-16"
  )

  current_portfolio <- accounting_expense_fixture(
    "13711539",
    "2026-10-19"
  )

  result <- reconcile_portfolio_return(
    previous_position,
    current_position,
    previous_portfolio,
    current_portfolio
  )

  expect_equal(
    result$summary$accounting_expense_effect,
    -778.23,
    tolerance = 0.01
  )

  expect_equal(
    result$summary$estimated_result,
    8153.33,
    tolerance = 0.03
  )

  expect_equal(
    result$summary$residual_brl,
    -247.07,
    tolerance = 0.05
  )

  expect_identical(
    result$summary$comparison_status,
    "within_tolerance"
  )
})


test_that("supports a stricter return tolerance", {
  previous_position <- accounting_expense_position(
    "linear",
    "2026-10-16"
  )

  current_position <- accounting_expense_position(
    "linear",
    "2026-10-19"
  )

  previous_portfolio <- accounting_expense_fixture(
    "13711539",
    "2026-10-16"
  )

  current_portfolio <- accounting_expense_fixture(
    "13711539",
    "2026-10-19"
  )

  result <- reconcile_portfolio_return(
    previous_position,
    current_position,
    previous_portfolio,
    current_portfolio,
    tolerance_pp = 0.0001
  )

  expect_identical(
    result$summary$comparison_status,
    "outside_tolerance"
  )
})


test_that("rejects mismatched report dates", {
  previous_position <- accounting_expense_position(
    "long_term",
    "2026-10-16"
  )

  current_position <- accounting_expense_position(
    "long_term",
    "2026-10-19"
  )

  wrong_portfolio <- accounting_expense_fixture(
    "13386735",
    "2026-10-16"
  )

  expect_error(
    reconcile_portfolio_return(
      previous_position,
      current_position,
      wrong_portfolio,
      wrong_portfolio
    )
  )
})


test_that("preserves source records and account groups", {
  previous <- accounting_expense_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_expense_fixture(
    "13386735",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  result <- estimate_accounting_expenses(
    previous,
    current
  )

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)

  expect_true(
    "source_records_previous" %in%
      names(result$effects)
  )

  expect_true(
    "source_records_current" %in%
      names(result$effects)
  )
})

test_that("excludes generic payables from expense P&L", {
  changes <- tibble::tibble(
    grp_n1 = c("20000", "20000"),
    grp_n2 = c("23700", "23700"),
    grp_n3 = c("23711", "23720"),
    group_name_previous = c(
      "TAXA DE GESTAO",
      "VALORES A PAGAR"
    ),
    group_name_current = c(
      "TAXA DE GESTAO",
      NA_character_
    ),
    previous_amount = c(-773.52, -5516.94),
    current_amount = c(-837.34, 0),
    change = c(-63.82, 5516.94)
  )

  result <- accounting_expense_effects(changes)

  expect_equal(nrow(result), 1L)

  expect_identical(
    result$grp_n3,
    "23711"
  )

  expect_equal(
    result$estimated_effect,
    -63.82
  )
})
