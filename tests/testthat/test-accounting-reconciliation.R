accounting_fixture <- local({
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
            as.Date(date)
          )
      }
    )

    if (length(selected) != 1L) {
      stop("Fixture contábil não encontrada.")
    }

    selected[[1L]]
  }
})


test_that("reconstructs real accounting balances", {
  cases <- tibble::tribble(
    ~code, ~date, ~assets, ~liabilities, ~nav,
    "13386735", "2026-10-16", 2418201.55, 8392.20, 2409809.35,
    "13386735", "2026-10-19", 2428375.33, 10120.53, 2418254.80,
    "13711539", "2026-10-16", 30896065.58, 73972.67, 30822092.91,
    "13711539", "2026-10-19", 30904913.72, 164909.02, 30740004.70
  )

  purrr::pwalk(
    cases,
    function(code, date, assets, liabilities, nav) {
      portfolio <- accounting_fixture(code, date)

      result <- reconcile_portfolio_accounting(portfolio)

      expect_s3_class(result, "portfolio_accounting")
      expect_equal(result$summary$total_assets, assets)
      expect_equal(result$summary$total_liabilities, liabilities)
      expect_equal(result$summary$calculated_nav, nav)

      expect_identical(result$summary$status, "equal")
      expect_equal(nrow(result$issues), 0L)

      expect_false(
        any(result$groups$grp_n3 == "26000")
      )
    }
  )
})


test_that("preserves source entries without mutation", {
  portfolio <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  original_entries <- portfolio$entries

  result <- reconcile_portfolio_accounting(portfolio)

  expect_identical(
    portfolio$entries,
    original_entries
  )

  expect_identical(
    result$entries$source_line,
    original_entries$source_line
  )

  expect_identical(
    result$entries$record_number,
    original_entries$record_number
  )
})


test_that("preserves accounting group source records", {
  portfolio <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- reconcile_portfolio_accounting(portfolio)

  group <- result$groups |>
    dplyr::filter(.data$grp_n3 == "19703")

  expect_equal(nrow(group), 1L)
  expect_equal(group$n_entries, 2L)

  expect_equal(
    nrow(group$source_records[[1L]]),
    2L
  )

  expect_equal(
    sum(group$source_records[[1L]]$market_value),
    group$amount
  )
})


test_that("reconciles Long Term accounting changes", {
  previous <- accounting_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- reconcile_accounting_changes(
    previous,
    current
  )

  expect_s3_class(
    result,
    "portfolio_accounting_changes"
  )

  expect_equal(result$summary$nav_change, 8445.45)
  expect_equal(result$summary$explained_change, 8445.45)
  expect_equal(result$summary$residual, 0)

  expect_identical(result$summary$status, "equal")

  expect_equal(
    result$summary$reported_return,
    1.32034965 / 1.31494548 - 1
  )
})


test_that("reconciles Linear accounting changes", {
  previous <- accounting_fixture(
    "13711539",
    "2026-10-16"
  )

  current <- accounting_fixture(
    "13711539",
    "2026-10-19"
  )

  result <- reconcile_accounting_changes(
    previous,
    current
  )

  expect_equal(result$summary$nav_change, -82088.21)
  expect_equal(result$summary$explained_change, -82088.21)
  expect_equal(result$summary$residual, 0)

  expect_identical(result$summary$status, "equal")
})


test_that("preserves negative contributions from liabilities", {
  previous <- accounting_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  result <- reconcile_accounting_changes(
    previous,
    current
  )

  adjustment <- result$changes |>
    dplyr::filter(.data$grp_n3 == "24005")

  expect_equal(nrow(adjustment), 1L)
  expect_identical(adjustment$change_type, "opened")
  expect_equal(adjustment$change, -62.36)
})


test_that("matches accounting codes despite label changes", {
  previous <- accounting_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  current$entries$no_grp_n3[
    current$entries$grp_n3 == "19703"
  ] <- "NOVO NOME CONTABIL"

  result <- reconcile_accounting_changes(
    previous,
    current
  )

  expect_equal(result$summary$residual, 0)
  expect_identical(result$summary$status, "equal")
})


test_that("flags unknown accounting classifications", {
  portfolio <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  portfolio$entries$grp_n1[[1L]] <- "99999"

  result <- reconcile_portfolio_accounting(portfolio)

  expect_identical(
    result$summary$status,
    "not_evaluated"
  )

  expect_true(any(
    result$issues$issue_type == "unknown_accounting_group"
  ))
})


test_that("rejects comparisons between different funds", {
  previous <- accounting_fixture(
    "13386735",
    "2026-10-16"
  )

  current <- accounting_fixture(
    "13711539",
    "2026-10-19"
  )

  expect_error(
    reconcile_accounting_changes(previous, current)
  )
})


test_that("requires increasing accounting dates", {
  previous <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  current <- accounting_fixture(
    "13386735",
    "2026-10-16"
  )

  expect_error(
    reconcile_accounting_changes(previous, current)
  )
})


test_that("validates monetary tolerance", {
  portfolio <- accounting_fixture(
    "13386735",
    "2026-10-19"
  )

  expect_error(
    reconcile_portfolio_accounting(
      portfolio,
      tolerance = -1
    )
  )

  expect_error(
    reconcile_portfolio_accounting(
      portfolio,
      tolerance = NA_real_
    )
  )
})
