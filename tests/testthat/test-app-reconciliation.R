
# tests/testthat/test-app-reconciliation.R

bc_app_test_source <- local({
  app_environment <- NULL

  function() {
    if (is.null(app_environment)) {
      root <- normalizePath(
        testthat::test_path("..", ".."),
        winslash = "/",
        mustWork = TRUE
      )

      loaded <- new.env(parent = globalenv())

      withr::with_dir(root, {
        source(
          file.path("plataforma", "app_reconciliation.R"),
          local = loaded
        )
      })

      app_environment <<- loaded
    }

    app_environment
  }
})


bc_app_test_portfolio_path <- local({
  catalog <- NULL

  function(portfolio_code, date) {
    if (is.null(catalog)) {
      paths <- list.files(
        testthat::test_path("fixtures", "daycoval"),
        pattern = "\\.csv$",
        full.names = TRUE
      )

      catalog <<- lapply(paths, function(path) {
        portfolio <- batercota::read_daycoval_portfolio(path)

        list(
          path = path,
          code = as.character(
            portfolio$metadata$codigo_carteira
          ),
          date = portfolio$metadata$data_posicao
        )
      })
    }

    matches <- vapply(
      catalog,
      function(item) {
        identical(item$code, portfolio_code) &&
          identical(item$date, as.Date(date))
      },
      logical(1)
    )

    if (sum(matches) != 1L) {
      stop(
        "Esperada exatamente uma Carteira Diária para ",
        portfolio_code,
        " em ",
        date,
        call. = FALSE
      )
    }

    catalog[[which(matches)]]$path
  }
})


bc_app_test_paths <- function() {
  list(
    portfolio = testthat::test_path(
      "fixtures",
      "daycoval",
      "Carteira Diária - 261022_155515.csv"
    ),
    position = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "long_term_2026-10-19.csv"
    ),
    xp = testthat::test_path(
      "fixtures",
      "xp",
      "Extrato_10850552_23_10_2026 18_01_12.pdf"
    ),
    previous_portfolio = bc_app_test_portfolio_path(
      "13386735",
      "2026-10-16"
    ),
    previous_position = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "long_term_2026-10-16.csv"
    )
  )
}


test_that("assets page contains local library selectors", {
  app <- bc_app_test_source()

  html <- paste(
    as.character(app$ui),
    collapse = ""
  )

  input_ids <- c(
    "portfolio_path_library_hash",
    "position_path_library_hash",
    "xp_path_library_hash",
    "previous_portfolio_path_library_hash",
    "previous_position_path_library_hash"
  )

  for (input_id in input_ids) {
    expect_true(
      grepl(
        paste0('id="', input_id, '"'),
        html,
        fixed = TRUE
      ),
      info = input_id
    )
  }
})


test_that("assets page imports, saves and reuses historical reports", {
  app <- bc_app_test_source()
  paths <- bc_app_test_paths()

  library_path <- tempfile(
    pattern = "batercota-app-library-"
  )

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  withr::local_options(
    batercota.report_library = library_path
  )

  shiny::testServer(app$server, {
    session$setInputs(
      fund = "long_term",
      portfolio_path = paths$portfolio,
      position_path = paths$position,
      xp_path = paths$xp,
      financial_tolerance = 100
    )

    session$setInputs(analyze = 1L)

    expect_null(read_error())

    first_analysis <- analysis()

    expect_false(is.null(first_analysis))

    expect_s3_class(
      first_analysis$external,
      "positions_reconciliation"
    )

    expect_s3_class(
      first_analysis$internal,
      "daycoval_reconciliation"
    )

    expect_identical(
      first_analysis$metadata$position_date,
      as.Date("2026-10-19")
    )

    expect_identical(
      current_reports()$fund,
      "long_term"
    )

    stored <- batercota::list_reports(
      library_path = library_path
    )

    expect_equal(nrow(stored), 3L)

    expect_setequal(
      stored$report_type,
      c(
        "daycoval_portfolio",
        "daycoval_position",
        "xp_statement"
      )
    )

    expect_true(all(
      stored$fund_key == "long_term"
    ))

    expect_true(all(
      stored$reference_date == as.Date("2026-10-19")
    ))

    stored_paths <- vapply(
      c(
        "daycoval_portfolio",
        "daycoval_position",
        "xp_statement"
      ),
      function(report_type) {
        record <- stored[
          stored$report_type == report_type,
          ,
          drop = FALSE
        ]

        batercota::load_report(
          hash = record$sha256[[1L]],
          library_path = library_path,
          parsed = FALSE
        )
      },
      character(1)
    )

    session$setInputs(
      portfolio_path = stored_paths[["daycoval_portfolio"]],
      position_path = stored_paths[["daycoval_position"]],
      xp_path = stored_paths[["xp_statement"]]
    )

    session$setInputs(analyze = 2L)

    expect_null(read_error())

    second_analysis <- analysis()

    expect_equal(
      second_analysis$positions,
      first_analysis$positions
    )

    expect_equal(
      second_analysis$external$summary,
      first_analysis$external$summary
    )

    expect_equal(
      nrow(batercota::list_reports(library_path)),
      3L
    )

    session$setInputs(
      previous_portfolio_path = paths$previous_portfolio,
      previous_position_path = paths$previous_position,
      return_tolerance = 0.05
    )

    session$setInputs(analyze_return = 1L)

    expect_null(return_error())

    result <- return_analysis()

    expect_false(is.null(result))

    expect_s3_class(
      result$reconciliation,
      "portfolio_return_reconciliation"
    )

    expect_s3_class(
      result$flows,
      "shareholder_flow_diagnostics"
    )

    stored_after_return <- batercota::list_reports(
      library_path = library_path
    )

    expect_equal(
      nrow(stored_after_return),
      5L
    )

    expect_setequal(
      unique(stored_after_return$reference_date),
      as.Date(c(
        "2026-10-16",
        "2026-10-19"
      ))
    )

    session$setInputs(fund = "linear")
    session$flushReact()

    expect_null(analysis())
    expect_null(current_reports())
    expect_null(return_analysis())
  })
})


test_that("assets page rejects reports from another fund", {
  app <- bc_app_test_source()
  paths <- bc_app_test_paths()

  library_path <- tempfile(
    pattern = "batercota-app-rejected-"
  )

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  withr::local_options(
    batercota.report_library = library_path
  )

  shiny::testServer(app$server, {
    session$setInputs(
      fund = "linear",
      portfolio_path = paths$portfolio,
      position_path = paths$position,
      xp_path = paths$xp,
      financial_tolerance = 100
    )

    session$setInputs(analyze = 1L)

    expect_null(analysis())
    expect_null(current_reports())

    expect_match(
      read_error(),
      "não pertence"
    )

    expect_equal(
      nrow(batercota::list_reports(library_path)),
      0L
    )
  })
})


test_that("app exposes date-based library selection", {
  app <- bc_app_test_source()

  html <- paste(
    as.character(app$ui),
    collapse = ""
  )

  ids <- c(
    "asset_reports-reference_date",
    "asset_reports-search",
    "asset_reports-apply",
    "previous_reports-reference_date",
    "previous_reports-search",
    "previous_reports-apply"
  )

  for (id in ids) {
    expect_match(
      html,
      id,
      fixed = TRUE
    )
  }
})

