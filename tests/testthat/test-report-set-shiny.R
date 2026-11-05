
# tests/testthat/test-report-set-shiny.R


source(
  testthat::test_path(
    "..",
    "..",
    "plataforma",
    "report_library_shiny.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "plataforma",
    "report_set_shiny.R"
  ),
  local = TRUE
)



report_set_shiny_fixture <- function() {
  testthat::test_path(
    "fixtures",
    "daycoval-position",
    "long_term_2026-10-19.csv"
  )
}


test_that("date picker uses the correct Shiny namespace", {
  ui <- bc_report_set_picker_ui(
    id = "asset_reports"
  )

  html <- paste(as.character(ui), collapse = "")

  expect_match(
    html,
    "asset_reports-reference_date",
    fixed = TRUE
  )

  expect_match(
    html,
    "asset_reports-search",
    fixed = TRUE
  )

  expect_match(
    html,
    "asset_reports-apply",
    fixed = TRUE
  )

  expect_match(
    html,
    "asset_reports-availability",
    fixed = TRUE
  )

  expect_match(
    html,
    "asset_reports-versions",
    fixed = TRUE
  )
})


test_that("resolves stored paths without requiring missing files", {
  library_path <- tempfile(
    pattern = "batercota-report-set-shiny-"
  )

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  saved <- batercota::save_report(
    report_set_shiny_fixture(),
    library_path
  )

  result <- batercota::resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = c(
      "daycoval_portfolio",
      "daycoval_position"
    ),
    library_path = library_path
  )

  paths <- bc_report_set_verified_paths(
    report_set = result,
    report_fields = c(
      portfolio_path = "daycoval_portfolio",
      position_path = "daycoval_position"
    ),
    library_path = library_path
  )

  expect_true(is.na(
    paths[["portfolio_path"]]
  ))

  expect_true(file.exists(
    paths[["position_path"]]
  ))

  expect_match(
    basename(paths[["position_path"]]),
    saved$sha256[[1L]],
    fixed = TRUE
  )
})


test_that("rejects incompatible report field mappings", {
  expect_error(
    bc_report_set_validate_fields(
      c(
        first = "daycoval_position",
        second = "daycoval_position"
      )
    ),
    "report_fields"
  )

  expect_error(
    bc_report_set_validate_fields(
      c(other = "fund_cash_plan")
    ),
    "report_fields"
  )
})


test_that("Shiny module searches reports for the selected fund", {
  library_path <- tempfile(
    pattern = "batercota-report-set-module-"
  )

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  withr::local_options(
    batercota.report_library = library_path
  )

  batercota::save_report(
    report_set_shiny_fixture(),
    library_path
  )

  selected_fund <- shiny::reactiveVal("long_term")
  revision <- shiny::reactiveVal(0L)

  shiny::testServer(
    bc_report_set_picker_server,
    args = list(
      selected_fund = selected_fund,
      report_fields = c(
        portfolio_path = "daycoval_portfolio",
        position_path = "daycoval_position",
        xp_path = "xp_statement"
      ),
      revision = revision
    ),
    {
      session$setInputs(
        reference_date = as.Date("2026-10-19")
      )

      session$setInputs(search = 1L)

      expect_null(report_set_error())

      result <- report_set_state()

      expect_s3_class(
        result,
        "batercota_report_set"
      )

      expect_identical(
        result$status,
        "incomplete"
      )

      expect_identical(
        result$reports$status,
        c("missing", "available", "missing")
      )

      selected_fund("linear")
      session$flushReact()

      expect_null(report_set_state())
    }
  )
})
