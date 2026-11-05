
# tests/testthat/test-report-set.R

report_set_test_library <- function() {
  tempfile(pattern = "batercota-report-set-")
}


report_set_test_fixtures <- function() {
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
    previous_position = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "long_term_2026-10-16.csv"
    ),
    linear_position = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "linear_2026-10-19.csv"
    )
  )
}


report_set_save_complete <- function(library_path) {
  fixtures <- report_set_test_fixtures()

  lapply(
    fixtures[c("portfolio", "position", "xp")],
    batercota::save_report,
    library_path = library_path
  )
}


test_that("resolves a complete set for the exact reference date", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  report_set_save_complete(library_path)

  result <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    library_path = library_path
  )

  expect_s3_class(result, "batercota_report_set")
  expect_identical(result$status, "ready")

  expect_identical(
    result$reference_date,
    as.Date("2026-10-19")
  )

  expect_setequal(
    result$reports$report_type,
    c(
      "daycoval_portfolio",
      "daycoval_position",
      "xp_statement"
    )
  )

  expect_true(all(
    result$reports$status == "available"
  ))

  expect_true(all(
    result$reports$n_candidates == 1L
  ))

  expect_true(all(
    grepl("^[a-f0-9]{64}$", result$reports$sha256)
  ))
})


test_that("identifies missing reports without guessing", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  fixtures <- report_set_test_fixtures()

  save_report(
    fixtures$position,
    library_path
  )

  result <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    library_path = library_path
  )

  expect_identical(result$status, "incomplete")

  expect_identical(
    result$reports$status,
    c("missing", "available", "missing")
  )

  expect_true(is.na(
    result$reports$sha256[[1L]]
  ))

  expect_true(is.na(
    result$reports$sha256[[3L]]
  ))
})


test_that("never substitutes a report from another date", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  fixtures <- report_set_test_fixtures()

  save_report(
    fixtures$previous_position,
    library_path
  )

  result <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = "daycoval_position",
    library_path = library_path
  )

  expect_identical(result$status, "incomplete")
  expect_identical(result$reports$status, "missing")
  expect_equal(nrow(result$candidates), 0L)
})


test_that("never substitutes a report from another fund", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  fixtures <- report_set_test_fixtures()

  save_report(
    fixtures$linear_position,
    library_path
  )

  result <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = "daycoval_position",
    library_path = library_path
  )

  expect_identical(result$reports$status, "missing")
  expect_equal(nrow(result$candidates), 0L)
})


test_that("requires explicit selection for competing versions", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  fixtures <- report_set_test_fixtures()

  modified <- tempfile(fileext = ".csv")

  on.exit(unlink(modified), add = TRUE)

  lines <- readLines(
    fixtures$position,
    warn = FALSE,
    encoding = "Latin1"
  )

  original_header <- lines[[1L]]

  lines[[1L]] <- sub(
    "^[0-9]+;",
    "999999;",
    original_header
  )

  expect_false(identical(
    lines[[1L]],
    original_header
  ))

  writeLines(
    lines,
    modified,
    useBytes = TRUE
  )

  first <- save_report(
    fixtures$position,
    library_path
  )

  second <- save_report(
    modified,
    library_path
  )

  expect_false(identical(
    first$sha256,
    second$sha256
  ))

  unresolved <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = "daycoval_position",
    library_path = library_path
  )

  expect_identical(unresolved$status, "ambiguous")

  expect_identical(
    unresolved$reports$status,
    "ambiguous"
  )

  expect_equal(
    unresolved$reports$n_candidates,
    2L
  )

  expect_true(is.na(
    unresolved$reports$sha256[[1L]]
  ))

  selected <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = "daycoval_position",
    selections = c(
      daycoval_position = first$sha256[[1L]]
    ),
    library_path = library_path
  )

  expect_identical(selected$status, "ready")

  expect_identical(
    selected$reports$sha256[[1L]],
    first$sha256[[1L]]
  )

  expect_error(
    resolve_report_set(
      fund = "long_term",
      reference_date = "2026-10-19",
      report_types = "daycoval_position",
      selections = c(
        daycoval_position = strrep("0", 64L)
      ),
      library_path = library_path
    ),
    "não corresponde"
  )
})


test_that("preserves the requested report type order", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  report_set_save_complete(library_path)

  result <- resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    report_types = c(
      "xp_statement",
      "daycoval_position"
    ),
    library_path = library_path
  )

  expect_identical(
    result$reports$report_type,
    c(
      "xp_statement",
      "daycoval_position"
    )
  )

  expect_identical(result$status, "ready")
})


test_that("validates fund, date, and selections", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  expect_error(
    resolve_report_set(
      fund = "unknown_fund",
      reference_date = "2026-10-19",
      library_path = library_path
    ),
    "Fundo não cadastrado"
  )

  expect_error(
    resolve_report_set(
      fund = "long_term",
      reference_date = "19/10/2026",
      library_path = library_path
    ),
    "reference_date"
  )

  expect_error(
    resolve_report_set(
      fund = "long_term",
      reference_date = "2026-02-31",
      library_path = library_path
    ),
    "reference_date"
  )

  expect_error(
    resolve_report_set(
      fund = "long_term",
      reference_date = "2026-10-19",
      report_types = c(
        "daycoval_position",
        "daycoval_position"
      ),
      library_path = library_path
    ),
    "duplicados"
  )

  expect_error(
    resolve_report_set(
      fund = "long_term",
      reference_date = "2026-10-19",
      selections = c(
        fund_cash_plan = strrep("a", 64L)
      ),
      library_path = library_path
    ),
    "selections"
  )
})


test_that("does not modify stored reports while resolving", {
  library_path <- report_set_test_library()

  on.exit(
    unlink(library_path, recursive = TRUE),
    add = TRUE
  )

  report_set_save_complete(library_path)

  before <- list_reports(library_path)

  resolve_report_set(
    fund = "long_term",
    reference_date = "2026-10-19",
    library_path = library_path
  )

  after <- list_reports(library_path)

  expect_identical(after, before)
})
