
report_library_fixture <- function(folder, filename) {
  testthat::test_path(
    "fixtures",
    folder,
    filename
  )
}


report_library_position_fixture <- function() {
  report_library_fixture(
    "daycoval-position",
    "linear_2026-10-16.csv"
  )
}


report_library_test_directory <- function() {
  tempfile(pattern = "batercota-library-")
}


test_that("creates an empty persistent library", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  reports <- list_reports(library_path)

  expect_s3_class(reports, "tbl_df")
  expect_equal(nrow(reports), 0L)

  expect_true(dir.exists(file.path(library_path, "objects")))
  expect_true(dir.exists(file.path(library_path, "records")))
  expect_true(file.exists(file.path(library_path, "index.rds")))
})


test_that("saves and restores a Daycoval position", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_position_fixture()
  saved <- save_report(source, library_path)

  expect_equal(nrow(saved), 1L)
  expect_identical(saved$status, "stored")
  expect_identical(saved$fund_key, "linear")
  expect_identical(saved$fund_code, "13711539")
  expect_identical(saved$report_type, "daycoval_position")

  expect_equal(
    saved$period_start,
    as.Date("2026-10-16")
  )

  expect_equal(
    saved$period_end,
    as.Date("2026-10-16")
  )

  expect_match(saved$sha256, "^[a-f0-9]{64}$")

  restored <- load_report(
    saved$sha256[[1L]],
    library_path
  )

  original <- read_daycoval_position(source)

  expect_s3_class(restored, "daycoval_position")
  expect_equal(restored$metadata, original$metadata)
  expect_equal(restored$positions, original$positions)
})


test_that("recognizes identical files with different names", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_position_fixture()

  renamed <- tempfile(fileext = ".csv")
  on.exit(unlink(renamed), add = TRUE)

  expect_true(file.copy(source, renamed))

  first <- save_report(source, library_path)
  second <- save_report(renamed, library_path)

  expect_identical(first$sha256, second$sha256)
  expect_identical(second$status, "duplicate")

  expect_equal(nrow(list_reports(library_path)), 1L)

  expect_identical(
    second$source_name,
    basename(source)
  )
})


test_that("preserves different versions of the same period", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_position_fixture()

  modified <- tempfile(fileext = ".csv")
  on.exit(unlink(modified), add = TRUE)

  lines <- readLines(
    source,
    warn = FALSE,
    encoding = "Latin1"
  )

  original_header <- lines[[1L]]

  lines[[1L]] <- sub(
    "^[0-9]+;",
    "999999;",
    original_header
  )

  expect_false(identical(lines[[1L]], original_header))

  writeLines(
    lines,
    modified,
    useBytes = TRUE
  )

  first <- save_report(source, library_path)
  second <- save_report(modified, library_path)

  expect_identical(first$status, "stored")
  expect_identical(second$status, "stored")

  expect_false(
    identical(first$sha256, second$sha256)
  )

  expect_identical(
    first$version_key,
    second$version_key
  )

  reports <- list_reports(library_path)

  expect_equal(nrow(reports), 2L)
  expect_equal(length(unique(reports$sha256)), 2L)

  expect_s3_class(
    load_report(first$sha256[[1L]], library_path),
    "daycoval_position"
  )

  expect_s3_class(
    load_report(second$sha256[[1L]], library_path),
    "daycoval_position"
  )
})


test_that("recognizes Daycoval financial reports", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  movement <- report_library_fixture(
    "daycoval-financial",
    "linear_movement_2026-10-16_2026-10-22.csv"
  )

  cash <- report_library_fixture(
    "daycoval-financial",
    "linear_cash_2026-10-16_2026-10-22.csv"
  )

  saved_movement <- save_report(movement, library_path)
  saved_cash <- save_report(cash, library_path)

  expect_identical(
    saved_movement$report_type,
    "daycoval_daily_movement"
  )

  expect_identical(
    saved_cash$report_type,
    "daycoval_cash_statement"
  )

  expect_equal(
    saved_movement$period_start,
    as.Date("2026-10-16")
  )

  expect_equal(
    saved_movement$period_end,
    as.Date("2026-10-22")
  )

  expect_equal(
    saved_cash$period_start,
    as.Date("2026-10-15")
  )

  expect_equal(
    saved_cash$period_end,
    as.Date("2026-10-22")
  )

  expect_s3_class(
    load_report(saved_movement$sha256[[1L]], library_path),
    "daycoval_daily_movement"
  )

  expect_s3_class(
    load_report(saved_cash$sha256[[1L]], library_path),
    "daycoval_cash_statement"
  )
})


test_that("recognizes XP statements from internal metadata", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_fixture(
    "xp",
    "Extrato_10850552_23_10_2026 18_01_12.pdf"
  )

  saved <- save_report(source, library_path)

  expect_identical(saved$report_type, "xp_statement")
  expect_identical(saved$fund_key, "long_term")
  expect_identical(saved$fund_code, "13386735")

  expect_equal(
    saved$reference_date,
    as.Date("2026-10-19")
  )

  expect_s3_class(
    load_report(saved$sha256[[1L]], library_path),
    "xp_statement"
  )
})


test_that("recognizes the operations workbook", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_fixture(
    "operations",
    "fund_operations.xlsx"
  )

  original <- read_fund_operations(source)
  saved <- save_report(source, library_path)

  expect_identical(
    saved$report_type,
    "fund_operations"
  )

  expect_equal(
    saved$period_start,
    original$metadata$operation_start
  )

  expect_equal(
    saved$period_end,
    original$metadata$operation_end
  )

  if (original$metadata$n_portfolios > 1L) {
    expect_identical(saved$fund_key, "multiple")
  }

  expect_s3_class(
    load_report(saved$sha256[[1L]], library_path),
    "fund_operations"
  )
})


test_that("recognizes a fund cash planning workbook", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas LINEAR.xlsx"
  )

  original <- read_fund_cash_plan(source)
  expect_identical(
    unique(original$shareholder_operations$fund_code),
    "60629171"
  )

  identified <- batercota:::bc_cash_plan_fund(original)

  expect_identical(
    identified$fund_key,
    "linear"
  )

  expect_identical(
    identified$fund_code,
    "13711539"
  )

  inconsistent <- original

  inconsistent$investment_operations$portfolio_fund_name[[1L]] <-
    "OUTRO FUNDO"

  expect_error(
    batercota:::bc_cash_plan_fund(inconsistent),
    "nomes de fundos inconsistentes"
  )
  saved <- save_report(source, library_path)

  expect_identical(saved$report_type, "fund_cash_plan")
  expect_identical(saved$fund_key, "linear")

  valid_dates <- original$cash_flows$date[
    !is.na(original$cash_flows$date)
  ]

  expect_equal(saved$period_start, min(valid_dates))
  expect_equal(saved$period_end, max(valid_dates))

  expect_s3_class(
    load_report(saved$sha256[[1L]], library_path),
    "fund_cash_plan"
  )
})


test_that("filters reports without modifying the catalog", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  linear <- report_library_position_fixture()

  long_term <- report_library_fixture(
    "daycoval-position",
    "long_term_2026-10-16.csv"
  )

  save_report(linear, library_path)
  save_report(long_term, library_path)

  filtered <- list_reports(
    library_path,
    fund = "linear",
    report_type = "daycoval_position"
  )

  expect_equal(nrow(filtered), 1L)
  expect_identical(filtered$fund_key, "linear")

  expect_equal(nrow(list_reports(library_path)), 2L)
})


test_that("maintains the index immediately after saving", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  index <- readRDS(
    file.path(library_path, "index.rds")
  )

  expect_equal(nrow(index), 1L)
  expect_identical(index$sha256, saved$sha256)
  expect_identical(index$fund_key, saved$fund_key)

  expect_identical(
    index$source_name,
    saved$source_name
  )
})


test_that("lists unchanged files without full hash verification", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  stored_path <- load_report(
    saved$sha256[[1L]],
    library_path,
    parsed = FALSE
  )

  connection <- file(stored_path, open = "r+b")
  writeBin(as.raw(0x00), connection)
  close(connection)

  # File size is unchanged, so the cached catalog remains usable.
  listed <- list_reports(library_path)

  expect_equal(nrow(listed), 1L)
  expect_identical(listed$sha256, saved$sha256)

  # Loading and explicit verification still detect the modification.
  expect_error(
    load_report(
      saved$sha256[[1L]],
      library_path,
      parsed = FALSE
    ),
    "SHA-256"
  )

  audit <- verify_report_library(library_path)

  expect_identical(
    audit$status,
    "hash_mismatch"
  )
})


test_that("verifies valid original files explicitly", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  save_report(
    report_library_position_fixture(),
    library_path
  )

  audit <- verify_report_library(library_path)

  expect_equal(nrow(audit), 1L)
  expect_identical(audit$status, "ok")

  expect_identical(
    audit$sha256,
    audit$actual_sha256
  )
})


test_that("recovers from a missing or damaged central index", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  index_path <- file.path(library_path, "index.rds")

  unlink(index_path)

  recovered <- list_reports(library_path)

  expect_equal(nrow(recovered), 1L)
  expect_identical(recovered$sha256, saved$sha256)

  writeLines("invalid index content", index_path)

  recovered_again <- list_reports(library_path)

  expect_equal(nrow(recovered_again), 1L)
  expect_identical(recovered_again$sha256, saved$sha256)

  expect_s3_class(readRDS(index_path), "tbl_df")
})


test_that("recovers a file with missing metadata", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  hash <- saved$sha256[[1L]]

  record_path <- file.path(
    library_path,
    "records",
    paste0(hash, ".rds")
  )

  unlink(record_path)

  expect_false(file.exists(record_path))

  audit <- verify_report_library(library_path)

  expect_identical(
    audit$status,
    "missing_metadata"
  )

  recovered <- list_reports(library_path)

  expect_true(file.exists(record_path))
  expect_equal(nrow(recovered), 1L)
  expect_identical(recovered$sha256[[1L]], hash)

  expect_s3_class(
    load_report(hash, library_path),
    "daycoval_position"
  )
})


test_that("recovers an original left by interrupted saving", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  source <- report_library_position_fixture()

  hash <- digest::digest(
    source,
    algo = "sha256",
    file = TRUE
  )

  destination <- file.path(
    library_path,
    "objects",
    substr(hash, 1L, 2L),
    paste0(hash, ".csv")
  )

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  expect_true(file.copy(source, destination))

  recovered <- list_reports(library_path)

  expect_equal(nrow(recovered), 1L)
  expect_identical(recovered$sha256[[1L]], hash)

  expect_true(file.exists(
    file.path(
      library_path,
      "records",
      paste0(hash, ".rds")
    )
  ))

  expect_s3_class(
    load_report(hash, library_path),
    "daycoval_position"
  )
})


test_that("repairs a corrupted metadata record explicitly", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  hash <- saved$sha256[[1L]]

  record_path <- file.path(
    library_path,
    "records",
    paste0(hash, ".rds")
  )

  writeLines("invalid metadata", record_path)

  repaired <- repair_report_library(library_path)

  expect_equal(nrow(repaired), 1L)
  expect_identical(repaired$sha256[[1L]], hash)

  expect_s3_class(readRDS(record_path), "tbl_df")

  expect_identical(
    verify_report_library(library_path)$status,
    "ok"
  )
})


test_that("ignores incomplete temporary files", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  object_directory <- file.path(
    library_path,
    "objects",
    substr(saved$sha256[[1L]], 1L, 2L)
  )

  temporary <- file.path(
    object_directory,
    ".incoming-interrupted"
  )

  writeLines("incomplete", temporary)

  expect_equal(nrow(list_reports(library_path)), 1L)
})


test_that("detects a changed file size during catalog checking", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  stored_path <- load_report(
    saved$sha256[[1L]],
    library_path,
    parsed = FALSE
  )

  writeLines("corrupted original", stored_path)

  expect_error(
    list_reports(library_path),
    "Integridade comprometida"
  )
})


test_that("rejects orphan metadata records", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  stored_path <- load_report(
    saved$sha256[[1L]],
    library_path,
    parsed = FALSE
  )

  unlink(stored_path)

  audit <- verify_report_library(library_path)

  expect_identical(
    audit$status,
    "missing_object"
  )

  expect_error(
    list_reports(library_path),
    "sem os arquivos originais"
  )
})


test_that("rejects unknown files without saving records", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  invalid <- tempfile(fileext = ".csv")
  on.exit(unlink(invalid), add = TRUE)

  writeLines(
    c(
      "1;UnknownLayout;C;field",
      "2;UnknownLayout;V;value"
    ),
    invalid
  )

  expect_error(
    save_report(invalid, library_path)
  )

  expect_equal(nrow(list_reports(library_path)), 0L)

  expect_error(
    save_report(
      tempfile(fileext = ".txt"),
      library_path
    )
  )
})


test_that("rejects invalid identifiers and missing reports", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  expect_error(
    load_report("not-a-sha256", library_path),
    "SHA-256"
  )

  expect_error(
    load_report(strrep("a", 64L), library_path),
    "não encontrado"
  )
})


test_that("reconstructs metadata independently of memory", {
  library_path <- report_library_test_directory()
  on.exit(unlink(library_path, recursive = TRUE), add = TRUE)

  saved <- save_report(
    report_library_position_fixture(),
    library_path
  )

  reconstructed <- list_reports(library_path)

  expect_identical(
    reconstructed$sha256,
    saved$sha256
  )

  expect_identical(
    reconstructed$source_name,
    saved$source_name
  )

  expect_identical(
    reconstructed$fund_key,
    saved$fund_key
  )
})



test_that("reconhece o código interno do Long Term", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas Long Term LB.xlsx"
  )

  testthat::skip_if_not(file.exists(path))

  cash_plan <- read_fund_cash_plan(path)

  expect_true(
    "56365598" %in%
      cash_plan$shareholder_operations$fund_code
  )

  reference <- bc_cash_plan_fund(cash_plan)

  expect_identical(reference$fund_key, "long_term")
  expect_identical(reference$fund_code, "13386735")
})


test_that("reconhece o código interno do Reta Alocação", {
  path <- testthat::test_path(
    "..",
    "..",
    "plataforma",
    "fundos - Movimentações Previstas Reta Alocação.xlsx"
  )

  testthat::skip_if_not(file.exists(path))

  cash_plan <- read_fund_cash_plan(path)

  expect_true(
    "54236091" %in%
      cash_plan$shareholder_operations$fund_code
  )

  reference <- bc_cash_plan_fund(cash_plan)

  expect_identical(reference$fund_key, "reta_alocacao")
  expect_identical(reference$fund_code, "13386778")
})
