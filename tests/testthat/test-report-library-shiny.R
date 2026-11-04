
source(
  testthat::test_path(
    "..",
    "..",
    "plataforma",
    "report_library_shiny.R"
  ),
  local = TRUE
)


bc_shiny_test_library <- function() {
  path <- tempfile(pattern = "batercota-shiny-library-")

  list(
    path = path,
    fixture = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "linear_2026-10-16.csv"
    )
  )
}


test_that("resolves the project library independently of working directory", {
  root <- bc_library_project_root()

  expect_true(file.exists(
    file.path(root, "DESCRIPTION")
  ))

  expected <- file.path(
    root,
    "data",
    "report_library"
  )

  previous <- getOption("batercota.report_library")

  on.exit(
    options(batercota.report_library = previous),
    add = TRUE
  )

  options(batercota.report_library = NULL)

  expect_identical(
    bc_library_shiny_path(),
    normalizePath(
      expected,
      winslash = "/",
      mustWork = FALSE
    )
  )
})


test_that("provides library choices filtered by fund and type", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  previous <- getOption("batercota.report_library")

  on.exit(
    options(batercota.report_library = previous),
    add = TRUE
  )

  options(batercota.report_library = setup$path)

  saved <- batercota::save_report(
    setup$fixture,
    setup$path
  )

  choices <- bc_library_choices(
    fund = "linear",
    report_type = "daycoval_position"
  )

  expect_length(choices, 1L)

  expect_identical(
    unname(choices),
    saved$sha256
  )

  expect_match(
    names(choices)[[1L]],
    "16/10/2026"
  )

  expect_match(
    names(choices)[[1L]],
    substr(saved$sha256[[1L]], 1L, 10L),
    fixed = TRUE
  )

  other_fund <- bc_library_choices(
    fund = "long_term",
    report_type = "daycoval_position"
  )

  expect_false(
    saved$sha256[[1L]] %in% unname(other_fund)
  )
})


test_that("restores stored reports with original source names", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  saved <- batercota::save_report(
    setup$fixture,
    setup$path
  )

  stored_path <- batercota::load_report(
    hash = saved$sha256[[1L]],
    library_path = setup$path,
    parsed = FALSE
  )

  restored <- bc_library_read(
    path = stored_path,
    report_type = "daycoval_position",
    library_path = setup$path
  )

  expect_s3_class(restored, "daycoval_position")

  expect_identical(
    restored$metadata$arquivo_origem,
    basename(setup$fixture)
  )

  expect_false(identical(
    basename(stored_path),
    basename(setup$fixture)
  ))
})


test_that("rejects a stored report with incompatible type", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  saved <- batercota::save_report(
    setup$fixture,
    setup$path
  )

  stored_path <- batercota::load_report(
    hash = saved$sha256[[1L]],
    library_path = setup$path,
    parsed = FALSE
  )

  expect_error(
    bc_library_read(
      path = stored_path,
      report_type = "daycoval_portfolio",
      library_path = setup$path
    ),
    "tipo incompatível"
  )
})


test_that("reads new files using their original importers", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  result <- bc_library_read(
    path = setup$fixture,
    report_type = "daycoval_position",
    library_path = setup$path
  )

  original <- batercota::read_daycoval_position(
    setup$fixture
  )

  expect_s3_class(result, "daycoval_position")
  expect_equal(result$metadata, original$metadata)
})


test_that("stores new imports and recognizes duplicates", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  first <- bc_library_save_paths(
    paths = setup$fixture,
    library_path = setup$path
  )

  second <- bc_library_save_paths(
    paths = setup$fixture,
    library_path = setup$path
  )

  expect_identical(first$status, "stored")
  expect_identical(second$status, "duplicate")

  expect_identical(
    first$sha256,
    second$sha256
  )

  expect_equal(
    nrow(batercota::list_reports(setup$path)),
    1L
  )
})


test_that("reports storage failures without discarding valid files", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  invalid <- tempfile(fileext = ".csv")

  result <- bc_library_save_paths(
    paths = c(setup$fixture, invalid),
    library_path = setup$path
  )

  expect_equal(nrow(result), 2L)

  expect_setequal(
    result$status,
    c("stored", "error")
  )

  expect_equal(
    nrow(batercota::list_reports(setup$path)),
    1L
  )

  expect_true(any(
    !is.na(result$error)
  ))
})


test_that("picker UI respects module namespaces", {
  ui <- bc_library_picker_ui(
    path_id = "opening_position_path",
    ns = shiny::NS("fluxos")
  )

  html <- paste(as.character(ui), collapse = "")

  expect_true(grepl(
    "fluxos-opening_position_path_library_hash",
    html,
    fixed = TRUE
  ))

  expect_true(grepl(
    "fluxos-opening_position_path_library_use",
    html,
    fixed = TRUE
  ))
})


test_that("recovers catalog choices after rebuilding the index", {
  setup <- bc_shiny_test_library()

  on.exit(
    unlink(setup$path, recursive = TRUE),
    add = TRUE
  )

  previous <- getOption("batercota.report_library")

  on.exit(
    options(batercota.report_library = previous),
    add = TRUE
  )

  options(batercota.report_library = setup$path)

  saved <- batercota::save_report(
    setup$fixture,
    setup$path
  )

  unlink(file.path(setup$path, "index.rds"))

  choices <- bc_library_choices(
    fund = "linear",
    report_type = "daycoval_position"
  )

  expect_identical(
    unname(choices),
    saved$sha256
  )
})
