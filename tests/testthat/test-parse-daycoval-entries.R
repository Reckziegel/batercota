
make_daycoval_entry_fixture <- function() {
  numeric_columns <- c(
    "qt",
    "qt_bloq",
    "qt_disp",
    "pu_cst",
    "vl_cst",
    "pu_mrc",
    "vl_mrc",
    "mrc_bloq",
    "vl_atv_pl",
    "n_cotas",
    "vl_cotas",
    "valor_total_atv"
  )

  entry <- tibble::as_tibble(
    stats::setNames(
      rep(list("0,0"), length(numeric_columns)),
      numeric_columns
    )
  )

  entry$source_line <- 7L
  entry$codigo_crt <- "13386778"
  entry$titulo <- "LONG.TERM"

  entry$grp_n1 <- "10000"
  entry$grp_n2 <- "16000"
  entry$grp_n3 <- "16007"

  entry$data_emis <- "19/10/2026"
  entry$data_venc <- ""
  entry$data_aplic <- "27/07/2026"

  entry$qt <- "924042,822322"
  entry$pu_mrc <- "1,3207144"
  entry$vl_mrc <- "1220080,00"

  entry
}


test_that("converts financial values and dates", {
  entry <- make_daycoval_entry_fixture()

  result <- parse_daycoval_entries(entry)

  expect_s3_class(result, "tbl_df")

  expect_equal(
    result$qt,
    924042.822322
  )

  expect_equal(
    result$pu_mrc,
    1.3207144
  )

  expect_equal(
    result$vl_mrc,
    1220080
  )

  expect_identical(
    result$data_emis,
    as.Date("2026-10-19")
  )

  expect_identical(
    result$data_aplic,
    as.Date("2026-07-27")
  )

  expect_true(is.na(result$data_venc))
})


test_that("preserves original identifiers", {
  entry <- make_daycoval_entry_fixture()

  result <- parse_daycoval_entries(entry)

  expect_identical(
    result$codigo_crt,
    "13386778"
  )

  expect_identical(
    result$titulo,
    "LONG.TERM"
  )

  expect_identical(
    result$grp_n1,
    "10000"
  )

  expect_identical(
    result$grp_n2,
    "16000"
  )

  expect_identical(
    result$grp_n3,
    "16007"
  )

  expect_identical(
    result$source_line,
    7L
  )
})


test_that("preserves negative financial values", {
  entry <- make_daycoval_entry_fixture()

  entry$vl_mrc <- "-801,40"
  entry$pu_mrc <- "-40,07"

  result <- parse_daycoval_entries(entry)

  expect_equal(result$vl_mrc, -801.40)
  expect_equal(result$pu_mrc, -40.07)
})


test_that("handles missing financial values", {
  entry <- make_daycoval_entry_fixture()

  entry$qt <- ""
  entry$pu_mrc <- NA_character_

  result <- parse_daycoval_entries(entry)

  expect_true(is.na(result$qt))
  expect_true(is.na(result$pu_mrc))
})


test_that("rejects invalid financial values", {
  entry <- make_daycoval_entry_fixture()

  entry$vl_mrc <- "valor inválido"

  expect_error(
    parse_daycoval_entries(entry),
    "Valor numérico inválido"
  )
})


test_that("rejects invalid dates", {
  entry <- make_daycoval_entry_fixture()

  entry$data_emis <- "31/02/2026"

  expect_error(
    parse_daycoval_entries(entry),
    "Data inválida"
  )
})


test_that("rejects missing required columns", {
  entry <- make_daycoval_entry_fixture()

  entry$grp_n3 <- NULL

  expect_error(
    parse_daycoval_entries(entry),
    "Colunas obrigatórias ausentes"
  )
})


test_that("does not modify accounting signs", {
  entry <- make_daycoval_entry_fixture()

  entry$grp_n1 <- "20000"
  entry$grp_n2 <- "23700"
  entry$grp_n3 <- "23711"
  entry$vl_mrc <- "3115,50"

  result <- parse_daycoval_entries(entry)

  expect_equal(result$vl_mrc, 3115.50)
  expect_identical(result$grp_n1, "20000")
})
