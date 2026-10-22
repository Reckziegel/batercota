make_daycoval_position_records <- function() {
  lines <- c(
    "1;P1030;V;13711539;19/10/2026",
    "2;P1030CF;V;13711539;CF 13823084;3785899,96",
    "3",
    "4;P1030;C;CdCrt;Data",
    "5;P1030CF;C;CdCrt;Titulo;VlMrc",
    "6;P1030Swap;C;CdCrt;CdTit;VlPatLiq"
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines(lines, path, useBytes = TRUE)

  read_daycoval_records(path)
}


test_that("discovers declared position layouts", {
  records <- make_daycoval_position_records()

  layouts <- parse_daycoval_position_layouts(records)

  expect_s3_class(layouts, "tbl_df")
  expect_equal(nrow(layouts), 3L)

  expect_identical(
    layouts$record_type,
    c("P1030", "P1030CF", "P1030Swap")
  )

  expect_identical(
    layouts$n_columns,
    c(2L, 3L, 3L)
  )

  expect_identical(
    layouts$n_records,
    c(1L, 1L, 0L)
  )
})


test_that("preserves original and normalized field names", {
  records <- make_daycoval_position_records()

  layouts <- parse_daycoval_position_layouts(records)

  fund_index <- match("P1030CF", layouts$record_type)

  expect_identical(
    layouts$original_names[[fund_index]],
    c("CdCrt", "Titulo", "VlMrc")
  )

  expect_identical(
    layouts$field_names[[fund_index]],
    c("cd_crt", "titulo", "vl_mrc")
  )
})


test_that("parses position values without type conversion", {
  records <- make_daycoval_position_records()

  layouts <- parse_daycoval_position_layouts(records)

  funds <- parse_daycoval_position_layout(
    records = records,
    layouts = layouts,
    record_type = "P1030CF"
  )

  expect_s3_class(funds, "tbl_df")
  expect_equal(nrow(funds), 1L)
  expect_equal(ncol(funds), 6L)

  expect_identical(funds$cd_crt, "13711539")
  expect_identical(funds$titulo, "CF 13823084")
  expect_identical(funds$vl_mrc, "3785899,96")

  expect_identical(funds$source_line, 2L)
  expect_identical(funds$record_number, "2")
})


test_that("retains declared layouts without observations", {
  records <- make_daycoval_position_records()

  layouts <- parse_daycoval_position_layouts(records)

  swaps <- parse_daycoval_position_layout(
    records = records,
    layouts = layouts,
    record_type = "P1030Swap"
  )

  expect_s3_class(swaps, "tbl_df")
  expect_equal(nrow(swaps), 0L)

  expect_named(
    swaps,
    c(
      "source_file",
      "source_line",
      "record_number",
      "cd_crt",
      "cd_tit",
      "vl_pat_liq"
    )
  )

  expect_type(swaps$cd_crt, "character")
})


test_that("preserves separator records in raw data", {
  records <- make_daycoval_position_records()

  expect_equal(nrow(records), 6L)
  expect_identical(records$raw_line[[3L]], "3")
  expect_true(is.na(records$record_type[[3L]]))

  expect_no_error(
    parse_daycoval_position_layouts(records)
  )
})


test_that("rejects position records without a header", {
  records <- make_daycoval_position_records()

  records <- records[-5L, ]

  expect_error(
    parse_daycoval_position_layouts(records),
    "sem cabeçalho"
  )
})


test_that("rejects duplicated layout headers", {
  records <- make_daycoval_position_records()

  records <- dplyr::bind_rows(
    records,
    records[5L, ]
  )

  expect_error(
    parse_daycoval_position_layouts(records),
    "exatamente um cabeçalho"
  )
})


test_that("rejects inconsistent record widths", {
  records <- make_daycoval_position_records()

  records$fields[[2L]] <- c(
    records$fields[[2L]],
    "EXTRA"
  )

  records$n_fields[[2L]] <- length(records$fields[[2L]])

  expect_error(
    parse_daycoval_position_layouts(records),
    "largura inválida"
  )
})


test_that("rejects duplicated normalized field names", {
  records <- make_daycoval_position_records()

  records$fields[[5L]] <- c(
    "5",
    "P1030CF",
    "C",
    "CdCrt",
    "VlMrc",
    "vl_mrc"
  )

  records$n_fields[[5L]] <- length(records$fields[[5L]])

  expect_error(
    parse_daycoval_position_layouts(records),
    "campos duplicados"
  )
})


test_that("rejects unknown layouts when parsing values", {
  records <- make_daycoval_position_records()

  layouts <- parse_daycoval_position_layouts(records)

  expect_error(
    parse_daycoval_position_layout(
      records = records,
      layouts = layouts,
      record_type = "P1030Unknown"
    ),
    "não foi encontrado"
  )
})
