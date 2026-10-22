make_daycoval_position_typing_fixture <- function() {
  metadata_fields <- c(
    "CdCrt", "CdCrtProd", "NomeCurto", "NomeLongo",
    "CNPJ", "TipoCarteira", "Administrador", "Gestor",
    "SELIC", "CETIP", "ANBID", "INEWS", "CBLC",
    "BMF", "Data", "NuPoscD0", "bComCCDI",
    "ISIN", "TipoPes", "CoGestor"
  )

  fund_fields <- c(
    "CdCrt", "TipoRenda", "Titulo", "DecQtd", "DecPU",
    "Administrador", "CNPJ", "Qtd", "QtdBlq", "VlMrc",
    "VlTaxas", "VlPatLiq", "DtApl", "bEstqAnt",
    "PU", "ISIN", "VlAtuBruto"
  )

  metadata <- stats::setNames(
    rep("", length(metadata_fields)),
    metadata_fields
  )

  metadata[c(
    "CdCrt", "NomeCurto", "CNPJ",
    "Data", "NuPoscD0", "TipoPes"
  )] <- c(
    "13711539",
    "LINEAR FIM CP",
    "60.629.171/0001-06",
    "19/10/2026",
    "-1",
    "S"
  )

  fund_1 <- stats::setNames(
    rep("", length(fund_fields)),
    fund_fields
  )

  fund_1[c(
    "CdCrt", "TipoRenda", "Titulo",
    "DecQtd", "DecPU", "CNPJ",
    "Qtd", "QtdBlq", "VlMrc",
    "VlTaxas", "VlPatLiq", "DtApl",
    "PU", "ISIN", "VlAtuBruto"
  )] <- c(
    "13711539",
    "500",
    "CF 13823084",
    "8",
    "9",
    "FZW781751643726",
    "88481,303388",
    "0,0",
    "3785899,96",
    "0,0",
    "30740004,70",
    "15/06/2026",
    "42,78757",
    "BRATV7CTF001",
    "3785899,96"
  )

  fund_2 <- fund_1

  fund_2[c(
    "TipoRenda", "Titulo", "DecQtd",
    "CNPJ", "Qtd", "VlMrc",
    "VlTaxas", "DtApl", "PU", "VlAtuBruto"
  )] <- c(
    "600",
    "FUNDO TESTE",
    "6",
    "11.392.165/0001-72",
    "1000,5",
    "19345,65",
    "",
    "",
    "19,335982",
    "19345,65"
  )

  make_record <- function(number, type, flag, values) {
    paste(
      c(number, type, flag, unname(values)),
      collapse = ";"
    )
  }

  lines <- c(
    make_record(1L, "P1030", "V", metadata),
    make_record(2L, "P1030CF", "V", fund_1),
    make_record(3L, "P1030CF", "V", fund_2),
    "4",
    make_record(5L, "P1030", "C", metadata_fields),
    make_record(6L, "P1030CF", "C", fund_fields)
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines(lines, path, useBytes = TRUE)

  records <- read_daycoval_records(path)
  layouts <- parse_daycoval_position_layouts(records)

  list(
    records = records,
    layouts = layouts
  )
}


test_that("types P1030 reference date", {
  fixture <- make_daycoval_position_typing_fixture()

  metadata <- parse_daycoval_position_layout(
    records = fixture$records,
    layouts = fixture$layouts,
    record_type = "P1030"
  )

  result <- parse_daycoval_position_types(
    metadata,
    record_type = "P1030"
  )

  expect_s3_class(result$data, "Date")

  expect_identical(
    result$data,
    as.Date("2026-10-19")
  )

  expect_identical(result$cd_crt, "13711539")
  expect_identical(result$nu_posc_d0, "-1")
  expect_identical(result$tipo_pes, "S")
})


test_that("types P1030CF financial fields", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    records = fixture$records,
    layouts = fixture$layouts,
    record_type = "P1030CF"
  )

  result <- parse_daycoval_position_types(
    funds,
    record_type = "P1030CF"
  )

  numeric_columns <- c(
    "qtd",
    "qtd_blq",
    "vl_mrc",
    "vl_taxas",
    "vl_pat_liq",
    "pu",
    "vl_atu_bruto"
  )

  expect_true(all(vapply(
    result[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_identical(
    result$dec_qtd,
    c(8L, 6L)
  )

  expect_identical(
    result$dec_pu,
    c(9L, 9L)
  )

  expect_equal(
    result$qtd,
    c(88481.303388, 1000.5)
  )

  expect_equal(
    result$vl_mrc,
    c(3785899.96, 19345.65)
  )

  expect_s3_class(result$dt_apl, "Date")

  expect_identical(
    result$dt_apl[[1L]],
    as.Date("2026-06-15")
  )

  expect_true(is.na(result$dt_apl[[2L]]))
  expect_true(is.na(result$vl_taxas[[2L]]))
})


test_that("preserves fund identifiers and classifications", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030CF"
  )

  result <- parse_daycoval_position_types(
    funds,
    "P1030CF"
  )

  expect_identical(
    result$tipo_renda,
    c("500", "600")
  )

  expect_identical(
    result$cnpj[[1L]],
    "FZW781751643726"
  )

  expect_identical(
    result$b_estq_ant,
    c("", "")
  )

  expect_identical(
    result$record_number,
    c("2", "3")
  )

  expect_identical(
    result$source_line,
    c(2L, 3L)
  )
})


test_that("rejects invalid financial values", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030CF"
  )

  funds$vl_mrc[[1L]] <- "ABC"

  expect_error(
    parse_daycoval_position_types(
      funds,
      "P1030CF"
    ),
    "numérico inválido"
  )
})


test_that("rejects invalid application dates", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030CF"
  )

  funds$dt_apl[[1L]] <- "31/02/2026"

  expect_error(
    parse_daycoval_position_types(
      funds,
      "P1030CF"
    ),
    "Data inválida"
  )
})


test_that("rejects missing typed columns", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030CF"
  )

  funds$qtd <- NULL

  expect_error(
    parse_daycoval_position_types(
      funds,
      "P1030CF"
    ),
    "Colunas obrigatórias ausentes"
  )
})


test_that("does not guess types for unsupported layouts", {
  fixture <- make_daycoval_position_typing_fixture()

  funds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030CF"
  )

  expect_error(
    parse_daycoval_position_types(
      funds,
      "P1030RV"
    ),
    "não está definida"
  )
})
