
make_daycoval_portfolio_fixture <- function() {
  field_names <- c(
    "CodigoCrt", "NomeCrt", "TpCrt", "Administrador",
    "Moeda", "DataEmis", "Titulo", "Qt", "QtBloq",
    "QtDisp", "PuCst", "VlCst", "PuMrc", "VlMrc",
    "MrcBloq", "VlAtvPL", "NCotas", "VlCotas",
    "NCotista", "ValorTotalAtv", "DataVenc",
    "DataAplic", "GrpN1", "GrpN2", "GrpN3"
  )

  field_names <- c(
    field_names,
    paste0(
      "OutroCampo",
      seq_len(67L - length(field_names))
    )
  )

  make_entry <- function(number, title, groups, market_value) {
    values <- stats::setNames(
      rep("", length(field_names)),
      field_names
    )

    values[c(
      "CodigoCrt", "NomeCrt", "TpCrt", "Administrador",
      "Moeda", "DataEmis", "NCotas", "VlCotas",
      "NCotista", "ValorTotalAtv"
    )] <- c(
      "13386778",
      "RETA ALOCACAO FIC FIM CP",
      "Fundo Multimercado",
      ".BANCO DAYCOVAL S/A",
      "Real",
      "19/10/2026",
      "7655579,92014227",
      "1,29509791",
      "1",
      "10164001,22"
    )

    values[c(
      "Titulo", "Qt", "PuMrc", "VlMrc",
      "GrpN1", "GrpN2", "GrpN3"
    )] <- c(
      title,
      "0,0",
      "0,0",
      market_value,
      groups
    )

    paste(
      c(number, "LayCrtDiaDespRef", "V", unname(values)),
      collapse = ";"
    )
  }

  lines <- c(
    make_entry(
      1L,
      "C/C",
      c("10000", "10100", "10108"),
      "1000,00"
    ),
    make_entry(
      2L,
      "PATLIQ",
      c("20000", "26000", "26000"),
      "9914725,55"
    ),
    "3;LayCrtDiaDespRefDet;V;Taxa Gestao;12,34",
    paste(
      "4", "RentabBenkmark", "V", "CDI",
      "0,05", "0,61", "11,18", "14,35",
      "29,76", "4,66",
      sep = ";"
    ),
    "5",
    paste(
      c("6", "LayCrtDiaDespRef", "C", field_names),
      collapse = ";"
    ),
    "7;LayCrtDiaDespRefDet;C;Historico;Valor",
    paste(
      "8", "RentabBenkmark", "C", "Indice",
      "Dia", "Mes", "Ano", "DozeMeses",
      "VinteQtMeses", "Inicio",
      sep = ";"
    )
  )

  path <- tempfile(fileext = ".csv")
  writeLines(lines, path, useBytes = TRUE)

  path
}


test_that("imports a complete Daycoval portfolio", {
  path <- make_daycoval_portfolio_fixture()
  on.exit(unlink(path), add = TRUE)

  portfolio <- read_daycoval_portfolio(path)

  expect_s3_class(portfolio, "daycoval_portfolio")

  expect_named(
    portfolio,
    c("metadata", "entries", "details", "raw")
  )

  expect_s3_class(portfolio$entries, "tbl_df")
  expect_s3_class(portfolio$details, "tbl_df")
  expect_s3_class(portfolio$raw, "tbl_df")

  expect_equal(nrow(portfolio$entries), 2L)
  expect_equal(nrow(portfolio$details), 1L)
  expect_equal(nrow(portfolio$raw), 8L)
})


test_that("extracts the correct portfolio metadata", {
  path <- make_daycoval_portfolio_fixture()
  on.exit(unlink(path), add = TRUE)

  portfolio <- read_daycoval_portfolio(path)

  expect_identical(
    portfolio$metadata$codigo_carteira,
    "13386778"
  )

  expect_identical(
    portfolio$metadata$data_posicao,
    as.Date("2026-10-19")
  )

  expect_equal(
    portfolio$metadata$patrimonio_liquido,
    9914725.55
  )

  expect_equal(
    portfolio$metadata$total_ativos,
    10164001.22
  )
})


test_that("preserves typed entries and accounting groups", {
  path <- make_daycoval_portfolio_fixture()
  on.exit(unlink(path), add = TRUE)

  portfolio <- read_daycoval_portfolio(path)

  patliq <- portfolio$entries |>
    dplyr::filter(.data$titulo == "PATLIQ")

  expect_equal(nrow(patliq), 1L)
  expect_equal(patliq$vl_mrc, 9914725.55)

  expect_identical(patliq$grp_n1, "20000")
  expect_identical(patliq$grp_n2, "26000")
  expect_identical(patliq$grp_n3, "26000")
})


test_that("preserves auxiliary and benchmark records", {
  path <- make_daycoval_portfolio_fixture()
  on.exit(unlink(path), add = TRUE)

  portfolio <- read_daycoval_portfolio(path)

  expect_identical(
    portfolio$details$historico,
    "Taxa Gestao"
  )

  expect_identical(
    portfolio$details$valor,
    "12,34"
  )

  benchmark_records <- portfolio$raw |>
    dplyr::filter(
      .data$record_type == "RentabBenkmark"
    )

  expect_equal(nrow(benchmark_records), 2L)
})


test_that("preserves source identifiers", {
  path <- make_daycoval_portfolio_fixture()
  on.exit(unlink(path), add = TRUE)

  portfolio <- read_daycoval_portfolio(
    path,
    source_name = "carteira_original.csv"
  )

  expect_identical(
    portfolio$metadata$arquivo_origem,
    "carteira_original.csv"
  )

  expect_identical(
    portfolio$entries$source_line,
    c(1L, 2L)
  )

  expect_identical(
    portfolio$metadata$registro_patliq,
    "2"
  )
})


test_that("rejects files without required portfolio layouts", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines("1;OutroLayout;V;ABC", path)

  expect_error(
    read_daycoval_portfolio(path),
    "layout"
  )
})
