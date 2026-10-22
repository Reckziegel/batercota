make_daycoval_position_instruments_fixture <- function() {
  rf_fields <- c(
    "CdCrt", "Mercado", "Papel", "CdTit", "Emissor",
    "CodCustodia", "RotCalc", "TpCalcPcp", "Digitacao",
    "bModifMult", "FatorAdic", "Codigo", "IndX",
    "TxEmis", "DtEmis", "DtVenc", "DtBase",
    "DtCompra", "PUCompra", "PUBruto", "Qtd",
    "QtdBlq", "TpAvalMrc", "VlCst", "VlMrc",
    "VlPatLiq", "DecTaxa", "DecQtd", "DecPu",
    "VlIRIOF", "bComCCDI", "DtIniUsoCCDI",
    "bEstAntigo", "ISIN"
  )

  rv_fields <- c(
    "CdCrt", "CCDI", "GrpCntbl", "NomeGrpCntbl",
    "NomeTit", "Espec", "CdTit", "DecPu",
    "DecQtd", "Qtd", "QtdBlq", "VlMrc",
    "VlPatLiq", "Data", "PUCompra",
    "PUAtualBruto", "ISIN"
  )

  future_fields <- c(
    "CdCrt", "MercadoBMFID", "Mercadoria",
    "Serie", "DataVenc", "Qtd", "VlNoc",
    "Ajuste", "VlPatLiq", "Ponto",
    "bComCCDI", "ISIN"
  )

  make_values <- function(fields, values) {
    result <- stats::setNames(
      rep("", length(fields)),
      fields
    )

    result[names(values)] <- values

    unname(result)
  }

  make_record <- function(number, type, flag, values) {
    paste(
      c(number, type, flag, values),
      collapse = ";"
    )
  }

  rf_1 <- make_values(
    rf_fields,
    c(
      CdCrt = "13386735",
      Mercado = "SELIC",
      Papel = "LETRA DO TESOURO NACIONAL",
      CdTit = "LTN00003E8D3",
      Emissor = "TESOURO",
      CodCustodia = "100000",
      RotCalc = "31",
      TpCalcPcp = "0",
      bModifMult = "0",
      FatorAdic = "0,0",
      TxEmis = "14,443001",
      DtEmis = "11/02/2022",
      DtVenc = "01/01/2029",
      DtBase = "11/02/2022",
      DtCompra = "16/06/2026",
      PUCompra = "711,0488262910798",
      PUBruto = "768,533401",
      Qtd = "213,0",
      TpAvalMrc = "1",
      VlCst = "158674,18",
      VlMrc = "163697,61",
      VlPatLiq = "2418254,8",
      DecTaxa = "6",
      DecQtd = "4",
      DecPu = "8",
      VlIRIOF = "0,0",
      DtIniUsoCCDI = "01/10/2004",
      ISIN = "BRSTNCLTN806"
    )
  )

  rf_2 <- make_values(
    rf_fields,
    c(
      CdCrt = "13386735",
      Mercado = "SELIC",
      Papel = "NOTA DO TESOURO NACIONAL - B",
      CdTit = "NTN-B002C3BC",
      Emissor = "TESOURO",
      CodCustodia = "760199",
      RotCalc = "32",
      TpCalcPcp = "5",
      Digitacao = "0",
      bModifMult = "0",
      FatorAdic = "0,0",
      Codigo = "ICA",
      IndX = "IPCA-IND",
      TxEmis = "6,0",
      DtEmis = "12/01/2022",
      DtVenc = "15/08/2060",
      DtBase = "15/07/2000",
      DtCompra = "16/06/2026",
      PUCompra = "4068,524166666667",
      PUBruto = "4354,865014",
      Qtd = "60,0",
      TpAvalMrc = "1",
      VlCst = "243445,96",
      VlMrc = "261291,9",
      VlPatLiq = "2418254,8",
      DecTaxa = "4",
      DecQtd = "0",
      DecPu = "8",
      VlIRIOF = "0,0",
      DtIniUsoCCDI = "01/10/2004",
      ISIN = "BRSTNCNTB690"
    )
  )

  rv_1 <- make_values(
    rv_fields,
    c(
      CdCrt = "13386735",
      CCDI = "0",
      GrpCntbl = "15815",
      NomeGrpCntbl = "COTAS FI. DE INDICE DE MERCADO",
      NomeTit = "XINA",
      Espec = "CI",
      CdTit = "XINA11",
      DecPu = "2",
      DecQtd = "0",
      Qtd = "5500,0",
      VlMrc = "36685,0",
      VlPatLiq = "2418254,8",
      Data = "19/10/2026",
      PUCompra = "8,86",
      PUAtualBruto = "6,67",
      ISIN = "BRXINACTF006"
    )
  )

  rv_2 <- make_values(
    rv_fields,
    c(
      CdCrt = "13386735",
      CCDI = "0",
      GrpCntbl = "16403",
      NomeGrpCntbl = "ACOES A VISTA - N1",
      NomeTit = "B3SA",
      Espec = "ON",
      CdTit = "B3SA3",
      DecPu = "6",
      DecQtd = "0",
      Qtd = "5500,0",
      VlMrc = "123750,0",
      VlPatLiq = "2418254,8",
      Data = "19/10/2026",
      PUCompra = "11,403712727272726",
      PUAtualBruto = "22,5",
      ISIN = "BRB3SAACNOR6"
    )
  )

  future <- make_values(
    future_fields,
    c(
      CdCrt = "13386735",
      MercadoBMFID = "2",
      Mercadoria = "DI1",
      Serie = "F33",
      DataVenc = "03/01/2033",
      Qtd = "8,0",
      VlNoc = "378406,56",
      Ajuste = "-320,56",
      VlPatLiq = "2418254,8",
      Ponto = "1,0",
      bComCCDI = "0",
      ISIN = "BRBMEFD1I678"
    )
  )

  lines <- c(
    make_record(1L, "P1030RF", "V", rf_1),
    make_record(2L, "P1030RF", "V", rf_2),
    make_record(3L, "P1030RV", "V", rv_1),
    make_record(4L, "P1030RV", "V", rv_2),
    make_record(5L, "P1030Futuros", "V", future),
    "6",
    make_record(7L, "P1030RF", "C", rf_fields),
    make_record(8L, "P1030RV", "C", rv_fields),
    make_record(9L, "P1030Futuros", "C", future_fields)
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


parse_daycoval_instrument_fixture <- function(
    fixture,
    record_type
) {
  data <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    record_type
  )

  parse_daycoval_position_types(data, record_type)
}


test_that("recognizes the three instrument layouts", {
  fixture <- make_daycoval_position_instruments_fixture()

  expect_identical(
    fixture$layouts$record_type,
    c("P1030RF", "P1030RV", "P1030Futuros")
  )

  expect_identical(
    fixture$layouts$n_columns,
    c(34L, 17L, 12L)
  )

  expect_identical(
    fixture$layouts$n_records,
    c(2L, 2L, 1L)
  )
})


test_that("types fixed income positions", {
  fixture <- make_daycoval_position_instruments_fixture()

  bonds <- parse_daycoval_instrument_fixture(
    fixture,
    "P1030RF"
  )

  expect_equal(nrow(bonds), 2L)

  numeric_columns <- c(
    "fator_adic",
    "tx_emis",
    "pu_compra",
    "pu_bruto",
    "qtd",
    "qtd_blq",
    "vl_cst",
    "vl_mrc",
    "vl_pat_liq",
    "vl_iriof"
  )

  expect_true(all(vapply(
    bonds[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_identical(
    bonds$dec_taxa,
    c(6L, 4L)
  )

  expect_identical(
    bonds$dec_qtd,
    c(4L, 0L)
  )

  expect_identical(
    bonds$dec_pu,
    c(8L, 8L)
  )

  expect_equal(
    bonds$qtd,
    c(213, 60)
  )

  expect_equal(
    bonds$vl_mrc,
    c(163697.61, 261291.9)
  )

  expect_true(all(is.na(bonds$qtd_blq)))
})


test_that("preserves fixed income dates and identifiers", {
  fixture <- make_daycoval_position_instruments_fixture()

  bonds <- parse_daycoval_instrument_fixture(
    fixture,
    "P1030RF"
  )

  date_columns <- c(
    "dt_emis",
    "dt_venc",
    "dt_base",
    "dt_compra",
    "dt_ini_uso_ccdi"
  )

  expect_true(all(vapply(
    bonds[date_columns],
    inherits,
    logical(1),
    what = "Date"
  )))

  expect_identical(
    bonds$dt_venc,
    as.Date(c("2029-01-01", "2060-08-15"))
  )

  expect_identical(
    bonds$dt_ini_uso_ccdi,
    as.Date(c("2004-10-01", "2004-10-01"))
  )

  expect_identical(
    bonds$cd_tit,
    c("LTN00003E8D3", "NTN-B002C3BC")
  )

  expect_identical(
    bonds$cod_custodia,
    c("100000", "760199")
  )

  expect_identical(
    bonds$rot_calc,
    c("31", "32")
  )

  expect_identical(
    bonds$b_modif_mult,
    c("0", "0")
  )

  expect_identical(
    bonds$ind_x,
    c("", "IPCA-IND")
  )

  expect_identical(
    bonds$source_line,
    c(1L, 2L)
  )
})


test_that("types equity positions without losing classifications", {
  fixture <- make_daycoval_position_instruments_fixture()

  equities <- parse_daycoval_instrument_fixture(
    fixture,
    "P1030RV"
  )

  expect_equal(nrow(equities), 2L)

  numeric_columns <- c(
    "qtd",
    "qtd_blq",
    "vl_mrc",
    "vl_pat_liq",
    "pu_compra",
    "pu_atual_bruto"
  )

  expect_true(all(vapply(
    equities[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_identical(
    equities$dec_pu,
    c(2L, 6L)
  )

  expect_identical(
    equities$dec_qtd,
    c(0L, 0L)
  )

  expect_equal(
    equities$vl_mrc,
    c(36685, 123750)
  )

  expect_equal(
    equities$pu_atual_bruto,
    c(6.67, 22.5)
  )

  expect_true(all(is.na(equities$qtd_blq)))

  expect_identical(
    equities$grp_cntbl,
    c("15815", "16403")
  )

  expect_identical(
    equities$ccdi,
    c("0", "0")
  )

  expect_identical(
    equities$cd_tit,
    c("XINA11", "B3SA3")
  )

  expect_s3_class(equities$data, "Date")

  expect_identical(
    equities$data,
    as.Date(c("2026-10-19", "2026-10-19"))
  )
})


test_that("types futures while preserving nocional and adjustment", {
  fixture <- make_daycoval_position_instruments_fixture()

  futures <- parse_daycoval_instrument_fixture(
    fixture,
    "P1030Futuros"
  )

  expect_equal(nrow(futures), 1L)

  numeric_columns <- c(
    "qtd",
    "vl_noc",
    "ajuste",
    "vl_pat_liq",
    "ponto"
  )

  expect_true(all(vapply(
    futures[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_equal(futures$qtd[[1L]], 8)
  expect_equal(futures$vl_noc[[1L]], 378406.56)
  expect_equal(futures$ajuste[[1L]], -320.56)
  expect_equal(futures$ponto[[1L]], 1)

  expect_identical(
    futures$data_venc,
    as.Date("2033-01-03")
  )

  expect_identical(futures$mercado_bmfid, "2")
  expect_identical(futures$b_com_ccdi, "0")
  expect_identical(futures$serie, "F33")
  expect_identical(futures$isin, "BRBMEFD1I678")
})


test_that("supports an instrument layout without records", {
  fixture <- make_daycoval_position_instruments_fixture()

  records <- fixture$records |>
    dplyr::filter(
      !(
        .data$record_type == "P1030Futuros" &
          .data$record_flag == "V"
      )
    )

  layouts <- parse_daycoval_position_layouts(records)

  futures <- parse_daycoval_position_layout(
    records,
    layouts,
    "P1030Futuros"
  )

  futures <- parse_daycoval_position_types(
    futures,
    "P1030Futuros"
  )

  expect_equal(nrow(futures), 0L)

  expect_type(futures$qtd, "double")
  expect_type(futures$ajuste, "double")
  expect_type(futures$vl_noc, "double")

  expect_s3_class(futures$data_venc, "Date")

  expect_type(futures$mercado_bmfid, "character")
  expect_type(futures$b_com_ccdi, "character")
})


test_that("rejects invalid fixed income prices", {
  fixture <- make_daycoval_position_instruments_fixture()

  bonds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030RF"
  )

  bonds$pu_compra[[1L]] <- "INVALIDO"

  expect_error(
    parse_daycoval_position_types(
      bonds,
      "P1030RF"
    ),
    "INVALIDO"
  )
})


test_that("rejects invalid fixed income dates", {
  fixture <- make_daycoval_position_instruments_fixture()

  bonds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030RF"
  )

  bonds$dt_venc[[1L]] <- "31/02/2029"

  expect_error(
    parse_daycoval_position_types(
      bonds,
      "P1030RF"
    ),
    "Data inválida"
  )
})


test_that("rejects invalid equity precision fields", {
  fixture <- make_daycoval_position_instruments_fixture()

  equities <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030RV"
  )

  equities$dec_pu[[1L]] <- "2,5"

  expect_error(
    parse_daycoval_position_types(
      equities,
      "P1030RV"
    ),
    "inteiro inválido"
  )
})


test_that("rejects invalid futures adjustments", {
  fixture <- make_daycoval_position_instruments_fixture()

  futures <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030Futuros"
  )

  futures$ajuste[[1L]] <- "INVALIDO"

  expect_error(
    parse_daycoval_position_types(
      futures,
      "P1030Futuros"
    ),
    "INVALIDO"
  )
})


test_that("requires the fixed income valuation fields", {
  fixture <- make_daycoval_position_instruments_fixture()

  bonds <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030RF"
  )

  bonds$vl_mrc <- NULL

  expect_error(
    parse_daycoval_position_types(
      bonds,
      "P1030RF"
    ),
    "Colunas obrigatórias ausentes"
  )
})
