make_daycoval_position_accounting_fixture <- function() {
  cash_fields <- c(
    "CdCrt",
    "GrpCntbl",
    "NomeGrpCntbl",
    "CdTit",
    "MercBlqFinAtu",
    "VlMrc",
    "VlPatLiq",
    "bComCCDI",
    "ContaCorr"
  )

  accounting_fields <- c(
    "CdCrt",
    "Grupo",
    "NomeGrp",
    "CdTit",
    "Operacao",
    "DtVenc",
    "CdTitOrigem",
    "Historico",
    "Valor",
    "SldTit",
    "TotGrp",
    "TotGeral",
    "VlPatLiq",
    "bComCCDI",
    "bEstqAnt"
  )

  make_values <- function(field_names, values) {
    result <- stats::setNames(
      rep("", length(field_names)),
      field_names
    )

    result[names(values)] <- values

    unname(result)
  }

  make_record <- function(number, record_type, flag, values) {
    paste(
      c(number, record_type, flag, values),
      collapse = ";"
    )
  }

  cash_1 <- make_values(
    cash_fields,
    c(
      CdCrt = "13711539",
      GrpCntbl = "10108",
      NomeGrpCntbl = "SALDO DE CAIXA",
      CdTit = "C/C",
      VlMrc = "1000,0",
      VlPatLiq = "30740004,7"
    )
  )

  cash_2 <- make_values(
    cash_fields,
    c(
      CdCrt = "13711539",
      GrpCntbl = "10108",
      NomeGrpCntbl = "SALDO DE CAIXA",
      CdTit = "CASH",
      MercBlqFinAtu = "0,0",
      VlMrc = "0,0",
      VlPatLiq = "0,0"
    )
  )

  receivable_1 <- make_values(
    accounting_fields,
    c(
      CdCrt = "13711539",
      Grupo = "10401",
      NomeGrp = "VALORES RECEBER - FUNDOS",
      CdTit = "RSG 29250121",
      Valor = "0,0",
      SldTit = "0,0",
      TotGrp = "100000,0",
      TotGeral = "104100,9",
      VlPatLiq = "30740004,7"
    )
  )

  receivable_2 <- make_values(
    accounting_fields,
    c(
      CdCrt = "13711539",
      Grupo = "10401",
      NomeGrp = "VALORES RECEBER - FUNDOS",
      CdTit = "RSG 47936719",
      Valor = "100000,0",
      SldTit = "100000,0",
      TotGrp = "100000,0",
      TotGeral = "104100,9",
      VlPatLiq = "30740004,7"
    )
  )

  receivable_3 <- make_values(
    accounting_fields,
    c(
      CdCrt = "13711539",
      Grupo = "19701",
      NomeGrp = "TAXA CVM DIFERIDO",
      CdTit = "CVM 122026",
      DtVenc = "31/12/2026",
      Valor = "4100,9",
      SldTit = "4100,9",
      TotGrp = "4100,9",
      TotGeral = "104100,9",
      VlPatLiq = "30740004,7"
    )
  )

  payable_1 <- make_values(
    accounting_fields,
    c(
      CdCrt = "13711539",
      Grupo = "21010",
      NomeGrp = "COTAS A RESGATAR",
      CdTit = "RESGA",
      Valor = "-156457,82",
      SldTit = "-156457,82",
      TotGrp = "-156457,82",
      TotGeral = "-156457,69",
      VlPatLiq = "30740004,7"
    )
  )

  payable_2 <- make_values(
    accounting_fields,
    c(
      CdCrt = "13711539",
      Grupo = "24005",
      NomeGrp = "CP AJUSTE COTA DEVEDOR",
      CdTit = "@AJCOD",
      DtVenc = "30/10/2026",
      Valor = "0,13",
      SldTit = "0,13",
      TotGrp = "0,13",
      TotGeral = "-156457,69",
      VlPatLiq = "30740004,7"
    )
  )

  lines <- c(
    make_record(1L, "P1030Caixa", "V", cash_1),
    make_record(2L, "P1030Caixa", "V", cash_2),
    make_record(3L, "P1030AReceber", "V", receivable_1),
    make_record(4L, "P1030AReceber", "V", receivable_2),
    make_record(5L, "P1030AReceber", "V", receivable_3),
    make_record(6L, "P1030APagar", "V", payable_1),
    make_record(7L, "P1030APagar", "V", payable_2),
    "8",
    make_record(9L, "P1030Caixa", "C", cash_fields),
    make_record(10L, "P1030AReceber", "C", accounting_fields),
    make_record(11L, "P1030APagar", "C", accounting_fields)
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


parse_daycoval_accounting_fixture <- function(
    fixture,
    record_type
) {
  raw_data <- parse_daycoval_position_layout(
    records = fixture$records,
    layouts = fixture$layouts,
    record_type = record_type
  )

  parse_daycoval_position_types(
    raw_data,
    record_type
  )
}


test_that("types cash balances without interpreting their meaning", {
  fixture <- make_daycoval_position_accounting_fixture()

  cash <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030Caixa"
  )

  expect_s3_class(cash, "tbl_df")
  expect_equal(nrow(cash), 2L)

  expect_identical(
    cash$grp_cntbl,
    c("10108", "10108")
  )

  expect_identical(
    cash$cd_tit,
    c("C/C", "CASH")
  )

  expect_type(cash$merc_blq_fin_atu, "double")
  expect_type(cash$vl_mrc, "double")
  expect_type(cash$vl_pat_liq, "double")

  expect_true(is.na(cash$merc_blq_fin_atu[[1L]]))
  expect_equal(cash$merc_blq_fin_atu[[2L]], 0)

  expect_equal(cash$vl_mrc, c(1000, 0))
  expect_equal(cash$vl_pat_liq, c(30740004.7, 0))

  expect_identical(
    cash$record_number,
    c("1", "2")
  )
})


test_that("types receivables and preserves repeated totals", {
  fixture <- make_daycoval_position_accounting_fixture()

  receivables <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030AReceber"
  )

  expect_equal(nrow(receivables), 3L)

  numeric_columns <- c(
    "valor",
    "sld_tit",
    "tot_grp",
    "tot_geral",
    "vl_pat_liq"
  )

  expect_true(all(vapply(
    receivables[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_identical(
    receivables$grupo,
    c("10401", "10401", "19701")
  )

  expect_equal(
    receivables$valor,
    c(0, 100000, 4100.9)
  )

  expect_equal(
    receivables$tot_grp,
    c(100000, 100000, 4100.9)
  )

  expect_equal(
    receivables$tot_geral,
    rep(104100.9, 3L)
  )

  expect_equal(
    sum(receivables$valor),
    104100.9
  )

  expect_false(
    isTRUE(all.equal(
      sum(receivables$tot_geral),
      104100.9
    ))
  )
})


test_that("preserves missing and populated due dates", {
  fixture <- make_daycoval_position_accounting_fixture()

  receivables <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030AReceber"
  )

  expect_s3_class(receivables$dt_venc, "Date")

  expect_true(is.na(receivables$dt_venc[[1L]]))
  expect_true(is.na(receivables$dt_venc[[2L]]))

  expect_identical(
    receivables$dt_venc[[3L]],
    as.Date("2026-12-31")
  )
})


test_that("preserves positive and negative payable entries", {
  fixture <- make_daycoval_position_accounting_fixture()

  payables <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030APagar"
  )

  expect_equal(nrow(payables), 2L)

  expect_equal(
    payables$valor,
    c(-156457.82, 0.13)
  )

  expect_equal(
    payables$sld_tit,
    c(-156457.82, 0.13)
  )

  expect_equal(
    payables$tot_geral,
    rep(-156457.69, 2L)
  )

  expect_identical(
    payables$cd_tit,
    c("RESGA", "@AJCOD")
  )

  expect_s3_class(payables$dt_venc, "Date")

  expect_true(is.na(payables$dt_venc[[1L]]))

  expect_identical(
    payables$dt_venc[[2L]],
    as.Date("2026-10-30")
  )
})


test_that("retains accounting source information", {
  fixture <- make_daycoval_position_accounting_fixture()

  receivables <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030AReceber"
  )

  payables <- parse_daycoval_accounting_fixture(
    fixture,
    "P1030APagar"
  )

  expect_identical(
    receivables$source_line,
    c(3L, 4L, 5L)
  )

  expect_identical(
    payables$source_line,
    c(6L, 7L)
  )

  expect_identical(
    payables$record_number,
    c("6", "7")
  )

  expect_true(all(
    receivables$source_line %in%
      fixture$records$source_line
  ))
})


test_that("rejects invalid cash market values", {
  fixture <- make_daycoval_position_accounting_fixture()

  cash <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030Caixa"
  )

  cash$vl_mrc[[1L]] <- "INVALIDO"

  expect_error(
    parse_daycoval_position_types(
      cash,
      "P1030Caixa"
    ),
    "INVALIDO"
  )
})


test_that("rejects invalid payable amounts", {
  fixture <- make_daycoval_position_accounting_fixture()

  payables <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030APagar"
  )

  payables$valor[[1L]] <- "INVALIDO"

  expect_error(
    parse_daycoval_position_types(
      payables,
      "P1030APagar"
    ),
    "INVALIDO"
  )
})


test_that("rejects invalid accounting due dates", {
  fixture <- make_daycoval_position_accounting_fixture()

  receivables <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030AReceber"
  )

  receivables$dt_venc[[3L]] <- "31/02/2026"

  expect_error(
    parse_daycoval_position_types(
      receivables,
      "P1030AReceber"
    ),
    "Data inválida"
  )
})


test_that("rejects missing accounting fields", {
  fixture <- make_daycoval_position_accounting_fixture()

  receivables <- parse_daycoval_position_layout(
    fixture$records,
    fixture$layouts,
    "P1030AReceber"
  )

  receivables$tot_grp <- NULL

  expect_error(
    parse_daycoval_position_types(
      receivables,
      "P1030AReceber"
    ),
    "Colunas obrigatórias ausentes"
  )
})


test_that("supports empty accounting layouts", {
  fixture <- make_daycoval_position_accounting_fixture()

  records <- fixture$records |>
    dplyr::filter(
      !(
        .data$record_type == "P1030APagar" &
          .data$record_flag == "V"
      )
    )

  layouts <- parse_daycoval_position_layouts(records)

  payables <- parse_daycoval_position_layout(
    records,
    layouts,
    "P1030APagar"
  )

  payables <- parse_daycoval_position_types(
    payables,
    "P1030APagar"
  )

  expect_equal(nrow(payables), 0L)

  expect_type(payables$valor, "double")
  expect_type(payables$tot_geral, "double")
  expect_s3_class(payables$dt_venc, "Date")

  expect_type(payables$grupo, "character")
})
