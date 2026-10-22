make_daycoval_position_import_fixture <- function() {
  schemas <- list(
    P1030 = c(
      "CdCrt", "NomeCurto", "NomeLongo",
      "CNPJ", "TipoCarteira", "Administrador",
      "Gestor", "Data"
    ),
    P1030RF = c(
      "CdCrt", "FatorAdic", "TxEmis",
      "PUCompra", "PUBruto", "Qtd", "QtdBlq",
      "VlCst", "VlMrc", "VlPatLiq", "VlIRIOF",
      "DecTaxa", "DecQtd", "DecPu",
      "DtEmis", "DtVenc", "DtBase",
      "DtCompra", "DtIniUsoCCDI"
    ),
    P1030CF = c(
      "CdCrt", "TipoRenda", "Titulo",
      "CNPJ", "DecQtd", "DecPU", "Qtd",
      "QtdBlq", "VlMrc", "VlTaxas",
      "VlPatLiq", "DtApl", "PU", "VlAtuBruto"
    ),
    P1030RV = c(
      "CdCrt", "DecPu", "DecQtd",
      "Qtd", "QtdBlq", "VlMrc",
      "VlPatLiq", "Data",
      "PUCompra", "PUAtualBruto"
    ),
    P1030Futuros = c(
      "CdCrt", "Qtd", "VlNoc", "Ajuste",
      "VlPatLiq", "Ponto", "DataVenc"
    ),
    P1030Caixa = c(
      "CdCrt", "CdTit", "MercBlqFinAtu",
      "VlMrc", "VlPatLiq"
    ),
    P1030AReceber = c(
      "CdCrt", "Grupo", "CdTit",
      "DtVenc", "Valor", "SldTit",
      "TotGrp", "TotGeral", "VlPatLiq"
    ),
    P1030APagar = c(
      "CdCrt", "Grupo", "CdTit",
      "DtVenc", "Valor", "SldTit",
      "TotGrp", "TotGeral", "VlPatLiq"
    ),
    P1030Rentabilidades = c(
      "CdCrt", "Data", "NumCotas",
      "VlCota", "VlPatLiq",
      "DecNumCotas", "DecVlCota",
      "IndiceBM", "VlIndiceBM",
      "VarCota", "VarIndiceBM", "PercBM"
    ),
    P1030MovCotistas = c(
      "CdCrt", "TotApl", "TotRgt",
      "NumDiasAgndApl", "NumDiasAgndRgt"
    ),
    P1030Swap = c(
      "CdCrt", "CdTit", "VlPatLiq"
    )
  )

  values <- list(
    list(
      type = "P1030",
      data = c(
        CdCrt = "13711539",
        NomeCurto = "LINEAR FIM CP",
        NomeLongo = "LINEAR FIM CP RL",
        CNPJ = "60.629.171/0001-06",
        TipoCarteira = "Fundo Multimercado",
        Administrador = "BANCO DAYCOVAL",
        Gestor = "Reta Asset",
        Data = "19/10/2026"
      )
    ),
    list(
      type = "P1030CF",
      data = c(
        CdCrt = "13711539",
        TipoRenda = "500",
        Titulo = "CF 13823084",
        CNPJ = "FZW781751643726",
        DecQtd = "8",
        DecPU = "9",
        Qtd = "88481,303388",
        QtdBlq = "0,0",
        VlMrc = "3785899,96",
        VlTaxas = "0,0",
        VlPatLiq = "30740004,7",
        DtApl = "15/06/2026",
        PU = "42,78757",
        VlAtuBruto = "3785899,96"
      )
    ),
    list(
      type = "P1030Caixa",
      data = c(
        CdCrt = "13711539",
        CdTit = "C/C",
        VlMrc = "1000,0",
        VlPatLiq = "30740004,7"
      )
    ),
    list(
      type = "P1030AReceber",
      data = c(
        CdCrt = "13711539",
        Grupo = "10401",
        CdTit = "RSG 47936719",
        Valor = "100000,0",
        SldTit = "100000,0",
        TotGrp = "100000,0",
        TotGeral = "104100,9",
        VlPatLiq = "30740004,7"
      )
    ),
    list(
      type = "P1030APagar",
      data = c(
        CdCrt = "13711539",
        Grupo = "21010",
        CdTit = "RESGA",
        Valor = "-156457,82",
        SldTit = "-156457,82",
        TotGrp = "-156457,82",
        TotGeral = "-164909,02",
        VlPatLiq = "30740004,7"
      )
    ),
    list(
      type = "P1030Rentabilidades",
      data = c(
        CdCrt = "13711539",
        Data = "16/10/2026",
        NumCotas = "25734946,60680873",
        VlCota = "1,19767464",
        VlPatLiq = "30822092,91",
        DecNumCotas = "8",
        DecVlCota = "8",
        IndiceBM = "CDI",
        VlIndiceBM = "30,0056375",
        VarCota = "0,053",
        VarIndiceBM = "0,0508",
        PercBM = "104,33"
      )
    ),
    list(
      type = "P1030Rentabilidades",
      data = c(
        CdCrt = "13711539",
        Data = "19/10/2026",
        NumCotas = "25659413,6047252",
        VlCota = "1,19800106",
        VlPatLiq = "30740004,7",
        DecNumCotas = "8",
        DecVlCota = "8",
        IndiceBM = "CDI",
        VlIndiceBM = "30,02087676",
        VarCota = "0,0273",
        VarIndiceBM = "0,0508",
        PercBM = "53,74"
      )
    ),
    list(
      type = "P1030MovCotistas",
      data = c(
        CdCrt = "13711539",
        TotApl = "0,0",
        TotRgt = "90488,63",
        NumDiasAgndApl = "1",
        NumDiasAgndRgt = "40"
      )
    ),
    list(
      type = "P1030Swap",
      data = c(
        CdCrt = "13711539",
        CdTit = "SWAP_TESTE",
        VlPatLiq = "30740004,7"
      )
    )
  )

  value_lines <- vapply(
    seq_along(values),
    function(i) {
      record <- values[[i]]
      fields <- schemas[[record$type]]

      data <- stats::setNames(
        rep("", length(fields)),
        fields
      )

      data[names(record$data)] <- record$data

      paste(
        c(i, record$type, "V", unname(data)),
        collapse = ";"
      )
    },
    character(1)
  )

  separator_number <- length(value_lines) + 1L

  header_lines <- vapply(
    seq_along(schemas),
    function(i) {
      paste(
        c(
          separator_number + i,
          names(schemas)[[i]],
          "C",
          schemas[[i]]
        ),
        collapse = ";"
      )
    },
    character(1)
  )

  lines <- c(
    value_lines,
    as.character(separator_number),
    header_lines
  )

  path <- tempfile(fileext = ".csv")

  writeLines(lines, path, useBytes = TRUE)

  path
}


test_that("imports a complete position object", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  position <- read_daycoval_position(path)

  expect_s3_class(position, "daycoval_position")

  expect_named(
    position,
    c(
      "metadata",
      "positions",
      "cash",
      "receivables",
      "payables",
      "returns",
      "shareholder_flows",
      "layouts",
      "raw"
    )
  )

  expect_named(
    position$positions,
    c(
      "fixed_income",
      "funds",
      "equities",
      "futures",
      "other"
    )
  )

  expect_equal(nrow(position$layouts), 11L)
  expect_equal(nrow(position$raw), 21L)

  expect_equal(nrow(position$positions$funds), 1L)
  expect_equal(nrow(position$cash), 1L)
  expect_equal(nrow(position$receivables), 1L)
  expect_equal(nrow(position$payables), 1L)
  expect_equal(nrow(position$returns), 2L)
  expect_equal(nrow(position$shareholder_flows), 1L)
})


test_that("extracts financial metadata from the correct date", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  position <- read_daycoval_position(path)

  expect_identical(
    position$metadata$codigo_carteira,
    "13711539"
  )

  expect_identical(
    position$metadata$data_posicao,
    as.Date("2026-10-19")
  )

  expect_equal(
    position$metadata$patrimonio_liquido,
    30740004.7
  )

  expect_equal(
    position$metadata$numero_cotas,
    25659413.6047252
  )

  expect_equal(
    position$metadata$valor_cota,
    1.19800106
  )

  expect_identical(
    position$metadata$indice_benchmark,
    "CDI"
  )
})


test_that("types shareholder flows", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  flows <- read_daycoval_position(path)$shareholder_flows

  expect_equal(nrow(flows), 1L)

  expect_type(flows$tot_apl, "double")
  expect_type(flows$tot_rgt, "double")

  expect_type(flows$num_dias_agnd_apl, "integer")
  expect_type(flows$num_dias_agnd_rgt, "integer")

  expect_equal(flows$tot_apl[[1L]], 0)
  expect_equal(flows$tot_rgt[[1L]], 90488.63)

  expect_identical(flows$num_dias_agnd_apl, 1L)
  expect_identical(flows$num_dias_agnd_rgt, 40L)

  expect_identical(flows$cd_crt, "13711539")
})


test_that("keeps empty instrument tables typed", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  position <- read_daycoval_position(path)

  expect_equal(
    nrow(position$positions$fixed_income),
    0L
  )

  expect_equal(
    nrow(position$positions$equities),
    0L
  )

  expect_equal(
    nrow(position$positions$futures),
    0L
  )

  expect_type(
    position$positions$fixed_income$vl_mrc,
    "double"
  )

  expect_type(
    position$positions$equities$qtd,
    "double"
  )

  expect_s3_class(
    position$positions$futures$data_venc,
    "Date"
  )
})


test_that("preserves untyped layouts and source records", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  position <- read_daycoval_position(path)

  expect_named(
    position$positions$other,
    "P1030Swap"
  )

  swaps <- position$positions$other$P1030Swap

  expect_equal(nrow(swaps), 1L)
  expect_identical(swaps$cd_tit, "SWAP_TESTE")

  expect_type(swaps$vl_pat_liq, "character")

  expect_identical(
    swaps$vl_pat_liq,
    "30740004,7"
  )

  expect_true(
    all(swaps$source_line %in% position$raw$source_line)
  )

  expect_true(
    any(is.na(position$raw$record_type))
  )

  expect_equal(
    sum(position$raw$record_flag == "V", na.rm = TRUE),
    9L
  )
})


test_that("rejects invalid shareholder flow periods", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  records <- read_daycoval_records(path)
  layouts <- parse_daycoval_position_layouts(records)

  flows <- parse_daycoval_position_layout(
    records,
    layouts,
    "P1030MovCotistas"
  )

  flows$num_dias_agnd_rgt[[1L]] <- "40,5"

  expect_error(
    parse_daycoval_position_types(
      flows,
      "P1030MovCotistas"
    ),
    "inteiro inválido"
  )
})


test_that("supports empty shareholder flow layouts", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  lines <- readLines(path)

  lines <- lines[
    !grepl(
      ";P1030MovCotistas;V;",
      lines,
      fixed = TRUE
    )
  ]

  writeLines(lines, path, useBytes = TRUE)

  position <- read_daycoval_position(path)

  flows <- position$shareholder_flows

  expect_equal(nrow(flows), 0L)
  expect_type(flows$tot_apl, "double")
  expect_type(flows$tot_rgt, "double")
  expect_type(flows$num_dias_agnd_rgt, "integer")
})


test_that("distinguishes absent optional layouts", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  lines <- readLines(path)

  lines <- lines[
    !grepl(
      ";P1030RF;C;",
      lines,
      fixed = TRUE
    )
  ]

  writeLines(lines, path, useBytes = TRUE)

  position <- read_daycoval_position(path)

  expect_null(position$positions$fixed_income)

  expect_false(
    "P1030RF" %in% position$layouts$record_type
  )
})


test_that("rejects missing required layouts", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  lines <- readLines(path)

  lines <- lines[
    !grepl(";P1030;", lines, fixed = TRUE)
  ]

  writeLines(lines, path, useBytes = TRUE)

  expect_error(
    read_daycoval_position(path),
    "Layouts obrigatórios ausentes"
  )
})


test_that("rejects records belonging to another portfolio", {
  path <- make_daycoval_position_import_fixture()
  on.exit(unlink(path), add = TRUE)

  lines <- readLines(path)

  index <- grep(
    ";P1030Swap;V;",
    lines,
    fixed = TRUE
  )

  lines[[index]] <- sub(
    "13711539",
    "99999999",
    lines[[index]],
    fixed = TRUE
  )

  writeLines(lines, path, useBytes = TRUE)

  expect_error(
    read_daycoval_position(path),
    "outra carteira"
  )
})
