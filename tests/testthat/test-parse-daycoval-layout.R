
make_daycoval_layout_fixture <- function() {
  field_names <- c(
    "CodigoCrt",
    "DataEmis",
    "Titulo",
    "Qt",
    "PuMrc",
    "VlMrc",
    "GrpN1",
    "GrpN2",
    "GrpN3",
    paste0("Campo", seq_len(58L))
  )

  entry_fields <- c(
    "13386778",
    "19/10/2026",
    "LONG.TERM",
    "924042,822322",
    "1,3207144",
    "1220080,00",
    "10000",
    "16000",
    "16007",
    rep("", 58L)
  )

  lines <- c(
    paste(
      c("1", "LayCrtDiaDespRef", "V", entry_fields),
      collapse = ";"
    ),
    "2;LayCrtDiaDespRefDet;V;Taxa de Gestão;100,00",
    "3",
    paste(
      c("4", "LayCrtDiaDespRef", "C", field_names),
      collapse = ";"
    ),
    "5;LayCrtDiaDespRefDet;C;Historico;Valor",
    paste(
      "6", "RentabBenkmark", "V", "CDI",
      "0,05", "0,61", "11,18", "14,35",
      "29,76", "4,66",
      sep = ";"
    ),
    paste(
      "7", "RentabBenkmark", "C", "Indice",
      "Dia", "Mes", "Ano", "DozeMeses",
      "VinteQtMeses", "Inicio",
      sep = ";"
    )
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  lines_latin1 <- iconv(
    lines,
    from = "UTF-8",
    to = "Latin1"
  )

  writeLines(lines_latin1, path, useBytes = TRUE)

  read_daycoval_records(path)
}


test_that("converts Daycoval field names to snake_case", {
  input <- c(
    "CodigoCrt",
    "DataEmis",
    "PuMrc",
    "VlMrc",
    "GrpN1",
    "NCotas",
    "VlCotaSemAmrtz"
  )

  expected <- c(
    "codigo_crt",
    "data_emis",
    "pu_mrc",
    "vl_mrc",
    "grp_n1",
    "n_cotas",
    "vl_cota_sem_amrtz"
  )

  expect_identical(
    daycoval_snake_case(input),
    expected
  )
})


test_that("parses main portfolio layout", {
  records <- make_daycoval_layout_fixture()

  result <- parse_daycoval_layout(
    records,
    record_type = "LayCrtDiaDespRef"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 70L)

  expect_identical(
    result$codigo_crt,
    "13386778"
  )

  expect_identical(
    result$data_emis,
    "19/10/2026"
  )

  expect_identical(
    result$titulo,
    "LONG.TERM"
  )

  expect_identical(
    result$qt,
    "924042,822322"
  )

  expect_identical(
    result$vl_mrc,
    "1220080,00"
  )

  expect_identical(
    result$grp_n3,
    "16007"
  )
})


test_that("preserves source information", {
  records <- make_daycoval_layout_fixture()

  result <- parse_daycoval_layout(
    records,
    record_type = "LayCrtDiaDespRef"
  )

  expect_identical(result$source_line, 1L)
  expect_identical(result$record_number, "1")

  expect_true("source_file" %in% names(result))
})


test_that("parses auxiliary detail records", {
  records <- make_daycoval_layout_fixture()

  result <- parse_daycoval_layout(
    records,
    record_type = "LayCrtDiaDespRefDet"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 5L)

  expect_identical(
    result$historico,
    "Taxa de Gestão"
  )

  expect_identical(
    result$valor,
    "100,00"
  )
})


test_that("parses benchmark records", {
  records <- make_daycoval_layout_fixture()

  result <- parse_daycoval_layout(
    records,
    record_type = "RentabBenkmark"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(ncol(result), 10L)

  expect_identical(result$indice, "CDI")
  expect_identical(result$dia, "0,05")
  expect_identical(result$mes, "0,61")
  expect_identical(result$ano, "11,18")
})


test_that("rejects unknown layouts", {
  records <- make_daycoval_layout_fixture()

  expect_error(
    parse_daycoval_layout(
      records,
      record_type = "LayoutDesconhecido"
    ),
    "layout conhecido"
  )
})


test_that("rejects records with incorrect field counts", {
  records <- make_daycoval_layout_fixture()

  records$n_fields[[1]] <- 69L

  expect_error(
    parse_daycoval_layout(
      records,
      record_type = "LayCrtDiaDespRef"
    ),
    "tamanho inesperado"
  )
})


test_that("requires exactly one layout header", {
  records <- make_daycoval_layout_fixture()

  records <- records |>
    dplyr::filter(
      !(
        .data$record_type == "LayCrtDiaDespRef" &
          .data$record_flag == "C"
      )
    )

  expect_error(
    parse_daycoval_layout(
      records,
      record_type = "LayCrtDiaDespRef"
    ),
    "exatamente um cabeçalho"
  )
})

