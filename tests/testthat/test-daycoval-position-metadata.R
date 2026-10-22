make_daycoval_position_metadata_fixture <- function() {
  cadastro <- tibble::tibble(
    source_file = "posicao.csv",
    source_line = 1L,
    record_number = "1",
    cd_crt = "13711539",
    nome_curto = "LINEAR FIM CP   ",
    nome_longo = "LINEAR FIM CP RESPONSABILIDADE LIMITADA ",
    cnpj = "60.629.171/0001-06",
    tipo_carteira = "Fundo Multimercado  ",
    administrador = ".BANCO DAYCOVAL S/A  ",
    gestor = "Reta Asset Ltda ",
    data = "19/10/2026"
  )

  rentabilidades <- tibble::tibble(
    source_file = c("posicao.csv", "posicao.csv"),
    source_line = c(2L, 3L),
    record_number = c("2", "3"),
    cd_crt = c("13711539", "13711539"),
    data = c("16/10/2026", "19/10/2026"),
    num_cotas = c(
      "25734946,60680873",
      "25659413,6047252"
    ),
    vl_cota = c("1,19767464", "1,19800106"),
    vl_pat_liq = c("30822092,91", "30740004,70"),
    dec_num_cotas = c("8", "8"),
    dec_vl_cota = c("8", "8"),
    indice_bm = c("CDI  ", "CDI  "),
    vl_indice_bm = c("30,0056375", "30,02087676"),
    var_cota = c("0,053", "0,0273"),
    var_indice_bm = c("0,0508", "0,0508"),
    perc_bm = c("104,33", "53,74")
  )

  list(
    cadastro = parse_daycoval_position_types(
      cadastro,
      "P1030"
    ),
    rentabilidades = parse_daycoval_position_types(
      rentabilidades,
      "P1030Rentabilidades"
    ),
    rentabilidades_raw = rentabilidades
  )
}


test_that("types P1030Rentabilidades fields", {
  fixture <- make_daycoval_position_metadata_fixture()
  result <- fixture$rentabilidades

  expect_s3_class(result$data, "Date")

  numeric_columns <- c(
    "num_cotas",
    "vl_cota",
    "vl_pat_liq",
    "vl_indice_bm",
    "var_cota",
    "var_indice_bm",
    "perc_bm"
  )

  expect_true(all(vapply(
    result[numeric_columns],
    is.double,
    logical(1)
  )))

  expect_identical(result$dec_num_cotas, c(8L, 8L))
  expect_identical(result$dec_vl_cota, c(8L, 8L))

  expect_equal(result$vl_cota[[2L]], 1.19800106)
  expect_equal(result$vl_pat_liq[[2L]], 30740004.70)

  expect_equal(result$var_cota[[2L]], 0.0273)
  expect_equal(result$var_indice_bm[[2L]], 0.0508)
  expect_equal(result$perc_bm[[2L]], 53.74)

  expect_identical(result$indice_bm[[2L]], "CDI  ")
})


test_that("extracts metadata for the exact position date", {
  fixture <- make_daycoval_position_metadata_fixture()

  metadata <- parse_daycoval_position_metadata(
    fixture$cadastro,
    fixture$rentabilidades
  )

  expect_identical(
    metadata$codigo_carteira,
    "13711539"
  )

  expect_identical(
    metadata$data_posicao,
    as.Date("2026-10-19")
  )

  expect_identical(
    metadata$nome_curto,
    "LINEAR FIM CP"
  )

  expect_identical(
    metadata$gestor,
    "Reta Asset Ltda"
  )

  expect_equal(
    metadata$patrimonio_liquido,
    30740004.70
  )

  expect_equal(
    metadata$numero_cotas,
    25659413.6047252
  )

  expect_equal(
    metadata$valor_cota,
    1.19800106
  )

  expect_identical(
    metadata$indice_benchmark,
    "CDI"
  )

  expect_identical(
    metadata$registro_rentabilidade,
    "3"
  )

  expect_identical(
    metadata$linha_rentabilidade,
    3L
  )
})


test_that("does not substitute an older NAV observation", {
  fixture <- make_daycoval_position_metadata_fixture()

  rentabilidades <- fixture$rentabilidades[1L, ]

  metadata <- parse_daycoval_position_metadata(
    fixture$cadastro,
    rentabilidades
  )

  expect_true(is.na(metadata$patrimonio_liquido))
  expect_true(is.na(metadata$numero_cotas))
  expect_true(is.na(metadata$valor_cota))
  expect_true(is.na(metadata$indice_benchmark))

  expect_true(is.na(metadata$registro_rentabilidade))
  expect_true(is.na(metadata$linha_rentabilidade))
})


test_that("supports empty profitability histories", {
  fixture <- make_daycoval_position_metadata_fixture()

  rentabilidades <- fixture$rentabilidades[0, ]

  metadata <- parse_daycoval_position_metadata(
    fixture$cadastro,
    rentabilidades
  )

  expect_identical(
    metadata$codigo_carteira,
    "13711539"
  )

  expect_true(is.na(metadata$patrimonio_liquido))
})


test_that("rejects duplicate observations for the position date", {
  fixture <- make_daycoval_position_metadata_fixture()

  rentabilidades <- dplyr::bind_rows(
    fixture$rentabilidades,
    fixture$rentabilidades[2L, ]
  )

  expect_error(
    parse_daycoval_position_metadata(
      fixture$cadastro,
      rentabilidades
    ),
    "mais de um registro"
  )
})


test_that("rejects profitability history from another portfolio", {
  fixture <- make_daycoval_position_metadata_fixture()

  fixture$rentabilidades$cd_crt[[1L]] <- "99999999"

  expect_error(
    parse_daycoval_position_metadata(
      fixture$cadastro,
      fixture$rentabilidades
    ),
    "outra carteira"
  )
})


test_that("requires exactly one portfolio registration", {
  fixture <- make_daycoval_position_metadata_fixture()

  cadastro <- dplyr::bind_rows(
    fixture$cadastro,
    fixture$cadastro
  )

  expect_error(
    parse_daycoval_position_metadata(
      cadastro,
      fixture$rentabilidades
    ),
    "exatamente um registro"
  )
})


test_that("rejects invalid profitability dates", {
  fixture <- make_daycoval_position_metadata_fixture()

  rentabilidades <- fixture$rentabilidades_raw
  rentabilidades$data[[2L]] <- "31/02/2026"

  expect_error(
    parse_daycoval_position_types(
      rentabilidades,
      "P1030Rentabilidades"
    ),
    "Data inválida"
  )
})


test_that("rejects invalid profitability numbers", {
  fixture <- make_daycoval_position_metadata_fixture()

  rentabilidades <- fixture$rentabilidades_raw
  rentabilidades$vl_cota[[2L]] <- "INVALIDO"

  expect_error(
    parse_daycoval_position_types(
      rentabilidades,
      "P1030Rentabilidades"
    ),
    "INVALIDO"
  )
})
