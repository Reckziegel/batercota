
make_daycoval_metadata_fixture <- function() {
  tibble::tibble(
    source_file = rep("carteira.csv", 3L),
    source_line = 1:3,
    record_number = as.character(1:3),
    codigo_crt = rep("13386778", 3L),
    nome_crt = rep("RETA ALOCACAO FIC FIM CP", 3L),
    tp_crt = rep("Fundo Multimercado", 3L),
    administrador = rep(".BANCO DAYCOVAL S/A", 3L),
    moeda = rep("Real", 3L),
    data_emis = rep(as.Date("2026-10-19"), 3L),
    n_cotas = rep(7655579.92014227, 3L),
    vl_cotas = rep(1.29509791, 3L),
    n_cotista = rep("1", 3L),
    valor_total_atv = rep(10164001.22, 3L),
    titulo = c("C/C", "TXGESTA A PG", "PATLIQ"),
    grp_n1 = c("10000", "20000", "20000"),
    grp_n2 = c("10100", "23700", "26000"),
    grp_n3 = c("10108", "23711", "26000"),
    vl_mrc = c(1000, 3115.50, 9914725.55)
  )
}


test_that("extracts portfolio metadata", {
  entries <- make_daycoval_metadata_fixture()

  result <- parse_daycoval_metadata(entries)

  expect_type(result, "list")

  expect_identical(result$codigo_carteira, "13386778")
  expect_identical(result$nome_carteira, "RETA ALOCACAO FIC FIM CP")
  expect_identical(result$tipo_carteira, "Fundo Multimercado")
  expect_identical(result$moeda, "Real")

  expect_identical(
    result$data_posicao,
    as.Date("2026-10-19")
  )

  expect_equal(result$numero_cotas, 7655579.92014227)
  expect_equal(result$valor_cota, 1.29509791)
  expect_identical(result$numero_cotistas, 1L)
})


test_that("distinguishes net asset value from total assets", {
  entries <- make_daycoval_metadata_fixture()

  result <- parse_daycoval_metadata(entries)

  expect_equal(result$patrimonio_liquido, 9914725.55)
  expect_equal(result$total_ativos, 10164001.22)

  expect_false(
    identical(result$patrimonio_liquido, result$total_ativos)
  )
})


test_that("preserves the source of the net asset value", {
  entries <- make_daycoval_metadata_fixture()

  result <- parse_daycoval_metadata(entries)

  expect_identical(result$arquivo_origem, "carteira.csv")
  expect_identical(result$linha_patliq, 3L)
  expect_identical(result$registro_patliq, "3")
})


test_that("rejects inconsistent portfolio identifiers", {
  entries <- make_daycoval_metadata_fixture()
  entries$codigo_crt[[1L]] <- "99999999"

  expect_error(
    parse_daycoval_metadata(entries),
    "codigo_crt"
  )
})


test_that("rejects inconsistent accounting dates", {
  entries <- make_daycoval_metadata_fixture()
  entries$data_emis[[1L]] <- as.Date("2026-10-16")

  expect_error(
    parse_daycoval_metadata(entries),
    "data_emis"
  )
})


test_that("requires exactly one PATLIQ entry", {
  entries <- make_daycoval_metadata_fixture()

  without_patliq <- entries |>
    dplyr::filter(.data$titulo != "PATLIQ")

  expect_error(
    parse_daycoval_metadata(without_patliq),
    "exatamente um lançamento"
  )

  duplicate_patliq <- dplyr::bind_rows(
    entries,
    entries |>
      dplyr::filter(.data$titulo == "PATLIQ")
  )

  expect_error(
    parse_daycoval_metadata(duplicate_patliq),
    "exatamente um lançamento"
  )
})


test_that("rejects invalid shareholder counts", {
  entries <- make_daycoval_metadata_fixture()
  entries$n_cotista <- "1,5"

  expect_error(
    parse_daycoval_metadata(entries),
    "n_cotista"
  )
})


test_that("rejects missing mandatory fields", {
  entries <- make_daycoval_metadata_fixture()
  entries$valor_total_atv <- NULL

  expect_error(
    parse_daycoval_metadata(entries),
    "Colunas obrigatórias ausentes"
  )
})


test_that("rejects empty portfolio entries", {
  entries <- make_daycoval_metadata_fixture()
  entries <- entries[0, ]

  expect_error(
    parse_daycoval_metadata(entries),
    "não contém lançamentos"
  )
})
