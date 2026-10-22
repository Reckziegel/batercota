
make_daycoval_benchmark_fixture <- function() {
  tibble::tibble(
    source_file = "carteira.csv",
    source_line = 35L,
    record_number = "35",
    indice = "CDI         ",
    dia = "0,050787989423661806",
    mes = "0,6111613403938954",
    ano = "11,186674953735665",
    doze_meses = "14,356368001290276",
    vinte_qt_meses = "29,763496155346104",
    inicio = "4,667963162227418"
  )
}


test_that("converts benchmark returns to numeric values", {
  benchmark <- make_daycoval_benchmark_fixture()

  result <- parse_daycoval_benchmark(benchmark)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)

  expect_identical(result$indice, "CDI")

  expect_equal(result$dia, 0.050787989423661806)
  expect_equal(result$mes, 0.6111613403938954)
  expect_equal(result$ano, 11.186674953735665)

  expect_equal(
    result$doze_meses,
    14.356368001290276
  )

  expect_equal(
    result$vinte_qt_meses,
    29.763496155346104
  )

  expect_equal(
    result$inicio,
    4.667963162227418
  )
})


test_that("preserves percentage-point conventions", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$dia <- "0,05"

  result <- parse_daycoval_benchmark(benchmark)

  expect_identical(result$dia, 0.05)
})


test_that("preserves benchmark identifiers and source", {
  benchmark <- make_daycoval_benchmark_fixture()

  result <- parse_daycoval_benchmark(benchmark)

  expect_identical(result$indice, "CDI")
  expect_identical(result$source_file, "carteira.csv")
  expect_identical(result$source_line, 35L)
  expect_identical(result$record_number, "35")
})


test_that("preserves negative benchmark returns", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$dia <- "-0,15"

  result <- parse_daycoval_benchmark(benchmark)

  expect_identical(result$dia, -0.15)
})


test_that("handles missing benchmark values", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$doze_meses <- ""
  benchmark$vinte_qt_meses <- NA_character_

  result <- parse_daycoval_benchmark(benchmark)

  expect_true(is.na(result$doze_meses))
  expect_true(is.na(result$vinte_qt_meses))
})


test_that("rejects invalid benchmark returns", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$dia <- "invalid"

  expect_error(
    parse_daycoval_benchmark(benchmark),
    "Valor numérico inválido"
  )
})


test_that("rejects missing benchmark identifiers", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$indice <- " "

  expect_error(
    parse_daycoval_benchmark(benchmark),
    "identificar o benchmark"
  )
})


test_that("rejects missing mandatory columns", {
  benchmark <- make_daycoval_benchmark_fixture()

  benchmark$ano <- NULL

  expect_error(
    parse_daycoval_benchmark(benchmark),
    "Colunas obrigatórias ausentes"
  )
})


test_that("supports multiple benchmark records", {
  benchmark <- make_daycoval_benchmark_fixture()

  second <- benchmark
  second$indice <- "IBOV"
  second$source_line <- 36L
  second$record_number <- "36"

  result <- parse_daycoval_benchmark(
    dplyr::bind_rows(benchmark, second)
  )

  expect_equal(nrow(result), 2L)
  expect_identical(result$indice, c("CDI", "IBOV"))
})
