
test_that("reads heterogeneous Daycoval records", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  lines <- c(
    "1;LayCrtDiaDespRef;V;13386735;;",
    "2;LayCrtDiaDespRefDet;V;Taxa;12,34",
    "3",
    paste(
      "4", "RentabBenkmark", "C", "Indice", "Dia",
      "Mes", "Ano", "DozeMeses", "VinteQtMeses", "Inicio",
      sep = ";"
    )
  )

  writeLines(lines, con = path, useBytes = TRUE)

  result <- read_daycoval_records(path)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4L)

  expect_identical(
    result$n_fields,
    c(6L, 5L, 1L, 10L)
  )

  expect_identical(
    result$record_type,
    c(
      "LayCrtDiaDespRef",
      "LayCrtDiaDespRefDet",
      NA_character_,
      "RentabBenkmark"
    )
  )

  expect_identical(
    result$record_flag,
    c("V", "V", NA_character_, "C")
  )

  expect_identical(
    result$source_line,
    seq_len(4L)
  )

  expect_identical(
    result$fields[[1]][5:6],
    c("", "")
  )

  expect_identical(result$raw_line, lines)
})


test_that("preserves original record identifiers", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  lines <- c(
    "10;LayCrtDiaDespRefDet;V;Taxa;12,34",
    "15;LayCrtDiaDespRefDet;V;Despesa;50,00"
  )

  writeLines(lines, con = path, useBytes = TRUE)

  result <- read_daycoval_records(path)

  expect_identical(
    result$record_number,
    c("10", "15")
  )

  expect_identical(
    result$source_line,
    c(1L, 2L)
  )

  expect_identical(
    result$source_file,
    rep(basename(path), 2L)
  )
})


test_that("preserves Latin-1 characters", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  line <- iconv(
    "1;LayCrtDiaDespRefDet;V;Taxa de Administração;12,34",
    from = "UTF-8",
    to = "Latin1"
  )

  writeLines(line, con = path, useBytes = TRUE)

  result <- read_daycoval_records(path)

  expect_identical(
    result$fields[[1]][4],
    "Taxa de Administração"
  )
})


test_that("preserves unknown record types", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  lines <- c(
    "1;NovoLayout;V;ABC;123",
    "2"
  )

  writeLines(lines, con = path, useBytes = TRUE)

  result <- read_daycoval_records(path)

  expect_equal(nrow(result), 2L)
  expect_identical(result$record_type[[1]], "NovoLayout")
  expect_true(is.na(result$record_type[[2]]))

  expect_identical(
    result$fields[[1]],
    c("1", "NovoLayout", "V", "ABC", "123")
  )
})


test_that("rejects missing and empty files", {
  missing_path <- tempfile(fileext = ".csv")

  expect_error(
    read_daycoval_records(missing_path),
    "arquivo CSV existente"
  )

  empty_path <- tempfile(fileext = ".csv")
  on.exit(unlink(empty_path), add = TRUE)

  file.create(empty_path)

  expect_error(
    read_daycoval_records(empty_path),
    "arquivo CSV está vazio"
  )
})


test_that("rejects invalid source names", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines("1;Teste;V;ABC", path)

  expect_error(
    read_daycoval_records(
      path = path,
      source_name = ""
    ),
    "source_name"
  )
})


test_that("rejects unsupported quoted fields", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines(
    '1;Teste;V;"Campo;com delimitador"',
    path
  )

  expect_error(
    read_daycoval_records(path),
    "contém aspas"
  )
})



