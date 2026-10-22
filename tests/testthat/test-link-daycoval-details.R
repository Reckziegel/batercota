
make_daycoval_details_fixture <- function() {
  entries <- tibble::tibble(
    source_file = rep("carteira.csv", 3L),
    source_line = c(10L, 13L, 16L),
    record_number = c("100", "101", "102"),
    titulo = c("TXADM A PG", "TXGESTA A PG", "TXCUST A PG"),
    vl_mrc = c(1714.32, 3115.84, 571.44)
  )

  details <- tibble::tibble(
    source_file = rep("carteira.csv", 5L),
    source_line = c(11L, 14L, 15L, 17L, 20L),
    record_number = c("21", "22", "23", "24", "25"),
    historico = c(
      "Pagamento Taxa Administracao",
      "Pagamento Taxa Gestao",
      "Outro detalhamento",
      "Valor divergente",
      "Detalhamento isolado"
    ),
    valor = c(
      "1714,32",
      "3115,84",
      "3115,84",
      "999,00",
      "571,44"
    )
  )

  list(entries = entries, details = details)
}


test_that("links adjacent details with matching amounts", {
  fixture <- make_daycoval_details_fixture()

  result <- link_daycoval_details(
    fixture$details,
    fixture$entries
  )

  expect_identical(
    result$entry_source_line[1:2],
    c(10L, 13L)
  )

  expect_identical(
    result$entry_record_number[1:2],
    c("100", "101")
  )

  expect_identical(
    result$link_status[1:2],
    c("linked", "linked")
  )
})


test_that("does not link nonadjacent details", {
  fixture <- make_daycoval_details_fixture()

  result <- link_daycoval_details(
    fixture$details,
    fixture$entries
  )

  expect_true(is.na(result$entry_source_line[[3L]]))
  expect_true(is.na(result$entry_source_line[[5L]]))

  expect_identical(result$link_status[[3L]], "unlinked")
  expect_identical(result$link_status[[5L]], "unlinked")
})


test_that("does not link details with different amounts", {
  fixture <- make_daycoval_details_fixture()

  result <- link_daycoval_details(
    fixture$details,
    fixture$entries
  )

  expect_true(is.na(result$entry_source_line[[4L]]))
  expect_identical(result$link_status[[4L]], "unlinked")
})


test_that("preserves all original detail information", {
  fixture <- make_daycoval_details_fixture()

  result <- link_daycoval_details(
    fixture$details,
    fixture$entries
  )

  expect_equal(nrow(result), nrow(fixture$details))

  expect_identical(
    result$historico,
    fixture$details$historico
  )

  expect_identical(
    result$valor,
    fixture$details$valor
  )

  expect_identical(
    result$source_line,
    fixture$details$source_line
  )
})


test_that("handles empty detail tables", {
  fixture <- make_daycoval_details_fixture()

  details <- fixture$details[0, ]

  result <- link_daycoval_details(
    details,
    fixture$entries
  )

  expect_equal(nrow(result), 0L)

  expect_true(
    all(
      c(
        "entry_source_line",
        "entry_record_number",
        "link_status"
      ) %in% names(result)
    )
  )
})


test_that("rejects duplicate entry source identifiers", {
  fixture <- make_daycoval_details_fixture()

  entries <- dplyr::bind_rows(
    fixture$entries,
    fixture$entries[1, ]
  )

  expect_error(
    link_daycoval_details(fixture$details, entries),
    "duplicados"
  )
})
