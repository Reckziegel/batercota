
make_daycoval_entry_types_fixture <- function() {
  numeric_columns <- c(
    "qt", "qt_bloq", "qt_disp",
    "pu_cst", "vl_cst", "pu_mrc", "vl_mrc",
    "mrc_bloq", "vl_atv_pl", "n_cotas",
    "vl_cotas", "valor_total_atv",
    "dia", "mes", "ano", "doze_meses",
    "vinte_qt_meses", "inicio",
    "n_cotas2", "vl_cotas2",
    "dia2", "mes2", "ano2",
    "doze_meses2", "vinte_qt_meses2",
    "inicio2", "n_cotista2",
    "vl_taxa", "vl_cota_sem_amrtz",
    "vl_cota_ant_amrtz"
  )

  values <- stats::setNames(
    rep(
      list(c("0,0", "0,0")),
      length(numeric_columns)
    ),
    numeric_columns
  )

  entries <- tibble::as_tibble(values)

  entries$source_line <- c(10L, 20L)
  entries$codigo_crt <- c("13386778", "13386778")
  entries$titulo <- c("LONG.TERM", "DI1F33 0022")

  entries$grp_n1 <- c("10000", "10000")
  entries$grp_n2 <- c("16000", "19100")
  entries$grp_n3 <- c("16007", "19120")

  entries$data_emis <- c("19/10/2026", "19/10/2026")
  entries$data_venc <- c("", "03/01/2033")
  entries$data_aplic <- c("27/07/2026", "15/06/2026")

  entries$qt <- c("924042,822322", "-152502,0")
  entries$pu_mrc <- c("1,3207144", "-0,6557291051920631")
  entries$vl_mrc <- c("1220080,00", "-801,40")

  entries$dia <- c("0,08217864424926447", "-0,15")
  entries$mes <- c("3,6083624459488206", "3,5")
  entries$dia2 <- c("", "")

  entries$vl_taxa <- c("6,0", "14,443001")

  entries$n_cotista <- c("1", "2")
  entries$dec_pu <- c("9", "8")
  entries$dec_num_cota <- c("8", "8")
  entries$dec_val_cota <- c("8", "8")
  entries$dec_qtd <- c("6", "0")

  entries$tit_in465 <- c("N", "N")
  entries$b_preco_extn <- c("S", "N")
  entries$b_calc_preco_intn <- c("N", "N")

  entries$rot_calc <- c("38", "39")
  entries$nu_posc_d0 <- c("-1", "-1")
  entries$crt_isenta_lim <- c("0", "0")

  entries
}


test_that("converts portfolio numeric fields", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_identical(
    result$qt,
    c(924042.822322, -152502)
  )

  expect_identical(
    result$vl_mrc,
    c(1220080, -801.40)
  )

  expect_equal(
    result$dia,
    c(0.08217864424926447, -0.15)
  )

  expect_identical(
    result$vl_taxa,
    c(6, 14.443001)
  )
})


test_that("types empty numeric fields", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_type(result$dia2, "double")

  expect_identical(
    result$dia2,
    c(NA_real_, NA_real_)
  )

  expect_type(result$n_cotas2, "double")
})


test_that("converts count and precision fields to integers", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_identical(
    result$n_cotista,
    c(1L, 2L)
  )

  expect_identical(
    result$dec_pu,
    c(9L, 8L)
  )

  expect_identical(
    result$dec_qtd,
    c(6L, 0L)
  )
})


test_that("converts S/N indicators to logical values", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_identical(
    result$b_preco_extn,
    c(TRUE, FALSE)
  )

  expect_identical(
    result$b_calc_preco_intn,
    c(FALSE, FALSE)
  )

  expect_identical(
    result$tit_in465,
    c(FALSE, FALSE)
  )
})


test_that("preserves operational codes as character", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_identical(
    result$rot_calc,
    c("38", "39")
  )

  expect_identical(
    result$nu_posc_d0,
    c("-1", "-1")
  )

  expect_identical(
    result$crt_isenta_lim,
    c("0", "0")
  )

  expect_identical(
    result$grp_n3,
    c("16007", "19120")
  )
})


test_that("rejects fractional integer fields", {
  entries <- make_daycoval_entry_types_fixture()

  entries$dec_qtd[[2L]] <- "2,5"

  expect_error(
    parse_daycoval_entries(entries),
    "Valor inteiro inválido"
  )
})


test_that("rejects integer overflow", {
  entries <- make_daycoval_entry_types_fixture()

  entries$n_cotista[[1L]] <- "999999999999999999999"

  expect_error(
    parse_daycoval_entries(entries),
    "Valor inteiro inválido"
  )
})


test_that("rejects unknown logical indicators", {
  entries <- make_daycoval_entry_types_fixture()

  entries$b_preco_extn[[1L]] <- "X"

  expect_error(
    parse_daycoval_entries(entries),
    "Indicador inválido"
  )
})


test_that("allows missing logical indicators", {
  entries <- make_daycoval_entry_types_fixture()

  entries$b_preco_extn <- c("", NA_character_)

  result <- parse_daycoval_entries(entries)

  expect_identical(
    result$b_preco_extn,
    c(NA, NA)
  )
})


test_that("preserves original column names and order", {
  entries <- make_daycoval_entry_types_fixture()

  result <- parse_daycoval_entries(entries)

  expect_identical(
    names(result),
    names(entries)
  )

  expect_identical(
    result$source_line,
    entries$source_line
  )
})
