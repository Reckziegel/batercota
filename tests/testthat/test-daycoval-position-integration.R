daycoval_position_real_cases <- function() {
  list(
    linear = list(
      file = "linear_2026-10-19.csv",
      codigo_carteira = "13711539",
      nome_curto = "LINEAR FIM CP",
      n_raw = 60L,
      n_values = 42L,
      patrimonio_liquido = 30740004.70,
      numero_cotas = 25659413.6047252,
      valor_cota = 1.19800106,
      counts = c(
        fixed_income = 0L,
        funds = 17L,
        equities = 0L,
        futures = 0L,
        cash = 1L,
        receivables = 10L,
        payables = 8L,
        returns = 4L,
        shareholder_flows = 1L
      ),
      cash_value = 1000,
      receivables_total = 104100.90,
      payables_total = -164909.02,
      shareholder_redemptions = 90488.63,
      redemption_days = 40L
    ),
    long_term = list(
      file = "long_term_2026-10-19.csv",
      codigo_carteira = "13386735",
      nome_curto = "LONG TERM LB FIF",
      n_raw = 69L,
      n_values = 51L,
      patrimonio_liquido = 2418254.80,
      numero_cotas = 1831526.06470122,
      valor_cota = 1.32034965,
      counts = c(
        fixed_income = 2L,
        funds = 9L,
        equities = 7L,
        futures = 1L,
        cash = 2L,
        receivables = 14L,
        payables = 10L,
        returns = 4L,
        shareholder_flows = 1L
      ),
      cash_value = 436.62,
      receivables_total = 70697.20,
      payables_total = -10120.53,
      shareholder_redemptions = 1396.03,
      redemption_days = 35L
    )
  )
}


read_daycoval_real_position <- function(case) {
  path <- testthat::test_path(
    "fixtures",
    "daycoval-position",
    case$file
  )

  read_daycoval_position(path)
}


test_that("imports both real Daycoval position reports", {
  cases <- daycoval_position_real_cases()

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    expect_s3_class(
      position,
      "daycoval_position"
    )

    expect_identical(
      position$metadata$codigo_carteira,
      case$codigo_carteira
    )

    expect_identical(
      position$metadata$nome_curto,
      case$nome_curto
    )

    expect_identical(
      position$metadata$data_posicao,
      as.Date("2026-10-19")
    )

    expect_equal(
      position$metadata$patrimonio_liquido,
      case$patrimonio_liquido
    )

    expect_equal(
      position$metadata$numero_cotas,
      case$numero_cotas
    )

    expect_equal(
      position$metadata$valor_cota,
      case$valor_cota
    )

    expect_identical(
      position$metadata$indice_benchmark,
      "CDI"
    )

    expect_equal(
      nrow(position$raw),
      case$n_raw
    )

    expect_equal(
      nrow(position$layouts),
      17L
    )
  }
})


test_that("matches all real layout record counts", {
  cases <- daycoval_position_real_cases()

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    actual <- c(
      fixed_income = nrow(position$positions$fixed_income),
      funds = nrow(position$positions$funds),
      equities = nrow(position$positions$equities),
      futures = nrow(position$positions$futures),
      cash = nrow(position$cash),
      receivables = nrow(position$receivables),
      payables = nrow(position$payables),
      returns = nrow(position$returns),
      shareholder_flows = nrow(position$shareholder_flows)
    )

    expect_equal(
      actual,
      case$counts
    )

    expect_equal(
      sum(position$layouts$n_records),
      case$n_values
    )

    expect_equal(
      sum(
        position$raw$record_flag == "V",
        na.rm = TRUE
      ),
      case$n_values
    )
  }
})


test_that("preserves every original value record exactly once", {
  cases <- daycoval_position_real_cases()

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    components <- list(
      position$positions$fixed_income,
      position$positions$funds,
      position$positions$equities,
      position$positions$futures,
      position$cash,
      position$receivables,
      position$payables,
      position$returns,
      position$shareholder_flows
    )

    components <- c(
      components,
      unname(position$positions$other)
    )

    component_lines <- unlist(
      lapply(
        components,
        function(data) {
          if (is.null(data)) {
            return(integer())
          }

          data$source_line
        }
      ),
      use.names = FALSE
    )

    imported_lines <- c(
      position$metadata$linha_cadastro,
      component_lines
    )

    value_lines <- position$raw$source_line[
      position$raw$record_flag %in% "V"
    ]

    expect_equal(
      length(imported_lines),
      case$n_values
    )

    expect_equal(
      sort(imported_lines),
      sort(value_lines)
    )

    expect_false(
      anyDuplicated(imported_lines) > 0L
    )

    expect_identical(
      position$raw$source_line,
      seq_len(case$n_raw)
    )

    expect_true(
      position$metadata$linha_cadastro %in% value_lines
    )
  }
})


test_that("preserves accounting balances and repeated totals", {
  cases <- daycoval_position_real_cases()

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    cash <- position$cash
    receivables <- position$receivables
    payables <- position$payables

    current_account <- cash |>
      dplyr::filter(
        stringr::str_trim(.data$cd_tit) == "C/C"
      )

    expect_equal(nrow(current_account), 1L)

    expect_equal(
      current_account$vl_mrc[[1L]],
      case$cash_value
    )

    expect_equal(
      unique(receivables$tot_geral),
      case$receivables_total
    )

    expect_equal(
      unique(payables$tot_geral),
      case$payables_total
    )

    expect_equal(
      sum(receivables$valor),
      case$receivables_total
    )

    expect_equal(
      sum(payables$valor),
      case$payables_total
    )

    expect_type(receivables$grupo, "character")
    expect_type(payables$grupo, "character")
  }
})


test_that("preserves actual shareholder movements", {
  cases <- daycoval_position_real_cases()

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    flows <- position$shareholder_flows

    expect_equal(nrow(flows), 1L)

    expect_equal(
      flows$tot_apl[[1L]],
      0
    )

    expect_equal(
      flows$tot_rgt[[1L]],
      case$shareholder_redemptions
    )

    expect_identical(
      flows$num_dias_agnd_apl,
      1L
    )

    expect_identical(
      flows$num_dias_agnd_rgt,
      case$redemption_days
    )
  }
})


test_that("retains untyped instrument layouts", {
  cases <- daycoval_position_real_cases()

  expected_other <- c(
    "P1030OC",
    "P1030Alug",
    "P1030BMFMercadoriaSC",
    "P1030TermoAcoes",
    "P1030OpVenda",
    "P1030Swap",
    "P1030OpCompra"
  )

  for (case in cases) {
    position <- read_daycoval_real_position(case)

    expect_named(
      position$positions$other,
      expected_other,
      ignore.order = TRUE
    )

    expect_true(all(vapply(
      position$positions$other,
      nrow,
      integer(1)
    ) == 0L))

    expect_true(all(vapply(
      position$positions$other,
      function(data) {
        all(vapply(
          data,
          is.character,
          logical(1)
        )) ||
          all(vapply(
            data[
              setdiff(names(data), "source_line")
            ],
            is.character,
            logical(1)
          ))
      },
      logical(1)
    )))
  }
})


test_that("preserves specific Linear portfolio details", {
  case <- daycoval_position_real_cases()$linear

  position <- read_daycoval_real_position(case)

  funds <- position$positions$funds

  expect_equal(nrow(funds), 17L)

  expect_true(
    "FZW781751643726" %in% funds$cnpj
  )

  expect_true(all(
    funds$tipo_renda == "500"
  ))

  receivables <- position$receivables

  compass <- receivables |>
    dplyr::filter(
      .data$cd_tit == "RSG 47936719"
    )

  expect_equal(nrow(compass), 1L)

  expect_equal(
    compass$valor[[1L]],
    100000
  )

  expect_equal(
    compass$sld_tit[[1L]],
    100000
  )

  payables <- position$payables

  adjustment <- payables |>
    dplyr::filter(
      stringr::str_trim(.data$cd_tit) == "@AJCOD"
    )

  expect_equal(nrow(adjustment), 1L)

  expect_equal(
    adjustment$valor[[1L]],
    0.13
  )
})


test_that("preserves specific Long Term instruments", {
  case <- daycoval_position_real_cases()$long_term

  position <- read_daycoval_real_position(case)

  bonds <- position$positions$fixed_income
  equities <- position$positions$equities
  futures <- position$positions$futures

  expect_setequal(
    bonds$cd_tit,
    c("LTN00003E8D3", "NTN-B002C3BC")
  )

  expect_setequal(
    stringr::str_trim(equities$cd_tit),
    c(
      "XINA11",
      "B3SA3",
      "BPAC11",
      "BRBI11",
      "SIMH3",
      "XPBR31",
      "ACWX39"
    )
  )

  expect_equal(nrow(futures), 1L)

  expect_identical(
    stringr::str_trim(futures$mercadoria),
    "DI1"
  )

  expect_identical(
    stringr::str_trim(futures$serie),
    "F33"
  )

  expect_equal(
    futures$vl_noc[[1L]],
    378406.56
  )

  expect_equal(
    futures$ajuste[[1L]],
    -320.56
  )

  expect_identical(
    futures$data_venc,
    as.Date("2033-01-03")
  )

  cash <- position$cash

  expect_equal(
    cash$vl_mrc,
    c(436.62, 0)
  )
})
