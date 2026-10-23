
xp_statement_fixture <- function(time) {
  testthat::test_path(
    "fixtures",
    "xp",
    paste0("Extrato_10850552_23_10_2026 18_01_", time, ".pdf")
  )
}


test_that("XP statement reconstructs horizontal lines without watermark", {
  expected_periods <- c(
    "12" = "Movimentação de 19/10/2026 a 19/10/2026",
    "36" = "Movimentação de 16/10/2026 a 19/10/2026"
  )

  for (time in names(expected_periods)) {
    raw <- xp_read_tokens(
      path = xp_statement_fixture(time),
      source_name = paste0("extrato_", time, ".pdf")
    )

    lines <- xp_reconstruct_lines(raw)

    first_page <- lines |>
      dplyr::filter(.data$source_page == 1L)

    expect_equal(
      sum(first_page$text == expected_periods[[time]]),
      1L
    )

    expect_equal(
      sum(first_page$text == paste0(
        "Emissão: 23/10/2026 18:01:", time
      )),
      1L
    )

    expect_equal(
      sum(first_page$text == "Emitido pelo assessor(a): A66348"),
      1L
    )

    expect_equal(
      sum(first_page$text == paste0(
        "LONG TERM LB FUNDO DE INVESTIMENTO MULTIMERCADO",
        " - 10850552"
      )),
      1L
    )

    expect_equal(
      sum(first_page$text == paste(
        "Fundo Cota Quantidade Valor Aplicado",
        "Valor Bruto IR IOF Valor Liquido"
      )),
      1L
    )

    expect_true(
      any(stringr::str_detect(
        first_page$text,
        "^Total na Instituição "
      ))
    )

    expect_lt(
      sum(lengths(lines$source_tokens)),
      nrow(raw)
    )
  }
})


test_that("XP statement imports consolidated positions and metadata", {
  statement <- read_xp_statement(xp_statement_fixture("12"))

  expect_s3_class(statement, "xp_statement")
  expect_named(
    statement,
    c("metadata", "positions", "balances", "lots", "raw")
  )
  expect_null(statement$balances)
  expect_null(statement$lots)

  metadata <- statement$metadata

  expect_identical(metadata$account_number, "10850552")
  expect_identical(
    metadata$portfolio_name,
    "LONG TERM LB FUNDO DE INVESTIMENTO MULTIMERCADO"
  )
  expect_identical(metadata$advisor_code, "A66348")
  expect_identical(metadata$position_date, as.Date("2026-10-19"))
  expect_identical(metadata$movement_start, as.Date("2026-10-19"))
  expect_identical(metadata$movement_end, as.Date("2026-10-19"))
  expect_identical(
    format(metadata$issued_at, "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo"),
    "2026-10-23 18:01:12"
  )

  expect_equal(metadata$total_invested, 1039890.93)
  expect_equal(metadata$total_gross, 1278079.29)
  expect_equal(metadata$total_income_tax, 0)
  expect_equal(metadata$total_iof, 0)
  expect_equal(metadata$total_net, 1278079.29)
})


test_that("XP statement preserves all eight reported positions", {
  statement <- read_xp_statement(xp_statement_fixture("12"))
  positions <- statement$positions

  expect_s3_class(positions, "tbl_df")
  expect_equal(nrow(positions), 8L)

  expect_named(
    positions,
    c(
      "asset_name",
      "unit_price",
      "quantity",
      "invested_value",
      "gross_value",
      "income_tax",
      "iof",
      "net_value",
      "source_file",
      "source_page",
      "source_row",
      "source_tokens"
    )
  )

  expect_identical(
    positions$asset_name,
    c(
      "Absolute Pace LB Advisory FIF em Cotas de FIA - RL",
      "AZ Quest Small Mid Caps FIF CIC em Ações RL",
      "BNP Paribas Small Caps FIF em Ações RL",
      "Genoa Capital Arpa Advisory CIC CIA RL",
      "Giant DAO Multifactor LB FIF em Cotas de FIM",
      "Ibiuna Long Biased FIF CIC Multi RL",
      "Trend Bolsas Emergentes FIM RL",
      "Trend Bolsas Europeias FIA RL"
    )
  )

  expect_equal(
    positions$unit_price,
    c(
      3.64585530, 10.42704950, 450.52331730, 2.45955820,
      2.00114560, 615.07008350, 1.48613121, 1.97375589
    )
  )

  expect_equal(
    positions$quantity,
    c(
      28746.79331751, 8929.75651200, 123.63476250,
      41790.25300791, 50109.32799390, 178.74806115,
      214970.86706779, 198597.08635037
    )
  )

  expect_equal(
    positions$gross_value,
    c(
      104806.64, 93111.01, 55700.35, 102785.56,
      100276.06, 109942.58, 319474.92, 391982.17
    )
  )

  expect_equal(
    positions$invested_value,
    c(
      67103.71, 69021.44, 42787.92, 73715.44,
      72754.26, 64508.16, 250000.00, 400000.00
    )
  )

  expect_equal(
    sum(round(positions$gross_value * 100)),
    127807929
  )

  expect_equal(
    sum(round(positions$invested_value * 100)),
    103989093
  )

  expect_true(all(positions$income_tax == 0))
  expect_true(all(positions$iof == 0))
  expect_equal(positions$net_value, positions$gross_value)
})


test_that("XP statements with different movement windows have identical positions", {
  one_day <- read_xp_statement(xp_statement_fixture("12"))
  four_days <- read_xp_statement(xp_statement_fixture("36"))

  expect_identical(
    four_days$metadata$movement_start,
    as.Date("2026-10-16")
  )

  expect_identical(
    four_days$metadata$movement_end,
    as.Date("2026-10-19")
  )

  expect_identical(
    format(
      four_days$metadata$issued_at,
      "%Y-%m-%d %H:%M:%S",
      tz = "America/Sao_Paulo"
    ),
    "2026-10-23 18:01:36"
  )

  comparison_columns <- c(
    "asset_name",
    "unit_price",
    "quantity",
    "invested_value",
    "gross_value",
    "income_tax",
    "iof",
    "net_value"
  )

  expect_identical(
    one_day$positions[comparison_columns],
    four_days$positions[comparison_columns]
  )

  expect_identical(
    one_day$metadata$position_date,
    four_days$metadata$position_date
  )

  expect_equal(
    one_day$metadata$total_gross,
    four_days$metadata$total_gross
  )
})


test_that("XP statement excludes watermark fragments from asset names", {
  expected_name <- "Genoa Capital Arpa Advisory CIC CIA RL"

  for (time in c("12", "36")) {
    statement <- read_xp_statement(xp_statement_fixture(time))
    positions <- statement$positions

    genoa <- positions |>
      dplyr::filter(
        stringr::str_starts(.data$asset_name, "Genoa Capital")
      )

    expect_equal(nrow(genoa), 1L)
    expect_identical(genoa$asset_name[[1L]], expected_name)

    expect_false(
      any(stringr::str_detect(
        positions$asset_name,
        "[0-9]{1,2}:[0-9]{1,2}"
      ))
    )
  }
})


test_that("XP statement keeps source coordinates and exact token references", {
  statement <- read_xp_statement(xp_statement_fixture("36"))
  raw <- statement$raw
  positions <- statement$positions

  expect_s3_class(raw, "tbl_df")

  expect_named(
    raw,
    c(
      "source_file",
      "source_page",
      "source_token",
      "text",
      "x",
      "y",
      "width",
      "height"
    )
  )

  expect_setequal(unique(raw$source_page), 1:3)
  expect_true(all(is.finite(raw$x)))
  expect_true(all(is.finite(raw$y)))
  expect_true(all(positions$source_page == 1L))
  expect_false(anyDuplicated(positions$source_row) > 0L)

  for (i in seq_len(nrow(positions))) {
    original <- raw[
      raw$source_page == positions$source_page[[i]] &
        raw$source_token %in% positions$source_tokens[[i]],
      ,
      drop = FALSE
    ]

    expect_gt(nrow(original), 0L)

    expect_true(
      any(original$text %in% c(
        "0,00",
        "1,48613121",
        "Cotas",
        "RL"
      ))
    )
  }
})


test_that("XP statement rejects invalid paths and number formats", {
  expect_error(
    read_xp_statement(
      file.path(tempdir(), "extrato_xp_inexistente.pdf")
    ),
    "path"
  )

  expect_error(
    read_xp_statement(
      xp_statement_fixture("12"),
      source_name = ""
    ),
    "source_name"
  )

  expect_equal(
    xp_parse_number("28.746,79331751", "quantity"),
    28746.79331751
  )

  expect_error(
    xp_parse_number("28,746.79331751", "quantity"),
    "quantity"
  )

  expect_error(
    xp_parse_number("", "quantity"),
    "quantity"
  )
})


