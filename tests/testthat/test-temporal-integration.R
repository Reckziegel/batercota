
xp_temporal_fixture <- function(case) {
  files <- c(
    long_term_16 = "Extrato_10850552_26_10_2026 18_50_28.pdf",
    linear_16 = "Extrato_10850753_26_10_2026 18_55_08.pdf",
    linear_19 = "Extrato_10850753_26_10_2026 18_54_45.pdf"
  )

  testthat::test_path(
    "fixtures",
    "xp",
    files[[case]]
  )
}

read_xp_temporal_case <- function(case) {
  read_xp_statement(xp_temporal_fixture(case))
}

test_that("imports XP statements from different dates and funds", {
  cases <- list(
    list(
      case = "long_term_16",
      account = "10850552",
      date = as.Date("2026-10-16"),
      n_positions = 8L,
      gross_value = 1272908.64
    ),
    list(
      case = "linear_16",
      account = "10850753",
      date = as.Date("2026-10-16"),
      n_positions = 16L,
      gross_value = 28628565.94
    ),
    list(
      case = "linear_19",
      account = "10850753",
      date = as.Date("2026-10-19"),
      n_positions = 16L,
      gross_value = 28536899.83
    )
  )

  purrr::walk(cases, function(case) {
    statement <- read_xp_temporal_case(case$case)

    expect_s3_class(statement, "xp_statement")

    expect_identical(
      statement$metadata$account_number,
      case$account
    )

    expect_identical(
      statement$metadata$position_date,
      case$date
    )

    expect_equal(
      nrow(statement$positions),
      case$n_positions
    )

    expect_equal(
      statement$metadata$total_gross,
      case$gross_value
    )

    expect_false(
      anyDuplicated(statement$positions$asset_name) > 0L
    )
  })
})

test_that("Long Term quantities remain unchanged between dates", {
  earlier <- read_xp_temporal_case("long_term_16")

  later <- read_xp_statement(
    testthat::test_path(
      "fixtures",
      "xp",
      "Extrato_10850552_23_10_2026 18_01_12.pdf"
    )
  )

  expect_identical(
    earlier$positions$asset_name,
    later$positions$asset_name
  )

  expect_equal(
    earlier$positions$quantity,
    later$positions$quantity
  )
})

test_that("Linear preserves positions around Compass redemption", {
  earlier <- read_xp_temporal_case("linear_16")
  later <- read_xp_temporal_case("linear_19")

  compass_name <- paste(
    "Compass High Yield 180 Advisory",
    "FIF em Cotas de FIM"
  )

  compass_16 <- earlier$positions |>
    dplyr::filter(.data$asset_name == .env$compass_name)

  compass_19 <- later$positions |>
    dplyr::filter(.data$asset_name == .env$compass_name)

  expect_equal(nrow(compass_16), 1L)
  expect_equal(nrow(compass_19), 1L)

  expect_equal(
    compass_16$quantity - compass_19$quantity,
    57990.72461556
  )

  expect_equal(compass_19$unit_price, 0)
  expect_equal(compass_19$gross_value, 1348570.36)

  unchanged_16 <- earlier$positions |>
    dplyr::filter(.data$asset_name != .env$compass_name) |>
    dplyr::arrange(.data$asset_name)

  unchanged_19 <- later$positions |>
    dplyr::filter(.data$asset_name != .env$compass_name) |>
    dplyr::arrange(.data$asset_name)

  expect_identical(
    unchanged_16$asset_name,
    unchanged_19$asset_name
  )

  expect_equal(
    unchanged_16$quantity,
    unchanged_19$quantity
  )
})

test_that("Linear Daycoval Position C imports a second date", {
  position <- read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      "linear_2026-10-16.csv"
    )
  )

  expect_s3_class(position, "daycoval_position")

  expect_identical(
    position$metadata$codigo_carteira,
    "13711539"
  )

  expect_identical(
    position$metadata$data_posicao,
    as.Date("2026-10-16")
  )

  expect_equal(
    position$metadata$patrimonio_liquido,
    30822092.91
  )

  expect_equal(nrow(position$positions$funds), 17L)

  expect_true(
    "TPUBI97062PT" %in%
      stringr::str_trim(position$positions$funds$titulo)
  )
})

test_that("Linear XP preserves long instrument names", {
  expected_name <- paste(
    "JGP Crédito Ecossistema 360 Advisory",
    "FIC de FIF Multimercado CP RL"
  )

  for (case in c("linear_16", "linear_19")) {
    statement <- read_xp_temporal_case(case)

    expect_true(
      expected_name %in% statement$positions$asset_name
    )
  }
})


test_that("XP excludes date watermark fragments from fund names", {
  expected_name <- "Trend Pós-Fixado FIC FIRF Simples RL"

  for (case in c("linear_16", "linear_19")) {
    statement <- read_xp_temporal_case(case)

    trend <- statement$positions |>
      dplyr::filter(
        stringr::str_detect(
          .data$asset_name,
          stringr::fixed("Trend Pós-Fixado")
        )
      )

    expect_equal(nrow(trend), 1L)

    expect_identical(
      trend$asset_name[[1L]],
      expected_name
    )
  }
})

