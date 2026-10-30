
reta_alocacao_test_case <- function() {
  report_dir <- Sys.getenv(
    "C:/Users/USUARIO/OneDrive/R/batercota/tests/testthat/fixtures/reta-alocacao",
    unset = ""
  )

  testthat::skip_if(
    !nzchar(report_dir),
    "Diretório de relatórios reais não configurado."
  )

  paths <- file.path(
    report_dir,
    c(
      "Carteira Diária - 261030_150115.csv",
      "Posição de Carteira C - 261030_150121.csv",
      "Extrato_10850442_30_10_2026 15_01_09.pdf"
    )
  )

  missing_files <- paths[!file.exists(paths)]

  if (length(missing_files) > 0L) {
    stop(
      "Relatórios não encontrados: ",
      paste(basename(missing_files), collapse = "; "),
      call. = FALSE
    )
  }

  list(
    portfolio = read_daycoval_portfolio(paths[[1L]]),
    position = read_daycoval_position(paths[[2L]]),
    xp = read_xp_statement(paths[[3L]])
  )
}


test_that("Reta Alocação has valid fund configuration", {
  reference <- batercota_fund_reference("reta_alocacao")

  expect_identical(
    reference$name,
    "Reta Alocação FIF"
  )

  expect_identical(
    reference$portfolio_code,
    "13386778"
  )

  expect_identical(
    reference$xp_account,
    "10850442"
  )

  expect_identical(
    reference$daycoval_only,
    "TPUBI97062ON"
  )

  expect_equal(nrow(reference$mapping), 6L)

  expect_equal(
    anyDuplicated(reference$mapping$xp_asset_name),
    0L
  )

  expect_equal(
    anyDuplicated(reference$mapping$daycoval_title),
    0L
  )

  expect_false(any(
    reference$daycoval_only %in%
      reference$mapping$daycoval_title
  ))

  expect_setequal(
    reference$mapping$daycoval_title,
    c(
      "CF 36017731",
      "CF 30493349",
      "CF 33520968",
      "LONG.TERM",
      "CF 38026814",
      "CF 26559284"
    )
  )
})


test_that("all configured funds have unique mappings", {
  funds <- c(
    "long_term",
    "linear",
    "reta_alocacao"
  )

  for (fund in funds) {
    reference <- batercota_fund_reference(fund)

    expect_gt(nrow(reference$mapping), 0L)

    expect_equal(
      anyDuplicated(reference$mapping$xp_asset_name),
      0L
    )

    expect_equal(
      anyDuplicated(reference$mapping$daycoval_title),
      0L
    )

    expect_false(any(
      reference$daycoval_only %in%
        reference$mapping$daycoval_title
    ))
  }
})


test_that("Reta Alocação imports real reports", {
  case <- reta_alocacao_test_case()

  expect_s3_class(
    case$portfolio,
    "daycoval_portfolio"
  )

  expect_s3_class(
    case$position,
    "daycoval_position"
  )

  expect_s3_class(
    case$xp,
    "xp_statement"
  )

  expect_identical(
    case$portfolio$metadata$codigo_carteira,
    "13386778"
  )

  expect_identical(
    case$position$metadata$codigo_carteira,
    "13386778"
  )

  expect_identical(
    case$xp$metadata$account_number,
    "10850442"
  )

  expect_identical(
    case$portfolio$metadata$data_posicao,
    as.Date("2026-10-19")
  )

  expect_identical(
    case$xp$metadata$position_date,
    as.Date("2026-10-19")
  )

  expect_equal(nrow(case$xp$positions), 6L)
})


test_that("Reta Alocação reconciles real positions", {
  case <- reta_alocacao_test_case()

  view <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "reta_alocacao",
    financial_tolerance = 100
  )

  expect_identical(
    view$metadata$fund,
    "reta_alocacao"
  )

  expect_equal(nrow(view$positions), 6L)

  expect_equal(
    view$internal$summary$matched,
    7L
  )

  expect_equal(
    nrow(view$internal$issues),
    0L
  )

  expect_equal(
    view$external$summary$matched,
    6L
  )

  expect_equal(
    view$external$summary$unmatched_xp,
    0L
  )

  expect_equal(
    view$external$summary$unmatched_daycoval,
    0L
  )

  expect_equal(
    view$external$summary$expected_daycoval_only,
    1L
  )

  expect_equal(
    sum(view$positions$market_value_difference),
    1.18,
    tolerance = 1e-6
  )

  expect_true(all(
    view$positions$status %in% c("OK", "Tolerada")
  ))

  expect_false(any(
    view$positions$status == "Divergente"
  ))
})


test_that("Reta Alocação preserves the Daycoval-only position", {
  case <- reta_alocacao_test_case()

  view <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "reta_alocacao"
  )

  expected_issue <- view$external$issues |>
    dplyr::filter(
      .data$issue_type == "expected_daycoval_only"
    )

  expect_equal(nrow(expected_issue), 1L)

  expect_identical(
    expected_issue$asset_identifier,
    "TPUBI97062ON"
  )

  expect_false(
    "TPUBI97062ON" %in%
      view$positions$daycoval_title
  )
})


test_that("Reta Alocação rejects incorrect XP account", {
  case <- reta_alocacao_test_case()

  case$xp$metadata$account_number <- "10850552"

  expect_error(
    build_reconciliation_screen(
      portfolio = case$portfolio,
      position = case$position,
      xp = case$xp,
      fund = "reta_alocacao"
    ),
    "extrato XP não pertence"
  )
})
