
# tests/testthat/test-fluxos-passivos-diagnostico.R

source(
  testthat::test_path(
    "..",
    "..",
    "plataforma",
    "mod_fluxos_passivos_diagnostico.R"
  ),
  local = TRUE
)


fluxos_diagnostic_fixture <- local({
  cached <- NULL

  function() {
    if (is.null(cached)) {
      paths <- list(
        plan = testthat::test_path(
          "..", "..", "plataforma",
          "fundos - Movimentações Previstas LINEAR.xlsx"
        ),
        movement = testthat::test_path(
          "fixtures", "daycoval-financial",
          "linear_movement_2026-10-16_2026-10-22.csv"
        ),
        cash = testthat::test_path(
          "fixtures", "daycoval-financial",
          "linear_cash_2026-10-16_2026-10-22.csv"
        ),
        opening = testthat::test_path(
          "fixtures", "daycoval-position",
          "linear_2026-10-16.csv"
        ),
        checkpoint = testthat::test_path(
          "fixtures", "daycoval-position",
          "linear_2026-10-19.csv"
        )
      )

      reports <- list(
        cash_plan = read_fund_cash_plan(paths$plan),
        daily_movement = read_daycoval_daily_movement(
          paths$movement
        ),
        cash_statement = read_daycoval_cash_statement(
          paths$cash
        ),
        opening_position = read_daycoval_position(
          paths$opening
        )
      )

      reconciliation <- build_shareholder_liability_timeline(
        cash_plan = reports$cash_plan,
        daily_movement = reports$daily_movement,
        cash_statement = reports$cash_statement,
        opening_position = reports$opening_position,
        positions = list(
          read_daycoval_position(paths$checkpoint)
        )
      )

      cached <<- list(
        reports = reports,
        reconciliation = reconciliation
      )
    }

    cached
  }
})


test_that("daily details preserve the accounting reconstruction", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    reconciliation = case$reconciliation,
    reports = case$reports,
    date = as.Date("2026-10-21")
  )

  expect_identical(
    details$date,
    as.Date("2026-10-21")
  )

  expect_equal(nrow(details$daily), 1L)

  expect_equal(
    details$daily$daycoval_conversions,
    32902.56
  )

  expect_equal(
    details$daily$verified_cash_settlements,
    90488.63
  )

  expect_equal(
    sum(details$accounting$Crédito),
    32902.56
  )

  expect_equal(
    sum(details$accounting$Débito),
    90488.63
  )
})


test_that("financial settlement retains the original reference", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-21")
  )

  expect_true(any(
    details$links$`Referência financeira` ==
      "1938143403"
  ))

  expect_true(any(
    details$cash$Referência == "1938143403"
  ))

  expect_true(all(
    details$links$Confirmação == "Confirmado"
  ))

  expect_true(all(
    details$cash$Confirmação == "Confirmado"
  ))
})


test_that("manager operations preserve their source references", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-21")
  )

  operations <- details$manager

  expect_gt(nrow(operations), 0L)

  expect_true(all(c(
    "ID operação",
    "Cotista",
    "Operação",
    "Valor previsto",
    "Arquivo",
    "Aba",
    "Linha"
  ) %in% names(operations)))

  expect_true(all(!is.na(operations$Arquivo)))
  expect_true(all(operations$Linha > 0L))
})


test_that("accounting and cash retain provenance", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-22")
  )

  expect_gt(nrow(details$accounting), 0L)
  expect_gt(nrow(details$cash), 0L)

  expect_true(all(
    details$accounting$Linha > 0L
  ))

  expect_true(all(
    details$cash$Linha > 0L
  ))

  expect_false(anyNA(details$cash$Referência))
})


test_that("days without payments remain empty", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-20")
  )

  expect_equal(nrow(details$cash), 0L)
  expect_equal(nrow(details$links), 0L)

  expect_gt(
    nrow(details$accounting),
    0L
  )
})


test_that("cash without a confirmed link is not marked verified", {
  case <- fluxos_diagnostic_fixture()

  case$reports <- unserialize(
    serialize(case$reports, NULL)
  )

  original <- case$reports$cash_statement$movements

  orphan <- original |>
    dplyr::filter(.data$reg == "1938143403")

  expect_equal(nrow(orphan), 1L)

  orphan$reg <- "orphan-test"
  orphan$source_line <- 999L

  case$reports$cash_statement$movements <- dplyr::bind_rows(
    original,
    orphan
  )

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-21")
  )

  unmatched <- details$cash |>
    dplyr::filter(.data$Referência == "orphan-test")

  expect_equal(nrow(unmatched), 1L)

  expect_identical(
    unmatched$Confirmação,
    "Não confirmado"
  )
})


test_that("daily issues are filtered by date", {
  case <- fluxos_diagnostic_fixture()

  details <- bc_fluxos_day_details(
    case$reconciliation,
    case$reports,
    as.Date("2026-10-21")
  )

  expect_true(any(
    details$issues$Pendência == "manager_difference"
  ))
})


test_that("rejects dates outside the reconciliation", {
  case <- fluxos_diagnostic_fixture()

  expect_error(
    bc_fluxos_day_details(
      case$reconciliation,
      case$reports,
      as.Date("2026-10-16")
    ),
    "não pertence ao período conciliado"
  )

  expect_error(
    bc_fluxos_day_details(
      case$reconciliation,
      case$reports,
      as.Date(NA)
    ),
    "data selecionada é inválida"
  )
})


test_that("selecting a row changes the diagnostic date", {
  case <- fluxos_diagnostic_fixture()

  reconciliation <- shiny::reactiveVal(
    case$reconciliation
  )

  reports <- shiny::reactiveVal(case$reports)
  selected_row <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_diagnostico_server,
    args = list(
      reconciliation = reconciliation,
      reports = reports,
      selected_row = selected_row
    ),
    {
      session$flushReact()

      expect_identical(
        selected_date(),
        as.Date("2026-10-22")
      )

      selected_row(3L)
      session$flushReact()

      expect_identical(
        selected_date(),
        as.Date("2026-10-21")
      )

      expect_identical(
        details()$date,
        as.Date("2026-10-21")
      )
    }
  )
})


test_that("removing a reconciliation clears the selected date", {
  case <- fluxos_diagnostic_fixture()

  reconciliation <- shiny::reactiveVal(
    case$reconciliation
  )

  reports <- shiny::reactiveVal(case$reports)
  selected_row <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_diagnostico_server,
    args = list(
      reconciliation = reconciliation,
      reports = reports,
      selected_row = selected_row
    ),
    {
      session$flushReact()

      expect_false(is.null(selected_date()))

      reconciliation(NULL)
      session$flushReact()

      expect_null(selected_date())
    }
  )
})
