
# tests/testthat/test-mod-fluxos-passivos.R

source(
  testthat::test_path(
    "..",
    "..",
    "plataforma",
    "mod_fluxos_passivos.R"
  ),
  local = TRUE
)


fluxos_test_paths <- function() {
  list(
    cash_plan = testthat::test_path(
      "..",
      "..",
      "plataforma",
      "fundos - Movimentações Previstas LINEAR.xlsx"
    ),
    movement = testthat::test_path(
      "fixtures",
      "daycoval-financial",
      "linear_movement_2026-10-16_2026-10-22.csv"
    ),
    cash = testthat::test_path(
      "fixtures",
      "daycoval-financial",
      "linear_cash_2026-10-16_2026-10-22.csv"
    ),
    opening = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "linear_2026-10-16.csv"
    ),
    checkpoint = testthat::test_path(
      "fixtures",
      "daycoval-position",
      "linear_2026-10-19.csv"
    )
  )
}


fluxos_test_inputs <- function() {
  paths <- fluxos_test_paths()

  list(
    cash_plan_path = paths$cash_plan,
    daily_movement_path = paths$movement,
    cash_statement_path = paths$cash,
    opening_position_path = paths$opening,
    reuse_position = FALSE
  )
}


test_that("UI does not contain a second fund selector", {
  ui <- mod_fluxos_passivos_ui("fluxos")
  html <- paste(as.character(ui), collapse = "")

  expect_false(grepl(
    'id="fluxos-fund"',
    html,
    fixed = TRUE
  ))

  expect_true(grepl(
    'id="fluxos-cash_plan_path"',
    html,
    fixed = TRUE
  ))
})


test_that("imports and reconstructs Linear liabilities", {
  selected_fund <- shiny::reactiveVal("linear")
  current_reports <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_passivos_server,
    args = list(
      selected_fund = selected_fund,
      current_reports = current_reports
    ),
    {
      do.call(session$setInputs, fluxos_test_inputs())

      session$setInputs(import_reports = 1L)

      expect_null(import_error())

      reports <- imported()
      result <- liability_timeline()

      expect_identical(reports$fund, "linear")

      expect_s3_class(
        result,
        "shareholder_liability_timeline"
      )

      expect_equal(
        result$timeline$date,
        as.Date(c(
          "2026-10-19",
          "2026-10-20",
          "2026-10-21",
          "2026-10-22"
        ))
      )

      expect_equal(
        result$timeline$daycoval_closing,
        c(
          156457.82,
          185636.67,
          128050.60,
          98871.75
        )
      )

      expect_equal(
        result$timeline$closing_difference,
        c(
          1532.87,
          1542.65,
          6072.66,
          6062.88
        ),
        tolerance = 0.01
      )
    }
  )
})


test_that("confirms payments using cash movements", {
  selected_fund <- shiny::reactiveVal("linear")
  current_reports <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_passivos_server,
    args = list(
      selected_fund = selected_fund,
      current_reports = current_reports
    ),
    {
      do.call(session$setInputs, fluxos_test_inputs())

      session$setInputs(import_reports = 1L)

      result <- liability_timeline()

      expect_identical(
        result$timeline$payment_status,
        c(
          "no_settlement",
          "no_settlement",
          "verified",
          "verified"
        )
      )

      expect_equal(
        result$timeline$verified_cash_settlements,
        c(0, 0, 90488.63, 29178.85)
      )

      expect_true(all(
        result$settlement_links$verification == "verified"
      ))
    }
  )
})


test_that("uses an existing position as a checkpoint", {
  paths <- fluxos_test_paths()

  selected_fund <- shiny::reactiveVal("linear")

  current_reports <- shiny::reactiveVal(list(
    fund = "linear",
    position = read_daycoval_position(paths$checkpoint)
  ))

  shiny::testServer(
    mod_fluxos_passivos_server,
    args = list(
      selected_fund = selected_fund,
      current_reports = current_reports
    ),
    {
      do.call(session$setInputs, fluxos_test_inputs())

      session$setInputs(import_reports = 1L)

      result <- liability_timeline()

      expect_null(import_error())

      expect_equal(nrow(result$checkpoints), 1L)

      expect_identical(
        result$checkpoints$date,
        as.Date("2026-10-19")
      )

      expect_identical(
        result$timeline$checkpoint_status[[1L]],
        "consistent"
      )
    }
  )
})


test_that("ignores checkpoints outside the period", {
  paths <- fluxos_test_paths()

  reports <- list(
    fund = "linear",
    fund_name = "Linear FIM CP RL",
    portfolio_code = "13711539",
    opening_date = as.Date("2026-10-19"),
    ending_date = as.Date("2026-10-22")
  )

  current <- list(
    fund = "linear",
    position = read_daycoval_position(paths$checkpoint)
  )

  checkpoints <- bc_fluxos_available_checkpoints(
    reports,
    current
  )

  expect_length(checkpoints, 0L)
})


test_that("fund changes invalidate previous reconciliation", {
  selected_fund <- shiny::reactiveVal("linear")
  current_reports <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_passivos_server,
    args = list(
      selected_fund = selected_fund,
      current_reports = current_reports
    ),
    {
      do.call(session$setInputs, fluxos_test_inputs())

      session$setInputs(import_reports = 1L)

      expect_false(is.null(liability_timeline()))

      selected_fund("long_term")
      session$flushReact()

      expect_null(imported())
      expect_null(liability_timeline())
    }
  )
})


test_that("rejects reports for a different selected fund", {
  selected_fund <- shiny::reactiveVal("long_term")
  current_reports <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_fluxos_passivos_server,
    args = list(
      selected_fund = selected_fund,
      current_reports = current_reports
    ),
    {
      do.call(session$setInputs, fluxos_test_inputs())

      session$setInputs(import_reports = 1L)

      expect_null(liability_timeline())

      expect_match(
        import_error(),
        "não pertencem ao fundo selecionado"
      )
    }
  )
})


test_that("daily table retains separate reconciliation statuses", {
  paths <- fluxos_test_paths()

  result <- build_shareholder_liability_timeline(
    cash_plan = read_fund_cash_plan(paths$cash_plan),
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

  expect_true(all(c(
    "checkpoint_status",
    "payment_status",
    "manager_status"
  ) %in% names(result$timeline)))

  expect_true(any(
    result$timeline$manager_status == "requires_review"
  ))

  expect_true(any(
    result$timeline$payment_status == "verified"
  ))

  expect_true(any(
    result$timeline$checkpoint_status == "not_checked"
  ))
})

