
screen_test_case <- function() {
  list(
    portfolio = read_daycoval_portfolio(
      testthat::test_path(
        "fixtures",
        "daycoval",
        "Carteira Diária - 261022_155515.csv"
      )
    ),
    position = read_daycoval_position(
      testthat::test_path(
        "fixtures",
        "daycoval-position",
        "long_term_2026-10-19.csv"
      )
    ),
    xp = read_xp_statement(
      testthat::test_path(
        "fixtures",
        "xp",
        "Extrato_10850552_23_10_2026 18_01_12.pdf"
      )
    )
  )
}


test_that("fund references have unique operational mappings", {
  for (fund in c("long_term", "linear")) {
    reference <- batercota_fund_reference(fund)

    expect_gt(nrow(reference$mapping), 0L)

    expect_false(anyDuplicated(
      reference$mapping$xp_asset_name
    ) > 0L)

    expect_false(anyDuplicated(
      reference$mapping$daycoval_title
    ) > 0L)

    expect_false(
      reference$daycoval_only %in%
        reference$mapping$daycoval_title
    )
  }
})


test_that("financial tolerance defines operational status", {
  differences <- c(
    0,
    0.01,
    -0.01,
    50,
    -100,
    100,
    100.01,
    -100.01,
    NA_real_
  )

  expected <- c(
    "OK",
    "Tolerada",
    "Tolerada",
    "Tolerada",
    "Tolerada",
    "Tolerada",
    "Divergente",
    "Divergente",
    "Não avaliado"
  )

  expect_identical(
    screen_financial_status(
      differences,
      tolerance = 100
    ),
    expected
  )
})


test_that("zero tolerance requires exact financial equality", {
  expect_identical(
    screen_financial_status(
      c(0, 0.01, -0.01, NA_real_),
      tolerance = 0
    ),
    c(
      "OK",
      "Divergente",
      "Divergente",
      "Não avaliado"
    )
  )
})


test_that("rejects invalid financial tolerance", {
  expect_error(
    screen_financial_status(10, tolerance = -1)
  )

  expect_error(
    screen_financial_status(10, tolerance = NA_real_)
  )

  expect_error(
    screen_financial_status(10, tolerance = Inf)
  )
})


test_that("screen preserves original reconciliation results", {
  case <- screen_test_case()

  view <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "long_term",
    financial_tolerance = 100
  )

  expect_identical(
    view$metadata$position_date,
    as.Date("2026-10-19")
  )

  expect_equal(
    view$metadata$financial_tolerance,
    100
  )

  expect_s3_class(
    view$external,
    "positions_reconciliation"
  )

  expect_s3_class(
    view$internal,
    "daycoval_reconciliation"
  )

  expect_setequal(
    view$positions$match_id,
    view$external$matches$match_id
  )

  expect_equal(
    nrow(view$positions),
    nrow(view$external$matches)
  )

  expect_true(any(
    view$issues$issue_type == "expected_daycoval_only"
  ))

  expect_equal(
    view$positions$quantity_difference,
    view$positions$daycoval_quantity -
      view$positions$xp_quantity
  )

  expect_equal(
    view$positions$market_value_difference,
    (
      round(view$positions$daycoval_market_value * 100) -
        round(view$positions$xp_market_value * 100)
    ) / 100
  )
})


test_that("screen identifies financially relevant differences", {
  case <- screen_test_case()

  view <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "long_term",
    financial_tolerance = 100
  )

  positions <- view$positions

  expect_true(all(
    positions$status[
      !is.na(positions$market_value_difference) &
        abs(positions$market_value_difference) > 100
    ] == "Divergente"
  ))

  expect_true(all(
    positions$status[
      !is.na(positions$market_value_difference) &
        abs(positions$market_value_difference) > 0 &
        abs(positions$market_value_difference) <= 100
    ] == "Tolerada"
  ))

  expect_true(all(
    positions$status[
      !is.na(positions$market_value_difference) &
        positions$market_value_difference == 0
    ] == "OK"
  ))
})


test_that("quantity changes do not change financial status", {
  case <- screen_test_case()

  original <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "long_term"
  )

  case$xp$positions$quantity[[1L]] <-
    case$xp$positions$quantity[[1L]] + 100

  modified <- build_reconciliation_screen(
    portfolio = case$portfolio,
    position = case$position,
    xp = case$xp,
    fund = "long_term"
  )

  original_status <- original$positions |>
    dplyr::select(
      "daycoval_title",
      "status"
    ) |>
    dplyr::arrange(.data$daycoval_title)

  modified_status <- modified$positions |>
    dplyr::select(
      "daycoval_title",
      "status"
    ) |>
    dplyr::arrange(.data$daycoval_title)

  expect_identical(
    original_status,
    modified_status
  )

  expect_false(identical(
    original$positions$quantity_difference,
    modified$positions$quantity_difference
  ))
})


test_that("screen rejects a different selected fund", {
  case <- screen_test_case()

  expect_error(
    build_reconciliation_screen(
      portfolio = case$portfolio,
      position = case$position,
      xp = case$xp,
      fund = "linear"
    )
  )
})


test_that("screen rejects mismatched reporting dates", {
  case <- screen_test_case()

  case$xp$metadata$position_date <-
    as.Date("2026-10-16")

  expect_error(
    build_reconciliation_screen(
      portfolio = case$portfolio,
      position = case$position,
      xp = case$xp,
      fund = "long_term"
    )
  )
})

