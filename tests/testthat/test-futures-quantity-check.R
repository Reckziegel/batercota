futures_check_fixture <- function(fund, date) {
  read_daycoval_position(
    testthat::test_path(
      "fixtures",
      "daycoval-position",
      paste0(fund, "_", date, ".csv")
    )
  )
}


test_that("confirms unchanged futures quantities", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  result <- check_futures_quantity_changes(previous, current)

  expect_s3_class(result, "futures_quantity_check")

  expect_equal(result$summary$n_positions, 1L)
  expect_equal(result$summary$n_unchanged, 1L)
  expect_equal(result$summary$n_pending, 0L)

  expect_identical(result$summary$status, "ok")
  expect_equal(result$changes$quantity_previous, 8)
  expect_equal(result$changes$quantity_current, 8)

  expect_equal(
    result$changes$adjustment_current,
    -320.56
  )

  expect_equal(nrow(result$issues), 0L)
})


test_that("flags quantity changes for operation checks", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$futures$qtd[[1L]] <- 7

  result <- check_futures_quantity_changes(previous, current)

  expect_equal(result$summary$n_pending, 1L)

  expect_identical(
    result$summary$status,
    "pending_operations"
  )

  expect_equal(result$changes$quantity_change, -1)

  expect_identical(
    result$changes$status,
    "quantity_change_requires_operations"
  )

  expect_equal(
    result$changes$adjustment_current,
    -320.56
  )
})


test_that("flags futures exits without imputing quantities", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  current$positions$futures <-
    current$positions$futures[0, ]

  result <- check_futures_quantity_changes(previous, current)

  expect_identical(
    result$changes$status,
    "exit_requires_operations"
  )

  expect_true(is.na(result$changes$quantity_change))
  expect_equal(result$summary$n_pending, 1L)
})


test_that("supports funds without futures positions", {
  previous <- futures_check_fixture(
    "linear",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "linear",
    "2026-10-19"
  )

  result <- check_futures_quantity_changes(previous, current)

  expect_equal(nrow(result$changes), 0L)
  expect_equal(result$summary$n_pending, 0L)

  expect_identical(
    result$summary$status,
    "no_positions"
  )
})


test_that("does not associate missing futures ISINs", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  previous$positions$futures$isin[[1L]] <- NA_character_
  current$positions$futures$isin[[1L]] <- NA_character_

  result <- check_futures_quantity_changes(previous, current)

  expect_equal(
    sum(result$changes$status == "missing_isin"),
    2L
  )

  expect_equal(result$summary$n_pending, 2L)
})


test_that("rejects incompatible dates", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  expect_error(
    check_futures_quantity_changes(previous, current)
  )
})


test_that("does not mutate futures positions", {
  previous <- futures_check_fixture(
    "long_term",
    "2026-10-16"
  )

  current <- futures_check_fixture(
    "long_term",
    "2026-10-19"
  )

  original_previous <- previous
  original_current <- current

  check_futures_quantity_changes(previous, current)

  expect_identical(previous, original_previous)
  expect_identical(current, original_current)
})
