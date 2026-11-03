
# R/return-partial-attribution.R

return_reconciliation_add_partial <- function(reconciliation) {
  if (!inherits(reconciliation, "portfolio_return_reconciliation")) {
    cli::cli_abort("Objeto de conciliação de rentabilidade inválido.")
  }

  components <- reconciliation$components
  summary <- reconciliation$summary
  instruments <- reconciliation$details$instruments
  expenses <- reconciliation$details$accounting_expenses

  previous_nav <- summary$previous_nav[[1L]]
  reported_result <- summary$reported_result_equivalent[[1L]]
  reported_return <- summary$reported_return_percent[[1L]]

  components$partial_estimated_result <- components$estimated_result

  expense_index <- which(
    components$component == "accounting_expenses"
  )

  if (length(expense_index) == 1L) {
    expense_summary <- expenses$summary

    expense_has_estimate <-
      expense_summary$n_estimated[[1L]] > 0L ||
      expense_summary$n_unestimated[[1L]] == 0L

    components$partial_estimated_result[expense_index] <- if (
      expense_has_estimate
    ) {
      expense_summary$partial_estimated_effect[[1L]]
    } else {
      NA_real_
    }
  }

  empty_components <- components$n_positions == 0L &
    components$n_unestimated == 0L

  components$partial_estimated_result[empty_components] <-
    dplyr::coalesce(
      components$partial_estimated_result[empty_components],
      0
    )

  components$partial_contribution_pp <- 100 *
    components$partial_estimated_result / previous_nav

  n_estimated <- sum(components$n_estimated)
  n_unestimated <- sum(components$n_unestimated)
  n_positions <- sum(components$n_positions)

  valid_components <- is.finite(
    components$partial_estimated_result
  )

  has_known_result <- any(
    valid_components & components$n_estimated > 0L
  )

  partial_estimated_result <- if (
    has_known_result || n_unestimated == 0L
  ) {
    sum(components$partial_estimated_result[valid_components])
  } else {
    NA_real_
  }

  partial_estimated_return <- 100 *
    partial_estimated_result / previous_nav

  position_coverage <- if (n_positions > 0L) {
    100 * n_estimated / n_positions
  } else {
    NA_real_
  }

  opening_coverage <- return_partial_opening_coverage(
    instruments$details
  )

  covered_opening_nav <- if (
    is.finite(opening_coverage$covered_value)
  ) {
    100 * opening_coverage$covered_value / previous_nav
  } else {
    NA_real_
  }

  components$estimate_status <- dplyr::case_when(
    components$n_unestimated == 0L ~ "complete",
    components$n_estimated > 0L ~ "partial",
    TRUE ~ "not_estimated"
  )

  summary$partial_estimated_result <- partial_estimated_result

  summary$partial_estimated_return_percent <-
    partial_estimated_return

  summary$partial_difference_pp <-
    partial_estimated_return - reported_return

  summary$partial_residual_brl <-
    partial_estimated_result - reported_result

  summary$n_estimated <- n_estimated
  summary$n_positions <- n_positions
  summary$position_coverage_percent <- position_coverage

  summary$opening_investment_value <-
    opening_coverage$total_value

  summary$covered_opening_investment_value <-
    opening_coverage$covered_value

  summary$opening_investment_coverage_percent <-
    opening_coverage$coverage_percent

  summary$covered_opening_nav_percent <- covered_opening_nav

  summary$estimate_completeness <- if (
    n_unestimated > 0L ||
    !is.finite(summary$estimated_result[[1L]])
  ) {
    "incomplete"
  } else {
    "complete"
  }

  reconciliation$components <- components
  reconciliation$summary <- summary

  reconciliation
}


return_partial_opening_coverage <- function(instrument_details) {
  investment_classes <- c(
    "funds",
    "fixed_income",
    "equities"
  )

  positions <- investment_classes |>
    lapply(function(component) {
      instrument_details[[component]]$contributions |>
        dplyr::select(
          "source_line_previous",
          "market_value_previous",
          "estimated_result"
        )
    }) |>
    dplyr::bind_rows()

  opening <- positions |>
    dplyr::filter(!is.na(.data$source_line_previous))

  unavailable <- list(
    total_value = NA_real_,
    covered_value = NA_real_,
    coverage_percent = NA_real_
  )

  if (
    nrow(opening) == 0L ||
    any(!is.finite(opening$market_value_previous))
  ) {
    return(unavailable)
  }

  opening_values <- abs(opening$market_value_previous)
  total_value <- sum(opening_values)

  if (!is.finite(total_value) || total_value <= 0) {
    return(unavailable)
  }

  estimated <- is.finite(opening$estimated_result)

  covered_value <- sum(opening_values[estimated])

  list(
    total_value = total_value,
    covered_value = covered_value,
    coverage_percent = 100 * covered_value / total_value
  )
}
