#' Estimate consolidated portfolio return
#'
#' Combines estimated returns from invested funds, direct fixed
#' income, equities, and reported futures adjustments.
#'
#' The estimate excludes expenses, cash returns, distributions,
#' and other accounting components not yet modeled.
#'
#' The comparison with the administrator's reported return is
#' provisional, even when the difference is within tolerance.
#'
#' @param previous Earlier `daycoval_position` object.
#' @param current Later `daycoval_position` object.
#' @param tolerance_pp Non-negative comparison tolerance in
#'   percentage points. Default is 0.05.
#'
#' @return A `portfolio_return_estimate` object containing
#'   metadata, components, summary, issues, and details.
#'
#' @export
estimate_portfolio_return <- function(
    previous,
    current,
    tolerance_pp = 0.05
) {
  direct_attribution_validate(previous, current)

  return_estimate_validate_tolerance(tolerance_pp)

  funds <- estimate_fund_contributions(previous, current)

  fixed_income <- estimate_fixed_income_contributions(
    previous,
    current
  )

  equities <- estimate_equity_contributions(
    previous,
    current
  )

  previous_nav <- previous$metadata$patrimonio_liquido

  futures <- estimate_futures_adjustments(
    position = current,
    previous_nav = previous_nav
  )

  futures_quantities <- check_futures_quantity_changes(
    previous,
    current
  )

  components <- dplyr::bind_rows(
    return_estimate_component("funds", funds),
    return_estimate_component(
      "fixed_income",
      fixed_income
    ),
    return_estimate_component("equities", equities),
    return_estimate_component("futures", futures)
  )

  estimated_result <- if (
    all(is.finite(components$estimated_result))
  ) {
    sum(components$estimated_result)
  } else {
    NA_real_
  }

  estimated_return <- 100 *
    estimated_result / previous_nav

  previous_price <- previous$metadata$valor_cota
  current_price <- current$metadata$valor_cota

  return_estimate_validate_prices(
    previous_price,
    current_price
  )

  reported_return <- 100 * (
    current_price / previous_price - 1
  )

  difference_pp <- estimated_return - reported_return

  n_unestimated <- sum(components$n_unestimated)

  comparison_status <- dplyr::case_when(
    !is.finite(difference_pp) ||
      n_unestimated > 0L ~ "not_evaluated",
    abs(difference_pp) <= tolerance_pp ~
      "within_tolerance",
    TRUE ~ "outside_tolerance"
  )

  issues <- return_estimate_issues(
    funds,
    fixed_income,
    equities,
    futures,
    futures_quantities
  )

  pending_operations <- issues |>
    dplyr::filter(
      .data$issue_type %in% c(
        "changed_quantity",
        "entered",
        "exited",
        "entry_requires_operations",
        "exit_requires_operations",
        "quantity_change_requires_operations"
      )
    )

  summary <- tibble::tibble(
    previous_nav = previous_nav,
    estimated_result = estimated_result,
    estimated_return_percent = estimated_return,
    reported_return_percent = reported_return,
    difference_pp = difference_pp,
    tolerance_pp = tolerance_pp,
    comparison_status = comparison_status,
    coverage_status = "partial_model",
    n_unestimated = n_unestimated,
    n_pending_operations = nrow(pending_operations)
  )

  structure(
    list(
      metadata = list(
        codigo_carteira = previous$metadata$codigo_carteira,
        previous_date = previous$metadata$data_posicao,
        current_date = current$metadata$data_posicao,
        methodology = "instrument_return_attribution",
        coverage_scope = "financial_instruments_only"
      ),
      components = components,
      summary = summary,
      issues = issues,
      details = list(
        funds = funds,
        fixed_income = fixed_income,
        equities = equities,
        futures = futures,
        futures_quantities = futures_quantities
      )
    ),
    class = "portfolio_return_estimate"
  )
}


return_estimate_validate_tolerance <- function(tolerance_pp) {
  valid <- is.numeric(tolerance_pp) &&
    length(tolerance_pp) == 1L &&
    !is.na(tolerance_pp) &&
    is.finite(tolerance_pp) &&
    tolerance_pp >= 0

  if (!valid) {
    cli::cli_abort(
      "`tolerance_pp` deve ser um número finito não negativo."
    )
  }

  invisible(tolerance_pp)
}


return_estimate_validate_prices <- function(
    previous_price,
    current_price
) {
  prices <- c(previous_price, current_price)

  if (
    !is.numeric(prices) ||
    length(prices) != 2L ||
    any(!is.finite(prices)) ||
    any(prices <= 0)
  ) {
    cli::cli_abort(
      "As cotas das duas datas devem ser positivas e finitas."
    )
  }

  invisible(TRUE)
}


return_estimate_component <- function(name, object) {
  summary <- object$summary

  tibble::tibble(
    component = name,
    estimated_result = summary$estimated_result,
    contribution_pp = summary$estimated_contribution_pp,
    n_positions = summary$n_positions,
    n_estimated = summary$n_estimated,
    n_unestimated = summary$n_unestimated
  )
}


return_estimate_issues <- function(
    funds,
    fixed_income,
    equities,
    futures,
    futures_quantities
) {
  dplyr::bind_rows(
    funds$issues |>
      dplyr::transmute(
        component = "funds",
        issue_type = .data$issue_type,
        identifier = .data$isin,
        title = .data$title
      ),
    fixed_income$issues |>
      dplyr::transmute(
        component = "fixed_income",
        issue_type = .data$issue_type,
        identifier = .data$isin,
        title = .data$title
      ),
    equities$issues |>
      dplyr::transmute(
        component = "equities",
        issue_type = .data$issue_type,
        identifier = .data$isin,
        title = .data$title
      ),
    futures$issues |>
      dplyr::transmute(
        component = "futures",
        issue_type = .data$issue_type,
        identifier = .data$isin,
        title = .data$contract
      ),
    futures_quantities$issues |>
      dplyr::transmute(
        component = "futures_quantities",
        issue_type = .data$issue_type,
        identifier = .data$isin,
        title = .data$contract
      )
  )
}
