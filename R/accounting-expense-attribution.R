#' Estimate accounting expense effects
#'
#' Estimates expense-related changes using two Daycoval
#' daily portfolios.
#'
#' Includes accrued expense liabilities in accounting groups
#' 23500, 23700, and 23800, and deferred expense assets in
#' group 19700.
#'
#' Positive effects are not automatically treated as gains,
#' because they may reflect settlements or new prepayments.
#'
#' Shareholder flows, futures, invested assets, and other
#' accounting groups are excluded.
#'
#' @param previous Earlier `daycoval_portfolio` object.
#' @param current Later `daycoval_portfolio` object.
#'
#' @return An `accounting_expense_attribution` object.
#'
#' @export
estimate_accounting_expenses <- function(
    previous,
    current
) {
  accounting <- reconcile_accounting_changes(
    previous,
    current
  )

  source_status <- c(
    accounting$previous$summary$status,
    accounting$current$summary$status
  )

  if (any(source_status != "equal")) {
    cli::cli_abort(
      "As carteiras devem fechar contabilmente antes
       da estimativa de despesas."
    )
  }

  effects <- accounting_expense_effects(
    accounting$changes
  )

  other_payables <- accounting$changes |>
    dplyr::filter(
      .data$grp_n1 == "20000",
      .data$grp_n2 == "23700",
      .data$grp_n3 == "23720"
    ) |>
    dplyr::filter(
      is.na(.data$change) |
        abs(.data$change) > 0.01
    )

  payable_issues <- other_payables |>
    dplyr::transmute(
      issue_type = "payable_balance_change",
      accounting_component = "other_payables",
      accounting_group = .data$grp_n3,
      group_name = dplyr::coalesce(
        .data$group_name_current,
        .data$group_name_previous
      ),
      previous_amount = .data$previous_amount,
      current_amount = .data$current_amount,
      change = .data$change
    )

  estimated <- effects$status == "estimated"

  partial_effect <- sum(
    effects$estimated_effect[estimated]
  )

  complete <- all(estimated)

  estimated_effect <- if (complete) {
    partial_effect
  } else {
    NA_real_
  }

  status <- dplyr::case_when(
    nrow(effects) == 0L ~ "no_expense_accounts",
    !complete ~ "pending_review",
    TRUE ~ "estimated"
  )

  summary <- tibble::tibble(
    estimated_effect = estimated_effect,
    partial_estimated_effect = partial_effect,
    n_accounts = nrow(effects),
    n_estimated = sum(estimated),
    n_unestimated = sum(!estimated),
    status = status
  )

  issues <- effects |>
    dplyr::filter(.data$status != "estimated") |>
    dplyr::transmute(
      issue_type = .data$status,
      accounting_component = .data$component,
      accounting_group = .data$grp_n3,
      group_name = .data$group_name,
      previous_amount = .data$previous_amount,
      current_amount = .data$current_amount,
      change = .data$change
    )

  issues <- dplyr::bind_rows(
    issues,
    payable_issues
  )

  structure(
    list(
      metadata = accounting$metadata,
      effects = effects,
      summary = summary,
      issues = issues,
      accounting = accounting
    ),
    class = "accounting_expense_attribution"
  )
}


accounting_expense_effects <- function(changes) {
  changes |>
    dplyr::filter(
      (
        .data$grp_n1 == "10000" &
          .data$grp_n2 == "19700"
      ) |
        (
          .data$grp_n1 == "20000" &
            .data$grp_n2 %in% c(
              "23500",
              "23700",
              "23800"
            )
        )
    ) |>
    dplyr::filter(
      !(
        .data$grp_n1 == "20000" &
          .data$grp_n2 == "23700" &
          .data$grp_n3 == "23720"
      )
    ) |>
    dplyr::mutate(
      component = dplyr::if_else(
        .data$grp_n1 == "10000",
        "deferred_expenses",
        "accrued_expenses"
      ),
      group_name = dplyr::coalesce(
        .data$group_name_current,
        .data$group_name_previous
      ),
      status = dplyr::case_when(
        !is.finite(.data$change) ~ "invalid_change",
        .data$change > 0 ~ "requires_cash_flow_review",
        TRUE ~ "estimated"
      ),
      estimated_effect = dplyr::if_else(
        .data$status == "estimated",
        .data$change,
        NA_real_
      )
    )
}


#' Reconcile estimated portfolio return with expense effects
#'
#' Combines instrument-level return attribution with changes
#' in expense-related accounting balances.
#'
#' The resulting return remains an approximation. Shareholder
#' flows, corporate actions, and other unmodeled accounting
#' components may explain residual differences.
#'
#' @param previous_position Earlier `daycoval_position`.
#' @param current_position Later `daycoval_position`.
#' @param previous_portfolio Earlier `daycoval_portfolio`.
#' @param current_portfolio Later `daycoval_portfolio`.
#' @param tolerance_pp Return tolerance in percentage points.
#'
#' @return A `portfolio_return_reconciliation` object.
#'
#' @export
reconcile_portfolio_return <- function(
    previous_position,
    current_position,
    previous_portfolio,
    current_portfolio,
    tolerance_pp = 0.05
) {
  return_estimate_validate_tolerance(tolerance_pp)

  return_reconciliation_validate_sources(
    previous_position,
    previous_portfolio
  )

  return_reconciliation_validate_sources(
    current_position,
    current_portfolio
  )

  instruments <- estimate_portfolio_return(
    previous = previous_position,
    current = current_position,
    tolerance_pp = tolerance_pp
  )

  expenses <- estimate_accounting_expenses(
    previous = previous_portfolio,
    current = current_portfolio
  )

  previous_nav <- previous_position$metadata$patrimonio_liquido

  components <- dplyr::bind_rows(
    instruments$components,
    tibble::tibble(
      component = "accounting_expenses",
      estimated_result = expenses$summary$estimated_effect,
      contribution_pp = 100 *
        expenses$summary$estimated_effect / previous_nav,
      n_positions = expenses$summary$n_accounts,
      n_estimated = expenses$summary$n_estimated,
      n_unestimated = expenses$summary$n_unestimated
    )
  )

  total_unestimated <- sum(components$n_unestimated)

  estimated_result <- if (
    total_unestimated == 0L &&
    all(is.finite(components$estimated_result))
  ) {
    sum(components$estimated_result)
  } else {
    NA_real_
  }

  estimated_return <- 100 *
    estimated_result / previous_nav

  reported_return <-
    instruments$summary$reported_return_percent

  reported_equivalent <- previous_nav *
    reported_return / 100

  difference_pp <- estimated_return - reported_return

  comparison_status <- dplyr::case_when(
    !is.finite(difference_pp) ~ "not_evaluated",
    abs(difference_pp) <= tolerance_pp ~
      "within_tolerance",
    TRUE ~ "outside_tolerance"
  )

  expense_issues <- expenses$issues |>
    dplyr::transmute(
      component = "accounting_expenses",
      issue_type = .data$issue_type,
      identifier = .data$accounting_group,
      title = .data$group_name
    )

  issues <- dplyr::bind_rows(
    instruments$issues,
    expense_issues
  )

  summary <- tibble::tibble(
    previous_nav = previous_nav,
    instruments_result =
      instruments$summary$estimated_result,
    accounting_expense_effect =
      expenses$summary$estimated_effect,
    estimated_result = estimated_result,
    reported_result_equivalent = reported_equivalent,
    estimated_return_percent = estimated_return,
    reported_return_percent = reported_return,
    residual_brl = estimated_result -
      reported_equivalent,
    difference_pp = difference_pp,
    tolerance_pp = tolerance_pp,
    comparison_status = comparison_status,
    coverage_status = "partial_model",
    n_unestimated = total_unestimated,
    n_pending_operations =
      instruments$summary$n_pending_operations,
    n_expense_issues = nrow(expense_issues)
  )

  structure(
    list(
      metadata = list(
        codigo_carteira =
          previous_position$metadata$codigo_carteira,
        previous_date =
          previous_position$metadata$data_posicao,
        current_date =
          current_position$metadata$data_posicao,
        methodology = "instruments_plus_accounting_expenses"
      ),
      components = components,
      summary = summary,
      issues = issues,
      details = list(
        instruments = instruments,
        accounting_expenses = expenses
      )
    ),
    class = "portfolio_return_reconciliation"
  )
}


return_reconciliation_validate_sources <- function(
    position,
    portfolio
) {
  if (
    !inherits(position, "daycoval_position") ||
    !inherits(portfolio, "daycoval_portfolio")
  ) {
    cli::cli_abort(
      "São necessários relatórios válidos de Posição C
       e Carteira Diária."
    )
  }

  position_metadata <- position$metadata
  portfolio_metadata <- portfolio$metadata

  same_fund <- identical(
    position_metadata$codigo_carteira,
    portfolio_metadata$codigo_carteira
  )

  same_date <- identical(
    position_metadata$data_posicao,
    portfolio_metadata$data_posicao
  )

  values <- c(
    position_metadata$patrimonio_liquido,
    portfolio_metadata$patrimonio_liquido
  )

  same_nav <- length(values) == 2L &&
    is.numeric(values) &&
    all(is.finite(values)) &&
    identical(
      round(values[[1L]] * 100),
      round(values[[2L]] * 100)
    )

  if (!same_fund || !same_date || !same_nav) {
    cli::cli_abort(
      "A Posição C e a Carteira Diária devem ter
       o mesmo fundo, a mesma data e o mesmo PL."
    )
  }

  invisible(TRUE)
}
