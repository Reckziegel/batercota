#' Diagnose shareholder flows and quota adjustments
#'
#' Compares the net shareholder flow implied by the change
#' in fund shares with the flow reported by Daycoval.
#'
#' Also identifies changes in accounting group 24005,
#' without treating those changes as investment performance.
#'
#' @param previous_position Earlier `daycoval_position`.
#' @param current_position Later `daycoval_position`.
#' @param previous_portfolio Earlier `daycoval_portfolio`.
#' @param current_portfolio Later `daycoval_portfolio`.
#' @param tolerance_brl Non-negative tolerance in BRL.
#'
#' @return A `shareholder_flow_diagnostics` object.
#'
#' @export
diagnose_shareholder_flows <- function(
    previous_position,
    current_position,
    previous_portfolio,
    current_portfolio,
    tolerance_brl = 0.05
) {
  direct_attribution_validate(
    previous_position,
    current_position
  )

  return_reconciliation_validate_sources(
    previous_position,
    previous_portfolio
  )

  return_reconciliation_validate_sources(
    current_position,
    current_portfolio
  )

  accounting_validate_tolerance(tolerance_brl)

  accounting <- reconcile_accounting_changes(
    previous_portfolio,
    current_portfolio
  )

  source_status <- c(
    accounting$previous$summary$status,
    accounting$current$summary$status
  )

  if (any(source_status != "equal")) {
    cli::cli_abort(
      "As carteiras devem fechar contabilmente."
    )
  }

  previous_shares <-
    previous_position$metadata$numero_cotas

  current_shares <-
    current_position$metadata$numero_cotas

  current_price <-
    current_position$metadata$valor_cota

  values <- c(
    previous_shares,
    current_shares,
    current_price
  )

  if (
    !is.numeric(values) ||
    length(values) != 3L ||
    any(!is.finite(values)) ||
    any(values <= 0)
  ) {
    cli::cli_abort(
      "Quantidades de cotas e valor da cota
       devem ser positivos e finitos."
    )
  }

  reported_flows <- shareholder_flow_record(
    current_position
  )

  share_change <- current_shares - previous_shares

  implied_net_flow <- round(
    share_change * current_price,
    2
  )

  reported_applications <- NA_real_
  reported_redemptions <- NA_real_
  reported_net_flow <- NA_real_

  if (nrow(reported_flows) == 1L) {
    reported_applications <-
      reported_flows$applications[[1L]]

    reported_redemptions <-
      reported_flows$redemptions[[1L]]

    reported_net_flow <-
      reported_flows$net_flow[[1L]]
  }

  flow_difference <- if (
    is.na(reported_net_flow)
  ) {
    NA_real_
  } else {
    round(
      implied_net_flow - reported_net_flow,
      2
    )
  }

  adjustments <- accounting$changes |>
    dplyr::filter(
      .data$grp_n1 == "20000",
      .data$grp_n2 == "24000",
      .data$grp_n3 == "24005"
    )

  adjustment_effect <- if (nrow(adjustments) == 0L) {
    0
  } else {
    sum(adjustments$change)
  }

  difference_vs_adjustment <- if (
    is.na(flow_difference)
  ) {
    NA_real_
  } else {
    round(
      flow_difference - adjustment_effect,
      2
    )
  }

  status <- dplyr::case_when(
    is.na(flow_difference) ~ "missing_flow_report",
    abs(flow_difference) <= tolerance_brl ~
      "within_tolerance",
    abs(difference_vs_adjustment) <= tolerance_brl ~
      "consistent_with_quota_adjustment",
    TRUE ~ "requires_review"
  )

  issues <- tibble::tibble(
    issue_type = character(),
    difference_brl = double()
  )

  if (status != "within_tolerance") {
    issues <- tibble::tibble(
      issue_type = status,
      difference_brl = flow_difference
    )
  }

  summary <- tibble::tibble(
    previous_shares = previous_shares,
    current_shares = current_shares,
    share_change = share_change,
    current_share_price = current_price,
    implied_net_flow = implied_net_flow,
    reported_applications = reported_applications,
    reported_redemptions = reported_redemptions,
    reported_net_flow = reported_net_flow,
    flow_difference = flow_difference,
    quota_adjustment_effect = adjustment_effect,
    difference_vs_adjustment = difference_vs_adjustment,
    tolerance_brl = tolerance_brl,
    status = status
  )

  structure(
    list(
      metadata = list(
        codigo_carteira =
          current_position$metadata$codigo_carteira,
        previous_date =
          previous_position$metadata$data_posicao,
        current_date =
          current_position$metadata$data_posicao,
        methodology = "share_change_vs_reported_flows"
      ),
      summary = summary,
      reported_flows = reported_flows,
      adjustments = adjustments,
      issues = issues,
      accounting = accounting
    ),
    class = "shareholder_flow_diagnostics"
  )
}


shareholder_flow_record <- function(position) {
  flows <- position$shareholder_flows

  if (is.null(flows) || nrow(flows) == 0L) {
    return(tibble::tibble(
      applications = double(),
      redemptions = double(),
      net_flow = double(),
      source_file = character(),
      source_line = integer(),
      record_number = character()
    ))
  }

  required_columns <- c(
    "tot_apl",
    "tot_rgt",
    "source_file",
    "source_line",
    "record_number"
  )

  missing_columns <- setdiff(
    required_columns,
    names(flows)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas de movimentação de cotistas ausentes:
       {.or {missing_columns}}."
    )
  }

  if (nrow(flows) != 1L) {
    cli::cli_abort(
      "Esperado um registro de movimentação
       de cotistas para a data."
    )
  }

  values <- c(
    flows$tot_apl,
    flows$tot_rgt
  )

  if (
    !is.numeric(values) ||
    any(!is.finite(values)) ||
    any(values < 0)
  ) {
    cli::cli_abort(
      "Aplicações e resgates devem ser
       valores numéricos não negativos."
    )
  }

  flows |>
    dplyr::transmute(
      applications = .data$tot_apl,
      redemptions = .data$tot_rgt,
      net_flow = .data$tot_apl - .data$tot_rgt,
      source_file = .data$source_file,
      source_line = .data$source_line,
      record_number = .data$record_number
    )
}
