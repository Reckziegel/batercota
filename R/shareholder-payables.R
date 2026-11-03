
#' Reconcile outstanding shareholder redemptions
#'
#' Compares Daycoval's outstanding redemption liabilities
#' with the movement implied by the manager's cash plan.
#'
#' Settlement dates in the cash plan are scheduled dates,
#' not evidence of actual payment.
#'
#' @param cash_plan A `fund_cash_plan` object.
#' @param previous_position Previous `daycoval_position`.
#' @param current_position Current `daycoval_position`.
#' @param tolerance_brl Absolute tolerance in BRL.
#' @param tolerance_pct Relative tolerance as a fraction.
#'
#' @return A `shareholder_payables_reconciliation` object.
#'
#' @export
reconcile_shareholder_payables <- function(
    cash_plan,
    previous_position,
    current_position,
    tolerance_brl = 100,
    tolerance_pct = 0.15
) {
  shareholder_payables_validate(
    cash_plan,
    previous_position,
    current_position,
    tolerance_brl,
    tolerance_pct
  )

  previous_date <- previous_position$metadata$data_posicao
  current_date <- current_position$metadata$data_posicao

  previous <- shareholder_payables_extract(previous_position)
  current <- shareholder_payables_extract(current_position)

  redemptions <- cash_plan$shareholder_operations |>
    dplyr::filter(
      grepl("^Resgate", .data$operation_type)
    ) |>
    dplyr::mutate(
      valid_record = dplyr::coalesce(
        .data$operation_type %in% c(
          "Resgate Total",
          "Resgate Bruto",
          "Resgate Cotas"
        ) &
          !is.na(.data$conversion_date) &
          !is.na(.data$liquidation_date) &
          !is.na(.data$gross_value) &
          is.finite(.data$gross_value) &
          .data$gross_value >= 0 &
          .data$liquidation_date >= .data$conversion_date,
        FALSE
      )
    )

  invalid_operations <- redemptions |>
    dplyr::filter(!.data$valid_record)

  valid_operations <- redemptions |>
    dplyr::filter(.data$valid_record)

  opening_operations <- valid_operations |>
    dplyr::filter(
      .data$conversion_date <= .env$previous_date,
      .data$liquidation_date > .env$previous_date
    )

  closing_operations <- valid_operations |>
    dplyr::filter(
      .data$conversion_date <= .env$current_date,
      .data$liquidation_date > .env$current_date
    )

  new_conversions <- valid_operations |>
    dplyr::filter(
      .data$conversion_date > .env$previous_date,
      .data$conversion_date <= .env$current_date
    )

  scheduled_payments <- valid_operations |>
    dplyr::filter(
      .data$liquidation_date > .env$previous_date,
      .data$liquidation_date <= .env$current_date
    )

  planned_opening <- round(
    sum(opening_operations$gross_value),
    2
  )

  planned_closing <- round(
    sum(closing_operations$gross_value),
    2
  )

  planned_new <- round(
    sum(new_conversions$gross_value),
    2
  )

  planned_payments <- round(
    sum(scheduled_payments$gross_value),
    2
  )

  reported_opening <- previous$amount
  reported_closing <- current$amount

  reported_change <- round(
    reported_closing - reported_opening,
    2
  )

  planned_change <- round(
    planned_new - planned_payments,
    2
  )

  anchored_closing <- round(
    reported_opening + planned_change,
    2
  )

  difference_brl <- round(
    reported_closing - anchored_closing,
    2
  )

  allowed_difference <- if (
    is.na(reported_change)
  ) {
    NA_real_
  } else {
    max(
      tolerance_brl,
      tolerance_pct * max(
        abs(reported_change),
        abs(planned_new),
        abs(planned_payments)
      )
    )
  }

  opening_gap <- round(
    reported_opening - planned_opening,
    2
  )

  closing_gap <- round(
    reported_closing - planned_closing,
    2
  )

  opening_allowance <- if (
    is.na(reported_opening)
  ) {
    NA_real_
  } else {
    max(
      tolerance_brl,
      tolerance_pct * max(
        abs(reported_opening),
        abs(planned_opening)
      )
    )
  }

  closing_allowance <- if (
    is.na(reported_closing)
  ) {
    NA_real_
  } else {
    max(
      tolerance_brl,
      tolerance_pct * max(
        abs(reported_closing),
        abs(planned_closing)
      )
    )
  }

  incomplete_internal <- nrow(invalid_operations) > 0L

  stock_status <- dplyr::case_when(
    is.na(reported_opening) |
      is.na(reported_closing) ~ "missing_daycoval",
    incomplete_internal ~ "incomplete_internal",
    abs(closing_gap) <= closing_allowance ~
      "within_tolerance",
    abs(opening_gap) > opening_allowance ~
      "opening_unmatched",
    TRUE ~ "requires_review"
  )

  bridge_status <- dplyr::case_when(
    is.na(reported_opening) |
      is.na(reported_closing) ~ "missing_daycoval",
    incomplete_internal ~ "incomplete_internal",
    abs(difference_brl) <= allowed_difference ~
      "within_tolerance_conditional",
    TRUE ~ "requires_review"
  )

  current_flows <- shareholder_flow_record(
    current_position
  )

  daycoval_new <- if (nrow(current_flows) == 1L) {
    current_flows$redemptions[[1L]]
  } else {
    NA_real_
  }

  summary <- tibble::tibble(
    previous_date = previous_date,
    current_date = current_date,
    reported_opening = reported_opening,
    reported_closing = reported_closing,
    reported_change = reported_change,
    planned_opening = planned_opening,
    planned_closing = planned_closing,
    opening_gap = opening_gap,
    closing_gap = closing_gap,
    planned_new_conversions = planned_new,
    planned_payments = planned_payments,
    planned_change = planned_change,
    daycoval_new_redemptions = daycoval_new,
    anchored_closing = anchored_closing,
    difference_brl = difference_brl,
    allowed_difference = allowed_difference,
    stock_status = stock_status,
    bridge_status = bridge_status
  )

  issues <- invalid_operations |>
    dplyr::transmute(
      issue_type = "invalid_internal_operation",
      source_file = .data$source_file,
      source_row = .data$source_row
    )

  if (stock_status == "opening_unmatched") {
    issues <- dplyr::bind_rows(
      issues,
      tibble::tibble(
        issue_type = "opening_unmatched",
        source_file = NA_character_,
        source_row = NA_integer_
      )
    )
  }

  if (!bridge_status %in% c(
    "within_tolerance_conditional"
  )) {
    issues <- dplyr::bind_rows(
      issues,
      tibble::tibble(
        issue_type = bridge_status,
        source_file = NA_character_,
        source_row = NA_integer_
      )
    )
  }

  structure(
    list(
      metadata = list(
        codigo_carteira =
          current_position$metadata$codigo_carteira,
        previous_date = previous_date,
        current_date = current_date,
        tolerance_brl = tolerance_brl,
        tolerance_pct = tolerance_pct,
        methodology = "opening_liability_plus_planned_flows",
        settlement_verified = FALSE
      ),
      summary = summary,
      opening_operations = opening_operations,
      closing_operations = closing_operations,
      new_conversions = new_conversions,
      scheduled_payments = scheduled_payments,
      invalid_operations = invalid_operations,
      previous_payables = previous$records,
      current_payables = current$records,
      issues = issues
    ),
    class = "shareholder_payables_reconciliation"
  )
}


shareholder_payables_extract <- function(position) {
  payables <- position$payables

  if (is.null(payables)) {
    return(list(
      amount = NA_real_,
      records = tibble::tibble()
    ))
  }

  required <- c("grupo", "valor", "source_line")

  if (
    !is.data.frame(payables) ||
    !all(required %in% names(payables))
  ) {
    cli::cli_abort(
      "O relatório Daycoval não contém as colunas
       necessárias de contas a pagar."
    )
  }

  records <- payables |>
    dplyr::filter(
      as.character(.data$grupo) == "21010"
    )

  if (nrow(records) == 0L) {
    return(list(
      amount = 0,
      records = records
    ))
  }

  values <- records$valor

  if (
    !is.numeric(values) ||
    any(!is.finite(values)) ||
    any(values > 0)
  ) {
    cli::cli_abort(
      "Os valores de cotas a resgatar devem ser
       numéricos, finitos e não positivos."
    )
  }

  list(
    amount = round(-sum(values), 2),
    records = records
  )
}


shareholder_payables_validate <- function(
    cash_plan,
    previous_position,
    current_position,
    tolerance_brl,
    tolerance_pct
) {
  if (!inherits(cash_plan, "fund_cash_plan")) {
    cli::cli_abort(
      "`cash_plan` deve ser {.cls fund_cash_plan}."
    )
  }

  positions <- list(
    previous_position,
    current_position
  )

  if (!all(vapply(
    positions,
    inherits,
    logical(1),
    "daycoval_position"
  ))) {
    cli::cli_abort(
      "As duas posições devem ser objetos
       {.cls daycoval_position}."
    )
  }

  previous_metadata <- previous_position$metadata
  current_metadata <- current_position$metadata

  if (
    !identical(
      previous_metadata$codigo_carteira,
      current_metadata$codigo_carteira
    ) ||
    !identical(
      previous_metadata$cnpj,
      current_metadata$cnpj
    )
  ) {
    cli::cli_abort(
      "As posições Daycoval pertencem a fundos diferentes."
    )
  }

  previous_date <- previous_metadata$data_posicao
  current_date <- current_metadata$data_posicao

  if (
    !inherits(previous_date, "Date") ||
    !inherits(current_date, "Date") ||
    length(previous_date) != 1L ||
    length(current_date) != 1L ||
    is.na(previous_date) ||
    is.na(current_date) ||
    previous_date >= current_date
  ) {
    cli::cli_abort(
      "As posições devem possuir datas válidas e crescentes."
    )
  }

  tolerances <- c(tolerance_brl, tolerance_pct)

  if (
    !is.numeric(tolerances) ||
    length(tolerances) != 2L ||
    any(!is.finite(tolerances)) ||
    any(tolerances < 0)
  ) {
    cli::cli_abort(
      "As tolerâncias devem ser finitas e não negativas."
    )
  }

  operations <- cash_plan$shareholder_operations

  required <- c(
    "source_file",
    "source_row",
    "fund_code",
    "operation_type",
    "conversion_date",
    "liquidation_date",
    "gross_value"
  )

  if (
    !is.data.frame(operations) ||
    !all(required %in% names(operations)) ||
    !inherits(operations$conversion_date, "Date") ||
    !inherits(operations$liquidation_date, "Date") ||
    !is.numeric(operations$gross_value)
  ) {
    cli::cli_abort(
      "O controle interno possui estrutura incompatível."
    )
  }

  expected_code <- substr(
    gsub("[^0-9]", "", current_metadata$cnpj),
    1L,
    8L
  )

  internal_codes <- unique(
    operations$fund_code[
      !is.na(operations$fund_code) &
        nzchar(operations$fund_code)
    ]
  )

  if (
    length(internal_codes) != 1L ||
    !identical(internal_codes, expected_code)
  ) {
    cli::cli_abort(
      "A planilha interna não corresponde ao fundo Daycoval."
    )
  }

  invisible(NULL)
}
