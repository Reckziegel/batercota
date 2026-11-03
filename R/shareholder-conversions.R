
#' Reconcile shareholder conversions with Daycoval
#'
#' Compares shareholder operations recorded by the manager with
#' the daily applications and redemptions reported by Daycoval.
#'
#' Internal financial amounts may be estimates. Differences are
#' evaluated using configurable absolute and relative tolerances.
#'
#' Only conversions on the current reporting date are compared.
#' Financial settlement is not assessed by this function.
#'
#' @param cash_plan A `fund_cash_plan` object.
#' @param shareholder_diagnostics A `shareholder_flow_diagnostics` object.
#' @param current_position Current `daycoval_position` object.
#' @param tolerance_brl Absolute tolerance in BRL.
#' @param tolerance_pct Relative tolerance as a fraction.
#'
#' @return A `shareholder_conversion_reconciliation` object.
#'
#' @export
reconcile_shareholder_conversions <- function(
    cash_plan,
    shareholder_diagnostics,
    current_position,
    tolerance_brl = 100,
    tolerance_pct = 0.15
) {
  shareholder_conversions_validate(
    cash_plan,
    shareholder_diagnostics,
    current_position,
    tolerance_brl,
    tolerance_pct
  )

  previous_date <- shareholder_diagnostics$metadata$previous_date
  current_date <- shareholder_diagnostics$metadata$current_date

  operations <- cash_plan$shareholder_operations |>
    dplyr::filter(
      !is.na(.data$conversion_date),
      .data$conversion_date == .env$current_date
    ) |>
    dplyr::mutate(
      flow_type = dplyr::case_when(
        .data$operation_type == "Aplicação" ~ "applications",
        .data$operation_type %in% c(
          "Resgate Bruto",
          "Resgate Total",
          "Resgate Cotas"
        ) ~ "redemptions",
        TRUE ~ NA_character_
      )
    )

  intermediate_operations <- cash_plan$shareholder_operations |>
    dplyr::filter(
      !is.na(.data$conversion_date),
      .data$conversion_date > .env$previous_date,
      .data$conversion_date < .env$current_date
    )

  unknown_operations <- operations |>
    dplyr::filter(is.na(.data$flow_type))

  report <- shareholder_diagnostics$summary

  summary <- dplyr::bind_rows(
    shareholder_conversions_compare(
      operations,
      flow_type = "applications",
      reported_amount = report$reported_applications[[1L]],
      tolerance_brl = tolerance_brl,
      tolerance_pct = tolerance_pct,
      has_unknown_operations = nrow(unknown_operations) > 0L
    ),
    shareholder_conversions_compare(
      operations,
      flow_type = "redemptions",
      reported_amount = report$reported_redemptions[[1L]],
      tolerance_brl = tolerance_brl,
      tolerance_pct = tolerance_pct,
      has_unknown_operations = nrow(unknown_operations) > 0L
    )
  )

  issues <- summary |>
    dplyr::filter(
      !.data$status %in% c("within_tolerance", "no_movement")
    ) |>
    dplyr::transmute(
      issue_type = .data$status,
      flow_type = .data$flow_type,
      source_file = NA_character_,
      source_row = NA_integer_
    )

  if (nrow(unknown_operations) > 0L) {
    issues <- dplyr::bind_rows(
      issues,
      unknown_operations |>
        dplyr::transmute(
          issue_type = "unknown_operation_type",
          flow_type = NA_character_,
          source_file = .data$source_file,
          source_row = .data$source_row
        )
    )
  }

  if (nrow(intermediate_operations) > 0L) {
    issues <- dplyr::bind_rows(
      issues,
      intermediate_operations |>
        dplyr::transmute(
          issue_type = "intermediate_conversion_not_compared",
          flow_type = NA_character_,
          source_file = .data$source_file,
          source_row = .data$source_row
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
        comparison_basis = "current_conversion_date",
        tolerance_brl = tolerance_brl,
        tolerance_pct = tolerance_pct,
        n_intermediate_operations = nrow(intermediate_operations)
      ),
      summary = summary,
      operations = operations,
      intermediate_operations = intermediate_operations,
      issues = issues,
      shareholder_diagnostics = shareholder_diagnostics
    ),
    class = "shareholder_conversion_reconciliation"
  )
}


shareholder_conversions_compare <- function(
    operations,
    flow_type,
    reported_amount,
    tolerance_brl,
    tolerance_pct,
    has_unknown_operations
) {
  selected <- operations |>
    dplyr::filter(.data$flow_type == .env$flow_type)

  n_operations <- nrow(selected)

  invalid_amount <- is.na(selected$gross_value) |
    !is.finite(selected$gross_value) |
    selected$gross_value < 0

  n_invalid_amounts <- sum(invalid_amount)

  planned_amount <- if (n_invalid_amounts > 0L) {
    NA_real_
  } else {
    round(sum(selected$gross_value), 2)
  }

  reported_amount <- as.numeric(reported_amount)

  difference_brl <- if (
    is.na(planned_amount) || is.na(reported_amount)
  ) {
    NA_real_
  } else {
    round(reported_amount - planned_amount, 2)
  }

  reference_amount <- if (is.na(difference_brl)) {
    NA_real_
  } else {
    max(abs(planned_amount), abs(reported_amount))
  }

  difference_pct <- if (is.na(reference_amount)) {
    NA_real_
  } else if (reference_amount == 0) {
    0
  } else {
    100 * abs(difference_brl) / reference_amount
  }

  allowed_difference <- if (is.na(reference_amount)) {
    NA_real_
  } else {
    max(
      tolerance_brl,
      tolerance_pct * reference_amount
    )
  }

  status <- dplyr::case_when(
    has_unknown_operations ~ "unclassified_operations",
    n_invalid_amounts > 0L ~ "incomplete_internal",
    is.na(reported_amount) ~ "missing_daycoval_report",
    n_operations == 0L && reported_amount > 0 ~
      "missing_internal_operation",
    n_operations > 0L &&
      planned_amount > 0 &&
      reported_amount == 0 ~ "missing_daycoval_operation",
    planned_amount == 0 && reported_amount == 0 ~
      "no_movement",
    abs(difference_brl) <= allowed_difference ~
      "within_tolerance",
    TRUE ~ "requires_review"
  )

  tibble::tibble(
    flow_type = flow_type,
    n_operations = n_operations,
    n_invalid_amounts = n_invalid_amounts,
    planned_amount = planned_amount,
    reported_amount = reported_amount,
    difference_brl = difference_brl,
    difference_pct = difference_pct,
    allowed_difference = allowed_difference,
    status = status
  )
}


shareholder_conversions_validate <- function(
    cash_plan,
    shareholder_diagnostics,
    current_position,
    tolerance_brl,
    tolerance_pct
) {
  if (!inherits(cash_plan, "fund_cash_plan")) {
    cli::cli_abort(
      "`cash_plan` deve ser {.cls fund_cash_plan}."
    )
  }

  if (!inherits(
    shareholder_diagnostics,
    "shareholder_flow_diagnostics"
  )) {
    cli::cli_abort(
      "`shareholder_diagnostics` deve ser
       {.cls shareholder_flow_diagnostics}."
    )
  }

  if (!inherits(current_position, "daycoval_position")) {
    cli::cli_abort(
      "`current_position` deve ser {.cls daycoval_position}."
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
      "As tolerâncias devem ser numéricas, finitas e não negativas."
    )
  }

  operations <- cash_plan$shareholder_operations

  required_columns <- c(
    "source_file",
    "source_row",
    "fund_code",
    "conversion_date",
    "operation_type",
    "gross_value"
  )

  if (
    !is.data.frame(operations) ||
    !all(required_columns %in% names(operations))
  ) {
    cli::cli_abort(
      "O controle interno não contém as colunas obrigatórias."
    )
  }

  if (
    !inherits(operations$conversion_date, "Date") ||
    !is.numeric(operations$gross_value)
  ) {
    cli::cli_abort(
      "As datas e os valores do controle interno possuem tipos inválidos."
    )
  }

  metadata <- shareholder_diagnostics$metadata

  previous_date <- metadata$previous_date
  current_date <- metadata$current_date

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
      "O diagnóstico deve conter datas válidas e crescentes."
    )
  }

  position_metadata <- current_position$metadata

  if (
    !identical(current_date, position_metadata$data_posicao) ||
    !identical(
      as.character(metadata$codigo_carteira),
      as.character(position_metadata$codigo_carteira)
    )
  ) {
    cli::cli_abort(
      "O diagnóstico não corresponde à Posição C atual."
    )
  }

  report <- shareholder_diagnostics$summary

  required_report <- c(
    "reported_applications",
    "reported_redemptions"
  )

  if (
    !is.data.frame(report) ||
    nrow(report) != 1L ||
    !all(required_report %in% names(report))
  ) {
    cli::cli_abort(
      "O diagnóstico de cotistas está incompleto."
    )
  }

  reported_values <- unlist(
    report[required_report],
    use.names = FALSE
  )

  if (
    !is.numeric(reported_values) ||
    any(!is.finite(reported_values[!is.na(reported_values)])) ||
    any(reported_values < 0, na.rm = TRUE)
  ) {
    cli::cli_abort(
      "Os valores reportados pelo Daycoval são inválidos."
    )
  }

  cnpj <- gsub("[^0-9]", "", position_metadata$cnpj)

  if (
    length(cnpj) != 1L ||
    is.na(cnpj) ||
    !grepl("^[0-9]{14}$", cnpj)
  ) {
    cli::cli_abort(
      "A Posição C não possui um CNPJ válido."
    )
  }

  expected_fund_code <- substr(cnpj, 1L, 8L)

  fund_codes <- unique(
    operations$fund_code[
      !is.na(operations$fund_code) &
        nzchar(operations$fund_code)
    ]
  )

  if (
    length(fund_codes) != 1L ||
    !identical(fund_codes, expected_fund_code)
  ) {
    cli::cli_abort(
      "A planilha de cotistas não corresponde ao fundo Daycoval."
    )
  }

  invisible(NULL)
}
