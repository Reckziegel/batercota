
#' Build a daily shareholder redemption liability timeline
#'
#' Reconciles accounting conversions and settlements with cash payments
#' and the manager's scheduled redemptions. The first Daycoval position
#' provides the closing liability used as the opening anchor.
#'
#' @param cash_plan A `fund_cash_plan` object.
#' @param daily_movement A `daycoval_daily_movement` object.
#' @param cash_statement A `daycoval_cash_statement` object.
#' @param opening_position Daycoval Posição C at the anchor date.
#' @param positions Optional list of later Daycoval positions for checkpoints.
#' @param tolerance_brl Absolute tolerance for internal estimates.
#' @param tolerance_pct Relative tolerance for internal estimates.
#'
#' @return A `shareholder_liability_timeline` object.
#' @export
build_shareholder_liability_timeline <- function(
    cash_plan,
    daily_movement,
    cash_statement,
    opening_position,
    positions = list(),
    tolerance_brl = 100,
    tolerance_pct = 0.15
) {
  shareholder_liability_validate(
    cash_plan, daily_movement, cash_statement,
    opening_position, positions, tolerance_brl, tolerance_pct
  )

  opening_date <- opening_position$metadata$data_posicao
  ending_date <- daily_movement$metadata$data_final
  opening_amount <- shareholder_payables_extract(opening_position)$amount

  if (!is.finite(opening_amount)) {
    cli::cli_abort("A posição inicial não contém passivo utilizável.")
  }

  checkpoints <- if (length(positions) == 0L) {
    tibble::tibble(date = as.Date(character()), reported_closing = double())
  } else {
    dplyr::bind_rows(lapply(positions, function(position) {
      tibble::tibble(
        date = position$metadata$data_posicao,
        reported_closing = shareholder_payables_extract(position)$amount
      )
    }))
  }

  accounting <- daily_movement$entries |>
    dplyr::filter(.data$grupo == "2.10.10")

  if (any(!is.finite(accounting$debito)) ||
      any(!is.finite(accounting$credito)) ||
      any(accounting$debito < 0) ||
      any(accounting$credito < 0)) {
    cli::cli_abort("O grupo 2.10.10 contém valores contábeis inválidos.")
  }

  settlement_links <- shareholder_liability_link_settlements(
    accounting, cash_statement$movements
  )

  all_dates <- sort(unique(c(
    daily_movement$entries$data_mov,
    cash_statement$movements$dt,
    cash_plan$shareholder_operations$conversion_date,
    cash_plan$shareholder_operations$liquidation_date,
    checkpoints$date
  )))

  dates <- all_dates[
    !is.na(all_dates) & all_dates > opening_date &
      all_dates <= ending_date
  ]

  if (length(dates) == 0L) {
    cli::cli_abort("Não há movimentações posteriores à posição inicial.")
  }

  accounting_daily <- accounting |>
    dplyr::filter(
      .data$data_mov > .env$opening_date,
      .data$data_mov <= .env$ending_date
    ) |>
    dplyr::group_by(.data$data_mov) |>
    dplyr::summarise(
      daycoval_conversions = round(sum(.data$credito), 2),
      daycoval_settlements = round(sum(.data$debito), 2),
      .groups = "drop"
    )

  verified_daily <- settlement_links |>
    dplyr::filter(.data$verification == "verified") |>
    dplyr::group_by(.data$data_mov) |>
    dplyr::summarise(
      verified_cash_settlements = round(sum(.data$debito), 2),
      .groups = "drop"
    )

  timeline <- tibble::tibble(date = dates) |>
    dplyr::left_join(
      accounting_daily, by = c("date" = "data_mov")
    ) |>
    dplyr::left_join(
      verified_daily, by = c("date" = "data_mov")
    ) |>
    dplyr::left_join(checkpoints, by = "date") |>
    dplyr::mutate(
      dplyr::across(
        c(
          "daycoval_conversions", "daycoval_settlements",
          "verified_cash_settlements"
        ),
        ~ dplyr::coalesce(.x, 0)
      ),
      daycoval_closing = round(
        .env$opening_amount +
          cumsum(.data$daycoval_conversions - .data$daycoval_settlements),
        2
      ),
      daycoval_opening = c(
        .env$opening_amount, head(.data$daycoval_closing, -1L)
      ),
      unverified_cash_settlements = round(
        .data$daycoval_settlements -
          .data$verified_cash_settlements,
        2
      )
    )

  fund_code <- substr(
    gsub("[^0-9]", "", opening_position$metadata$cnpj),
    1L, 8L
  )

  redemptions <- cash_plan$shareholder_operations |>
    dplyr::filter(
      .data$fund_code == .env$fund_code,
      .data$operation_type %in% c(
        "Resgate Total",
        "Resgate Bruto",
        "Resgate Cotas"
      )
    )

  in_scope <- (
    !is.na(redemptions$conversion_date) &
      redemptions$conversion_date > opening_date &
      redemptions$conversion_date <= ending_date
  ) | (
    !is.na(redemptions$liquidation_date) &
      redemptions$liquidation_date > opening_date &
      redemptions$liquidation_date <= ending_date
  ) | (
    !is.na(redemptions$conversion_date) &
      redemptions$conversion_date <= opening_date &
      (is.na(redemptions$liquidation_date) |
         redemptions$liquidation_date > opening_date)
  )

  relevant <- redemptions[in_scope, , drop = FALSE]

  invalid <- relevant |>
    dplyr::filter(
      is.na(.data$conversion_date) |
        is.na(.data$liquidation_date) |
        is.na(.data$gross_value) |
        !is.finite(.data$gross_value) |
        .data$gross_value < 0 |
        .data$liquidation_date < .data$conversion_date
    )

  plan_complete <- nrow(invalid) == 0L

  if (plan_complete) {
    planned_opening <- round(sum(
      relevant$gross_value[
        relevant$conversion_date <= opening_date &
          relevant$liquidation_date > opening_date
      ]
    ), 2)

    timeline$manager_conversions <- vapply(dates, function(date) {
      round(sum(relevant$gross_value[
        relevant$conversion_date == date
      ]), 2)
    }, numeric(1))

    timeline$manager_scheduled_payments <- vapply(dates, function(date) {
      round(sum(relevant$gross_value[
        relevant$liquidation_date == date
      ]), 2)
    }, numeric(1))

    manager_changes <- timeline$manager_conversions -
      timeline$manager_scheduled_payments

    timeline$manager_closing <- round(
      planned_opening + cumsum(manager_changes), 2
    )
    timeline$manager_opening <- c(
      planned_opening, head(timeline$manager_closing, -1L)
    )
    timeline$manager_anchored_closing <- round(
      opening_amount + cumsum(manager_changes), 2
    )
  } else {
    planned_opening <- NA_real_
    timeline$manager_conversions <- NA_real_
    timeline$manager_scheduled_payments <- NA_real_
    timeline$manager_opening <- NA_real_
    timeline$manager_closing <- NA_real_
    timeline$manager_anchored_closing <- NA_real_
  }

  timeline <- timeline |>
    dplyr::mutate(
      conversion_difference = round(
        .data$daycoval_conversions - .data$manager_conversions, 2
      ),
      settlement_difference = round(
        .data$daycoval_settlements -
          .data$manager_scheduled_payments, 2
      ),
      closing_difference = round(
        .data$daycoval_closing - .data$manager_anchored_closing, 2
      ),
      checkpoint_difference = round(
        .data$daycoval_closing - .data$reported_closing, 2
      ),
      checkpoint_status = dplyr::case_when(
        is.na(.data$reported_closing) ~ "not_checked",
        abs(.data$checkpoint_difference) <= 0.05 ~ "consistent",
        TRUE ~ "requires_review"
      ),
      payment_status = dplyr::case_when(
        abs(.data$unverified_cash_settlements) > 0.01 ~
          "requires_review",
        .data$daycoval_settlements > 0 ~ "verified",
        TRUE ~ "no_settlement"
      ),
      conversion_allowance = pmax(
        .env$tolerance_brl,
        .env$tolerance_pct * pmax(
          .data$daycoval_conversions, .data$manager_conversions
        )
      ),
      settlement_allowance = pmax(
        .env$tolerance_brl,
        .env$tolerance_pct * pmax(
          .data$daycoval_settlements,
          .data$manager_scheduled_payments
        )
      ),
      manager_status = dplyr::case_when(
        is.na(.data$manager_conversions) ~ "incomplete_internal",
        abs(.data$conversion_difference) <=
          .data$conversion_allowance &
          abs(.data$settlement_difference) <=
          .data$settlement_allowance ~ "within_tolerance",
        TRUE ~ "requires_review"
      )
    )

  issues <- invalid |>
    dplyr::transmute(
      issue_type = "invalid_internal_redemption",
      date = .data$conversion_date,
      source_file = .data$source_file,
      source_line = .data$source_row,
      reference = .data$operation_id
    )

  unverified <- settlement_links |>
    dplyr::filter(.data$verification != "verified") |>
    dplyr::transmute(
      issue_type = .data$verification,
      date = .data$data_mov,
      source_file = .data$source_file,
      source_line = .data$source_line,
      reference = .data$reg_cp
    )

  unmatched_cash <- cash_statement$movements |>
    dplyr::filter(
      .data$tit_cp == "RESGA",
      !.data$reg %in% settlement_links$reg_cp
    ) |>
    dplyr::transmute(
      issue_type = "cash_redemption_without_liability_entry",
      date = .data$dt,
      source_file = .data$source_file,
      source_line = .data$source_line,
      reference = .data$reg
    )

  daily_issues <- timeline |>
    dplyr::filter(
      .data$manager_status == "requires_review" |
        .data$checkpoint_status == "requires_review"
    ) |>
    dplyr::transmute(
      issue_type = dplyr::if_else(
        .data$checkpoint_status == "requires_review",
        "checkpoint_mismatch", "manager_difference"
      ),
      date = .data$date,
      source_file = NA_character_,
      source_line = NA_integer_,
      reference = NA_character_
    )

  issues <- dplyr::bind_rows(
    issues, unverified, unmatched_cash, daily_issues
  )

  structure(
    list(
      metadata = list(
        codigo_carteira = opening_position$metadata$codigo_carteira,
        opening_date = opening_date,
        ending_date = ending_date,
        opening_liability = opening_amount,
        manager_opening_liability = planned_opening,
        settlement_verification = "accounting_reference_vs_cash",
        tolerance_brl = tolerance_brl,
        tolerance_pct = tolerance_pct
      ),
      timeline = timeline,
      accounting_entries = accounting,
      settlement_links = settlement_links,
      manager_operations = relevant,
      checkpoints = checkpoints,
      issues = issues
    ),
    class = "shareholder_liability_timeline"
  )
}


shareholder_liability_link_settlements <- function(accounting, cash) {
  settlements <- accounting |>
    dplyr::filter(.data$debito > 0)

  matched_index <- match(settlements$reg_cp, cash$reg)

  match_counts <- vapply(settlements$reg_cp, function(reference) {
    if (is.na(reference) || !nzchar(reference)) {
      return(0L)
    }
    sum(!is.na(cash$reg) & cash$reg == reference)
  }, integer(1))

  settlements$cash_source_line <- cash$source_line[matched_index]
  settlements$cash_date <- cash$dt[matched_index]
  settlements$cash_credit <- cash$vl_cred[matched_index]
  settlements$cash_counterpart <- cash$tit_cp[matched_index]

  settlements$verification <- dplyr::case_when(
    match_counts == 0L ~ "missing_cash_movement",
    match_counts > 1L ~ "ambiguous_cash_reference",
    is.na(settlements$cash_date) |
      settlements$cash_date != settlements$data_mov ~ "date_mismatch",
    is.na(settlements$cash_credit) |
      abs(settlements$cash_credit - settlements$debito) > 0.01 ~
      "amount_mismatch",
    is.na(settlements$cash_counterpart) |
      settlements$cash_counterpart != "RESGA" ~
      "unexpected_cash_counterpart",
    TRUE ~ "verified"
  )

  settlements
}


shareholder_liability_validate <- function(
    cash_plan,
    daily_movement,
    cash_statement,
    opening_position,
    positions,
    tolerance_brl,
    tolerance_pct
) {
  expected_classes <- c(
    "fund_cash_plan", "daycoval_daily_movement",
    "daycoval_cash_statement", "daycoval_position"
  )
  inputs <- list(
    cash_plan, daily_movement, cash_statement, opening_position
  )

  if (!all(vapply(seq_along(inputs), function(i) {
    inherits(inputs[[i]], expected_classes[[i]])
  }, logical(1)))) {
    cli::cli_abort("Os objetos fornecidos possuem classes incompatíveis.")
  }

  if (!is.list(positions) || !all(vapply(
    positions, inherits, logical(1), "daycoval_position"
  ))) {
    cli::cli_abort("`positions` deve ser uma lista de Posições C.")
  }

  tolerances <- c(tolerance_brl, tolerance_pct)
  if (!is.numeric(tolerances) || length(tolerances) != 2L ||
      any(!is.finite(tolerances)) || any(tolerances < 0)) {
    cli::cli_abort("As tolerâncias devem ser finitas e não negativas.")
  }

  code <- as.character(opening_position$metadata$codigo_carteira)
  report_codes <- c(
    daily_movement$metadata$codigo_carteira,
    cash_statement$metadata$codigo_carteira,
    vapply(positions, function(x) {
      as.character(x$metadata$codigo_carteira)
    }, character(1))
  )

  if (length(code) != 1L || is.na(code) ||
      anyNA(report_codes) || any(report_codes != code)) {
    cli::cli_abort("Os relatórios pertencem a carteiras diferentes.")
  }

  opening_date <- opening_position$metadata$data_posicao
  ending_date <- daily_movement$metadata$data_final

  if (!inherits(opening_date, "Date") ||
      !inherits(ending_date, "Date") ||
      length(opening_date) != 1L || length(ending_date) != 1L ||
      is.na(opening_date) || is.na(ending_date) ||
      opening_date >= ending_date) {
    cli::cli_abort("As datas da posição inicial e do movimento são inválidas.")
  }

  if (cash_statement$metadata$closing_date < ending_date ||
      cash_statement$metadata$opening_date > opening_date) {
    cli::cli_abort("O Demonstrativo de Caixa não cobre o período analisado.")
  }

  dates <- vapply(positions, function(x) {
    as.numeric(x$metadata$data_posicao)
  }, numeric(1))

  if (anyDuplicated(dates) ||
      any(!is.finite(dates)) ||
      any(dates <= as.numeric(opening_date)) ||
      any(dates > as.numeric(ending_date))) {
    cli::cli_abort("As posições de conferência possuem datas inválidas.")
  }

  expected_code <- substr(
    gsub("[^0-9]", "", opening_position$metadata$cnpj),
    1L, 8L
  )
  internal_codes <- unique(
    cash_plan$shareholder_operations$fund_code[
      !is.na(cash_plan$shareholder_operations$fund_code)
    ]
  )

  if (length(internal_codes) != 1L ||
      !identical(internal_codes, expected_code)) {
    cli::cli_abort("O controle da gestora não pertence ao fundo informado.")
  }

  invisible(NULL)
}
