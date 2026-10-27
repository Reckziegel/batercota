
#' Reconcile investment position changes across dates
#'
#' Compares two XP-Daycoval reconciliations for the same portfolio.
#' Assets are identified by Daycoval operational identifiers.
#' Operations are associated by invested fund ISIN and conversion date.
#'
#' Missing positions are not replaced with zero quantities.
#' Financial changes are calculated from reported values, in cents.
#' Linked operations are supporting evidence, not proof that a
#' quantity change has been fully reconciled.
#'
#' @param previous Earlier `positions_reconciliation` object.
#' @param current Later `positions_reconciliation` object.
#' @param operations A `fund_operations` object.
#' @param previous_position Earlier `daycoval_position` object.
#' @param current_position Later `daycoval_position` object.
#'
#' @return A `position_changes` object with `metadata`, `changes`,
#'   `links`, and `issues`.
#'
#' @export
reconcile_position_changes <- function(
    previous,
    current,
    operations,
    previous_position,
    current_position
) {
  position_changes_validate(
    previous,
    current,
    operations,
    previous_position,
    current_position
  )

  start_date <- previous$metadata$position_date
  end_date <- current$metadata$position_date

  portfolio_cnpj <- position_explanation_cnpj(
    current_position$metadata$cnpj
  )

  before <- position_changes_holdings(
    previous,
    previous_position,
    "previous"
  )

  after <- position_changes_holdings(
    current,
    current_position,
    "current"
  )

  previous_unmatched <- previous$issues$asset_identifier[
    previous$issues$issue_type == "unmatched_daycoval"
  ]

  current_unmatched <- current$issues$asset_identifier[
    current$issues$issue_type == "unmatched_daycoval"
  ]

  changes <- dplyr::full_join(
    before,
    after,
    by = "daycoval_title"
  ) |>
    dplyr::mutate(
      change_type = dplyr::case_when(
        !is.na(.data$present_previous) &
          !is.na(.data$present_current) ~ "held",
        is.na(.data$present_previous) &
          .data$daycoval_title %in% previous_unmatched ~
          "unverified_entry",
        is.na(.data$present_current) &
          .data$daycoval_title %in% current_unmatched ~
          "unverified_exit",
        is.na(.data$present_previous) ~ "entered",
        TRUE ~ "exited"
      ),
      xp_quantity_change = dplyr::if_else(
        .data$change_type == "held",
        .data$xp_quantity_current -
          .data$xp_quantity_previous,
        NA_real_
      ),
      daycoval_quantity_change = dplyr::if_else(
        .data$change_type == "held",
        .data$daycoval_quantity_current -
          .data$daycoval_quantity_previous,
        NA_real_
      ),
      xp_market_value_change = dplyr::if_else(
        .data$change_type == "held",
        (
          round(.data$xp_market_value_current * 100) -
            round(.data$xp_market_value_previous * 100)
        ) / 100,
        NA_real_
      ),
      daycoval_market_value_change = dplyr::if_else(
        .data$change_type == "held",
        (
          round(.data$daycoval_market_value_current * 100) -
            round(.data$daycoval_market_value_previous * 100)
        ) / 100,
        NA_real_
      ),
      isin_conflict = !is.na(.data$asset_isin_previous) &
        !is.na(.data$asset_isin_current) &
        .data$asset_isin_previous != .data$asset_isin_current,
      asset_isin = dplyr::if_else(
        .data$isin_conflict,
        NA_character_,
        dplyr::coalesce(
          .data$asset_isin_current,
          .data$asset_isin_previous
        )
      )
    )

  duplicated_isin <- !is.na(changes$asset_isin) & (
    duplicated(changes$asset_isin) |
      duplicated(changes$asset_isin, fromLast = TRUE)
  )

  changes$identifier_issue <- dplyr::case_when(
    changes$isin_conflict ~ "isin_conflict",
    duplicated_isin ~ "ambiguous_isin",
    is.na(changes$asset_isin) ~ "missing_isin",
    TRUE ~ NA_character_
  )

  changes$asset_isin[
    !is.na(changes$identifier_issue)
  ] <- NA_character_

  portfolio_operations <- operations$operations |>
    dplyr::filter(
      .data$portfolio_fund_cnpj == .env$portfolio_cnpj
    ) |>
    dplyr::mutate(
      asset_isin = dplyr::na_if(
        stringr::str_trim(.data$asset_isin),
        ""
      )
    )

  conversions <- portfolio_operations |>
    dplyr::filter(
      !is.na(.data$conversion_date),
      .data$conversion_date > .env$start_date,
      .data$conversion_date <= .env$end_date
    )

  links <- conversions |>
    dplyr::filter(!is.na(.data$asset_isin)) |>
    dplyr::inner_join(
      changes |>
        dplyr::filter(!is.na(.data$asset_isin)) |>
        dplyr::select(
          "daycoval_title",
          "asset_isin"
        ),
      by = "asset_isin"
    ) |>
    dplyr::mutate(
      signed_quantity = dplyr::case_when(
        is.na(.data$quantity) |
          .data$quantity <= 0 ~ NA_real_,
        .data$operation_type == "Aplicação" ~
          .data$quantity,
        .data$operation_type %in% c(
          "Resgate Cotas",
          "Resgate Bruto",
          "Resgate Total"
        ) ~ -.data$quantity,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::arrange(
      .data$daycoval_title,
      .data$conversion_date,
      .data$source_row
    )

  operation_summary <- links |>
    dplyr::group_by(.data$daycoval_title) |>
    dplyr::summarise(
      n_conversions = dplyr::n(),
      signed_operation_quantity = if (
        anyNA(.data$signed_quantity)
      ) {
        NA_real_
      } else {
        sum(.data$signed_quantity)
      },
      .groups = "drop"
    )

  changes <- changes |>
    dplyr::left_join(
      operation_summary,
      by = "daycoval_title"
    ) |>
    dplyr::mutate(
      n_conversions = dplyr::coalesce(
        .data$n_conversions,
        0L
      ),
      operation_status = dplyr::case_when(
        .data$n_conversions == 0L ~
          "no_linked_conversion",
        is.na(.data$signed_operation_quantity) ~
          "quantity_unavailable",
        TRUE ~ "quantity_reported_unconfirmed"
      )
    ) |>
    dplyr::arrange(.data$daycoval_title)

  identifier_issues <- changes |>
    dplyr::filter(!is.na(.data$identifier_issue)) |>
    dplyr::transmute(
      issue_type = .data$identifier_issue,
      asset_identifier = .data$daycoval_title,
      source_file = dplyr::coalesce(
        .data$position_source_file_current,
        .data$position_source_file_previous
      ),
      source_row = dplyr::coalesce(
        .data$position_source_line_current,
        .data$position_source_line_previous
      )
    )

  operation_issues <- conversions |>
    dplyr::filter(
      is.na(.data$asset_isin) |
        !.data$asset_isin %in% changes$asset_isin
    ) |>
    dplyr::transmute(
      issue_type = dplyr::if_else(
        is.na(.data$asset_isin),
        "operation_missing_isin",
        "operation_without_matched_position"
      ),
      asset_identifier = .data$asset_identifier,
      source_file = .data$source_file,
      source_row = .data$source_row
    )

  unknown_conversion <- portfolio_operations |>
    dplyr::filter(
      is.na(.data$conversion_date),
      !is.na(.data$operation_date),
      .data$operation_date > .env$start_date,
      .data$operation_date <= .env$end_date
    ) |>
    dplyr::transmute(
      issue_type = "conversion_date_unknown",
      asset_identifier = .data$asset_identifier,
      source_file = .data$source_file,
      source_row = .data$source_row
    )

  issues <- dplyr::bind_rows(
    position_changes_source_issues(previous, "previous"),
    position_changes_source_issues(current, "current"),
    identifier_issues,
    operation_issues,
    unknown_conversion
  ) |>
    dplyr::arrange(
      .data$issue_type,
      .data$asset_identifier,
      .data$source_row
    )

  metadata <- list(
    previous_date = start_date,
    current_date = end_date,
    daycoval_portfolio_code =
      current$metadata$daycoval_portfolio_code,
    xp_account_number =
      current$metadata$xp_account_number,
    portfolio_cnpj = portfolio_cnpj,
    identity_rule = "daycoval_operational_identifier",
    operation_rule = "isin_and_conversion_date",
    quantity_policy = "reported_without_imputation"
  )

  structure(
    list(
      metadata = metadata,
      changes = changes,
      links = links,
      issues = issues
    ),
    class = "position_changes"
  )
}


position_changes_validate <- function(
    previous,
    current,
    operations,
    previous_position,
    current_position
) {
  if (
    !inherits(previous, "positions_reconciliation") ||
    !inherits(current, "positions_reconciliation")
  ) {
    cli::cli_abort(
      "As duas conciliações devem ser objetos {.cls positions_reconciliation}."
    )
  }

  if (!inherits(operations, "fund_operations")) {
    cli::cli_abort(
      "`operations` deve ser {.cls fund_operations}."
    )
  }

  if (
    !inherits(previous_position, "daycoval_position") ||
    !inherits(current_position, "daycoval_position")
  ) {
    cli::cli_abort(
      "As duas posições devem ser objetos {.cls daycoval_position}."
    )
  }

  start_date <- previous$metadata$position_date
  end_date <- current$metadata$position_date

  if (
    !inherits(start_date, "Date") ||
    !inherits(end_date, "Date") ||
    length(start_date) != 1L ||
    length(end_date) != 1L ||
    is.na(start_date) ||
    is.na(end_date) ||
    start_date >= end_date
  ) {
    cli::cli_abort(
      "As conciliações devem possuir datas válidas e crescentes."
    )
  }

  if (
    !identical(
      as.character(previous$metadata$daycoval_portfolio_code),
      as.character(current$metadata$daycoval_portfolio_code)
    ) ||
    !identical(
      as.character(previous$metadata$xp_account_number),
      as.character(current$metadata$xp_account_number)
    )
  ) {
    cli::cli_abort(
      "As conciliações pertencem a fundos diferentes."
    )
  }

  for (case in list(
    list(previous, previous_position),
    list(current, current_position)
  )) {
    reconciliation <- case[[1L]]
    position <- case[[2L]]

    if (
      !identical(
        reconciliation$metadata$position_date,
        position$metadata$data_posicao
      ) ||
      !identical(
        as.character(
          reconciliation$metadata$daycoval_portfolio_code
        ),
        as.character(position$metadata$codigo_carteira)
      )
    ) {
      cli::cli_abort(
        "A Posição C não corresponde à respectiva conciliação."
      )
    }

    required <- c(
      "daycoval_title",
      "xp_asset_name",
      "xp_quantity",
      "daycoval_quantity",
      "xp_market_value",
      "daycoval_market_value"
    )

    if (
      !is.data.frame(reconciliation$matches) ||
      !all(required %in% names(reconciliation$matches))
    ) {
      cli::cli_abort(
        "A conciliação não contém posições válidas."
      )
    }

    if (
      anyNA(reconciliation$matches$daycoval_title) ||
      anyDuplicated(reconciliation$matches$daycoval_title)
    ) {
      cli::cli_abort(
        "A conciliação contém identificadores duplicados ou ausentes."
      )
    }

    if (
      !is.data.frame(position$positions$funds) ||
      !all(
        c("titulo", "isin", "source_file", "source_line") %in%
        names(position$positions$funds)
      )
    ) {
      cli::cli_abort(
        "A Posição C não contém os identificadores necessários."
      )
    }
  }

  previous_cnpj <- position_explanation_cnpj(
    previous_position$metadata$cnpj
  )

  current_cnpj <- position_explanation_cnpj(
    current_position$metadata$cnpj
  )

  if (!identical(previous_cnpj, current_cnpj)) {
    cli::cli_abort(
      "As Posições C pertencem a CNPJs diferentes."
    )
  }

  required_operations <- c(
    "portfolio_fund_cnpj",
    "asset_identifier",
    "asset_isin",
    "source_file",
    "source_row",
    "operation_type",
    "operation_date",
    "conversion_date",
    "quantity"
  )

  if (
    !is.data.frame(operations$operations) ||
    !all(required_operations %in% names(operations$operations))
  ) {
    cli::cli_abort(
      "O histórico de operações está incompleto."
    )
  }

  invisible(NULL)
}


position_changes_holdings <- function(
    reconciliation,
    position,
    suffix
) {
  funds <- position$positions$funds |>
    dplyr::transmute(
      daycoval_title = stringr::str_trim(.data$titulo),
      asset_isin = dplyr::na_if(
        stringr::str_trim(.data$isin),
        ""
      ),
      position_source_file = .data$source_file,
      position_source_line = .data$source_line
    )

  if (
    anyNA(funds$daycoval_title) ||
    any(!nzchar(funds$daycoval_title)) ||
    anyDuplicated(funds$daycoval_title)
  ) {
    cli::cli_abort(
      "A Posição C contém identificadores operacionais ambíguos."
    )
  }

  reconciliation$matches |>
    dplyr::transmute(
      daycoval_title = .data$daycoval_title,
      xp_asset_name = .data$xp_asset_name,
      xp_quantity = .data$xp_quantity,
      daycoval_quantity = .data$daycoval_quantity,
      xp_market_value = .data$xp_market_value,
      daycoval_market_value = .data$daycoval_market_value,
      present = TRUE
    ) |>
    dplyr::left_join(
      funds,
      by = "daycoval_title"
    ) |>
    dplyr::rename_with(
      ~ paste0(.x, "_", suffix),
      -daycoval_title
    )
}


position_changes_source_issues <- function(
    reconciliation,
    suffix
) {
  reconciliation$issues |>
    dplyr::filter(
      .data$issue_type != "expected_daycoval_only"
    ) |>
    dplyr::transmute(
      issue_type = paste0(suffix, "_", .data$issue_type),
      asset_identifier = .data$asset_identifier,
      source_file = .data$source_file,
      source_row = .data$source_row
    )
}
