
#' Explain position differences using fund operations
#'
#' Associates investment fund operations with previously reconciled XP
#' and Daycoval positions. Instrument identity is established through
#' the invested fund ISIN recorded in Daycoval Position C, combined
#' with the portfolio fund CNPJ from the operations workbook.
#'
#' Operations may be related to a position without explaining its
#' difference. Quantitative consistency is classified as unconfirmed,
#' never as proof of reconciliation.
#'
#' @param reconciliation A `positions_reconciliation` object.
#' @param operations A `fund_operations` object.
#' @param daycoval_position A `daycoval_position` object.
#'
#' @return A `position_explanations` object containing `metadata`,
#'   `links`, `candidates`, `assessments`, `issues`, and `summary`.
#'
#' @export
explain_position_differences <- function(
    reconciliation,
    operations,
    daycoval_position
) {
  position_explanation_validate(
    reconciliation,
    operations,
    daycoval_position
  )

  position_date <- reconciliation$metadata$position_date
  portfolio_cnpj <- position_explanation_cnpj(
    daycoval_position$metadata$cnpj
  )

  anchors <- position_explanation_anchors(
    reconciliation,
    daycoval_position
  )

  portfolio_operations <- operations$operations |>
    dplyr::filter(
      .data$portfolio_fund_cnpj == .env$portfolio_cnpj
    ) |>
    dplyr::mutate(
      asset_isin = dplyr::na_if(
        stringr::str_trim(.data$asset_isin),
        ""
      ),
      phase = position_explanation_phase(
        .data$operation_date,
        .data$conversion_date,
        .data$liquidation_date,
        position_date
      )
    ) |>
    dplyr::mutate(
      is_relevant = .data$phase %in% c(
        "date_unknown",
        "conversion_unknown",
        "pending_conversion",
        "conversion_on_position_date",
        "pending_settlement",
        "settlement_unknown"
      )
    )

  links <- portfolio_operations |>
    dplyr::filter(!is.na(.data$asset_isin)) |>
    dplyr::inner_join(
      anchors |>
        dplyr::filter(!is.na(.data$asset_isin)) |>
        dplyr::select(
          "match_id",
          "xp_asset_name",
          "daycoval_title",
          "asset_isin"
        ),
      by = "asset_isin"
    ) |>
    dplyr::mutate(
      may_affect_quantity = .data$phase %in% c(
        "conversion_unknown",
        "pending_conversion",
        "conversion_on_position_date"
      ),
      signed_quantity = dplyr::case_when(
        !.data$may_affect_quantity ~ NA_real_,
        is.na(.data$quantity) ~ NA_real_,
        .data$quantity <= 0 ~ NA_real_,
        .data$operation_type == "Aplicação" ~ .data$quantity,
        .data$operation_type %in% c(
          "Resgate Cotas",
          "Resgate Bruto",
          "Resgate Total"
        ) ~ -.data$quantity,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::arrange(
      .data$match_id,
      .data$operation_date,
      .data$source_row
    )

  candidates <- links |>
    dplyr::filter(.data$is_relevant)

  assessments <- position_explanation_assess(
    anchors = anchors,
    links = links,
    candidates = candidates
  )

  issues <- position_explanation_issues(
    anchors = anchors,
    portfolio_operations = portfolio_operations
  )

  summary <- tibble::tibble(
    matched_positions = nrow(anchors),
    portfolio_operations = nrow(portfolio_operations),
    linked_operations = nrow(links),
    relevant_operations = nrow(candidates),
    quantity_candidates = sum(
      candidates$may_affect_quantity
    ),
    quantity_differences = sum(
      !is.na(assessments$quantity_difference) &
        assessments$quantity_difference != 0
    ),
    quantitatively_consistent_unconfirmed = sum(
      assessments$status == "quantity_consistent_unconfirmed"
    ),
    issues = nrow(issues)
  )

  metadata <- list(
    position_date = position_date,
    portfolio_cnpj = portfolio_cnpj,
    daycoval_portfolio_code =
      reconciliation$metadata$daycoval_portfolio_code,
    xp_account_number =
      reconciliation$metadata$xp_account_number,
    daycoval_position_file =
      daycoval_position$metadata$arquivo_origem,
    operations_source_file =
      operations$metadata$source_file,
    association_rule = "portfolio_cnpj_and_invested_isin_v1",
    quantity_policy = "reported_quantities_without_tolerance"
  )

  structure(
    list(
      metadata = metadata,
      links = links,
      candidates = candidates,
      assessments = assessments,
      issues = issues,
      summary = summary
    ),
    class = "position_explanations"
  )
}


position_explanation_cnpj <- function(x) {
  valid <- is.character(x) &&
    length(x) == 1L &&
    !is.na(x)

  if (!valid) {
    cli::cli_abort(
      "A Posição C deve informar o CNPJ do fundo cotista."
    )
  }

  value <- gsub("[^0-9]", "", x)

  if (!grepl("^[0-9]{14}$", value)) {
    cli::cli_abort(
      "O CNPJ do fundo cotista informado na Posição C é inválido."
    )
  }

  value
}


position_explanation_validate <- function(
    reconciliation,
    operations,
    daycoval_position
) {
  if (!inherits(reconciliation, "positions_reconciliation")) {
    cli::cli_abort(
      "`reconciliation` deve ser {.cls positions_reconciliation}."
    )
  }

  if (!inherits(operations, "fund_operations")) {
    cli::cli_abort(
      "`operations` deve ser {.cls fund_operations}."
    )
  }

  if (!inherits(daycoval_position, "daycoval_position")) {
    cli::cli_abort(
      "`daycoval_position` deve ser {.cls daycoval_position}."
    )
  }

  reconciliation_metadata <- c(
    "position_date",
    "daycoval_portfolio_code",
    "xp_account_number"
  )

  position_metadata <- c(
    "codigo_carteira",
    "data_posicao",
    "cnpj",
    "arquivo_origem"
  )

  if (!is.list(reconciliation$metadata) ||
      !all(reconciliation_metadata %in%
           names(reconciliation$metadata))) {
    cli::cli_abort(
      "Os metadados da conciliação estão incompletos."
    )
  }

  if (!is.list(daycoval_position$metadata) ||
      !all(position_metadata %in%
           names(daycoval_position$metadata))) {
    cli::cli_abort(
      "Os metadados da Posição C estão incompletos."
    )
  }

  date <- reconciliation$metadata$position_date
  position_date <- daycoval_position$metadata$data_posicao

  valid_date <- function(x) {
    inherits(x, "Date") &&
      length(x) == 1L &&
      !is.na(x)
  }

  if (!valid_date(date) || !valid_date(position_date)) {
    cli::cli_abort(
      "A conciliação e a Posição C precisam de datas válidas."
    )
  }

  if (!identical(date, position_date)) {
    cli::cli_abort(
      "A conciliação e a Posição C possuem datas diferentes."
    )
  }

  if (!identical(
    as.character(
      reconciliation$metadata$daycoval_portfolio_code
    ),
    as.character(
      daycoval_position$metadata$codigo_carteira
    )
  )) {
    cli::cli_abort(
      "A conciliação e a Posição C identificam carteiras diferentes."
    )
  }

  required_matches <- c(
    "match_id",
    "xp_asset_name",
    "daycoval_title",
    "xp_quantity",
    "daycoval_quantity",
    "xp_market_value",
    "daycoval_market_value"
  )

  if (!is.data.frame(reconciliation$matches) ||
      !all(required_matches %in%
           names(reconciliation$matches))) {
    cli::cli_abort(
      "A conciliação não contém as associações obrigatórias."
    )
  }

  if (anyDuplicated(reconciliation$matches$match_id) ||
      anyNA(reconciliation$matches$match_id)) {
    cli::cli_abort(
      "A conciliação contém `match_id` duplicado ou ausente."
    )
  }

  required_funds <- c(
    "source_file",
    "source_line",
    "record_number",
    "titulo",
    "isin"
  )

  funds <- daycoval_position$positions$funds

  if (!is.data.frame(funds) ||
      !all(required_funds %in% names(funds))) {
    cli::cli_abort(
      "A Posição C não contém os identificadores dos fundos."
    )
  }

  required_operations <- c(
    "source_file",
    "source_row",
    "portfolio_fund_cnpj",
    "asset_isin",
    "asset_identifier",
    "operation_type",
    "operation_date",
    "conversion_date",
    "liquidation_date",
    "value",
    "quantity",
    "net_value"
  )

  records <- operations$operations

  if (!is.data.frame(records) ||
      !all(required_operations %in% names(records))) {
    cli::cli_abort(
      "O importador de operações não contém as colunas obrigatórias."
    )
  }

  for (field in c(
    "operation_date",
    "conversion_date",
    "liquidation_date"
  )) {
    if (!inherits(records[[field]], "Date")) {
      cli::cli_abort(
        "A coluna {.field {field}} deve ser do tipo Date."
      )
    }
  }

  if (!is.numeric(records$quantity) ||
      any(!is.na(records$quantity) &
          !is.finite(records$quantity))) {
    cli::cli_abort(
      "As quantidades das operações devem ser numéricas."
    )
  }

  if (!is.character(records$asset_isin) ||
      !is.character(records$portfolio_fund_cnpj)) {
    cli::cli_abort(
      "Os identificadores das operações devem ser textos."
    )
  }

  if (anyNA(records$portfolio_fund_cnpj) ||
      any(!grepl(
        "^[0-9]{14}$",
        records$portfolio_fund_cnpj
      ))) {
    cli::cli_abort(
      "O histórico contém CNPJs de fundos cotistas inválidos."
    )
  }

  if (anyDuplicated(records[
    c("source_file", "source_row")
  ])) {
    cli::cli_abort(
      "O histórico contém referências de origem duplicadas."
    )
  }

  position_explanation_cnpj(
    daycoval_position$metadata$cnpj
  )

  invisible(NULL)
}


position_explanation_anchors <- function(
    reconciliation,
    daycoval_position
) {
  funds <- daycoval_position$positions$funds |>
    dplyr::transmute(
      daycoval_title = stringr::str_trim(.data$titulo),
      asset_isin = dplyr::na_if(
        stringr::str_trim(.data$isin),
        ""
      ),
      position_source_file = .data$source_file,
      position_source_line = .data$source_line,
      position_record_number = .data$record_number
    )

  if (anyNA(funds$daycoval_title) ||
      any(!nzchar(funds$daycoval_title)) ||
      anyDuplicated(funds$daycoval_title)) {
    cli::cli_abort(
      "A Posição C possui identificadores operacionais ambíguos."
    )
  }

  anchors <- reconciliation$matches |>
    dplyr::left_join(
      funds,
      by = "daycoval_title"
    )

  present_isin <- anchors$asset_isin[
    !is.na(anchors$asset_isin)
  ]

  if (anyDuplicated(present_isin)) {
    cli::cli_abort(
      "Um mesmo ISIN está associado a múltiplas posições conciliadas."
    )
  }

  anchors
}


position_explanation_phase <- function(
    operation_date,
    conversion_date,
    liquidation_date,
    position_date
) {
  dplyr::case_when(
    is.na(operation_date) ~ "date_unknown",
    operation_date > position_date ~ "future_order",
    is.na(conversion_date) ~ "conversion_unknown",
    conversion_date > position_date ~ "pending_conversion",
    conversion_date == position_date ~ "conversion_on_position_date",
    !is.na(liquidation_date) &
      liquidation_date >= position_date ~ "pending_settlement",
    is.na(liquidation_date) ~ "settlement_unknown",
    TRUE ~ "completed"
  )
}


position_explanation_assess <- function(
    anchors,
    links,
    candidates
) {
  purrr::map_dfr(
    seq_len(nrow(anchors)),
    function(i) {
      record <- anchors[i, ]
      match_id <- record$match_id[[1L]]

      historical <- links |>
        dplyr::filter(.data$match_id == .env$match_id)

      relevant <- candidates |>
        dplyr::filter(.data$match_id == .env$match_id)

      quantity_candidates <- relevant |>
        dplyr::filter(.data$may_affect_quantity)

      quantity_difference <- (
        record$daycoval_quantity[[1L]] -
          record$xp_quantity[[1L]]
      )

      market_value_difference <- (
        round(record$daycoval_market_value[[1L]] * 100) -
          round(record$xp_market_value[[1L]] * 100)
      ) / 100

      n_quantity <- nrow(quantity_candidates)

      incomplete <- n_quantity > 0L &&
        any(is.na(quantity_candidates$signed_quantity))

      signed_quantity <- if (n_quantity == 0L) {
        NA_real_
      } else if (incomplete) {
        NA_real_
      } else {
        sum(quantity_candidates$signed_quantity)
      }

      status <- position_explanation_status(
        quantity_difference = quantity_difference,
        asset_isin = record$asset_isin[[1L]],
        n_relevant = nrow(relevant),
        n_quantity = n_quantity,
        incomplete = incomplete,
        signed_quantity = signed_quantity
      )

      tibble::tibble(
        match_id = match_id,
        xp_asset_name = record$xp_asset_name[[1L]],
        daycoval_title = record$daycoval_title[[1L]],
        asset_isin = record$asset_isin[[1L]],
        daycoval_quantity = record$daycoval_quantity[[1L]],
        xp_quantity = record$xp_quantity[[1L]],
        quantity_difference = quantity_difference,
        market_value_difference = market_value_difference,
        n_historical_operations = nrow(historical),
        n_relevant_operations = nrow(relevant),
        n_quantity_candidates = n_quantity,
        signed_candidate_quantity = signed_quantity,
        status = status
      )
    }
  )
}


position_explanation_status <- function(
    quantity_difference,
    asset_isin,
    n_relevant,
    n_quantity,
    incomplete,
    signed_quantity
) {
  if (is.na(quantity_difference)) {
    return("quantity_not_evaluated")
  }

  if (quantity_difference == 0) {
    return("quantity_equal")
  }

  if (is.na(asset_isin)) {
    return("identifier_unavailable")
  }

  if (n_relevant == 0L) {
    return("no_relevant_operations")
  }

  if (n_quantity == 0L) {
    return("settlement_only")
  }

  if (incomplete || is.na(signed_quantity)) {
    return("quantity_not_determined")
  }

  if (abs(quantity_difference) == abs(signed_quantity)) {
    return("quantity_consistent_unconfirmed")
  }

  "operations_do_not_reconcile"
}


position_explanation_issues <- function(
    anchors,
    portfolio_operations
) {
  missing_identifiers <- anchors |>
    dplyr::filter(is.na(.data$asset_isin)) |>
    dplyr::transmute(
      match_id = .data$match_id,
      issue_type = "missing_position_isin",
      source_file = .data$position_source_file,
      source_row = .data$position_source_line,
      asset_identifier = .data$daycoval_title
    )

  unidentified_operations <- portfolio_operations |>
    dplyr::filter(
      is.na(.data$asset_isin),
      .data$is_relevant
    ) |>
    dplyr::transmute(
      match_id = NA_character_,
      issue_type = "unidentified_relevant_operation",
      source_file = .data$source_file,
      source_row = .data$source_row,
      asset_identifier = .data$asset_identifier
    )

  dplyr::bind_rows(
    missing_identifiers,
    unidentified_operations
  ) |>
    dplyr::arrange(
      .data$issue_type,
      .data$source_file,
      .data$source_row
    )
}
