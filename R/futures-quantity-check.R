#' Check futures quantity changes
#'
#' Compares futures quantities across two Daycoval positions.
#'
#' Quantity changes, entries, and exits are flagged for
#' reconciliation with trading operations.
#'
#' This function does not calculate futures investment results
#' or infer whether a quantity change is operationally correct.
#'
#' @param previous Earlier `daycoval_position` object.
#' @param current Later `daycoval_position` object.
#'
#' @return A `futures_quantity_check` object containing
#'   metadata, changes, summary, and issues.
#'
#' @export
check_futures_quantity_changes <- function(
    previous,
    current
) {
  direct_attribution_validate(previous, current)

  before <- futures_quantity_holdings(previous, "previous")
  after <- futures_quantity_holdings(current, "current")

  changes <- dplyr::full_join(
    before,
    after,
    by = "isin",
    na_matches = "never"
  ) |>
    dplyr::mutate(
      contract = dplyr::coalesce(
        .data$contract_current,
        .data$contract_previous
      ),
      quantity_change = dplyr::if_else(
        !is.na(.data$source_line_previous) &
          !is.na(.data$source_line_current),
        .data$quantity_current - .data$quantity_previous,
        NA_real_
      ),
      status = dplyr::case_when(
        is.na(.data$isin) ~ "missing_isin",
        is.na(.data$source_line_previous) ~
          "entry_requires_operations",
        is.na(.data$source_line_current) ~
          "exit_requires_operations",
        !is.finite(.data$quantity_previous) |
          !is.finite(.data$quantity_current) ~
          "invalid_quantity",
        .data$quantity_change == 0 ~ "unchanged_quantity",
        TRUE ~ "quantity_change_requires_operations"
      )
    ) |>
    dplyr::arrange(.data$contract, .data$isin)

  issues <- changes |>
    dplyr::filter(
      .data$status != "unchanged_quantity"
    ) |>
    dplyr::transmute(
      issue_type = .data$status,
      isin = .data$isin,
      contract = .data$contract,
      quantity_previous = .data$quantity_previous,
      quantity_current = .data$quantity_current,
      quantity_change = .data$quantity_change,
      source_file_previous = .data$source_file_previous,
      source_line_previous = .data$source_line_previous,
      source_file_current = .data$source_file_current,
      source_line_current = .data$source_line_current
    )

  status <- dplyr::case_when(
    nrow(changes) == 0L ~ "no_positions",
    nrow(issues) > 0L ~ "pending_operations",
    TRUE ~ "ok"
  )

  summary <- tibble::tibble(
    n_positions = nrow(changes),
    n_unchanged = sum(
      changes$status == "unchanged_quantity"
    ),
    n_pending = nrow(issues),
    status = status
  )

  structure(
    list(
      metadata = list(
        codigo_carteira = previous$metadata$codigo_carteira,
        previous_date = previous$metadata$data_posicao,
        current_date = current$metadata$data_posicao,
        methodology = "quantity_change_control"
      ),
      changes = changes,
      summary = summary,
      issues = issues
    ),
    class = "futures_quantity_check"
  )
}


futures_quantity_holdings <- function(position, side) {
  side <- match.arg(side, c("previous", "current"))

  futures <- position$positions$futures

  if (is.null(futures)) {
    holdings <- tibble::tibble(
      isin = character(),
      contract = character(),
      quantity = double(),
      adjustment = double(),
      source_file = character(),
      source_line = integer(),
      record_number = character()
    )
  } else {
    required_columns <- c(
      "source_file",
      "source_line",
      "record_number",
      "mercadoria",
      "serie",
      "isin",
      "qtd",
      "ajuste"
    )

    missing_columns <- setdiff(
      required_columns,
      names(futures)
    )

    if (length(missing_columns) > 0L) {
      cli::cli_abort(
        "Colunas ausentes em futuros:
         {.or {missing_columns}}."
      )
    }

    if (
      !is.numeric(futures$qtd) ||
      !is.numeric(futures$ajuste)
    ) {
      cli::cli_abort(
        "Quantidades e ajustes devem ser numéricos."
      )
    }

    holdings <- futures |>
      dplyr::transmute(
        isin = dplyr::na_if(
          stringr::str_to_upper(
            stringr::str_trim(.data$isin)
          ),
          ""
        ),
        contract = stringr::str_squish(
          paste(.data$mercadoria, .data$serie)
        ),
        quantity = .data$qtd,
        adjustment = .data$ajuste,
        source_file = .data$source_file,
        source_line = .data$source_line,
        record_number = .data$record_number
      )
  }

  valid_isins <- holdings$isin[
    !is.na(holdings$isin)
  ]

  if (anyDuplicated(valid_isins) > 0L) {
    cli::cli_abort(
      "Existem ISINs duplicados nos futuros da
       posição {.val {side}}."
    )
  }

  holdings |>
    dplyr::rename_with(
      .fn = ~ paste0(.x, "_", side),
      .cols = -dplyr::all_of("isin")
    )
}
