#' Estimate investment fund return contributions
#'
#' Estimates the contribution of investment fund positions
#' between two Daycoval reporting dates.
#'
#' Positions are matched using ISIN. For unchanged quantities,
#' the result is the change in reported market value.
#'
#' For changed quantities, the result is approximated using
#' the opening quantity and the change in unit price.
#'
#' Entries, exits, and invalid observations are not assigned
#' an estimated investment result.
#'
#' @param previous Earlier `daycoval_position` object.
#' @param current Later `daycoval_position` object.
#'
#' @return A `fund_return_attribution` object containing
#'   metadata, contributions, summary, and issues.
#'
#' @export
estimate_fund_contributions <- function(
    previous,
    current
) {
  fund_attribution_validate(previous, current)

  previous_holdings <- fund_attribution_holdings(
    previous,
    side = "previous"
  )

  current_holdings <- fund_attribution_holdings(
    current,
    side = "current"
  )

  previous_nav <- previous$metadata$patrimonio_liquido

  contributions <- dplyr::full_join(
    previous_holdings,
    current_holdings,
    by = "isin",
    na_matches = "never"
  ) |>
    dplyr::mutate(
      title = dplyr::coalesce(
        .data$title_current,
        .data$title_previous
      ),
      quantity_change = dplyr::if_else(
        !is.na(.data$source_line_previous) &
          !is.na(.data$source_line_current),
        .data$quantity_current - .data$quantity_previous,
        NA_real_
      ),
      financial_change = dplyr::if_else(
        !is.na(.data$source_line_previous) &
          !is.na(.data$source_line_current),
        .data$market_value_current -
          .data$market_value_previous,
        NA_real_
      ),
      status = dplyr::case_when(
        is.na(.data$isin) ~ "missing_isin",
        is.na(.data$source_line_previous) ~ "entered",
        is.na(.data$source_line_current) ~ "exited",
        !is.finite(.data$quantity_previous) |
          !is.finite(.data$quantity_current) ~
          "invalid_quantity",
        !is.finite(.data$market_value_previous) |
          !is.finite(.data$market_value_current) ~
          "invalid_market_value",
        !is.finite(.data$unit_price_previous) |
          !is.finite(.data$unit_price_current) |
          .data$unit_price_previous <= 0 |
          .data$unit_price_current <= 0 ~
          "invalid_price",
        .data$quantity_change == 0 ~
          "unchanged_quantity",
        TRUE ~ "changed_quantity"
      ),
      estimated_result = dplyr::case_when(
        .data$status == "unchanged_quantity" ~
          .data$financial_change,
        .data$status == "changed_quantity" ~
          .data$quantity_previous * (
            .data$unit_price_current -
              .data$unit_price_previous
          ),
        TRUE ~ NA_real_
      ),
      quantity_effect_approx = dplyr::if_else(
        .data$status == "changed_quantity",
        .data$financial_change - .data$estimated_result,
        NA_real_
      ),
      contribution_pp = 100 *
        .data$estimated_result / previous_nav
    ) |>
    dplyr::arrange(
      .data$title,
      .data$isin
    )

  estimated <- !is.na(contributions$estimated_result)

  estimated_result <- if (any(estimated)) {
    sum(contributions$estimated_result[estimated])
  } else {
    NA_real_
  }

  previous_value <- sum(
    previous_holdings$market_value_previous
  )

  covered_value <- sum(
    contributions$market_value_previous[estimated],
    na.rm = TRUE
  )

  coverage <- if (
    is.finite(previous_value) &&
    previous_value > 0
  ) {
    covered_value / previous_value
  } else {
    NA_real_
  }

  summary <- tibble::tibble(
    previous_fund_value = previous_value,
    estimated_result = estimated_result,
    estimated_contribution_pp = 100 *
      estimated_result / previous_nav,
    opening_value_coverage = coverage,
    n_positions = nrow(contributions),
    n_estimated = sum(estimated),
    n_quantity_changed = sum(
      contributions$status == "changed_quantity"
    ),
    n_unestimated = sum(!estimated)
  )

  issues <- contributions |>
    dplyr::filter(
      .data$status != "unchanged_quantity"
    ) |>
    dplyr::transmute(
      issue_type = .data$status,
      isin = .data$isin,
      title = .data$title,
      source_file_previous = .data$source_file_previous,
      source_line_previous = .data$source_line_previous,
      source_file_current = .data$source_file_current,
      source_line_current = .data$source_line_current
    )

  structure(
    list(
      metadata = list(
        codigo_carteira = previous$metadata$codigo_carteira,
        previous_date = previous$metadata$data_posicao,
        current_date = current$metadata$data_posicao,
        previous_nav = previous_nav,
        methodology = "opening_quantity_price_change"
      ),
      contributions = contributions,
      summary = summary,
      issues = issues
    ),
    class = "fund_return_attribution"
  )
}


fund_attribution_validate <- function(previous, current) {
  for (object in list(previous, current)) {
    if (!inherits(object, "daycoval_position")) {
      cli::cli_abort(
        "As posições devem ser objetos {.cls daycoval_position}."
      )
    }

    nav <- object$metadata$patrimonio_liquido

    if (
      !is.numeric(nav) ||
      length(nav) != 1L ||
      !is.finite(nav) ||
      nav <= 0
    ) {
      cli::cli_abort(
        "As posições devem possuir patrimônio líquido positivo."
      )
    }
  }

  if (!identical(
    previous$metadata$codigo_carteira,
    current$metadata$codigo_carteira
  )) {
    cli::cli_abort(
      "As posições pertencem a carteiras diferentes."
    )
  }

  previous_date <- previous$metadata$data_posicao
  current_date <- current$metadata$data_posicao

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
      "As datas devem ser válidas e crescentes."
    )
  }

  invisible(TRUE)
}


fund_attribution_holdings <- function(
    position,
    side
) {
  side <- match.arg(
    side,
    c("previous", "current")
  )

  funds <- position$positions$funds

  if (is.null(funds)) {
    cli::cli_abort(
      "A posição não possui o layout de fundos investidos."
    )
  }

  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "titulo",
    "isin",
    "cnpj",
    "qtd",
    "pu",
    "vl_mrc"
  )

  missing_columns <- setdiff(
    required_columns,
    names(funds)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas ausentes em fundos investidos:
       {.or {missing_columns}}."
    )
  }

  numeric_columns <- c("qtd", "pu", "vl_mrc")

  valid_numeric <- vapply(
    funds[numeric_columns],
    is.numeric,
    logical(1)
  )

  if (!all(valid_numeric)) {
    cli::cli_abort(
      "Quantidades, preços e valores devem ser numéricos."
    )
  }

  holdings <- funds |>
    dplyr::transmute(
      isin = dplyr::na_if(
        stringr::str_to_upper(
          stringr::str_trim(.data$isin)
        ),
        ""
      ),
      original_isin = .data$isin,
      title = stringr::str_trim(.data$titulo),
      cnpj = .data$cnpj,
      quantity = .data$qtd,
      unit_price = .data$pu,
      market_value = .data$vl_mrc,
      source_file = .data$source_file,
      source_line = .data$source_line,
      record_number = .data$record_number
    )

  valid_isins <- holdings$isin[
    !is.na(holdings$isin)
  ]

  if (anyDuplicated(valid_isins) > 0L) {
    cli::cli_abort(
      "Existem ISINs duplicados na posição {.val {side}}."
    )
  }

  holdings |>
    dplyr::rename_with(
      .fn = ~ paste0(.x, "_", side),
      .cols = -dplyr::all_of("isin")
    )
}
