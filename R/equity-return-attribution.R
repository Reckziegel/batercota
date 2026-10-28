#' Estimate equity return contributions
#'
#' Estimates the contribution of directly held equities, ETFs,
#' and BDRs between two Daycoval reporting dates.
#'
#' Unchanged positions use reported market value differences.
#' Changed positions use opening quantity times price change.
#'
#' Entries, exits, and invalid observations remain unestimated.
#' Corporate actions and distributions require separate handling.
#'
#' @param previous Earlier `daycoval_position` object.
#' @param current Later `daycoval_position` object.
#'
#' @return An `equity_return_attribution` object containing
#'   metadata, contributions, summary, and issues.
#'
#' @export
estimate_equity_contributions <- function(
    previous,
    current
) {
  direct_attribution_validate(previous, current)

  before <- equity_attribution_holdings(previous, "previous")
  after <- equity_attribution_holdings(current, "current")

  previous_nav <- previous$metadata$patrimonio_liquido

  contributions <- dplyr::full_join(
    before,
    after,
    by = "isin",
    na_matches = "never"
  ) |>
    dplyr::mutate(
      title = dplyr::coalesce(
        .data$title_current,
        .data$title_previous
      ),
      asset_group = dplyr::coalesce(
        .data$asset_group_current,
        .data$asset_group_previous
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
        (
          round(.data$market_value_current * 100) -
            round(.data$market_value_previous * 100)
        ) / 100,
        NA_real_
      ),
      status = dplyr::case_when(
        is.na(.data$isin) ~ "missing_isin",
        is.na(.data$source_line_previous) ~ "entered",
        is.na(.data$source_line_current) ~ "exited",
        !is.finite(.data$quantity_previous) |
          !is.finite(.data$quantity_current) |
          .data$quantity_previous == 0 |
          .data$quantity_current == 0 ~ "invalid_quantity",
        !is.finite(.data$market_value_previous) |
          !is.finite(.data$market_value_current) ~
          "invalid_market_value",
        !is.finite(.data$unit_price_previous) |
          !is.finite(.data$unit_price_current) |
          .data$unit_price_previous <= 0 |
          .data$unit_price_current <= 0 ~ "invalid_price",
        .data$quantity_change == 0 ~ "unchanged_quantity",
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
      non_price_change_approx = dplyr::if_else(
        .data$status == "changed_quantity",
        .data$financial_change - .data$estimated_result,
        NA_real_
      ),
      contribution_pp = 100 *
        .data$estimated_result / previous_nav
    ) |>
    dplyr::arrange(.data$title, .data$isin)

  estimated <- !is.na(contributions$estimated_result)

  estimated_result <- if (any(estimated)) {
    sum(contributions$estimated_result[estimated])
  } else if (nrow(contributions) == 0L) {
    0
  } else {
    NA_real_
  }

  previous_value <- sum(before$market_value_previous)

  covered_value <- sum(
    contributions$market_value_previous[estimated]
  )

  coverage <- if (
    is.finite(previous_value) &&
    previous_value > 0
  ) {
    covered_value / previous_value
  } else {
    NA_real_
  }

  status <- dplyr::case_when(
    nrow(contributions) == 0L ~ "no_positions",
    any(!estimated) ~ "partial",
    TRUE ~ "complete"
  )

  summary <- tibble::tibble(
    previous_equity_value = previous_value,
    estimated_result = estimated_result,
    estimated_contribution_pp = 100 *
      estimated_result / previous_nav,
    opening_value_coverage = coverage,
    n_positions = nrow(contributions),
    n_estimated = sum(estimated),
    n_quantity_changed = sum(
      contributions$status == "changed_quantity"
    ),
    n_unestimated = sum(!estimated),
    status = status
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
    class = "equity_return_attribution"
  )
}


equity_attribution_holdings <- function(position, side) {
  side <- match.arg(side, c("previous", "current"))

  equities <- position$positions$equities

  if (is.null(equities)) {
    holdings <- tibble::tibble(
      isin = character(),
      original_isin = character(),
      title = character(),
      asset_group = character(),
      quantity = double(),
      unit_price = double(),
      market_value = double(),
      source_file = character(),
      source_line = integer(),
      record_number = character()
    )
  } else {
    required_columns <- c(
      "source_file",
      "source_line",
      "record_number",
      "cd_tit",
      "nome_grp_cntbl",
      "isin",
      "qtd",
      "pu_atual_bruto",
      "vl_mrc"
    )

    missing_columns <- setdiff(
      required_columns,
      names(equities)
    )

    if (length(missing_columns) > 0L) {
      cli::cli_abort(
        "Colunas ausentes em renda variável:
         {.or {missing_columns}}."
      )
    }

    numeric_columns <- c(
      "qtd",
      "pu_atual_bruto",
      "vl_mrc"
    )

    valid_numeric <- vapply(
      equities[numeric_columns],
      is.numeric,
      logical(1)
    )

    if (!all(valid_numeric)) {
      cli::cli_abort(
        "Quantidades, preços e valores devem ser numéricos."
      )
    }

    holdings <- equities |>
      dplyr::transmute(
        isin = dplyr::na_if(
          stringr::str_to_upper(
            stringr::str_trim(.data$isin)
          ),
          ""
        ),
        original_isin = .data$isin,
        title = stringr::str_trim(.data$cd_tit),
        asset_group = stringr::str_trim(
          .data$nome_grp_cntbl
        ),
        quantity = .data$qtd,
        unit_price = .data$pu_atual_bruto,
        market_value = .data$vl_mrc,
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
      "Existem ISINs duplicados na posição {.val {side}}."
    )
  }

  holdings |>
    dplyr::rename_with(
      .fn = ~ paste0(.x, "_", side),
      .cols = -dplyr::all_of("isin")
    )
}
