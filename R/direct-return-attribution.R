#' Estimate fixed income return contributions
#'
#' Estimates the contribution of directly held fixed income
#' securities between two Daycoval reporting dates.
#'
#' For unchanged quantities, the reported market value change
#' is used. For changed quantities, the opening quantity
#' multiplied by the unit price change is used.
#'
#' Trades, coupons, amortizations, and other cash flows may
#' require additional reconciliation.
#'
#' @param previous Earlier `daycoval_position` object.
#' @param current Later `daycoval_position` object.
#'
#' @return A `fixed_income_return_attribution` object.
#'
#' @export
estimate_fixed_income_contributions <- function(
    previous,
    current
) {
  direct_attribution_validate(previous, current)

  before <- fixed_income_holdings(previous, "previous")
  after <- fixed_income_holdings(current, "current")

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
      security_name = dplyr::coalesce(
        .data$security_name_current,
        .data$security_name_previous
      ),
      quantity_change = dplyr::if_else(
        !is.na(.data$source_line_previous) &
          !is.na(.data$source_line_current),
        .data$quantity_current - .data$quantity_previous,
        NA_real_
      ),
      financial_change = (
        round(.data$market_value_current * 100) -
          round(.data$market_value_previous * 100)
      ) / 100,
      status = dplyr::case_when(
        is.na(.data$isin) ~ "missing_isin",
        is.na(.data$source_line_previous) ~ "entered",
        is.na(.data$source_line_current) ~ "exited",
        !is.finite(.data$quantity_previous) |
          !is.finite(.data$quantity_current) |
          .data$quantity_previous <= 0 |
          .data$quantity_current <= 0 ~ "invalid_quantity",
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
    )

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

  summary <- tibble::tibble(
    previous_fixed_income_value = previous_value,
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
    class = "fixed_income_return_attribution"
  )
}


direct_attribution_validate <- function(previous, current) {
  for (position in list(previous, current)) {
    if (!inherits(position, "daycoval_position")) {
      cli::cli_abort(
        "As posições devem ser objetos {.cls daycoval_position}."
      )
    }

    nav <- position$metadata$patrimonio_liquido

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


fixed_income_holdings <- function(position, side) {
  side <- match.arg(side, c("previous", "current"))

  bonds <- position$positions$fixed_income

  if (is.null(bonds)) {
    holdings <- tibble::tibble(
      isin = character(),
      original_isin = character(),
      title = character(),
      security_name = character(),
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
      "papel",
      "isin",
      "qtd",
      "pu_bruto",
      "vl_mrc"
    )

    missing_columns <- setdiff(
      required_columns,
      names(bonds)
    )

    if (length(missing_columns) > 0L) {
      cli::cli_abort(
        "Colunas ausentes em renda fixa:
         {.or {missing_columns}}."
      )
    }

    numeric_columns <- c("qtd", "pu_bruto", "vl_mrc")

    valid_numeric <- vapply(
      bonds[numeric_columns],
      is.numeric,
      logical(1)
    )

    if (!all(valid_numeric)) {
      cli::cli_abort(
        "Quantidades, preços e valores devem ser numéricos."
      )
    }

    holdings <- bonds |>
      dplyr::transmute(
        isin = dplyr::na_if(
          stringr::str_to_upper(
            stringr::str_trim(.data$isin)
          ),
          ""
        ),
        original_isin = .data$isin,
        title = stringr::str_trim(.data$cd_tit),
        security_name = stringr::str_trim(.data$papel),
        quantity = .data$qtd,
        unit_price = .data$pu_bruto,
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


#' Extract daily futures adjustments
#'
#' Uses the adjustment reported for each futures contract
#' in the current Daycoval position.
#'
#' The previous day's adjustment is not subtracted.
#' Notional values are never interpreted as investment results.
#'
#' @param position A `daycoval_position` object.
#' @param previous_nav Optional previous net asset value,
#'   used to calculate percentage-point contributions.
#'
#' @return A `futures_adjustment_attribution` object.
#'
#' @export
estimate_futures_adjustments <- function(
    position,
    previous_nav = NULL
) {
  if (!inherits(position, "daycoval_position")) {
    cli::cli_abort(
      "`position` deve ser {.cls daycoval_position}."
    )
  }

  if (!is.null(previous_nav)) {
    valid_nav <- is.numeric(previous_nav) &&
      length(previous_nav) == 1L &&
      is.finite(previous_nav) &&
      previous_nav > 0

    if (!valid_nav) {
      cli::cli_abort(
        "`previous_nav` deve ser positivo e finito."
      )
    }
  }

  futures <- position$positions$futures

  if (is.null(futures)) {
    futures <- tibble::tibble(
      source_file = character(),
      source_line = integer(),
      record_number = character(),
      mercadoria = character(),
      serie = character(),
      data_venc = as.Date(character()),
      isin = character(),
      qtd = double(),
      vl_noc = double(),
      ajuste = double()
    )
  }

  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "mercadoria",
    "serie",
    "data_venc",
    "isin",
    "qtd",
    "vl_noc",
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

  numeric_columns <- c("qtd", "vl_noc", "ajuste")

  valid_numeric <- vapply(
    futures[numeric_columns],
    is.numeric,
    logical(1)
  )

  if (!all(valid_numeric)) {
    cli::cli_abort(
      "Os valores de futuros devem ser numéricos."
    )
  }

  contributions <- futures |>
    dplyr::transmute(
      isin = .data$isin,
      contract = stringr::str_squish(
        paste(.data$mercadoria, .data$serie)
      ),
      maturity = .data$data_venc,
      quantity = .data$qtd,
      notional = .data$vl_noc,
      adjustment = .data$ajuste,
      estimated_result = dplyr::if_else(
        is.finite(.data$ajuste),
        .data$ajuste,
        NA_real_
      ),
      status = dplyr::if_else(
        is.finite(.data$ajuste),
        "reported_adjustment",
        "invalid_adjustment"
      ),
      source_file = .data$source_file,
      source_line = .data$source_line,
      record_number = .data$record_number
    )

  contributions$contribution_pp <- if (
    is.null(previous_nav)
  ) {
    rep(NA_real_, nrow(contributions))
  } else {
    100 * contributions$estimated_result / previous_nav
  }

  estimated <- !is.na(contributions$estimated_result)

  estimated_result <- if (any(estimated)) {
    sum(contributions$estimated_result[estimated])
  } else if (nrow(contributions) == 0L) {
    0
  } else {
    NA_real_
  }

  estimated_contribution_pp <- if (
    is.null(previous_nav)
  ) {
    NA_real_
  } else {
    100 * estimated_result / previous_nav
  }

  status <- dplyr::case_when(
    nrow(contributions) == 0L ~ "no_open_positions",
    any(!estimated) ~ "partial",
    TRUE ~ "reported"
  )

  summary <- tibble::tibble(
    estimated_result = estimated_result,
    estimated_contribution_pp = estimated_contribution_pp,
    n_positions = nrow(contributions),
    n_estimated = sum(estimated),
    n_unestimated = sum(!estimated),
    status = status
  )

  issues <- contributions |>
    dplyr::filter(
      .data$status != "reported_adjustment"
    ) |>
    dplyr::transmute(
      issue_type = .data$status,
      contract = .data$contract,
      isin = .data$isin,
      source_file = .data$source_file,
      source_line = .data$source_line
    )

  structure(
    list(
      metadata = list(
        codigo_carteira = position$metadata$codigo_carteira,
        position_date = position$metadata$data_posicao,
        previous_nav = previous_nav,
        methodology = "reported_daily_adjustment",
        coverage_scope = "current_reported_contracts"
      ),
      contributions = contributions,
      summary = summary,
      issues = issues
    ),
    class = "futures_adjustment_attribution"
  )
}
