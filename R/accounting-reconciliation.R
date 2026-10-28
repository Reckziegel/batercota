#' Reconcile Daycoval portfolio accounting
#'
#' Reconstructs total assets, liabilities, and net asset value
#' from the accounting entries in a Daycoval daily portfolio.
#'
#' Original entries and source references are preserved.
#' The PATLIQ entry is excluded from the accounting sum.
#'
#' @param portfolio A `daycoval_portfolio` object.
#' @param tolerance Non-negative monetary tolerance in BRL.
#'
#' @return A `portfolio_accounting` object containing metadata,
#'   entries, groups, checks, summary, and issues.
#'
#' @export
reconcile_portfolio_accounting <- function(
    portfolio,
    tolerance = 0.01
) {
  if (!inherits(portfolio, "daycoval_portfolio")) {
    cli::cli_abort(
      "`portfolio` deve ser um objeto {.cls daycoval_portfolio}."
    )
  }

  accounting_validate_tolerance(tolerance)

  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "titulo",
    "grp_n1",
    "grp_n2",
    "grp_n3",
    "no_grp_n3",
    "vl_mrc"
  )

  missing_columns <- setdiff(
    required_columns,
    names(portfolio$entries)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas contábeis ausentes: {.or {missing_columns}}."
    )
  }

  metadata <- portfolio$metadata

  entries <- portfolio$entries |>
    dplyr::mutate(
      accounting_side = dplyr::case_when(
        .data$titulo == "PATLIQ" &
          .data$grp_n1 == "20000" &
          .data$grp_n2 == "26000" &
          .data$grp_n3 == "26000" ~ "equity",
        .data$grp_n1 == "10000" ~ "asset",
        .data$grp_n1 == "20000" &
          .data$grp_n2 != "26000" ~ "liability",
        TRUE ~ "unknown"
      ),
      signed_amount = dplyr::case_when(
        .data$accounting_side == "asset" ~ .data$vl_mrc,
        .data$accounting_side == "liability" ~ -.data$vl_mrc,
        TRUE ~ NA_real_
      )
    )

  issues <- entries |>
    dplyr::filter(
      .data$accounting_side == "unknown" |
        !is.finite(.data$vl_mrc)
    ) |>
    dplyr::transmute(
      issue_type = dplyr::case_when(
        .data$accounting_side == "unknown" ~
          "unknown_accounting_group",
        TRUE ~ "invalid_market_value"
      ),
      title = .data$titulo,
      accounting_group = .data$grp_n3,
      source_file = .data$source_file,
      source_line = .data$source_line
    )

  asset_values <- entries$signed_amount[
    entries$accounting_side == "asset"
  ]

  liability_values <- entries$signed_amount[
    entries$accounting_side == "liability"
  ]

  total_assets <- sum(asset_values)
  total_liabilities <- -sum(liability_values)

  calculated_nav <- if (
    nrow(issues) > 0L ||
    !is.finite(total_assets) ||
    !is.finite(total_liabilities)
  ) {
    NA_real_
  } else {
    total_assets - total_liabilities
  }

  checks <- tibble::tibble(
    metric = c("total_assets", "net_assets"),
    calculated = c(
      total_assets,
      calculated_nav
    ),
    reported = c(
      metadata$total_ativos,
      metadata$patrimonio_liquido
    )
  ) |>
    dplyr::mutate(
      difference = (
        round(.data$calculated * 100) -
          round(.data$reported * 100)
      ) / 100,
      status = vapply(
        .data$difference,
        accounting_check_status,
        character(1),
        tolerance = tolerance
      )
    )

  status <- dplyr::case_when(
    any(checks$status == "not_evaluated") ~ "not_evaluated",
    any(checks$status == "divergent") ~ "divergent",
    any(checks$status == "tolerated") ~ "tolerated",
    TRUE ~ "equal"
  )

  summary <- tibble::tibble(
    total_assets = total_assets,
    total_liabilities = total_liabilities,
    calculated_nav = calculated_nav,
    reported_nav = metadata$patrimonio_liquido,
    nav_difference = checks$difference[
      checks$metric == "net_assets"
    ],
    status = status
  )

  groups <- accounting_group_balances(entries)

  structure(
    list(
      metadata = metadata,
      entries = entries,
      groups = groups,
      checks = checks,
      summary = summary,
      issues = issues
    ),
    class = "portfolio_accounting"
  )
}


accounting_validate_tolerance <- function(tolerance) {
  valid <- is.numeric(tolerance) &&
    length(tolerance) == 1L &&
    !is.na(tolerance) &&
    is.finite(tolerance) &&
    tolerance >= 0

  if (!valid) {
    cli::cli_abort(
      "`tolerance` deve ser um número finito não negativo."
    )
  }

  invisible(tolerance)
}


accounting_check_status <- function(
    difference,
    tolerance
) {
  if (is.na(difference) || !is.finite(difference)) {
    return("not_evaluated")
  }

  difference_cents <- round(difference * 100)
  tolerance_cents <- round(tolerance * 100)

  if (difference_cents == 0) {
    return("equal")
  }

  if (abs(difference_cents) <= tolerance_cents) {
    return("tolerated")
  }

  "divergent"
}


accounting_group_balances <- function(entries) {
  entries |>
    dplyr::filter(
      .data$accounting_side %in% c("asset", "liability")
    ) |>
    dplyr::group_by(
      .data$grp_n1,
      .data$grp_n2,
      .data$grp_n3,
      .data$accounting_side
    ) |>
    dplyr::summarise(
      group_name = dplyr::first(.data$no_grp_n3),
      amount = sum(.data$signed_amount),
      n_entries = dplyr::n(),
      source_records = list(
        dplyr::pick(
          source_file,
          source_line,
          record_number,
          titulo,
          vl_mrc
        ) |>
          dplyr::rename(
            title = titulo,
            market_value = vl_mrc
          )
      ),
      .groups = "drop"
    )
}


#' Reconcile accounting changes across dates
#'
#' Compares accounting balances from two Daycoval portfolios
#' belonging to the same fund.
#'
#' Groups are matched using accounting codes rather than
#' descriptive names. Missing groups represent zero balances
#' only when both source portfolios are complete.
#'
#' Changes represent accounting balance movements, not
#' investment performance.
#'
#' @param previous Earlier `daycoval_portfolio` object.
#' @param current Later `daycoval_portfolio` object.
#' @param tolerance Monetary tolerance in BRL.
#'
#' @return A `portfolio_accounting_changes` object.
#'
#' @export
reconcile_accounting_changes <- function(
    previous,
    current,
    tolerance = 0.01
) {
  if (
    !inherits(previous, "daycoval_portfolio") ||
    !inherits(current, "daycoval_portfolio")
  ) {
    cli::cli_abort(
      "Ambos os objetos devem ser {.cls daycoval_portfolio}."
    )
  }

  accounting_validate_tolerance(tolerance)

  previous_metadata <- previous$metadata
  current_metadata <- current$metadata

  if (!identical(
    previous_metadata$codigo_carteira,
    current_metadata$codigo_carteira
  )) {
    cli::cli_abort(
      "As carteiras pertencem a fundos diferentes."
    )
  }

  previous_date <- previous_metadata$data_posicao
  current_date <- current_metadata$data_posicao

  if (
    !inherits(previous_date, "Date") ||
    !inherits(current_date, "Date") ||
    is.na(previous_date) ||
    is.na(current_date) ||
    previous_date >= current_date
  ) {
    cli::cli_abort(
      "As datas devem ser válidas e crescentes."
    )
  }

  previous_accounting <- reconcile_portfolio_accounting(
    previous,
    tolerance = tolerance
  )

  current_accounting <- reconcile_portfolio_accounting(
    current,
    tolerance = tolerance
  )

  if (
    nrow(previous_accounting$issues) > 0L ||
    nrow(current_accounting$issues) > 0L
  ) {
    cli::cli_abort(
      "Existem lançamentos contábeis não classificados
       ou inválidos nas carteiras."
    )
  }

  keys <- c(
    "grp_n1",
    "grp_n2",
    "grp_n3",
    "accounting_side"
  )

  changes <- dplyr::full_join(
    previous_accounting$groups,
    current_accounting$groups,
    by = keys,
    suffix = c("_previous", "_current")
  ) |>
    dplyr::mutate(
      change_type = dplyr::case_when(
        is.na(.data$n_entries_previous) ~ "opened",
        is.na(.data$n_entries_current) ~ "closed",
        TRUE ~ "continued"
      ),
      previous_amount = dplyr::coalesce(
        .data$amount_previous,
        0
      ),
      current_amount = dplyr::coalesce(
        .data$amount_current,
        0
      ),
      change = (
        round(.data$current_amount * 100) -
          round(.data$previous_amount * 100)
      ) / 100
    ) |>
    dplyr::arrange(
      .data$grp_n1,
      .data$grp_n2,
      .data$grp_n3
    )

  nav_change <- (
    round(current_metadata$patrimonio_liquido * 100) -
      round(previous_metadata$patrimonio_liquido * 100)
  ) / 100

  explained_change <- sum(changes$change)

  residual <- (
    round(nav_change * 100) -
      round(explained_change * 100)
  ) / 100

  source_statuses <- c(
    previous_accounting$summary$status,
    current_accounting$summary$status
  )

  status <- if (
    any(source_statuses %in% c("divergent", "not_evaluated"))
  ) {
    "source_issue"
  } else {
    accounting_check_status(
      difference = residual,
      tolerance = tolerance
    )
  }

  reported_return <- if (
    previous_metadata$valor_cota > 0
  ) {
    current_metadata$valor_cota /
      previous_metadata$valor_cota - 1
  } else {
    NA_real_
  }

  summary <- tibble::tibble(
    previous_nav = previous_metadata$patrimonio_liquido,
    current_nav = current_metadata$patrimonio_liquido,
    nav_change = nav_change,
    explained_change = explained_change,
    residual = residual,
    previous_shares = previous_metadata$numero_cotas,
    current_shares = current_metadata$numero_cotas,
    reported_return = reported_return,
    status = status
  )

  structure(
    list(
      metadata = list(
        codigo_carteira = current_metadata$codigo_carteira,
        previous_date = previous_date,
        current_date = current_date,
        tolerance = tolerance
      ),
      previous = previous_accounting,
      current = current_accounting,
      changes = changes,
      summary = summary
    ),
    class = "portfolio_accounting_changes"
  )
}
