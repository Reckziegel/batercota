
#' Reconcile XP and Daycoval investment fund positions
#'
#' Compares investment fund holdings reported by Daycoval and XP on
#' the same position date.
#'
#' Instruments are associated exclusively through an explicit mapping
#' of XP fund names to Daycoval operational identifiers. Quantities and
#' market values are never used to establish instrument identity.
#'
#' Quantity differences are evaluated without numerical tolerances.
#' Financial amounts are compared in integer cents.
#'
#' @param daycoval A `daycoval_portfolio` object.
#' @param xp An `xp_statement` object.
#' @param mapping A data frame containing `xp_asset_name` and
#'   `daycoval_title`.
#' @param daycoval_only Character vector of Daycoval operational
#'   identifiers explicitly recognized as held outside XP.
#'
#' @return A `positions_reconciliation` object containing `metadata`,
#'   `matches`, `checks`, `issues`, and `summary`.
#'
#' @export
reconcile_positions <- function(
    daycoval,
    xp,
    mapping,
    daycoval_only = character()
) {
  position_reconciliation_validate_inputs(daycoval, xp)

  records <- position_reconciliation_prepare(daycoval, xp)

  mapping <- position_reconciliation_validate_mapping(
    mapping = mapping,
    daycoval_only = daycoval_only,
    daycoval_funds = records$daycoval
  )

  matches <- position_reconciliation_match(
    daycoval_funds = records$daycoval,
    xp_funds = records$xp,
    mapping = mapping
  )

  checks <- position_reconciliation_checks(matches)

  issues <- position_reconciliation_issues(
    daycoval_funds = records$daycoval,
    xp_funds = records$xp,
    matches = matches,
    daycoval_only = daycoval_only
  )

  summary <- position_reconciliation_summary(
    daycoval_funds = records$daycoval,
    xp_funds = records$xp,
    matches = matches,
    checks = checks,
    issues = issues
  )

  metadata <- list(
    daycoval_portfolio_code = daycoval$metadata$codigo_carteira,
    daycoval_portfolio_name = daycoval$metadata$nome_carteira,
    xp_account_number = xp$metadata$account_number,
    xp_portfolio_name = xp$metadata$portfolio_name,
    position_date = daycoval$metadata$data_posicao,
    daycoval_source_file = daycoval$metadata$arquivo_origem,
    xp_source_file = xp$metadata$source_file,
    component = "funds",
    matching_rule = "explicit_operational_mapping_v1",
    daycoval_only = daycoval_only
  )

  structure(
    list(
      metadata = metadata,
      matches = matches,
      checks = checks,
      issues = issues,
      summary = summary
    ),
    class = "positions_reconciliation"
  )
}


position_reconciliation_validate_inputs <- function(daycoval, xp) {
  if (!inherits(daycoval, "daycoval_portfolio")) {
    cli::cli_abort(
      "`daycoval` deve ser um objeto {.cls daycoval_portfolio}."
    )
  }

  if (!inherits(xp, "xp_statement")) {
    cli::cli_abort(
      "`xp` deve ser um objeto {.cls xp_statement}."
    )
  }

  required_daycoval_metadata <- c(
    "codigo_carteira",
    "nome_carteira",
    "data_posicao",
    "arquivo_origem"
  )

  required_xp_metadata <- c(
    "account_number",
    "portfolio_name",
    "position_date",
    "source_file"
  )

  if (!is.list(daycoval$metadata) ||
      !all(required_daycoval_metadata %in% names(daycoval$metadata))) {
    cli::cli_abort("Os metadados Daycoval estão incompletos.")
  }

  if (!is.list(xp$metadata) ||
      !all(required_xp_metadata %in% names(xp$metadata))) {
    cli::cli_abort("Os metadados XP estão incompletos.")
  }

  daycoval_date <- daycoval$metadata$data_posicao
  xp_date <- xp$metadata$position_date

  valid_date <- function(x) {
    inherits(x, "Date") &&
      length(x) == 1L &&
      !is.na(x)
  }

  if (!valid_date(daycoval_date) || !valid_date(xp_date)) {
    cli::cli_abort("As fontes devem possuir datas de posição válidas.")
  }

  if (!identical(daycoval_date, xp_date)) {
    cli::cli_abort(
      "Daycoval e XP possuem datas de posição diferentes."
    )
  }

  required_daycoval <- c(
    "source_file",
    "source_line",
    "record_number",
    "titulo",
    "grp_n1",
    "grp_n3",
    "qt",
    "vl_mrc"
  )

  required_xp <- c(
    "asset_name",
    "quantity",
    "gross_value",
    "source_file",
    "source_page",
    "source_row",
    "source_tokens"
  )

  if (!is.data.frame(daycoval$entries) ||
      !all(required_daycoval %in% names(daycoval$entries))) {
    cli::cli_abort(
      "A Carteira Diária não possui as colunas obrigatórias."
    )
  }

  if (!is.data.frame(xp$positions) ||
      !all(required_xp %in% names(xp$positions))) {
    cli::cli_abort(
      "A posição consolidada XP não possui as colunas obrigatórias."
    )
  }

  financial_columns <- list(
    daycoval_quantity = daycoval$entries$qt,
    daycoval_value = daycoval$entries$vl_mrc,
    xp_quantity = xp$positions$quantity,
    xp_value = xp$positions$gross_value
  )

  for (field in names(financial_columns)) {
    values <- financial_columns[[field]]

    if (!is.numeric(values) ||
        any(!is.na(values) & !is.finite(values))) {
      cli::cli_abort(
        "O campo {.field {field}} deve conter números finitos ou NA."
      )
    }
  }

  invisible(NULL)
}


position_reconciliation_prepare <- function(daycoval, xp) {
  fund_groups <- c(
    "15801",
    "15803",
    "15805",
    "15812",
    "16007",
    "16008"
  )

  daycoval_funds <- daycoval$entries |>
    dplyr::filter(
      stringr::str_trim(.data$grp_n1) == "10000",
      stringr::str_trim(.data$grp_n3) %in% fund_groups
    ) |>
    dplyr::transmute(
      daycoval_title = stringr::str_trim(.data$titulo),
      daycoval_quantity = .data$qt,
      daycoval_market_value = .data$vl_mrc,
      daycoval_group = .data$grp_n3,
      daycoval_source_file = .data$source_file,
      daycoval_source_line = .data$source_line,
      daycoval_record_number = .data$record_number
    )

  xp_funds <- xp$positions |>
    dplyr::transmute(
      xp_asset_name = stringr::str_trim(.data$asset_name),
      xp_quantity = .data$quantity,
      xp_market_value = .data$gross_value,
      xp_source_file = .data$source_file,
      xp_source_page = .data$source_page,
      xp_source_row = .data$source_row,
      xp_source_tokens = .data$source_tokens
    )

  invalid_daycoval <- is.na(daycoval_funds$daycoval_title) |
    !nzchar(daycoval_funds$daycoval_title)

  invalid_xp <- is.na(xp_funds$xp_asset_name) |
    !nzchar(xp_funds$xp_asset_name)

  if (any(invalid_daycoval) || any(invalid_xp)) {
    cli::cli_abort(
      "As posições contêm identificadores de ativos ausentes."
    )
  }

  if (anyDuplicated(daycoval_funds$daycoval_title)) {
    cli::cli_abort(
      "A Carteira Diária contém identificadores de fundos duplicados."
    )
  }

  if (anyDuplicated(xp_funds$xp_asset_name)) {
    cli::cli_abort(
      "O extrato XP contém nomes de fundos duplicados."
    )
  }

  list(
    daycoval = daycoval_funds,
    xp = xp_funds
  )
}


position_reconciliation_validate_mapping <- function(
    mapping,
    daycoval_only,
    daycoval_funds
) {
  required <- c("xp_asset_name", "daycoval_title")

  if (!is.data.frame(mapping) ||
      !all(required %in% names(mapping))) {
    cli::cli_abort(
      "`mapping` deve conter {.or {required}}."
    )
  }

  mapping <- tibble::as_tibble(mapping) |>
    dplyr::select(dplyr::all_of(required))

  if (!is.character(mapping$xp_asset_name) ||
      !is.character(mapping$daycoval_title)) {
    cli::cli_abort(
      "Os identificadores do `mapping` devem ser textos."
    )
  }

  mapping <- mapping |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        stringr::str_trim
      )
    )

  invalid_mapping <- is.na(mapping$xp_asset_name) |
    !nzchar(mapping$xp_asset_name) |
    is.na(mapping$daycoval_title) |
    !nzchar(mapping$daycoval_title)

  if (any(invalid_mapping)) {
    cli::cli_abort(
      "`mapping` contém identificadores vazios."
    )
  }

  if (anyDuplicated(mapping$xp_asset_name) ||
      anyDuplicated(mapping$daycoval_title)) {
    cli::cli_abort(
      "`mapping` contém identificadores duplicados."
    )
  }

  if (!is.character(daycoval_only) ||
      anyNA(daycoval_only) ||
      any(!nzchar(stringr::str_trim(daycoval_only))) ||
      anyDuplicated(daycoval_only)) {
    cli::cli_abort(
      "`daycoval_only` deve conter identificadores únicos e válidos."
    )
  }

  missing_only <- setdiff(
    daycoval_only,
    daycoval_funds$daycoval_title
  )

  if (length(missing_only) > 0L) {
    cli::cli_abort(
      "Exceções não encontradas no Daycoval: {.or {missing_only}}."
    )
  }

  conflicting <- intersect(
    mapping$daycoval_title,
    daycoval_only
  )

  if (length(conflicting) > 0L) {
    cli::cli_abort(
      "Uma posição não pode estar no `mapping` e em `daycoval_only`."
    )
  }

  mapping
}


position_reconciliation_match <- function(
    daycoval_funds,
    xp_funds,
    mapping
) {
  mapping |>
    dplyr::inner_join(
      xp_funds,
      by = "xp_asset_name"
    ) |>
    dplyr::inner_join(
      daycoval_funds,
      by = "daycoval_title"
    ) |>
    dplyr::arrange(.data$xp_asset_name) |>
    dplyr::mutate(
      match_id = sprintf(
        "positions_%04d",
        dplyr::row_number()
      ),
      component = "funds",
      match_type = "explicit",
      rule_id = "explicit_operational_mapping_v1",
      .before = 1L
    )
}


position_reconciliation_check_field <- function(
    matches,
    field,
    daycoval_column,
    xp_column
) {
  values <- matches |>
    dplyr::transmute(
      match_id = .data$match_id,
      component = "funds",
      field = .env$field,
      daycoval_value = .data[[daycoval_column]],
      xp_value = .data[[xp_column]]
    )

  if (field == "market_value") {
    difference <- (
      round(values$daycoval_value * 100) -
        round(values$xp_value * 100)
    ) / 100
  } else {
    difference <- values$daycoval_value - values$xp_value
  }

  values |>
    dplyr::mutate(
      difference = difference,
      status = dplyr::case_when(
        is.na(.data$daycoval_value) |
          is.na(.data$xp_value) ~ "not_evaluated",
        .data$difference == 0 ~ "equal",
        TRUE ~ "different"
      )
    )
}


position_reconciliation_checks <- function(matches) {
  quantities <- position_reconciliation_check_field(
    matches = matches,
    field = "quantity",
    daycoval_column = "daycoval_quantity",
    xp_column = "xp_quantity"
  )

  market_values <- position_reconciliation_check_field(
    matches = matches,
    field = "market_value",
    daycoval_column = "daycoval_market_value",
    xp_column = "xp_market_value"
  )

  dplyr::bind_rows(
    quantities,
    market_values
  ) |>
    dplyr::arrange(.data$match_id, .data$field)
}


position_reconciliation_issues <- function(
    daycoval_funds,
    xp_funds,
    matches,
    daycoval_only
) {
  daycoval_issues <- daycoval_funds |>
    dplyr::anti_join(
      matches,
      by = "daycoval_title"
    ) |>
    dplyr::transmute(
      component = "funds",
      issue_type = dplyr::if_else(
        .data$daycoval_title %in% .env$daycoval_only,
        "expected_daycoval_only",
        "unmatched_daycoval"
      ),
      source_report = "daycoval",
      asset_identifier = .data$daycoval_title,
      source_file = .data$daycoval_source_file,
      source_page = NA_integer_,
      source_line = .data$daycoval_source_line,
      source_row = NA_integer_,
      record_number = .data$daycoval_record_number
    )

  xp_issues <- xp_funds |>
    dplyr::anti_join(
      matches,
      by = "xp_asset_name"
    ) |>
    dplyr::transmute(
      component = "funds",
      issue_type = "unmatched_xp",
      source_report = "xp",
      asset_identifier = .data$xp_asset_name,
      source_file = .data$xp_source_file,
      source_page = .data$xp_source_page,
      source_line = NA_integer_,
      source_row = .data$xp_source_row,
      record_number = NA_character_
    )

  dplyr::bind_rows(
    daycoval_issues,
    xp_issues
  ) |>
    dplyr::arrange(
      .data$issue_type,
      .data$source_report,
      .data$asset_identifier
    )
}


position_reconciliation_summary <- function(
    daycoval_funds,
    xp_funds,
    matches,
    checks,
    issues
) {
  match_results <- checks |>
    dplyr::group_by(.data$match_id) |>
    dplyr::summarise(
      equal = all(.data$status == "equal"),
      different = any(.data$status == "different"),
      .groups = "drop"
    )

  tibble::tibble(
    component = "funds",
    daycoval_records = nrow(daycoval_funds),
    xp_records = nrow(xp_funds),
    matched = nrow(matches),
    matched_equal = sum(match_results$equal),
    matched_different = sum(match_results$different),
    matched_not_evaluated = sum(
      !match_results$equal & !match_results$different
    ),
    unmatched_daycoval = sum(
      issues$issue_type == "unmatched_daycoval"
    ),
    unmatched_xp = sum(
      issues$issue_type == "unmatched_xp"
    ),
    expected_daycoval_only = sum(
      issues$issue_type == "expected_daycoval_only"
    )
  )
}
