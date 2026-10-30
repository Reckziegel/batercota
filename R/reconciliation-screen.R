
#' Classify financial reconciliation differences
#'
#' A financial difference is evaluated in integer cents.
#' Quantity differences are preserved separately and do not
#' determine the operational reconciliation status.
#'
#' @param difference Numeric vector of financial differences.
#' @param tolerance Maximum accepted absolute difference in BRL.
#'
#' @return Character vector of financial statuses.
screen_financial_status <- function(
    difference,
    tolerance = 100
) {
  valid_tolerance <- is.numeric(tolerance) &&
    length(tolerance) == 1L &&
    !is.na(tolerance) &&
    is.finite(tolerance) &&
    tolerance >= 0

  if (!valid_tolerance) {
    cli::cli_abort(
      "`tolerance` deve ser um número finito não negativo."
    )
  }

  if (!is.numeric(difference) ||
      any(!is.na(difference) & !is.finite(difference))) {
    cli::cli_abort(
      "`difference` deve conter valores numéricos finitos ou NA."
    )
  }

  difference_cents <- round(difference * 100)
  tolerance_cents <- round(tolerance * 100)

  dplyr::case_when(
    is.na(difference_cents) ~ "Não avaliado",
    difference_cents == 0 ~ "OK",
    abs(difference_cents) <= tolerance_cents ~ "Tolerada",
    TRUE ~ "Divergente"
  )
}


batercota_fund_reference <- function(fund) {
  if (!is.character(fund) ||
      length(fund) != 1L ||
      is.na(fund)) {
    cli::cli_abort("Fundo inválido.")
  }

  if (fund == "long_term") {
    return(list(
      name = "Long Term LB FIM",
      portfolio_code = "13386735",
      xp_account = "10850552",
      daycoval_only = "TPUBI97062PQ",
      mapping = tibble::tribble(
        ~xp_asset_name, ~daycoval_title,
        "Absolute Pace LB Advisory FIF em Cotas de FIA - RL",
        "CF 32892827",
        "AZ Quest Small Mid Caps FIF CIC em Ações RL",
        "CF 11392165",
        "BNP Paribas Small Caps FIF em Ações RL",
        "CF 11108013",
        "Genoa Capital Arpa Advisory CIC CIA RL",
        "CF 39317019",
        "Giant DAO Multifactor LB FIF em Cotas de FIM",
        "CF 40920066",
        "Ibiuna Long Biased FIF CIC Multi RL",
        "FIM LONG_BIA",
        "Trend Bolsas Emergentes FIM RL",
        "CF 40212899",
        "Trend Bolsas Europeias FIA RL",
        "CF 39227204"
      )
    ))
  }

  if (fund == "linear") {
    return(list(
      name = "Linear FIM CP RL",
      portfolio_code = "13711539",
      xp_account = "10850753",
      daycoval_only = "TPUBI97062PT",
      mapping = tibble::tibble(
        xp_asset_name = c(
          "Arx Vinson Advisory FIC de FIF RF CP RL",
          "Compass High Yield 180 Advisory FIF em Cotas de FIM",
          "Compass Yield 30 FIF RF CP LP RL",
          "Ibiuna Credit FIF CIC Multimercado CP RL",
          "JGP Crédito Ecossistema 360 Advisory FIC de FIF Multimercado CP RL",
          "Legacy Capital Credit Master CIM CP RL - Subclasse Legacy Credit Advisory",
          "Pátria Crédito Estruturado 365 FIDC",
          "Porto Seguro Ipê RF FIC FI CP LP",
          "Root Capital High Yield Advisory FIC de FIF Multimercado CP RL",
          "SPX Seahawk Advisory FIC FIRF CP LP",
          "SulAmérica Crédito Ativo FIRF CP LP",
          "Tivio Alt Credito High Yield 180 FIDC RL",
          "Trend Pós-Fixado FIC FIRF Simples RL",
          "Valora Vanguard FIC de FIDC RL",
          "Vinland Crédito Estruturado FIDC RL",
          "XP Crédito Estruturado 360 FIDC RL"
        ),
        daycoval_title = c(
          "CF 41888492",
          "CF 47936719",
          "CF 36318479",
          "CF 37310657",
          "CF 45683300",
          "CF 208056707",
          "CF 49983891",
          "CF 35378376",
          "CF 34431610",
          "CF 35648999",
          "CF 13823084",
          "CF 58038938",
          "CF 26559284",
          "CF 59657690",
          "CF 47640954",
          "CF 27227796"
        )
      )
    ))
  }

  cli::cli_abort(
    "Fundo não configurado: {.val {fund}}."
  )
}



#' Classify financial reconciliation differences
#'
#' A financial difference is evaluated in integer cents.
#' Quantity differences are preserved separately and do not
#' determine the operational reconciliation status.
#'
#' @param difference Numeric vector of financial differences.
#' @param tolerance Maximum accepted absolute difference in BRL.
#'
#' @return Character vector of financial statuses.
screen_financial_status <- function(
    difference,
    tolerance = 100
) {
  valid_tolerance <- is.numeric(tolerance) &&
    length(tolerance) == 1L &&
    !is.na(tolerance) &&
    is.finite(tolerance) &&
    tolerance >= 0

  if (!valid_tolerance) {
    cli::cli_abort(
      "`tolerance` deve ser um número finito não negativo."
    )
  }

  if (!is.numeric(difference) ||
      any(!is.na(difference) & !is.finite(difference))) {
    cli::cli_abort(
      "`difference` deve conter valores numéricos finitos ou NA."
    )
  }

  difference_cents <- round(difference * 100)
  tolerance_cents <- round(tolerance * 100)

  dplyr::case_when(
    is.na(difference_cents) ~ "Não avaliado",
    difference_cents == 0 ~ "OK",
    abs(difference_cents) <= tolerance_cents ~ "Tolerada",
    TRUE ~ "Divergente"
  )
}


batercota_fund_reference <- function(fund) {
  if (!is.character(fund) ||
      length(fund) != 1L ||
      is.na(fund)) {
    cli::cli_abort("Fundo inválido.")
  }

  if (fund == "long_term") {
    return(list(
      name = "Long Term LB FIM",
      portfolio_code = "13386735",
      xp_account = "10850552",
      daycoval_only = "TPUBI97062PQ",
      mapping = tibble::tribble(
        ~xp_asset_name, ~daycoval_title,
        "Absolute Pace LB Advisory FIF em Cotas de FIA - RL",
        "CF 32892827",
        "AZ Quest Small Mid Caps FIF CIC em Ações RL",
        "CF 11392165",
        "BNP Paribas Small Caps FIF em Ações RL",
        "CF 11108013",
        "Genoa Capital Arpa Advisory CIC CIA RL",
        "CF 39317019",
        "Giant DAO Multifactor LB FIF em Cotas de FIM",
        "CF 40920066",
        "Ibiuna Long Biased FIF CIC Multi RL",
        "FIM LONG_BIA",
        "Trend Bolsas Emergentes FIM RL",
        "CF 40212899",
        "Trend Bolsas Europeias FIA RL",
        "CF 39227204"
      )
    ))
  }

  if (fund == "linear") {
    return(list(
      name = "Linear FIM CP RL",
      portfolio_code = "13711539",
      xp_account = "10850753",
      daycoval_only = "TPUBI97062PT",
      mapping = tibble::tibble(
        xp_asset_name = c(
          "Arx Vinson Advisory FIC de FIF RF CP RL",
          "Compass High Yield 180 Advisory FIF em Cotas de FIM",
          "Compass Yield 30 FIF RF CP LP RL",
          "Ibiuna Credit FIF CIC Multimercado CP RL",
          "JGP Crédito Ecossistema 360 Advisory FIC de FIF Multimercado CP RL",
          "Legacy Capital Credit Master CIM CP RL - Subclasse Legacy Credit Advisory",
          "Pátria Crédito Estruturado 365 FIDC",
          "Porto Seguro Ipê RF FIC FI CP LP",
          "Root Capital High Yield Advisory FIC de FIF Multimercado CP RL",
          "SPX Seahawk Advisory FIC FIRF CP LP",
          "SulAmérica Crédito Ativo FIRF CP LP",
          "Tivio Alt Credito High Yield 180 FIDC RL",
          "Trend Pós-Fixado FIC FIRF Simples RL",
          "Valora Vanguard FIC de FIDC RL",
          "Vinland Crédito Estruturado FIDC RL",
          "XP Crédito Estruturado 360 FIDC RL"
        ),
        daycoval_title = c(
          "CF 41888492",
          "CF 47936719",
          "CF 36318479",
          "CF 37310657",
          "CF 45683300",
          "CF 208056707",
          "CF 49983891",
          "CF 35378376",
          "CF 34431610",
          "CF 35648999",
          "CF 13823084",
          "CF 58038938",
          "CF 26559284",
          "CF 59657690",
          "CF 47640954",
          "CF 27227796"
        )
      )
    ))
  }

  if (fund == "reta_alocacao") {
    return(list(
      name = "Reta Alocação FIF",
      portfolio_code = "13386778",
      xp_account = "10850442",
      daycoval_only = "TPUBI97062ON",
      mapping = tibble::tribble(
        ~xp_asset_name, ~daycoval_title,
        "Genoa Capital Radar Advisory CIC Multimercado RL",
        "CF 36017731",
        "Ibiuna Hedge ST Advisory FIF CIC Multimercado RL",
        "CF 30493349",
        "Kapitalo K10 Advisory FIF em Cotas de FIM",
        "CF 33520968",
        "Long Term LB FIM RL",
        "LONG.TERM",
        "Trend Inflação Longa FIF FIRF RL",
        "CF 38026814",
        "Trend Pós-Fixado FIC FIRF Simples RL",
        "CF 26559284"
      )
    ))
  }

  cli::cli_abort(
    "Fundo não configurado: {.val {fund}}."
  )
}


build_reconciliation_screen <- function(
    portfolio,
    position,
    xp,
    fund,
    financial_tolerance = 100
) {
  reference <- batercota_fund_reference(fund)

  if (!inherits(portfolio, "daycoval_portfolio") ||
      !inherits(position, "daycoval_position") ||
      !inherits(xp, "xp_statement")) {
    cli::cli_abort(
      "Os relatórios devem ser importados antes da conciliação."
    )
  }

  if (!identical(
    as.character(portfolio$metadata$codigo_carteira),
    reference$portfolio_code
  )) {
    cli::cli_abort(
      "A Carteira Diária não pertence ao fundo selecionado."
    )
  }

  if (!identical(
    as.character(xp$metadata$account_number),
    reference$xp_account
  )) {
    cli::cli_abort(
      "O extrato XP não pertence ao fundo selecionado."
    )
  }

  internal <- reconcile_daycoval(
    portfolio = portfolio,
    position = position
  )

  external <- reconcile_positions(
    daycoval = portfolio,
    xp = xp,
    mapping = reference$mapping,
    daycoval_only = reference$daycoval_only
  )

  quantity_checks <- external$checks |>
    dplyr::filter(.data$field == "quantity") |>
    dplyr::transmute(
      match_id = .data$match_id,
      quantity_difference = .data$difference,
      quantity_status = .data$status
    )

  value_checks <- external$checks |>
    dplyr::filter(.data$field == "market_value") |>
    dplyr::transmute(
      match_id = .data$match_id,
      market_value_difference = .data$difference,
      market_value_status = .data$status
    )

  positions <- external$matches |>
    dplyr::left_join(
      quantity_checks,
      by = "match_id"
    ) |>
    dplyr::left_join(
      value_checks,
      by = "match_id"
    ) |>
    dplyr::mutate(
      status = screen_financial_status(
        .data$market_value_difference,
        tolerance = financial_tolerance
      ),
      status_order = match(
        .data$status,
        c("Não avaliado", "Divergente", "Tolerada", "OK")
      )
    ) |>
    dplyr::arrange(
      .data$status_order,
      dplyr::desc(abs(.data$market_value_difference)),
      .data$xp_asset_name
    ) |>
    dplyr::select(-status_order)

  external_issues <- external$issues |>
    dplyr::transmute(
      origin = "XP x Daycoval",
      issue_type = .data$issue_type,
      asset_identifier = .data$asset_identifier,
      source_file = .data$source_file,
      source_line = dplyr::coalesce(
        .data$source_line,
        .data$source_row
      )
    )

  internal_issues <- internal$issues |>
    dplyr::transmute(
      origin = "Daycoval interno",
      issue_type = .data$issue_type,
      asset_identifier = .data$key,
      source_file = .data$source_file,
      source_line = .data$source_line
    )

  issues <- dplyr::bind_rows(
    external_issues,
    internal_issues
  )

  list(
    metadata = list(
      fund = fund,
      fund_name = reference$name,
      portfolio_code = reference$portfolio_code,
      xp_account = reference$xp_account,
      position_date = external$metadata$position_date,
      financial_tolerance = financial_tolerance
    ),
    positions = positions,
    issues = issues,
    external = external,
    internal = internal
  )
}

