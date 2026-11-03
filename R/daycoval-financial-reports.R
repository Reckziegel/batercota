
#' Read Daycoval daily accounting movements
#'
#' Imports individual accounting entries from a Daycoval
#' Movimento Diário CSVBR report.
#'
#' Single-day and multi-day reports use the same reader.
#' Debit and credit entries are preserved separately.
#'
#' @param path Path to a Daycoval CSVBR file.
#' @param source_name Name identifying the source file.
#'
#' @return A `daycoval_daily_movement` object.
#'
#' @export
read_daycoval_daily_movement <- function(
    path,
    source_name = basename(path)
) {
  source <- daycoval_financial_source(
    path = path,
    source_name = source_name,
    required_layouts = "P0115_QuebraPag"
  )

  entries <- parse_daycoval_position_layout(
    records = source$raw,
    layouts = source$layouts,
    record_type = "P0115_QuebraPag"
  )

  entries <- daycoval_financial_parse_fields(
    data = entries,
    numeric_columns = c("quantidade", "debito", "credito"),
    date_columns = c("data_mov", "dt_compra"),
    text_columns = c(
      "carteira", "nome_crt", "moeda", "grupo",
      "titulo", "nome_tit", "especificacao",
      "historico", "operacao", "contra_partida",
      "tipo_mov", "reg", "reg_cp"
    )
  )

  if (nrow(entries) > 0L && anyNA(entries$data_mov)) {
    cli::cli_abort(
      "O Movimento Diário contém lançamentos sem data válida."
    )
  }

  codes <- unique(entries$carteira)
  codes <- codes[!is.na(codes) & nzchar(codes)]

  if (length(codes) != 1L) {
    cli::cli_abort(
      "O Movimento Diário deve identificar uma única carteira."
    )
  }

  dates <- sort(unique(entries$data_mov))

  metadata <- list(
    source_file = source_name,
    codigo_carteira = codes[[1L]],
    data_inicial = min(dates),
    data_final = max(dates),
    dates = dates,
    n_entries = nrow(entries),
    n_days = length(dates)
  )

  structure(
    list(
      metadata = metadata,
      entries = entries,
      layouts = source$layouts,
      raw = source$raw
    ),
    class = "daycoval_daily_movement"
  )
}


#' Read Daycoval cash statement
#'
#' Imports cash transactions from a Daycoval Demonstrativo
#' de Caixa CSVBR report.
#'
#' Detail records with `tp_det == "2"` represent individual
#' financial movements. Records with `tp_det == "3"` contain
#' daily totals and are not counted as transactions.
#'
#' @param path Path to a Daycoval CSVBR file.
#' @param source_name Name identifying the source file.
#'
#' @return A `daycoval_cash_statement` object.
#'
#' @export
read_daycoval_cash_statement <- function(
    path,
    source_name = basename(path)
) {
  required_layouts <- c(
    "P1051",
    "P1051SaldoIni",
    "P1051Det",
    "P1051SaldoFim"
  )

  source <- daycoval_financial_source(
    path = path,
    source_name = source_name,
    required_layouts = required_layouts
  )

  parsed <- lapply(
    required_layouts,
    function(record_type) {
      data <- parse_daycoval_position_layout(
        records = source$raw,
        layouts = source$layouts,
        record_type = record_type
      )

      daycoval_financial_parse_fields(
        data = data,
        numeric_columns = c("vl_deb", "vl_cred", "sld"),
        date_columns = c("dt", "dt_exibicao"),
        text_columns = c(
          "tp_det", "reg", "crt", "nm_crt", "nm_moeda",
          "hist", "tit", "tit_cp", "tp_mov"
        )
      )
    }
  )

  names(parsed) <- required_layouts

  header <- parsed$P1051
  opening <- parsed$P1051SaldoIni
  closing <- parsed$P1051SaldoFim
  details <- parsed$P1051Det

  if (
    nrow(header) != 1L ||
    nrow(opening) != 1L ||
    nrow(closing) != 1L
  ) {
    cli::cli_abort(
      "O Demonstrativo de Caixa deve conter um único
       cabeçalho, saldo inicial e saldo final."
    )
  }

  if (
    is.na(opening$dt[[1L]]) ||
    is.na(closing$dt[[1L]]) ||
    is.na(opening$sld[[1L]]) ||
    is.na(closing$sld[[1L]]) ||
    opening$dt[[1L]] > closing$dt[[1L]]
  ) {
    cli::cli_abort(
      "As datas e os saldos de abertura e fechamento são inválidos."
    )
  }

  valid_detail_types <- c("2", "3")

  if (
    anyNA(details$tp_det) ||
    any(!details$tp_det %in% valid_detail_types) ||
    anyNA(details$dt)
  ) {
    cli::cli_abort(
      "O Demonstrativo de Caixa contém detalhes inválidos."
    )
  }

  codes <- unique(c(
    header$crt,
    opening$crt,
    closing$crt,
    details$crt
  ))

  codes <- codes[!is.na(codes) & nzchar(codes)]

  if (length(codes) != 1L) {
    cli::cli_abort(
      "O Demonstrativo de Caixa deve identificar uma única carteira."
    )
  }

  movements <- details |>
    dplyr::filter(.data$tp_det == "2") |>
    dplyr::mutate(
      cash_change = dplyr::if_else(
        is.na(.data$vl_deb) & is.na(.data$vl_cred),
        NA_real_,
        dplyr::coalesce(.data$vl_deb, 0) -
          dplyr::coalesce(.data$vl_cred, 0)
      )
    )

  daily_totals <- details |>
    dplyr::filter(.data$tp_det == "3")

  opening_balance <- opening$sld[[1L]]
  closing_balance <- closing$sld[[1L]]

  complete_movements <- all(!is.na(movements$cash_change))

  net_cash_flow <- if (complete_movements) {
    round(sum(movements$cash_change), 2)
  } else {
    NA_real_
  }

  expected_closing <- if (complete_movements) {
    round(opening_balance + net_cash_flow, 2)
  } else {
    NA_real_
  }

  balance_difference <- if (complete_movements) {
    round(closing_balance - expected_closing, 2)
  } else {
    NA_real_
  }

  issues <- tibble::tibble(
    issue_type = character(),
    difference_brl = double()
  )

  if (!complete_movements) {
    issues <- tibble::tibble(
      issue_type = "incomplete_cash_movement",
      difference_brl = NA_real_
    )
  } else if (abs(balance_difference) > 0.02) {
    issues <- tibble::tibble(
      issue_type = "closing_balance_mismatch",
      difference_brl = balance_difference
    )
  }

  metadata <- list(
    source_file = source_name,
    codigo_carteira = codes[[1L]],
    opening_date = opening$dt[[1L]],
    closing_date = closing$dt[[1L]],
    dates = sort(unique(movements$dt)),
    opening_balance = opening_balance,
    closing_balance = closing_balance,
    net_cash_flow = net_cash_flow,
    expected_closing = expected_closing,
    balance_difference = balance_difference,
    n_movements = nrow(movements),
    n_daily_totals = nrow(daily_totals),
    n_issues = nrow(issues)
  )

  structure(
    list(
      metadata = metadata,
      movements = movements,
      daily_totals = daily_totals,
      header = header,
      opening = opening,
      closing = closing,
      issues = issues,
      layouts = source$layouts,
      raw = source$raw
    ),
    class = "daycoval_cash_statement"
  )
}


daycoval_financial_source <- function(
    path,
    source_name,
    required_layouts
) {
  records <- read_daycoval_records(
    path = path,
    source_name = source_name
  )

  layouts <- parse_daycoval_position_layouts(records)

  missing_layouts <- setdiff(
    required_layouts,
    layouts$record_type
  )

  if (length(missing_layouts) > 0L) {
    cli::cli_abort(
      "O relatório não contém os layouts obrigatórios:
       {.or {missing_layouts}}."
    )
  }

  list(raw = records, layouts = layouts)
}



daycoval_financial_parse_fields <- function(
    data,
    numeric_columns = character(),
    date_columns = character(),
    text_columns = character()
) {
  required <- c(
    "source_line",
    numeric_columns,
    date_columns,
    text_columns
  )

  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "O layout financeiro não contém as colunas:
       {.or {missing_columns}}."
    )
  }

  for (field in numeric_columns) {
    values <- stringr::str_trim(data[[field]])

    missing <- is.na(values) | values == ""

    valid_pattern <- paste0(
      "^[+-]?",
      "(?:[0-9]+|[0-9]{1,3}(?:\\.[0-9]{3})+)",
      "(?:,[0-9]+)?$"
    )

    valid <- stringr::str_detect(
      values,
      valid_pattern
    )

    invalid <- which(!missing & !valid)

    if (length(invalid) > 0L) {
      i <- invalid[[1L]]

      cli::cli_abort(
        "Valor numérico inválido em {.field {field}},
         linha {data$source_line[[i]]}:
         {.val {values[[i]]}}."
      )
    }

    normalized <- stringr::str_replace_all(
      values,
      stringr::fixed("."),
      ""
    )

    data[[field]] <- daycoval_parse_number(
      x = normalized,
      field = field,
      source_line = data$source_line
    )
  }

  for (field in date_columns) {
    data[[field]] <- daycoval_parse_date(
      x = data[[field]],
      field = field,
      source_line = data$source_line
    )
  }

  for (field in text_columns) {
    data[[field]] <- stringr::str_trim(data[[field]])
  }

  data
}

