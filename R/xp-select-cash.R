
#' Parse Brazilian monetary amounts
#'
#' @keywords internal
xp_cash_parse_amount <- function(value, field) {
  value <- trimws(as.character(value))
  value[is.na(value) | !nzchar(value)] <- NA_character_

  valid <- is.na(value) |
    grepl(
      "^-?(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+)(?:,[0-9]{1,2})?$",
      value
    ) |
    grepl("^-?[0-9]+(?:\\.[0-9]+)?$", value)

  if (any(!valid)) {
    cli::cli_abort(
      "Valores monetários inválidos em {.field {field}}."
    )
  }

  brazilian <- !is.na(value) & grepl(",", value, fixed = TRUE)

  value[brazilian] <- gsub(
    ",",
    ".",
    gsub(".", "", value[brazilian], fixed = TRUE),
    fixed = TRUE
  )

  as.numeric(value)
}


#' Parse dates from an XP cash statement
#'
#' @keywords internal
xp_cash_parse_date <- function(value) {
  date <- as.Date(value, format = "%d/%m/%Y")

  if (
    anyNA(date) ||
    any(format(date, "%d/%m/%Y") != value)
  ) {
    cli::cli_abort("Data inválida no Demonstrativo de Caixa XP.")
  }

  date
}


#' Classify XP cash movements
#'
#' @keywords internal
xp_cash_classify <- function(description) {
  text <- stringr::str_to_lower(description)

  sweep <- stringr::str_detect(text, "sant cash black")
  redemption <- stringr::str_detect(text, "^resgate do fundo")
  application <- stringr::str_detect(text, "^aplica")

  dplyr::case_when(
    stringr::str_detect(text, "^resgate de cotista") ~
      "shareholder_redemption",

    application & stringr::str_detect(text, "cotista") ~
      "shareholder_subscription",

    redemption & sweep ~ "cash_sweep_redemption",
    application & sweep ~ "cash_sweep_subscription",

    redemption ~ "investee_fund_redemption",
    application ~ "investee_fund_subscription",

    stringr::str_detect(text, "^resgate de outros fundos") ~
      "investee_fund_adjustment",

    stringr::str_detect(text, "^ajuste de futuros") ~
      "futures_adjustment",

    stringr::str_detect(text, "^rebate") ~
      "fee_rebate",

    stringr::str_detect(text, "^taxa") ~
      "operating_expense",

    stringr::str_detect(text, "^corretagens") ~
      "brokerage",

    TRUE ~ "unclassified"
  )
}


#' Read the metadata of an XP cash workbook
#'
#' @keywords internal
xp_cash_metadata <- function(cells) {
  get_field <- function(label) {
    index <- which(cells$field_1 == label)

    if (length(index) != 1L) {
      cli::cli_abort(
        "Metadado ausente ou duplicado: {.val {label}}."
      )
    }

    value <- cells$field_2[[index]]

    if (is.na(value) || !nzchar(value)) {
      cli::cli_abort(
        "Metadado vazio: {.val {label}}."
      )
    }

    value
  }

  client <- get_field("Cliente:")

  if (!stringr::str_detect(
    client,
    "^CUST4325 - XP - RETA - LINEAR SELECT FIM CP$"
  )) {
    cli::cli_abort(
      "O Demonstrativo de Caixa não pertence ao Linear Select."
    )
  }

  period <- stringr::str_match(
    get_field("Data de Posição:"),
    paste0(
      "^([0-9]{2}/[0-9]{2}/[0-9]{4})",
      " - ",
      "([0-9]{2}/[0-9]{2}/[0-9]{4})$"
    )
  )

  if (anyNA(period)) {
    cli::cli_abort("Período de posição inválido.")
  }

  period_start <- xp_cash_parse_date(period[1L, 2L])
  period_end <- xp_cash_parse_date(period[1L, 3L])

  if (period_start > period_end) {
    cli::cli_abort("O período de posição está invertido.")
  }

  issued_at <- as.POSIXct(
    get_field("Data de Emissão:"),
    format = "%d/%m/%Y %H:%M:%S",
    tz = "America/Sao_Paulo"
  )

  if (is.na(issued_at)) {
    cli::cli_abort("Data de emissão inválida.")
  }

  list(
    fund_key = "linear_select",
    portfolio_code = "CUST4325",
    fund_cnpj = "57593831000131",
    period_start = period_start,
    period_end = period_end,
    issued_at = issued_at
  )
}


#' Read an XP cash statement
#'
#' Imports the original XLSX cash statement, validates daily
#' balances and classifies financial movements.
#'
#' @param path Path to an XP cash statement XLSX.
#'
#' @return An `xp_select_cash_statement` containing metadata,
#'   transactions, daily_balances and raw records.
#'
#' @export
read_xp_select_cash <- function(path) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    file.exists(path) &&
    !dir.exists(path) &&
    tolower(tools::file_ext(path)) == "xlsx"

  if (!valid_path) {
    cli::cli_abort(
      "`path` deve apontar para um XLSX existente."
    )
  }

  sheets <- readxl::excel_sheets(path)

  if (!"caixa" %in% sheets) {
    cli::cli_abort(
      "O arquivo não possui a aba {.val caixa}."
    )
  }

  cells <- readxl::read_excel(
    path,
    sheet = "caixa",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  if (ncol(cells) < 5L || nrow(cells) < 7L) {
    cli::cli_abort("O Demonstrativo de Caixa está incompleto.")
  }

  cells <- cells[, seq_len(5L)]

  names(cells) <- paste0("field_", seq_len(5L))

  cells <- cells |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ trimws(.x)
      )
    )

  metadata <- xp_cash_metadata(cells)
  metadata$source_file <- basename(path)

  header <- which(
    cells$field_1 == "Data" &
      cells$field_2 == "Histórico" &
      cells$field_3 == "Entradas" &
      cells$field_4 == "Saídas" &
      cells$field_5 == "Saldo"
  )

  if (length(header) != 1L || header == nrow(cells)) {
    cli::cli_abort(
      "Cabeçalho financeiro inválido no XLSX."
    )
  }

  rows <- cells[
    seq.int(header + 1L, nrow(cells)),
    ,
    drop = FALSE
  ]

  rows <- rows |>
    dplyr::mutate(
      source_row = seq.int(header + 1L, nrow(cells))
    ) |>
    dplyr::filter(
      !is.na(.data$field_1) |
        !is.na(.data$field_2)
    )

  if (
    nrow(rows) == 0L ||
    anyNA(rows$field_1) ||
    anyNA(rows$field_2)
  ) {
    cli::cli_abort("O XLSX não possui movimentações válidas.")
  }

  records <- rows |>
    dplyr::transmute(
      source_row = .data$source_row,
      date = xp_cash_parse_date(.data$field_1),
      description = stringr::str_squish(.data$field_2),
      inflow = xp_cash_parse_amount(
        .data$field_3,
        "Entradas"
      ),
      outflow = xp_cash_parse_amount(
        .data$field_4,
        "Saídas"
      ),
      balance = xp_cash_parse_amount(
        .data$field_5,
        "Saldo"
      )
    ) |>
    dplyr::mutate(
      record_type = dplyr::case_when(
        .data$description == "Saldo Anterior" ~ "opening",
        .data$description == "Saldo" ~ "closing",
        TRUE ~ "transaction"
      )
    )

  if (
    anyNA(records$inflow) ||
    anyNA(records$outflow) ||
    any(records$inflow < 0) ||
    any(records$outflow > 0)
  ) {
    cli::cli_abort(
      "Entradas e saídas possuem sinais inválidos."
    )
  }

  opening <- records |>
    dplyr::filter(.data$record_type == "opening")

  closing <- records |>
    dplyr::filter(.data$record_type == "closing")

  transactions <- records |>
    dplyr::filter(.data$record_type == "transaction")

  if (
    nrow(opening) != 1L ||
    is.na(opening$balance[[1L]]) ||
    opening$date[[1L]] >= metadata$period_start
  ) {
    cli::cli_abort("Saldo de abertura inválido.")
  }


  if (
    nrow(closing) == 0L ||
    anyNA(closing$date) ||
    anyNA(closing$balance) ||
    anyDuplicated(closing$date) > 0L ||
    any(closing$date < metadata$period_start) ||
    any(closing$date > metadata$period_end) ||
    min(closing$date) <= opening$date[[1L]] ||
    max(closing$date) != metadata$period_end
  ) {
    cli::cli_abort(
      "Saldos de fechamento ausentes ou inconsistentes."
    )
  }


  if (
    anyNA(transactions$balance) ||
    any(transactions$date < metadata$period_start) ||
    any(transactions$date > metadata$period_end)
  ) {
    # Movimentações não devem apresentar saldo de fechamento.
    if (
      any(!is.na(transactions$balance)) ||
      any(transactions$date < metadata$period_start) ||
      any(transactions$date > metadata$period_end)
    ) {
      cli::cli_abort(
        "Movimentações com datas ou saldos inválidos."
      )
    }
  }

  if (any(!transactions$date %in% closing$date)) {
    cli::cli_abort(
      "Existem movimentações sem saldo diário correspondente."
    )
  }

  transactions <- transactions |>
    dplyr::mutate(
      amount = round(.data$inflow + .data$outflow, 2),
      category = xp_cash_classify(.data$description),
      asset_code = stringr::str_match(
        .data$description,
        "\\[([[:alnum:]]+)\\]$"
      )[, 2L],
      review = .data$category %in% c(
        "unclassified",
        "investee_fund_adjustment"
      ) |
        (
          .data$category == "shareholder_redemption" &
            .data$amount > 0
        ) |
        (
          .data$category == "investee_fund_redemption" &
            .data$amount < 0
        )
    ) |>
    dplyr::select(
      .data$source_row,
      .data$date,
      .data$description,
      .data$category,
      .data$asset_code,
      .data$inflow,
      .data$outflow,
      .data$amount,
      .data$review
    )

  movements <- transactions |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(
      inflows = sum(.data$inflow),
      outflows = sum(.data$outflow),
      n_transactions = dplyr::n(),
      .groups = "drop"
    )

  daily_balances <- closing |>
    dplyr::transmute(
      date = .data$date,
      reported_closing = .data$balance
    ) |>
    dplyr::left_join(movements, by = "date") |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      inflows = dplyr::coalesce(.data$inflows, 0),
      outflows = dplyr::coalesce(.data$outflows, 0),
      n_transactions = dplyr::coalesce(
        .data$n_transactions,
        0L
      ),
      opening_balance = dplyr::lag(
        .data$reported_closing,
        default = opening$balance[[1L]]
      ),
      calculated_closing = round(
        .data$opening_balance +
          .data$inflows +
          .data$outflows,
        2
      ),
      difference = round(
        .data$calculated_closing -
          .data$reported_closing,
        2
      ),
      status = dplyr::if_else(
        abs(.data$difference) <= 0.01,
        "OK",
        "Divergente"
      )
    ) |>
    dplyr::select(
      .data$date,
      .data$opening_balance,
      .data$inflows,
      .data$outflows,
      .data$calculated_closing,
      .data$reported_closing,
      .data$difference,
      .data$n_transactions,
      .data$status
    )

  metadata$opening_balance <- opening$balance[[1L]]

  metadata$closing_balance <- dplyr::last(
    daily_balances$reported_closing
  )

  metadata$n_transactions <- nrow(transactions)
  metadata$n_days <- nrow(daily_balances)

  structure(
    list(
      metadata = metadata,
      transactions = transactions,
      daily_balances = daily_balances,
      raw = records
    ),
    class = "xp_select_cash_statement"
  )
}


#' Compare historical movements across XP cash reports
#'
#' Identifies additions and removals in dates common to two
#' report versions. Original row order does not affect matching.
#'
#' @param previous Previous `xp_select_cash_statement`.
#' @param current Current `xp_select_cash_statement`.
#'
#' @return A tibble of historical movement revisions.
#'
#' @export
compare_xp_select_cash_versions <- function(
    previous,
    current
) {
  if (
    !inherits(previous, "xp_select_cash_statement") ||
    !inherits(current, "xp_select_cash_statement")
  ) {
    cli::cli_abort(
      "Ambos os objetos devem ser relatórios de caixa XP."
    )
  }

  start <- max(
    previous$metadata$period_start,
    current$metadata$period_start
  )

  end <- min(
    previous$metadata$period_end,
    current$metadata$period_end
  )

  if (start > end) {
    cli::cli_abort(
      "Os relatórios não possuem datas em comum."
    )
  }

  prepare <- function(object, count_name) {
    object$transactions |>
      dplyr::filter(
        .data$date >= .env$start,
        .data$date <= .env$end
      ) |>
      dplyr::mutate(
        inflow_cents = round(.data$inflow * 100),
        outflow_cents = round(.data$outflow * 100)
      ) |>
      dplyr::count(
        .data$date,
        .data$description,
        .data$inflow_cents,
        .data$outflow_cents,
        name = count_name
      )
  }

  previous_rows <- prepare(previous, "n_previous")
  current_rows <- prepare(current, "n_current")

  dplyr::full_join(
    previous_rows,
    current_rows,
    by = c(
      "date",
      "description",
      "inflow_cents",
      "outflow_cents"
    )
  ) |>
    dplyr::mutate(
      n_previous = dplyr::coalesce(.data$n_previous, 0L),
      n_current = dplyr::coalesce(.data$n_current, 0L),
      count_change = .data$n_current - .data$n_previous,
      financial_effect = .data$count_change *
        (.data$inflow_cents + .data$outflow_cents) / 100,
      status = dplyr::if_else(
        .data$count_change > 0,
        "Adicionado",
        "Removido"
      )
    ) |>
    dplyr::filter(.data$count_change != 0L) |>
    dplyr::transmute(
      date = .data$date,
      description = .data$description,
      inflow = .data$inflow_cents / 100,
      outflow = .data$outflow_cents / 100,
      count_change = .data$count_change,
      financial_effect = .data$financial_effect,
      status = .data$status
    ) |>
    dplyr::arrange(.data$date, .data$description)
}
