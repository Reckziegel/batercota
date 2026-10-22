
#' Read Daycoval CSV records
#'
#' Reads heterogeneous records from a Daycoval/Sinqia CSV file,
#' preserving their original values, field positions, record types,
#' and source information.
#'
#' The function performs structural reading only. Financial values,
#' dates, and accounting classifications are not interpreted.
#'
#' @param path Path to a Daycoval CSV file.
#' @param source_name Name used to identify the source file.
#' @param encoding Character encoding of the CSV file.
#'
#' @return A tibble containing the original records, their fields,
#'   and source information.
#'
#' @export
read_daycoval_records <- function(
    path,
    source_name = basename(path),
    encoding = "Latin1"
) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path)

  if (!valid_path || !file.exists(path) || dir.exists(path)) {
    cli::cli_abort("`path` deve apontar para um arquivo CSV existente.")
  }

  valid_source <- is.character(source_name) &&
    length(source_name) == 1L &&
    !is.na(source_name) &&
    nzchar(source_name)

  if (!valid_source) {
    cli::cli_abort("`source_name` deve ser um texto não vazio.")
  }

  lines <- readr::read_lines(
    file = path,
    locale = readr::locale(encoding = encoding),
    skip_empty_rows = FALSE,
    progress = FALSE
  )

  if (length(lines) == 0L) {
    cli::cli_abort("O arquivo CSV está vazio.")
  }

  if (any(grepl('"', lines, fixed = TRUE))) {
    cli::cli_abort(
      "O arquivo contém aspas e exige validação do delimitador."
    )
  }

  fields <- stringr::str_split(
    string = lines,
    pattern = stringr::fixed(";")
  )

  tibble::tibble(
    source_file = source_name,
    source_line = seq_along(lines),
    record_number = daycoval_get_field(fields, 1L),
    record_type = daycoval_get_field(fields, 2L),
    record_flag = daycoval_get_field(fields, 3L),
    n_fields = lengths(fields),
    fields = fields,
    raw_line = lines
  )
}


daycoval_get_field <- function(fields, position) {
  purrr::map_chr(fields, function(values) {
    if (length(values) < position) {
      return(NA_character_)
    }

    value <- values[[position]]

    if (is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }

    value
  })
}


daycoval_snake_case <- function(x) {
  x |>
    stringr::str_replace_all(
      "([A-Z]+)([A-Z][a-z])",
      "\\1_\\2"
    ) |>
    stringr::str_replace_all(
      "([a-z0-9])([A-Z])",
      "\\1_\\2"
    ) |>
    stringr::str_replace_all("[^[:alnum:]]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "") |>
    stringr::str_to_lower()
}


parse_daycoval_layout <- function(records, record_type) {
  layout_widths <- c(
    LayCrtDiaDespRef = 70L,
    LayCrtDiaDespRefDet = 5L,
    RentabBenkmark = 10L
  )

  if (
    length(record_type) != 1L ||
    is.na(record_type) ||
    !record_type %in% names(layout_widths)
  ) {
    cli::cli_abort("`record_type` deve identificar um layout conhecido.")
  }

  layout_records <- records |>
    dplyr::filter(.data$record_type == .env$record_type)

  if (
    nrow(layout_records) == 0L ||
    any(!layout_records$record_flag %in% c("C", "V"))
  ) {
    cli::cli_abort("O layout contém registros ausentes ou inválidos.")
  }

  expected_width <- unname(layout_widths[[record_type]])

  if (any(layout_records$n_fields != expected_width)) {
    cli::cli_abort("O layout contém registros com tamanho inesperado.")
  }

  headers <- layout_records |>
    dplyr::filter(.data$record_flag == "C")

  if (nrow(headers) != 1L) {
    cli::cli_abort("O layout deve conter exatamente um cabeçalho.")
  }

  field_names <- headers$fields[[1]][-seq_len(3L)] |>
    daycoval_snake_case()

  if (
    anyNA(field_names) ||
    any(!nzchar(field_names)) ||
    anyDuplicated(field_names)
  ) {
    cli::cli_abort("O cabeçalho contém nomes de campos inválidos.")
  }

  values <- layout_records |>
    dplyr::filter(.data$record_flag == "V")

  data_fields <- purrr::map(
    values$fields,
    function(x) x[-seq_len(3L)]
  )

  data_matrix <- matrix(
    data = unlist(data_fields, use.names = FALSE),
    nrow = nrow(values),
    ncol = length(field_names),
    byrow = TRUE,
    dimnames = list(NULL, field_names)
  )

  dplyr::bind_cols(
    values |>
      dplyr::select(
        "source_file",
        "source_line",
        "record_number"
      ),
    tibble::as_tibble(data_matrix)
  )
}


daycoval_parse_number <- function(x, field, source_line) {
  value <- stringr::str_trim(x)
  missing <- is.na(value) | value == ""

  parsed <- suppressWarnings(
    readr::parse_double(
      value,
      na = "",
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  )

  invalid <- which(
    !missing & (is.na(parsed) | !is.finite(parsed))
  )

  if (length(invalid) > 0L) {
    i <- invalid[[1L]]

    cli::cli_abort(
      "Valor numérico inválido em {.field {field}}, linha
       {source_line[[i]]}: {.val {x[[i]]}}."
    )
  }

  parsed
}


daycoval_parse_date <- function(x, field, source_line) {
  value <- stringr::str_trim(x)
  missing <- is.na(value) | value == ""

  parsed <- suppressWarnings(
    readr::parse_date(
      value,
      format = "%d/%m/%Y",
      na = ""
    )
  )

  invalid <- which(!missing & is.na(parsed))

  if (length(invalid) > 0L) {
    i <- invalid[[1L]]

    cli::cli_abort(
      "Data inválida em {.field {field}}, linha
       {source_line[[i]]}: {.val {x[[i]]}}."
    )
  }

  parsed
}


parse_daycoval_entries <- function(entries) {
  numeric_columns <- c(
    "qt",
    "qt_bloq",
    "qt_disp",
    "pu_cst",
    "vl_cst",
    "pu_mrc",
    "vl_mrc",
    "mrc_bloq",
    "vl_atv_pl",
    "n_cotas",
    "vl_cotas",
    "valor_total_atv"
  )

  date_columns <- c(
    "data_emis",
    "data_venc",
    "data_aplic"
  )

  required_columns <- c(
    "source_line",
    "codigo_crt",
    "titulo",
    "grp_n1",
    "grp_n2",
    "grp_n3",
    numeric_columns,
    date_columns
  )

  missing_columns <- setdiff(
    required_columns,
    names(entries)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  entries |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_columns),
        ~ daycoval_parse_number(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      ),
      dplyr::across(
        dplyr::all_of(date_columns),
        ~ daycoval_parse_date(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      )
    )
}


daycoval_metadata_value <- function(entries, field) {
  values <- entries[[field]]

  if (is.character(values)) {
    values <- stringr::str_trim(values)
    values[values == ""] <- NA_character_
  }

  unique_values <- unique(values)

  if (length(unique_values) != 1L || anyNA(unique_values)) {
    cli::cli_abort(
      "O campo {.field {field}} deve possuir um único valor
       válido em todos os lançamentos."
    )
  }

  unique_values[1L]
}


#' Extract Daycoval portfolio metadata
#'
#' Extracts portfolio identification, accounting date, share information,
#' total assets, and net asset value from parsed Daycoval portfolio entries.
#'
#' The net asset value is obtained from the PATLIQ accounting entry,
#' rather than from the total assets reported in the CSV.
#'
#' @param entries A tibble of typed portfolio entries, as returned by
#'   `parse_daycoval_entries()`.
#'
#' @return A named list containing the portfolio metadata and the source
#'   identifiers of the net asset value record.
#'
#' @keywords internal
parse_daycoval_metadata <- function(entries) {
  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "codigo_crt",
    "nome_crt",
    "tp_crt",
    "administrador",
    "moeda",
    "data_emis",
    "n_cotas",
    "vl_cotas",
    "n_cotista",
    "valor_total_atv",
    "titulo",
    "grp_n1",
    "grp_n2",
    "grp_n3",
    "vl_mrc"
  )

  missing_columns <- setdiff(required_columns, names(entries))

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  if (nrow(entries) == 0L) {
    cli::cli_abort("A carteira não contém lançamentos.")
  }

  data_posicao <- daycoval_metadata_value(entries, "data_emis")

  if (!inherits(data_posicao, "Date")) {
    cli::cli_abort(
      "O campo {.field data_emis} deve possuir a classe Date."
    )
  }

  numero_cotas <- daycoval_metadata_value(entries, "n_cotas")
  valor_cota <- daycoval_metadata_value(entries, "vl_cotas")
  total_ativos <- daycoval_metadata_value(entries, "valor_total_atv")

  financial_values <- list(
    n_cotas = numero_cotas,
    vl_cotas = valor_cota,
    valor_total_atv = total_ativos
  )

  valid_financial_values <- vapply(
    financial_values,
    function(x) is.numeric(x) && is.finite(x),
    logical(1)
  )

  if (!all(valid_financial_values)) {
    cli::cli_abort(
      "Os campos financeiros do metadata devem conter
       valores numéricos finitos."
    )
  }

  numero_cotistas_raw <- daycoval_metadata_value(
    entries,
    "n_cotista"
  )

  if (!grepl("^[0-9]+$", as.character(numero_cotistas_raw))) {
    cli::cli_abort(
      "O campo {.field n_cotista} deve conter um inteiro
       não negativo."
    )
  }

  numero_cotistas <- suppressWarnings(
    as.integer(numero_cotistas_raw)
  )

  if (is.na(numero_cotistas)) {
    cli::cli_abort(
      "O campo {.field n_cotista} está fora do intervalo
       de inteiros suportado."
    )
  }

  patliq <- entries |>
    dplyr::filter(
      stringr::str_trim(.data$titulo) == "PATLIQ",
      .data$grp_n1 == "20000",
      .data$grp_n2 == "26000",
      .data$grp_n3 == "26000"
    )

  if (nrow(patliq) != 1L) {
    cli::cli_abort(
      "A carteira deve conter exatamente um lançamento
       PATLIQ no grupo contábil 20000/26000/26000."
    )
  }

  patrimonio_liquido <- patliq$vl_mrc[[1L]]

  if (
    !is.numeric(patrimonio_liquido) ||
    !is.finite(patrimonio_liquido)
  ) {
    cli::cli_abort(
      "O patrimônio líquido em PATLIQ deve ser
       um valor numérico finito."
    )
  }

  list(
    codigo_carteira = daycoval_metadata_value(entries, "codigo_crt"),
    nome_carteira = daycoval_metadata_value(entries, "nome_crt"),
    tipo_carteira = daycoval_metadata_value(entries, "tp_crt"),
    administrador = daycoval_metadata_value(entries, "administrador"),
    moeda = daycoval_metadata_value(entries, "moeda"),
    data_posicao = data_posicao,
    numero_cotas = numero_cotas,
    valor_cota = valor_cota,
    patrimonio_liquido = patrimonio_liquido,
    total_ativos = total_ativos,
    numero_cotistas = numero_cotistas,
    arquivo_origem = daycoval_metadata_value(entries, "source_file"),
    linha_patliq = patliq$source_line[[1L]],
    registro_patliq = patliq$record_number[[1L]]
  )
}


#' Read a Daycoval daily portfolio
#'
#' Imports a Daycoval/Sinqia daily portfolio CSV file, including
#' portfolio metadata, accounting entries, auxiliary details, and
#' original source records.
#'
#' The importer preserves the original accounting classifications
#' and does not perform financial reconciliation or valuation.
#'
#' @param path Path to a Daycoval daily portfolio CSV file.
#' @param source_name Name used to identify the original source file.
#' @param encoding Character encoding of the CSV file.
#'
#' @return An object of class `daycoval_portfolio`, containing:
#' \describe{
#'   \item{metadata}{Portfolio identification and financial metadata.}
#'   \item{entries}{Typed accounting entries and investment positions.}
#'   \item{details}{Auxiliary accounting details.}
#'   \item{raw}{All original CSV records and their source information.}
#' }
#'
#' @export
read_daycoval_portfolio <- function(
    path,
    source_name = basename(path),
    encoding = "Latin1"
) {
  records <- read_daycoval_records(
    path = path,
    source_name = source_name,
    encoding = encoding
  )

  entries <- records |>
    parse_daycoval_layout("LayCrtDiaDespRef") |>
    parse_daycoval_entries()

  metadata <- parse_daycoval_metadata(entries)

  metadata$benchmark <- records |>
    parse_daycoval_layout("RentabBenkmark") |>
    parse_daycoval_benchmark()

  details <- records |>
    parse_daycoval_layout("LayCrtDiaDespRefDet") |>
    link_daycoval_details(entries)

  structure(
    list(
      metadata = metadata,
      entries = entries,
      details = details,
      raw = records
    ),
    class = "daycoval_portfolio"
  )
}


link_daycoval_details <- function(details, entries) {
  required_details <- c(
    "source_file",
    "source_line",
    "valor"
  )

  required_entries <- c(
    "source_file",
    "source_line",
    "record_number",
    "vl_mrc"
  )

  if (
    !all(required_details %in% names(details)) ||
    !all(required_entries %in% names(entries))
  ) {
    cli::cli_abort(
      "Os lançamentos e detalhamentos não possuem
       as colunas necessárias para associação."
    )
  }

  if (!is.numeric(entries$vl_mrc)) {
    cli::cli_abort(
      "O campo {.field vl_mrc} deve ser numérico."
    )
  }

  if (anyDuplicated(entries[required_entries[1:2]]) > 0L) {
    cli::cli_abort(
      "Os lançamentos possuem identificadores de origem duplicados."
    )
  }

  if (nrow(details) == 0L) {
    return(
      details |>
        dplyr::mutate(
          entry_source_line = integer(),
          entry_record_number = character(),
          link_status = character()
        )
    )
  }

  entry_index <- entries |>
    dplyr::transmute(
      source_file = .data$source_file,
      candidate_source_line = .data$source_line,
      candidate_record_number = .data$record_number,
      candidate_value = .data$vl_mrc
    )

  result <- details |>
    dplyr::mutate(
      candidate_source_line = .data$source_line - 1L
    ) |>
    dplyr::left_join(
      entry_index,
      by = c("source_file", "candidate_source_line")
    )

  detail_value <- daycoval_parse_number(
    x = result$valor,
    field = "valor",
    source_line = result$source_line
  )

  matched <- !is.na(result$candidate_record_number) &
    !is.na(detail_value) &
    !is.na(result$candidate_value) &
    detail_value == result$candidate_value

  result |>
    dplyr::mutate(
      entry_source_line = dplyr::if_else(
        matched,
        .data$candidate_source_line,
        NA_integer_
      ),
      entry_record_number = dplyr::if_else(
        matched,
        .data$candidate_record_number,
        NA_character_
      ),
      link_status = dplyr::if_else(
        matched,
        "linked",
        "unlinked"
      )
    ) |>
    dplyr::select(
      -"candidate_source_line",
      -"candidate_record_number",
      -"candidate_value"
    )
}


parse_daycoval_benchmark <- function(benchmark) {
  numeric_columns <- c(
    "dia",
    "mes",
    "ano",
    "doze_meses",
    "vinte_qt_meses",
    "inicio"
  )

  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "indice",
    numeric_columns
  )

  missing_columns <- setdiff(
    required_columns,
    names(benchmark)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  benchmark <- benchmark |>
    dplyr::mutate(
      indice = stringr::str_trim(.data$indice)
    )

  if (
    anyNA(benchmark$indice) ||
    any(!nzchar(benchmark$indice))
  ) {
    cli::cli_abort(
      "O campo {.field indice} deve identificar o benchmark."
    )
  }

  benchmark |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_columns),
        ~ daycoval_parse_number(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      )
    )
}


daycoval_parse_integer <- function(x, field, source_line) {
  value <- stringr::str_trim(x)
  missing <- is.na(value) | value == ""

  valid_format <- grepl("^[0-9]+$", value)

  parsed <- suppressWarnings(
    as.integer(value)
  )

  invalid <- which(
    !missing & (!valid_format | is.na(parsed))
  )

  if (length(invalid) > 0L) {
    i <- invalid[[1L]]

    cli::cli_abort(
      "Valor inteiro inválido em {.field {field}}, linha
       {source_line[[i]]}: {.val {x[[i]]}}."
    )
  }

  parsed
}



daycoval_parse_flag <- function(x, field, source_line) {
  value <- stringr::str_trim(x)
  missing <- is.na(value) | value == ""

  invalid <- which(
    !missing & !(value %in% c("S", "N"))
  )

  if (length(invalid) > 0L) {
    i <- invalid[[1L]]

    cli::cli_abort(
      "Indicador inválido em {.field {field}}, linha
       {source_line[[i]]}: {.val {x[[i]]}}."
    )
  }

  result <- rep(NA, length(value))

  result[which(value == "S")] <- TRUE
  result[which(value == "N")] <- FALSE

  result
}


parse_daycoval_entries <- function(entries) {
  core_numeric_columns <- c(
    "qt",
    "qt_bloq",
    "qt_disp",
    "pu_cst",
    "vl_cst",
    "pu_mrc",
    "vl_mrc",
    "mrc_bloq",
    "vl_atv_pl",
    "n_cotas",
    "vl_cotas",
    "valor_total_atv"
  )

  additional_numeric_columns <- c(
    "dia",
    "mes",
    "ano",
    "doze_meses",
    "vinte_qt_meses",
    "inicio",
    "n_cotas2",
    "vl_cotas2",
    "dia2",
    "mes2",
    "ano2",
    "doze_meses2",
    "vinte_qt_meses2",
    "inicio2",
    "n_cotista2",
    "vl_taxa",
    "vl_cota_sem_amrtz",
    "vl_cota_ant_amrtz"
  )

  integer_columns <- c(
    "n_cotista",
    "dec_pu",
    "dec_num_cota",
    "dec_val_cota",
    "dec_qtd"
  )

  date_columns <- c(
    "data_emis",
    "data_venc",
    "data_aplic"
  )

  logical_columns <- c(
    "tit_in465",
    "b_preco_extn",
    "b_calc_preco_intn"
  )

  required_columns <- c(
    "source_line",
    "codigo_crt",
    "titulo",
    "grp_n1",
    "grp_n2",
    "grp_n3",
    core_numeric_columns,
    date_columns
  )

  missing_columns <- setdiff(
    required_columns,
    names(entries)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  numeric_columns <- c(
    core_numeric_columns,
    additional_numeric_columns
  )

  numeric_columns <- intersect(
    numeric_columns,
    names(entries)
  )

  integer_columns <- intersect(
    integer_columns,
    names(entries)
  )

  logical_columns <- intersect(
    logical_columns,
    names(entries)
  )

  entries |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_columns),
        ~ daycoval_parse_number(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      ),
      dplyr::across(
        dplyr::all_of(integer_columns),
        ~ daycoval_parse_integer(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      ),
      dplyr::across(
        dplyr::all_of(date_columns),
        ~ daycoval_parse_date(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      ),
      dplyr::across(
        dplyr::all_of(logical_columns),
        ~ daycoval_parse_flag(
          x = .x,
          field = dplyr::cur_column(),
          source_line = source_line
        )
      )
    )
}

