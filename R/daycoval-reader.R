
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

