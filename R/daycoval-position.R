parse_daycoval_position_layouts <- function(records) {
  required_columns <- c(
    "source_file",
    "source_line",
    "record_number",
    "record_type",
    "record_flag",
    "n_fields",
    "fields"
  )

  missing_columns <- setdiff(required_columns, names(records))

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  separators <- records |>
    dplyr::filter(is.na(.data$record_type))

  if (any(separators$n_fields != 1L)) {
    cli::cli_abort(
      "O arquivo contém registros sem tipo e com estrutura inválida."
    )
  }

  content <- records |>
    dplyr::filter(!is.na(.data$record_type))

  if (nrow(content) == 0L) {
    cli::cli_abort("O arquivo não contém layouts.")
  }

  valid_flags <- c("C", "V")

  if (any(is.na(content$record_flag) |
          !(content$record_flag %in% valid_flags))) {
    cli::cli_abort(
      "O arquivo contém indicadores de registro inválidos."
    )
  }

  headers <- content |>
    dplyr::filter(.data$record_flag == "C")

  if (nrow(headers) == 0L) {
    cli::cli_abort(
      "O arquivo não contém cabeçalhos de layout."
    )
  }

  duplicated_types <- headers$record_type[
    duplicated(headers$record_type)
  ]

  if (length(duplicated_types) > 0L) {
    record_type <- duplicated_types[[1L]]

    cli::cli_abort(
      "O layout {.val {record_type}} deve conter exatamente um cabeçalho."
    )
  }

  data_types <- content |>
    dplyr::filter(.data$record_flag == "V") |>
    dplyr::pull(.data$record_type) |>
    unique()

  missing_headers <- setdiff(data_types, headers$record_type)

  if (length(missing_headers) > 0L) {
    cli::cli_abort(
      "Registros de dados sem cabeçalho: {.or {missing_headers}}."
    )
  }

  purrr::map_dfr(seq_len(nrow(headers)), function(i) {
    header <- headers[i, ]

    record_type <- header$record_type[[1L]]
    record_width <- header$n_fields[[1L]]

    original_names <- header$fields[[1L]][-seq_len(3L)]
    field_names <- daycoval_snake_case(original_names)

    if (
      length(field_names) == 0L ||
      anyNA(field_names) ||
      any(!nzchar(field_names))
    ) {
      cli::cli_abort(
        "O layout {.val {record_type}} contém nomes de campos inválidos."
      )
    }

    if (anyDuplicated(field_names)) {
      cli::cli_abort(
        "O layout {.val {record_type}} contém nomes de campos duplicados."
      )
    }

    source_columns <- c(
      "source_file",
      "source_line",
      "record_number"
    )

    if (any(field_names %in% source_columns)) {
      cli::cli_abort(
        "O layout {.val {record_type}} possui campos reservados."
      )
    }

    values <- content |>
      dplyr::filter(
        .data$record_type == .env$record_type,
        .data$record_flag == "V"
      )

    invalid_width <- which(values$n_fields != record_width)

    if (length(invalid_width) > 0L) {
      source_line <- values$source_line[[invalid_width[[1L]]]]

      cli::cli_abort(
        "O layout {.val {record_type}} possui largura inválida na linha
         {source_line}."
      )
    }

    tibble::tibble(
      record_type = record_type,
      header_source_line = header$source_line,
      header_record_number = header$record_number,
      record_width = record_width,
      n_columns = length(field_names),
      n_records = nrow(values),
      original_names = list(original_names),
      field_names = list(field_names)
    )
  })
}


parse_daycoval_position_layout <- function(
    records,
    layouts,
    record_type
) {
  valid_record_type <- is.character(record_type) &&
    length(record_type) == 1L &&
    !is.na(record_type) &&
    nzchar(record_type)

  if (!valid_record_type) {
    cli::cli_abort(
      "`record_type` deve identificar um layout válido."
    )
  }

  required_columns <- c(
    "record_type",
    "record_width",
    "field_names"
  )

  if (!all(required_columns %in% names(layouts))) {
    cli::cli_abort(
      "`layouts` não contém as informações obrigatórias."
    )
  }

  layout_index <- match(record_type, layouts$record_type)

  if (is.na(layout_index)) {
    cli::cli_abort(
      "O layout {.val {record_type}} não foi encontrado."
    )
  }

  field_names <- layouts$field_names[[layout_index]]
  record_width <- layouts$record_width[[layout_index]]

  values <- records |>
    dplyr::filter(
      .data$record_type == .env$record_type,
      .data$record_flag == "V"
    )

  if (any(values$n_fields != record_width)) {
    cli::cli_abort(
      "O layout {.val {record_type}} contém registros inválidos."
    )
  }

  data_fields <- purrr::map(
    values$fields,
    ~ .x[-seq_len(3L)]
  )

  data_matrix <- matrix(
    data = as.character(
      unlist(data_fields, use.names = FALSE)
    ),
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

parse_daycoval_position_types <- function(data, record_type) {
  valid_record_type <- is.character(record_type) &&
    length(record_type) == 1L &&
    !is.na(record_type) &&
    nzchar(record_type)

  if (!valid_record_type) {
    cli::cli_abort(
      "`record_type` deve identificar um layout válido."
    )
  }

  if (record_type == "P1030") {
    numeric_columns <- character()
    integer_columns <- character()
    date_columns <- "data"
  } else if (record_type == "P1030CF") {
    numeric_columns <- c(
      "qtd",
      "qtd_blq",
      "vl_mrc",
      "vl_taxas",
      "vl_pat_liq",
      "pu",
      "vl_atu_bruto"
    )

    integer_columns <- c(
      "dec_qtd",
      "dec_pu"
    )

    date_columns <- "dt_apl"
  } else if (record_type == "P1030Rentabilidades") {
    numeric_columns <- c(
      "num_cotas",
      "vl_cota",
      "vl_pat_liq",
      "vl_indice_bm",
      "var_cota",
      "var_indice_bm",
      "perc_bm"
    )

    integer_columns <- c(
      "dec_num_cotas",
      "dec_vl_cota"
    )

    date_columns <- "data"
  } else {
    cli::cli_abort(
      "A tipagem do layout {.val {record_type}} não está definida."
    )
  }

  required_columns <- c(
    "source_line",
    numeric_columns,
    integer_columns,
    date_columns
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas obrigatórias ausentes: {.or {missing_columns}}."
    )
  }

  data |>
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
      )
    )
}

parse_daycoval_position_metadata <- function(
    cadastro,
    rentabilidades
) {
  required_cadastro <- c(
    "source_file",
    "source_line",
    "record_number",
    "cd_crt",
    "nome_curto",
    "nome_longo",
    "cnpj",
    "tipo_carteira",
    "administrador",
    "gestor",
    "data"
  )

  required_rentabilidades <- c(
    "source_line",
    "record_number",
    "cd_crt",
    "data",
    "num_cotas",
    "vl_cota",
    "vl_pat_liq",
    "indice_bm"
  )

  missing_cadastro <- setdiff(
    required_cadastro,
    names(cadastro)
  )

  missing_rentabilidades <- setdiff(
    required_rentabilidades,
    names(rentabilidades)
  )

  if (length(missing_cadastro) > 0L) {
    cli::cli_abort(
      "Colunas cadastrais ausentes: {.or {missing_cadastro}}."
    )
  }

  if (length(missing_rentabilidades) > 0L) {
    cli::cli_abort(
      "Colunas de rentabilidade ausentes:
       {.or {missing_rentabilidades}}."
    )
  }

  if (nrow(cadastro) != 1L) {
    cli::cli_abort(
      "O layout P1030 deve conter exatamente um registro cadastral."
    )
  }

  if (
    !inherits(cadastro$data, "Date") ||
    !inherits(rentabilidades$data, "Date")
  ) {
    cli::cli_abort(
      "As datas devem ser convertidas antes da extração dos metadados."
    )
  }

  codigo_carteira <- stringr::str_trim(
    cadastro$cd_crt[[1L]]
  )

  data_posicao <- cadastro$data[[1L]]

  if (
    is.na(codigo_carteira) ||
    !nzchar(codigo_carteira) ||
    is.na(data_posicao)
  ) {
    cli::cli_abort(
      "O cadastro deve identificar a carteira e a data da posição."
    )
  }

  codigos_rentabilidade <- stringr::str_trim(
    rentabilidades$cd_crt
  )

  invalid_codes <- is.na(codigos_rentabilidade) |
    codigos_rentabilidade != codigo_carteira

  if (any(invalid_codes)) {
    cli::cli_abort(
      "O histórico de rentabilidades contém outra carteira."
    )
  }

  matching_rows <- which(
    !is.na(rentabilidades$data) &
      rentabilidades$data == data_posicao
  )

  if (length(matching_rows) > 1L) {
    cli::cli_abort(
      "O histórico contém mais de um registro para a data da posição."
    )
  }

  has_match <- length(matching_rows) == 1L

  text_value <- function(field) {
    value <- stringr::str_trim(cadastro[[field]][[1L]])

    if (is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }

    value
  }

  financial_value <- function(field) {
    if (!has_match) {
      return(NA_real_)
    }

    rentabilidades[[field]][[matching_rows]]
  }

  benchmark <- NA_character_
  registro_rentabilidade <- NA_character_
  linha_rentabilidade <- NA_integer_

  if (has_match) {
    i <- matching_rows[[1L]]

    benchmark <- stringr::str_trim(
      rentabilidades$indice_bm[[i]]
    )

    if (is.na(benchmark) || !nzchar(benchmark)) {
      benchmark <- NA_character_
    }

    registro_rentabilidade <- rentabilidades$record_number[[i]]
    linha_rentabilidade <- rentabilidades$source_line[[i]]
  }

  list(
    arquivo_origem = cadastro$source_file[[1L]],
    codigo_carteira = codigo_carteira,
    nome_curto = text_value("nome_curto"),
    nome_longo = text_value("nome_longo"),
    cnpj = text_value("cnpj"),
    tipo_carteira = text_value("tipo_carteira"),
    administrador = text_value("administrador"),
    gestor = text_value("gestor"),
    data_posicao = data_posicao,
    patrimonio_liquido = financial_value("vl_pat_liq"),
    numero_cotas = financial_value("num_cotas"),
    valor_cota = financial_value("vl_cota"),
    indice_benchmark = benchmark,
    registro_cadastro = cadastro$record_number[[1L]],
    linha_cadastro = cadastro$source_line[[1L]],
    registro_rentabilidade = registro_rentabilidade,
    linha_rentabilidade = linha_rentabilidade
  )
}
