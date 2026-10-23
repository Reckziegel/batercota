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
  } else if (record_type == "P1030Caixa") {
    numeric_columns <- c(
      "merc_blq_fin_atu",
      "vl_mrc",
      "vl_pat_liq"
    )

    integer_columns <- character()
    date_columns <- character()
  } else if (record_type %in% c(
    "P1030AReceber",
    "P1030APagar"
  )) {
    numeric_columns <- c(
      "valor",
      "sld_tit",
      "tot_grp",
      "tot_geral",
      "vl_pat_liq"
    )

    integer_columns <- character()
    date_columns <- "dt_venc"
  } else if (record_type == "P1030RF") {
    numeric_columns <- c(
      "fator_adic",
      "tx_emis",
      "pu_compra",
      "pu_bruto",
      "qtd",
      "qtd_blq",
      "vl_cst",
      "vl_mrc",
      "vl_pat_liq",
      "vl_iriof"
    )

    integer_columns <- c(
      "dec_taxa",
      "dec_qtd",
      "dec_pu"
    )

    date_columns <- c(
      "dt_emis",
      "dt_venc",
      "dt_base",
      "dt_compra",
      "dt_ini_uso_ccdi"
    )
  } else if (record_type == "P1030RV") {
    numeric_columns <- c(
      "qtd",
      "qtd_blq",
      "vl_mrc",
      "vl_pat_liq",
      "pu_compra",
      "pu_atual_bruto"
    )

    integer_columns <- c(
      "dec_pu",
      "dec_qtd"
    )

    date_columns <- "data"
  } else if (record_type == "P1030Futuros") {
    numeric_columns <- c(
      "qtd",
      "vl_noc",
      "ajuste",
      "vl_pat_liq",
      "ponto"
    )

    integer_columns <- character()
    date_columns <- "data_venc"
  } else if (record_type == "P1030MovCotistas") {
    numeric_columns <- c(
      "tot_apl",
      "tot_rgt"
    )

    integer_columns <- c(
      "num_dias_agnd_apl",
      "num_dias_agnd_rgt"
    )

    date_columns <- character()
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

#' Read a Daycoval portfolio position report
#'
#' Imports a Daycoval "Posição de Carteira C" CSV report,
#' preserving its original records, declared layouts, and
#' financial information.
#'
#' Supported layouts are converted to their corresponding
#' data types. Other declared layouts are retained as
#' character tables under `positions$other`.
#'
#' @param path Path to the Daycoval position CSV file.
#' @param source_name Name identifying the source file.
#' @param encoding Character encoding of the CSV file.
#'
#' @return An object of class `daycoval_position` containing
#'   `metadata`, `positions`, `cash`, `receivables`, `payables`,
#'   `returns`, `shareholder_flows`, `layouts`, and `raw`.
#'   Optional layouts absent from the CSV return `NULL`.
#'
#' @export
read_daycoval_position <- function(
    path,
    source_name = basename(path),
    encoding = "Latin1"
) {
  records <- read_daycoval_records(
    path = path,
    source_name = source_name,
    encoding = encoding
  )

  layouts <- parse_daycoval_position_layouts(records)

  required_layouts <- c(
    "P1030",
    "P1030Rentabilidades"
  )

  missing_layouts <- setdiff(
    required_layouts,
    layouts$record_type
  )

  if (length(missing_layouts) > 0L) {
    cli::cli_abort(
      "Layouts obrigatórios ausentes: {.or {missing_layouts}}."
    )
  }

  typed_layouts <- c(
    "P1030",
    "P1030RF",
    "P1030CF",
    "P1030RV",
    "P1030Futuros",
    "P1030Caixa",
    "P1030AReceber",
    "P1030APagar",
    "P1030Rentabilidades",
    "P1030MovCotistas"
  )

  parsed <- purrr::map(
    layouts$record_type,
    function(record_type) {
      data <- parse_daycoval_position_layout(
        records = records,
        layouts = layouts,
        record_type = record_type
      )

      if (record_type %in% typed_layouts) {
        return(
          parse_daycoval_position_types(
            data = data,
            record_type = record_type
          )
        )
      }

      data
    }
  )

  names(parsed) <- layouts$record_type

  get_layout <- function(record_type) {
    if (!record_type %in% names(parsed)) {
      return(NULL)
    }

    parsed[[record_type]]
  }

  metadata <- parse_daycoval_position_metadata(
    cadastro = get_layout("P1030"),
    rentabilidades = get_layout("P1030Rentabilidades")
  )

  codigo_carteira <- metadata$codigo_carteira

  for (record_type in names(parsed)) {
    data <- parsed[[record_type]]

    if (!"cd_crt" %in% names(data) || nrow(data) == 0L) {
      next
    }

    identifiers <- stringr::str_trim(data$cd_crt)

    invalid <- is.na(identifiers) |
      identifiers != codigo_carteira

    if (any(invalid)) {
      cli::cli_abort(
        "O layout {.val {record_type}} contém registros
         de outra carteira."
      )
    }
  }

  other_layouts <- parsed[
    !(names(parsed) %in% typed_layouts)
  ]

  positions <- list(
    fixed_income = get_layout("P1030RF"),
    funds = get_layout("P1030CF"),
    equities = get_layout("P1030RV"),
    futures = get_layout("P1030Futuros"),
    other = other_layouts
  )

  structure(
    list(
      metadata = metadata,
      positions = positions,
      cash = get_layout("P1030Caixa"),
      receivables = get_layout("P1030AReceber"),
      payables = get_layout("P1030APagar"),
      returns = get_layout("P1030Rentabilidades"),
      shareholder_flows = get_layout("P1030MovCotistas"),
      layouts = layouts,
      raw = records
    ),
    class = "daycoval_position"
  )
}


#' Reconcile Daycoval portfolio reports
#'
#' Reconciles a Daycoval daily portfolio with a Position C report,
#' preserving source references, matching decisions, and financial
#' differences.
#'
#' The initial implementation supports investment fund positions only.
#' Matching is based on operational identifiers, independently of
#' quantities and market values.
#'
#' @param portfolio A `daycoval_portfolio` object.
#' @param position A `daycoval_position` object.
#' @param components Character scalar specifying the component to reconcile.
#'   Currently, only `"funds"` is supported.
#'
#' @return A `daycoval_reconciliation` object containing:
#' \describe{
#'   \item{metadata}{Report identification and reconciliation scope.}
#'   \item{matches}{Unique associations between source records.}
#'   \item{checks}{Financial comparisons for associated records.}
#'   \item{issues}{Unmatched, ambiguous, or unclassified records.}
#'   \item{summary}{Reconciliation counts and coverage.}
#' }
#'
#' @export
reconcile_daycoval <- function(
    portfolio,
    position,
    components = "funds"
) {
  daycoval_validate_reconciliation(
    portfolio = portfolio,
    position = position,
    components = components
  )

  daycoval_reconcile_funds(
    portfolio = portfolio,
    position = position
  )
}


daycoval_validate_reconciliation <- function(
    portfolio,
    position,
    components
) {
  if (!inherits(portfolio, "daycoval_portfolio")) {
    cli::cli_abort(
      "`portfolio` deve ser um objeto {.cls daycoval_portfolio}."
    )
  }

  if (!inherits(position, "daycoval_position")) {
    cli::cli_abort(
      "`position` deve ser um objeto {.cls daycoval_position}."
    )
  }

  if (!identical(components, "funds")) {
    cli::cli_abort(
      "`components` deve ser {.val funds} nesta versão."
    )
  }

  required_metadata <- c(
    "codigo_carteira",
    "data_posicao",
    "arquivo_origem"
  )

  for (object in list(portfolio, position)) {
    if (!is.list(object$metadata) ||
        !all(required_metadata %in% names(object$metadata))) {
      cli::cli_abort(
        "Os relatórios não possuem os metadados obrigatórios."
      )
    }

    metadata <- object$metadata

    valid_code <- is.character(metadata$codigo_carteira) &&
      length(metadata$codigo_carteira) == 1L &&
      !is.na(metadata$codigo_carteira) &&
      nzchar(stringr::str_trim(metadata$codigo_carteira))

    valid_date <- inherits(metadata$data_posicao, "Date") &&
      length(metadata$data_posicao) == 1L &&
      !is.na(metadata$data_posicao)

    if (!valid_code || !valid_date) {
      cli::cli_abort(
        "Os relatórios devem possuir carteira e data válidas."
      )
    }
  }

  if (!identical(
    stringr::str_trim(portfolio$metadata$codigo_carteira),
    stringr::str_trim(position$metadata$codigo_carteira)
  )) {
    cli::cli_abort(
      "Os relatórios pertencem a carteiras diferentes."
    )
  }

  if (!identical(
    portfolio$metadata$data_posicao,
    position$metadata$data_posicao
  )) {
    cli::cli_abort(
      "Os relatórios possuem datas diferentes."
    )
  }

  invisible(NULL)
}


daycoval_require_reconciliation_columns <- function(
    data,
    columns,
    source
) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.val {source}} deve conter uma tabela válida."
    )
  }

  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Colunas ausentes em {.val {source}}: {.or {missing_columns}}."
    )
  }

  invisible(NULL)
}


daycoval_prepare_fund_candidates <- function(portfolio, position) {
  fund_groups <- c(
    "15801",
    "15803",
    "15805",
    "15812",
    "16007",
    "16008"
  )

  entries <- portfolio$entries
  funds <- position$positions$funds

  if (is.null(funds)) {
    funds <- tibble::tibble(
      source_file = character(),
      source_line = integer(),
      record_number = character(),
      titulo = character(),
      qtd = double(),
      vl_mrc = double(),
      cnpj = character(),
      isin = character()
    )
  }

  daycoval_require_reconciliation_columns(
    data = entries,
    columns = c(
      "source_file",
      "source_line",
      "record_number",
      "titulo",
      "grp_n1",
      "grp_n2",
      "grp_n3",
      "qt",
      "vl_mrc"
    ),
    source = "portfolio$entries"
  )

  daycoval_require_reconciliation_columns(
    data = funds,
    columns = c(
      "source_file",
      "source_line",
      "record_number",
      "titulo",
      "qtd",
      "vl_mrc",
      "cnpj",
      "isin"
    ),
    source = "position$positions$funds"
  )

  if (!is.numeric(entries$qt) ||
      !is.numeric(entries$vl_mrc) ||
      !is.numeric(funds$qtd) ||
      !is.numeric(funds$vl_mrc)) {
    cli::cli_abort(
      "Quantidades e valores de mercado devem ser numéricos."
    )
  }

  position_funds <- funds |>
    dplyr::transmute(
      key = dplyr::na_if(
        stringr::str_trim(.data$titulo),
        ""
      ),
      position_source_file = .data$source_file,
      position_source_line = .data$source_line,
      position_record_number = .data$record_number,
      position_title = .data$titulo,
      position_quantity = .data$qtd,
      position_market_value = .data$vl_mrc,
      position_cnpj = .data$cnpj,
      position_isin = .data$isin
    )

  position_keys <- unique(
    position_funds$key[!is.na(position_funds$key)]
  )

  portfolio_funds <- entries |>
    dplyr::mutate(
      key = dplyr::na_if(
        stringr::str_trim(.data$titulo),
        ""
      ),
      accounting_group = stringr::str_trim(.data$grp_n3),
      mapped_group = .data$grp_n1 == "10000" &
        .data$accounting_group %in% fund_groups
    ) |>
    dplyr::filter(
      .data$mapped_group |
        (!is.na(.data$key) & .data$key %in% position_keys)
    ) |>
    dplyr::transmute(
      key = .data$key,
      portfolio_source_file = .data$source_file,
      portfolio_source_line = .data$source_line,
      portfolio_record_number = .data$record_number,
      portfolio_title = .data$titulo,
      portfolio_group_n1 = .data$grp_n1,
      portfolio_group_n2 = .data$grp_n2,
      portfolio_group_n3 = .data$grp_n3,
      portfolio_quantity = .data$qt,
      portfolio_market_value = .data$vl_mrc,
      classification_status = dplyr::if_else(
        .data$mapped_group,
        "mapped",
        "unmapped"
      )
    )

  if (anyDuplicated(portfolio_funds[
    c("portfolio_source_file", "portfolio_source_line")
  ])) {
    cli::cli_abort(
      "A Carteira Diária contém referências de origem duplicadas."
    )
  }

  if (anyDuplicated(position_funds[
    c("position_source_file", "position_source_line")
  ])) {
    cli::cli_abort(
      "A Posição C contém referências de origem duplicadas."
    )
  }

  list(
    portfolio = portfolio_funds,
    position = position_funds,
    fund_groups = fund_groups
  )
}


daycoval_match_funds <- function(portfolio_funds, position_funds) {
  portfolio_counts <- portfolio_funds |>
    dplyr::filter(!is.na(.data$key)) |>
    dplyr::count(.data$key, name = "n_portfolio")

  position_counts <- position_funds |>
    dplyr::filter(!is.na(.data$key)) |>
    dplyr::count(.data$key, name = "n_position")

  counts <- dplyr::full_join(
    portfolio_counts,
    position_counts,
    by = "key"
  ) |>
    dplyr::mutate(
      n_portfolio = dplyr::coalesce(.data$n_portfolio, 0L),
      n_position = dplyr::coalesce(.data$n_position, 0L)
    )

  matched_keys <- counts |>
    dplyr::filter(
      .data$n_portfolio == 1L,
      .data$n_position == 1L
    ) |>
    dplyr::pull(.data$key)

  ambiguous_keys <- counts |>
    dplyr::filter(
      .data$n_portfolio > 0L,
      .data$n_position > 0L,
      .data$n_portfolio > 1L | .data$n_position > 1L
    ) |>
    dplyr::pull(.data$key)

  matches <- portfolio_funds |>
    dplyr::filter(.data$key %in% matched_keys) |>
    dplyr::inner_join(
      position_funds |>
        dplyr::filter(.data$key %in% matched_keys),
      by = "key"
    ) |>
    dplyr::arrange(.data$key) |>
    dplyr::mutate(
      match_id = sprintf(
        "funds_%04d",
        dplyr::row_number()
      ),
      component = "funds",
      match_type = "direct",
      rule_id = "funds_operational_title_v1",
      .before = 1L
    )

  list(
    matches = matches,
    ambiguous_keys = ambiguous_keys
  )
}


daycoval_check_fund_field <- function(
    matches,
    field,
    portfolio_column,
    position_column
) {
  matches |>
    dplyr::transmute(
      match_id = .data$match_id,
      component = "funds",
      field = .env$field,
      portfolio_value = .data[[portfolio_column]],
      position_value = .data[[position_column]]
    ) |>
    dplyr::mutate(
      difference = .data$portfolio_value - .data$position_value,
      status = dplyr::case_when(
        is.na(.data$portfolio_value) |
          is.na(.data$position_value) ~ "not_evaluated",
        .data$portfolio_value == .data$position_value ~ "equal",
        TRUE ~ "different"
      )
    )
}


daycoval_check_funds <- function(matches) {
  quantities <- daycoval_check_fund_field(
    matches = matches,
    field = "quantity",
    portfolio_column = "portfolio_quantity",
    position_column = "position_quantity"
  )

  market_values <- daycoval_check_fund_field(
    matches = matches,
    field = "market_value",
    portfolio_column = "portfolio_market_value",
    position_column = "position_market_value"
  )

  dplyr::bind_rows(
    quantities,
    market_values
  ) |>
    dplyr::arrange(.data$match_id, .data$field)
}


daycoval_fund_issues <- function(
    portfolio_funds,
    position_funds,
    matches,
    ambiguous_keys
) {
  portfolio_issues <- portfolio_funds |>
    dplyr::anti_join(
      matches,
      by = c(
        "portfolio_source_file",
        "portfolio_source_line"
      )
    ) |>
    dplyr::transmute(
      component = "funds",
      issue_type = dplyr::if_else(
        !is.na(.data$key) & .data$key %in% ambiguous_keys,
        "ambiguous",
        "unmatched_portfolio"
      ),
      key = .data$key,
      source_report = "portfolio",
      source_file = .data$portfolio_source_file,
      source_line = .data$portfolio_source_line,
      record_number = .data$portfolio_record_number,
      accounting_group = .data$portfolio_group_n3
    )

  position_issues <- position_funds |>
    dplyr::anti_join(
      matches,
      by = c(
        "position_source_file",
        "position_source_line"
      )
    ) |>
    dplyr::transmute(
      component = "funds",
      issue_type = dplyr::if_else(
        !is.na(.data$key) & .data$key %in% ambiguous_keys,
        "ambiguous",
        "unmatched_position"
      ),
      key = .data$key,
      source_report = "position",
      source_file = .data$position_source_file,
      source_line = .data$position_source_line,
      record_number = .data$position_record_number,
      accounting_group = NA_character_
    )

  classification_issues <- matches |>
    dplyr::filter(.data$classification_status == "unmapped") |>
    dplyr::transmute(
      component = "funds",
      issue_type = "unmapped_accounting_group",
      key = .data$key,
      source_report = "portfolio",
      source_file = .data$portfolio_source_file,
      source_line = .data$portfolio_source_line,
      record_number = .data$portfolio_record_number,
      accounting_group = .data$portfolio_group_n3
    )

  dplyr::bind_rows(
    portfolio_issues,
    position_issues,
    classification_issues
  ) |>
    dplyr::arrange(
      .data$issue_type,
      .data$source_report,
      .data$source_line
    )
}


daycoval_summarise_funds <- function(
    portfolio_funds,
    position_funds,
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
    portfolio_records = nrow(portfolio_funds),
    position_records = nrow(position_funds),
    matched = nrow(matches),
    matched_equal = sum(match_results$equal),
    matched_different = sum(match_results$different),
    matched_not_evaluated = sum(
      !match_results$equal & !match_results$different
    ),
    unmatched_portfolio = sum(
      issues$issue_type == "unmatched_portfolio"
    ),
    unmatched_position = sum(
      issues$issue_type == "unmatched_position"
    ),
    ambiguous_records = sum(
      issues$issue_type == "ambiguous"
    ),
    unmapped_accounting_groups = sum(
      issues$issue_type == "unmapped_accounting_group"
    )
  )
}


daycoval_reconcile_funds <- function(portfolio, position) {
  candidates <- daycoval_prepare_fund_candidates(
    portfolio = portfolio,
    position = position
  )

  portfolio_funds <- candidates$portfolio
  position_funds <- candidates$position

  matching <- daycoval_match_funds(
    portfolio_funds = portfolio_funds,
    position_funds = position_funds
  )

  matches <- matching$matches

  checks <- daycoval_check_funds(matches)

  issues <- daycoval_fund_issues(
    portfolio_funds = portfolio_funds,
    position_funds = position_funds,
    matches = matches,
    ambiguous_keys = matching$ambiguous_keys
  )

  summary <- daycoval_summarise_funds(
    portfolio_funds = portfolio_funds,
    position_funds = position_funds,
    matches = matches,
    checks = checks,
    issues = issues
  )

  metadata <- list(
    codigo_carteira = portfolio$metadata$codigo_carteira,
    data_posicao = portfolio$metadata$data_posicao,
    arquivo_carteira = portfolio$metadata$arquivo_origem,
    arquivo_posicao = position$metadata$arquivo_origem,
    components = "funds",
    mapped_fund_groups = candidates$fund_groups,
    classification_scope = "observed_groups_and_shared_titles"
  )

  structure(
    list(
      metadata = metadata,
      matches = matches,
      checks = checks,
      issues = issues,
      summary = summary
    ),
    class = "daycoval_reconciliation"
  )
}

