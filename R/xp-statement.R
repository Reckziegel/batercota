
#' Read an XP consolidated shareholder statement
#'
#' Imports the consolidated investment positions and document metadata from
#' an XP shareholder statement PDF. Original text tokens and their page
#' coordinates are preserved for auditability.
#'
#' Only the consolidated positions are imported in this version. Historical
#' balances and application lots are reserved for subsequent milestones.
#'
#' The financial amounts displayed by XP are preserved as reported. They are
#' not reconstructed from the published unit prices and quantities.
#'
#' @param path Path to an XP consolidated shareholder statement PDF.
#' @param source_name Name identifying the original source file.
#'
#' @return An object of class `xp_statement` containing `metadata`,
#'   `positions`, `balances`, `lots`, and `raw`. The `balances` and `lots`
#'   components are `NULL` until their importers are implemented.
#'
#' @export
read_xp_statement <- function(path, source_name = basename(path)) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path)

  if (!valid_path || !file.exists(path) || dir.exists(path)) {
    cli::cli_abort("`path` deve apontar para um arquivo PDF existente.")
  }

  valid_source <- is.character(source_name) &&
    length(source_name) == 1L &&
    !is.na(source_name) &&
    nzchar(source_name)

  if (!valid_source) {
    cli::cli_abort("`source_name` deve ser um texto não vazio.")
  }

  raw <- xp_read_tokens(path, source_name)
  lines <- xp_reconstruct_lines(raw)
  metadata <- xp_parse_metadata(lines, source_name)
  consolidated <- xp_parse_positions(raw, lines)

  metadata <- c(metadata, consolidated$totals)
  xp_validate_totals(consolidated$positions, metadata)

  structure(
    list(
      metadata = metadata,
      positions = consolidated$positions,
      balances = NULL,
      lots = NULL,
      raw = raw
    ),
    class = "xp_statement"
  )
}


xp_read_tokens <- function(path, source_name) {
  pages <- tryCatch(
    pdftools::pdf_data(path),
    error = function(error) {
      cli::cli_abort(
        "Não foi possível extrair o texto do PDF XP: {conditionMessage(error)}."
      )
    }
  )

  if (length(pages) == 0L) {
    cli::cli_abort("O PDF XP não possui páginas.")
  }

  tokens <- purrr::map2_dfr(
    pages,
    seq_along(pages),
    function(page, page_number) {
      required <- c("text", "x", "y", "width", "height")

      if (!all(required %in% names(page))) {
        cli::cli_abort(
          "A página {page_number} não contém os dados posicionais esperados."
        )
      }

      tibble::as_tibble(page) |>
        dplyr::transmute(
          source_file = source_name,
          source_page = as.integer(page_number),
          source_token = dplyr::row_number(),
          text = as.character(.data$text),
          x = as.numeric(.data$x),
          y = as.numeric(.data$y),
          width = as.numeric(.data$width),
          height = as.numeric(.data$height)
        )
    }
  )

  if (nrow(tokens) == 0L ||
      anyNA(tokens[c("text", "x", "y", "width", "height")]) ||
      any(!is.finite(as.matrix(tokens[c("x", "y", "width", "height")]))) ||
      any(tokens$height <= 0)) {
    cli::cli_abort("O PDF XP contém elementos textuais ausentes ou inválidos.")
  }

  tokens
}


xp_reconstruct_lines <- function(raw) {
  horizontal <- raw |>
    dplyr::group_by(.data$source_page) |>
    dplyr::mutate(
      typical_height = stats::median(.data$height),
      max_horizontal_height = 2.5 * .data$typical_height,
      character_count = pmax(nchar(.data$text), 1L),
      character_ratio = .data$width /
        (.data$height * .data$character_count)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$x >= 0,
      .data$height <= .data$max_horizontal_height,
      .data$character_ratio < 0.75
    ) |>
    dplyr::arrange(.data$source_page, .data$y, .data$x) |>
    dplyr::group_by(.data$source_page) |>
    dplyr::mutate(
      source_row = cumsum(c(
        TRUE,
        diff(.data$y) > stats::median(.data$height) / 3
      ))
    ) |>
    dplyr::ungroup()

  if (nrow(horizontal) == 0L) {
    cli::cli_abort("Não foram identificadas linhas horizontais no PDF XP.")
  }

  horizontal |>
    dplyr::group_by(.data$source_page, .data$source_row) |>
    dplyr::summarise(
      y = stats::median(.data$y),
      text = paste(.data$text, collapse = " "),
      source_tokens = list(.data$source_token),
      .groups = "drop"
    )
}


xp_unique_line <- function(lines, pattern, label) {
  matches <- lines |>
    dplyr::filter(stringr::str_detect(.data$text, pattern))

  if (nrow(matches) != 1L) {
    cli::cli_abort(paste0(
      "O PDF XP deve conter exatamente uma linha de {.val {label}}; ",
      "foram encontradas {nrow(matches)}."
    ))
  }

  matches
}


xp_parse_date <- function(value, field) {
  parsed <- as.Date(value, format = "%d/%m/%Y")

  if (length(parsed) != 1L ||
      is.na(parsed) ||
      format(parsed, "%d/%m/%Y") != value) {
    cli::cli_abort("Data inválida em {.field {field}}: {.val {value}}.")
  }

  parsed
}


xp_parse_metadata <- function(lines, source_name) {
  first_page <- lines |>
    dplyr::filter(.data$source_page == min(lines$source_page))

  movement <- xp_unique_line(
    first_page,
    "^Movimentação de [0-9]{2}/[0-9]{2}/[0-9]{4} a ",
    "período de movimentação"
  )

  period <- stringr::str_match(
    movement$text[[1L]],
    paste0(
      "^Movimentação de ([0-9]{2}/[0-9]{2}/[0-9]{4}) a ",
      "([0-9]{2}/[0-9]{2}/[0-9]{4})$"
    )
  )

  if (anyNA(period)) {
    cli::cli_abort("O período de movimentação possui formato inesperado.")
  }

  emission <- xp_unique_line(
    first_page,
    "^Emissão: [0-9]{2}/[0-9]{2}/[0-9]{4} ",
    "data de emissão"
  )

  issue <- stringr::str_match(
    emission$text[[1L]],
    paste0(
      "^Emissão: ([0-9]{2}/[0-9]{2}/[0-9]{4}) ",
      "([0-9]{2}:[0-9]{2}:[0-9]{2})$"
    )
  )

  if (anyNA(issue)) {
    cli::cli_abort("A data de emissão possui formato inesperado.")
  }

  issued_at <- as.POSIXct(
    paste(issue[1L, 2L], issue[1L, 3L]),
    format = "%d/%m/%Y %H:%M:%S",
    tz = "America/Sao_Paulo"
  )

  if (is.na(issued_at) ||
      format(issued_at, "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo") !=
      paste(issue[1L, 2L], issue[1L, 3L])) {
    cli::cli_abort("A data e hora de emissão são inválidas.")
  }

  account_lines <- lines |>
    dplyr::filter(
      .data$source_page == movement$source_page[[1L]],
      .data$y > movement$y[[1L]],
      .data$y < emission$y[[1L]],
      stringr::str_detect(.data$text, " - [0-9]+$")
    )

  if (nrow(account_lines) != 1L) {
    cli::cli_abort("Não foi possível identificar a conta e o fundo cotista XP.")
  }

  account <- stringr::str_match(
    account_lines$text[[1L]],
    "^(.+) - ([0-9]+)$"
  )

  advisor <- xp_unique_line(
    first_page,
    "^Emitido pelo assessor\\(a\\): ",
    "código do assessor"
  )

  advisor_code <- stringr::str_match(
    advisor$text[[1L]],
    "^Emitido pelo assessor\\(a\\): ([^[:space:]]+)$"
  )[, 2L]

  if (is.na(advisor_code)) {
    cli::cli_abort("O código do assessor XP é inválido.")
  }

  date_lines <- lines |>
    dplyr::filter(
      stringr::str_detect(
        .data$text,
        "^Posição de Investimentos em [0-9]{2}/[0-9]{2}/[0-9]{4}$"
      )
    )

  if (nrow(date_lines) == 0L) {
    cli::cli_abort("O PDF XP não informa a data das posições.")
  }

  position_dates <- stringr::str_match(
    date_lines$text,
    "^Posição de Investimentos em ([0-9]{2}/[0-9]{2}/[0-9]{4})$"
  )[, 2L]

  if (anyNA(position_dates) || length(unique(position_dates)) != 1L) {
    cli::cli_abort("O PDF XP contém datas de posição inconsistentes.")
  }

  movement_start <- xp_parse_date(period[1L, 2L], "movement_start")
  movement_end <- xp_parse_date(period[1L, 3L], "movement_end")

  if (movement_start > movement_end) {
    cli::cli_abort("O período de movimentação está invertido.")
  }

  list(
    source_file = source_name,
    account_number = account[1L, 3L],
    portfolio_name = account[1L, 2L],
    advisor_code = advisor_code,
    position_date = xp_parse_date(position_dates[[1L]], "position_date"),
    movement_start = movement_start,
    movement_end = movement_end,
    issued_at = issued_at
  )
}


xp_line_tokens <- function(raw, line) {
  raw |>
    dplyr::filter(
      .data$source_page == line$source_page[[1L]],
      .data$source_token %in% line$source_tokens[[1L]]
    ) |>
    dplyr::arrange(.data$x)
}


xp_column_anchors <- function(header_tokens) {
  labels <- c(
    unit_price = "Cota",
    quantity = "Quantidade",
    invested_value = "Aplicado",
    gross_value = "Bruto",
    income_tax = "IR",
    iof = "IOF",
    net_value = "Liquido"
  )

  anchors <- vapply(
    labels,
    function(label) {
      matches <- which(header_tokens$text == label)

      if (length(matches) != 1L) {
        cli::cli_abort(
          "Cabeçalho XP inválido: coluna {.val {label}} ausente ou ambígua."
        )
      }

      i <- matches[[1L]]
      header_tokens$x[[i]] + header_tokens$width[[i]]
    },
    numeric(1)
  )

  if (is.unsorted(anchors, strictly = TRUE)) {
    cli::cli_abort("As colunas financeiras do extrato XP estão fora de ordem.")
  }

  anchors
}


xp_numeric_tokens <- function(tokens, anchors) {
  values <- tokens |>
    dplyr::filter(
      stringr::str_detect(
        .data$text,
        "^-?(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+),[0-9]+$"
      )
    )

  if (nrow(values) == 0L) {
    values$field <- character()
    return(values)
  }

  right <- values$x + values$width

  index <- vapply(
    right,
    function(value) which.min(abs(value - anchors)),
    integer(1)
  )

  values$field <- names(anchors)[index]
  values
}


xp_parse_number <- function(value, field) {
  valid <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl(
      "^-?(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+),[0-9]+$",
      value,
      perl = TRUE
    )

  if (!valid) {
    cli::cli_abort("Valor inválido em {.field {field}}: {.val {value}}.")
  }

  parsed <- as.numeric(
    gsub(",", ".", gsub(".", "", value, fixed = TRUE), fixed = TRUE)
  )

  if (!is.finite(parsed)) {
    cli::cli_abort("Valor não finito em {.field {field}}: {.val {value}}.")
  }

  parsed
}


xp_require_cells <- function(values, expected, section, source_page, source_row) {
  duplicate <- anyDuplicated(values$field) > 0L
  missing <- setdiff(expected, values$field)
  extra <- setdiff(values$field, expected)

  if (duplicate || length(missing) > 0L || length(extra) > 0L) {
    cli::cli_abort(paste0(
      "Colunas inválidas em {section}, página {source_page}, ",
      "linha {source_row}."
    ))
  }

  parsed <- vapply(
    expected,
    function(field) {
      text <- values$text[values$field == field]
      xp_parse_number(text, field)
    },
    numeric(1)
  )

  parsed
}



xp_parse_positions <- function(raw, lines) {
  header <- xp_unique_line(
    lines,
    "^Fundo Cota Quantidade Valor Aplicado Valor Bruto IR IOF Valor Liquido$",
    "cabeçalho de posição consolidada"
  )

  total <- xp_unique_line(
    lines,
    "^Total na Instituição ",
    "total consolidado"
  )

  if (header$source_page[[1L]] != total$source_page[[1L]] ||
      header$y[[1L]] >= total$y[[1L]]) {
    cli::cli_abort(
      "A seção de posição consolidada possui limites inválidos."
    )
  }

  anchors <- xp_column_anchors(xp_line_tokens(raw, header))
  position_fields <- names(anchors)
  amount_fields <- setdiff(
    position_fields,
    c("unit_price", "quantity")
  )

  records <- lines |>
    dplyr::filter(
      .data$source_page == header$source_page[[1L]],
      .data$y > header$y[[1L]],
      .data$y < total$y[[1L]]
    )

  positions <- purrr::map_dfr(
    seq_len(nrow(records)),
    function(i) {
      row <- records[i, ]
      tokens <- xp_line_tokens(raw, row)
      numeric_tokens <- xp_numeric_tokens(tokens, anchors)

      if (nrow(numeric_tokens) == 0L) {
        return(NULL)
      }

      values <- xp_require_cells(
        values = numeric_tokens,
        expected = position_fields,
        section = "posição consolidada",
        source_page = row$source_page[[1L]],
        source_row = row$source_row[[1L]]
      )

      numeric_source_tokens <- numeric_tokens$source_token

      unit_price_x <- numeric_tokens$x[
        numeric_tokens$field == "unit_price"
      ]

      quantity_x <- numeric_tokens$x[
        numeric_tokens$field == "quantity"
      ]

      # Nomes longos podem se sobrepor à coluna de cota.
      # Exclui fragmentos isolados da marca-d'água XP.
      name_tokens <- tokens |>
        dplyr::filter(
          !(.data$source_token %in% .env$numeric_source_tokens),
          .data$x < .env$quantity_x,
          (
            .data$x < .env$unit_price_x |
              stringr::str_detect(
                .data$text,
                "^[[:alpha:]]+$"
              )
          ),
          !stringr::str_detect(
            .data$text,
            "^[0-9]{1,2}:[0-9]{1,2}$"
          ),
          !stringr::str_detect(
            .data$text,
            "^-[0-9]{2}$"
          )
        )

      asset_name <- paste(
        name_tokens$text,
        collapse = " "
      )

      asset_name <- stringr::str_squish(asset_name)

      if (!nzchar(asset_name)) {
        source_page <- row$source_page[[1L]]
        source_row <- row$source_row[[1L]]

        cli::cli_abort(paste0(
          "Fundo investido sem identificação na página ",
          "{source_page}, linha {source_row}."
        ))
      }

      tibble::tibble(
        asset_name = asset_name,
        unit_price = values[["unit_price"]],
        quantity = values[["quantity"]],
        invested_value = values[["invested_value"]],
        gross_value = values[["gross_value"]],
        income_tax = values[["income_tax"]],
        iof = values[["iof"]],
        net_value = values[["net_value"]],
        source_file = raw$source_file[[1L]],
        source_page = row$source_page[[1L]],
        source_row = row$source_row[[1L]],
        source_tokens = list(c(
          name_tokens$source_token,
          numeric_tokens$source_token
        ))
      )
    }
  )

  if (
    nrow(positions) == 0L ||
    anyDuplicated(positions$asset_name)
  ) {
    cli::cli_abort(
      "A posição consolidada não possui fundos distintos válidos."
    )
  }

  total_tokens <- xp_line_tokens(raw, total)
  total_values <- xp_numeric_tokens(total_tokens, anchors)

  reported <- xp_require_cells(
    values = total_values,
    expected = amount_fields,
    section = "total na instituição",
    source_page = total$source_page[[1L]],
    source_row = total$source_row[[1L]]
  )

  names(reported) <- c(
    "total_invested",
    "total_gross",
    "total_income_tax",
    "total_iof",
    "total_net"
  )

  list(
    positions = positions,
    totals = as.list(reported)
  )
}



xp_validate_totals <- function(positions, metadata) {
  fields <- c(
    invested_value = "total_invested",
    gross_value = "total_gross",
    income_tax = "total_income_tax",
    iof = "total_iof",
    net_value = "total_net"
  )

  for (field in names(fields)) {
    calculated_cents <- sum(round(positions[[field]] * 100))
    reported_cents <- round(metadata[[fields[[field]]]] * 100)

    if (calculated_cents != reported_cents) {
      cli::cli_abort(paste0(
        "A soma de {.field {field}} não coincide com o total ",
        "informado pela XP."
      ))
    }
  }

  net_cents <- round(positions$net_value * 100)
  after_tax_cents <- round(positions$gross_value * 100) -
    round(positions$income_tax * 100) -
    round(positions$iof * 100)

  if (any(net_cents != after_tax_cents)) {
    cli::cli_abort("O valor líquido não corresponde ao bruto menos IR e IOF.")
  }

  invisible(NULL)
}

