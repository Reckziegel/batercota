
#' Read investment fund operations
#'
#' Imports applications and redemptions from the fund operations
#' workbook. The importer preserves the original operation types,
#' financial amounts, share quantities, and relevant dates.
#'
#' Missing `value` is allowed only for `Resgate Total`. A missing
#' amount is not replaced with zero or inferred from another field.
#'
#' Invalid dates are represented as `NA` and reported in `issues`.
#' Missing ISINs are accepted because historical records may not
#' contain valid security identifiers.
#'
#' @param path Path to the operations Excel workbook.
#' @param sheet Worksheet name or number.
#' @param source_name Name identifying the original workbook.
#'
#' @return An object of class `fund_operations` containing
#'   `metadata`, `operations`, `issues`, and `raw`.
#'
#' @export
read_fund_operations <- function(
    path,
    sheet = 1L,
    source_name = basename(path)
) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path)

  if (!valid_path || !file.exists(path) || dir.exists(path)) {
    cli::cli_abort(
      "`path` deve apontar para uma planilha Excel existente."
    )
  }

  valid_source <- is.character(source_name) &&
    length(source_name) == 1L &&
    !is.na(source_name) &&
    nzchar(source_name)

  if (!valid_source) {
    cli::cli_abort("`source_name` deve ser um texto não vazio.")
  }

  valid_sheet <- length(sheet) == 1L &&
    !is.na(sheet) &&
    (
      (is.character(sheet) && nzchar(sheet)) ||
        (is.numeric(sheet) && sheet >= 1 && sheet == floor(sheet))
    )

  if (!valid_sheet) {
    cli::cli_abort("`sheet` deve identificar uma aba válida.")
  }

  expected_columns <- c(
    "portfolio_fund_cnpj",
    "portfolio_fund_name",
    "asset_identifier",
    "asset_isin",
    "conversion_date",
    "liquidation_date",
    "operation_date",
    "operation_type",
    "value",
    "Quantidade",
    "Vl Líquido"
  )

  workbook <- tryCatch(
    readxl::read_excel(
      path = path,
      sheet = sheet,
      col_types = rep("list", length(expected_columns)),
      na = character(),
      trim_ws = FALSE,
      .name_repair = "minimal"
    ),
    error = function(error) {
      cli::cli_abort(
        "Não foi possível ler a planilha de operações: {conditionMessage(error)}."
      )
    }
  )

  if (!identical(names(workbook), expected_columns)) {
    cli::cli_abort(
      "A planilha possui colunas diferentes do layout esperado."
    )
  }

  if (nrow(workbook) == 0L) {
    cli::cli_abort("A planilha de operações está vazia.")
  }

  raw <- workbook |>
    dplyr::mutate(
      source_file = source_name,
      source_row = as.integer(dplyr::row_number() + 1L),
      .before = 1L
    )

  operations <- fund_operations_parse_records(raw)

  fund_operations_validate_semantics(operations)

  issues <- fund_operations_date_issues(
    raw = raw,
    operations = operations
  )

  valid_dates <- operations$operation_date[
    !is.na(operations$operation_date)
  ]

  if (length(valid_dates) == 0L) {
    cli::cli_abort(
      "A planilha não contém datas de operação válidas."
    )
  }

  metadata <- list(
    source_file = source_name,
    sheet = sheet,
    n_operations = nrow(operations),
    n_portfolios = dplyr::n_distinct(
      operations$portfolio_fund_cnpj
    ),
    operation_start = min(valid_dates),
    operation_end = max(valid_dates),
    n_total_redemptions = sum(
      operations$is_total_redemption
    ),
    n_total_redemptions_without_value = sum(
      operations$is_total_redemption &
        is.na(operations$value)
    ),
    n_missing_isin = sum(is.na(operations$asset_isin)),
    n_issues = nrow(issues)
  )

  structure(
    list(
      metadata = metadata,
      operations = operations,
      issues = issues,
      raw = raw
    ),
    class = "fund_operations"
  )
}


fund_operations_missing <- function(x) {
  if (length(x) == 0L) {
    return(TRUE)
  }

  if (length(x) != 1L) {
    return(FALSE)
  }

  if (is.na(x)) {
    return(TRUE)
  }

  is.character(x) && !nzchar(stringr::str_trim(x))
}


fund_operations_text <- function(x, field, source_row) {
  if (fund_operations_missing(x)) {
    return(NA_character_)
  }

  if (!is.character(x) || length(x) != 1L) {
    cli::cli_abort(
      "Texto inválido em {.field {field}}, linha {source_row}."
    )
  }

  value <- stringr::str_trim(x)

  if (identical(field, "asset_isin") &&
      value %in% c("#N/A", "#VALUE!", "#REF!", "#NUM!")) {
    return(NA_character_)
  }

  value
}


fund_operations_cnpj <- function(x, source_row) {
  if (fund_operations_missing(x)) {
    cli::cli_abort(
      "CNPJ ausente na linha {source_row}."
    )
  }

  if (is.numeric(x) &&
      length(x) == 1L &&
      is.finite(x) &&
      x >= 0 &&
      x < 1e14 &&
      x == floor(x)) {
    value <- sprintf("%014.0f", x)
  } else if (is.character(x) && length(x) == 1L) {
    value <- stringr::str_trim(x)
    value <- gsub("[./-]", "", value)
  } else {
    cli::cli_abort(
      "CNPJ inválido na linha {source_row}."
    )
  }

  if (!grepl("^[0-9]{14}$", value)) {
    cli::cli_abort(
      "CNPJ inválido na linha {source_row}: {.val {value}}."
    )
  }

  value
}


fund_operations_number <- function(x, field, source_row) {
  if (fund_operations_missing(x)) {
    return(NA_real_)
  }

  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x)) {
    cli::cli_abort(
      "Número inválido em {.field {field}}, linha {source_row}."
    )
  }

  as.numeric(x)
}


fund_operations_date <- function(x) {
  if (fund_operations_missing(x)) {
    return(as.Date(NA))
  }

  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }

  if (!is.character(x) || length(x) != 1L) {
    return(as.Date(NA))
  }

  value <- stringr::str_trim(x)

  valid_format <- grepl(
    "^[0-9]{2}/[0-9]{2}/[0-9]{4}$",
    value
  )

  if (!valid_format) {
    return(as.Date(NA))
  }

  parsed <- suppressWarnings(
    as.Date(value, format = "%d/%m/%Y")
  )

  if (is.na(parsed) ||
      format(parsed, "%d/%m/%Y") != value) {
    return(as.Date(NA))
  }

  parsed
}


fund_operations_dates <- function(cells) {
  dates <- purrr::map_chr(
    cells,
    function(cell) {
      value <- fund_operations_date(cell)

      if (is.na(value)) {
        return(NA_character_)
      }

      format(value, "%Y-%m-%d")
    }
  )

  as.Date(dates)
}


fund_operations_parse_records <- function(raw) {
  rows <- raw$source_row

  operations <- tibble::tibble(
    source_file = raw$source_file,
    source_row = rows,
    portfolio_fund_cnpj = purrr::map2_chr(
      raw$portfolio_fund_cnpj,
      rows,
      fund_operations_cnpj
    ),
    portfolio_fund_name = purrr::map2_chr(
      raw$portfolio_fund_name,
      rows,
      ~ fund_operations_text(
        .x, "portfolio_fund_name", .y
      )
    ),
    asset_identifier = purrr::map2_chr(
      raw$asset_identifier,
      rows,
      ~ fund_operations_text(
        .x, "asset_identifier", .y
      )
    ),
    asset_isin = purrr::map2_chr(
      raw$asset_isin,
      rows,
      ~ fund_operations_text(
        .x, "asset_isin", .y
      )
    ),
    conversion_date = fund_operations_dates(
      raw$conversion_date
    ),
    liquidation_date = fund_operations_dates(
      raw$liquidation_date
    ),
    operation_date = fund_operations_dates(
      raw$operation_date
    ),
    operation_type = purrr::map2_chr(
      raw$operation_type,
      rows,
      ~ fund_operations_text(
        .x, "operation_type", .y
      )
    ),
    value = purrr::map2_dbl(
      raw$value,
      rows,
      ~ fund_operations_number(.x, "value", .y)
    ),
    quantity = purrr::map2_dbl(
      raw$Quantidade,
      rows,
      ~ fund_operations_number(.x, "quantity", .y)
    ),
    net_value = purrr::map2_dbl(
      raw[["Vl Líquido"]],
      rows,
      ~ fund_operations_number(.x, "net_value", .y)
    )
  )

  operations |>
    dplyr::mutate(
      is_total_redemption = .data$operation_type == "Resgate Total"
    )
}


fund_operations_validate_semantics <- function(operations) {
  required_text <- c(
    "portfolio_fund_cnpj",
    "portfolio_fund_name",
    "asset_identifier",
    "operation_type"
  )

  for (field in required_text) {
    invalid <- is.na(operations[[field]]) |
      !nzchar(operations[[field]])

    if (any(invalid)) {
      source_row <- operations$source_row[
        which(invalid)[[1L]]
      ]

      cli::cli_abort(
        "Campo obrigatório {.field {field}} ausente na linha {source_row}."
      )
    }
  }

  valid_types <- c(
    "Aplicação",
    "Resgate Cotas",
    "Resgate Bruto",
    "Resgate Total"
  )

  unexpected <- which(
    !operations$operation_type %in% valid_types
  )

  if (length(unexpected) > 0L) {
    i <- unexpected[[1L]]
    source_row <- operations$source_row[[i]]
    operation_type <- operations$operation_type[[i]]

    cli::cli_abort(
      "Operação desconhecida na linha {source_row}: {.val {operation_type}}."
    )
  }

  missing_value <- which(
    is.na(operations$value) &
      !operations$is_total_redemption
  )

  if (length(missing_value) > 0L) {
    source_row <- operations$source_row[
      missing_value[[1L]]
    ]

    cli::cli_abort(
      "Operação sem valor na linha {source_row}, mas não é Resgate Total."
    )
  }

  invisible(NULL)
}


fund_operations_cell_label <- function(x) {
  if (fund_operations_missing(x)) {
    return(NA_character_)
  }

  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }

  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }

  as.character(x)
}


fund_operations_date_issues <- function(raw, operations) {
  date_fields <- c(
    "conversion_date",
    "liquidation_date",
    "operation_date"
  )

  purrr::map_dfr(
    date_fields,
    function(field) {
      indexes <- which(is.na(operations[[field]]))
      original <- raw[[field]][indexes]

      tibble::tibble(
        source_file = raw$source_file[indexes],
        source_row = raw$source_row[indexes],
        field = rep(field, length(indexes)),
        issue_type = purrr::map_chr(
          original,
          ~ if (fund_operations_missing(.x)) {
            "missing_date"
          } else {
            "invalid_date"
          }
        ),
        raw_value = purrr::map_chr(
          original,
          fund_operations_cell_label
        )
      )
    }
  ) |>
    dplyr::arrange(
      .data$source_row,
      .data$field
    )
}
