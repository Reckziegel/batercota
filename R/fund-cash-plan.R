
#' Read a fund cash-flow planning workbook
#'
#' Imports shareholder operations, investment fund operations,
#' and daily projected cash flows from an internal workbook.
#'
#' Excel formulas are not recalculated. Their saved values are read
#' as reported, and formula provenance is retained for cash balances.
#'
#' @param path Path to the Excel workbook.
#' @param source_name Name identifying the source workbook.
#'
#' @return A `fund_cash_plan` object containing `metadata`,
#'   `shareholder_operations`, `investment_operations`,
#'   `cash_flows`, `issues`, and `raw`.
#'
#' @export
read_fund_cash_plan <- function(
    path,
    source_name = basename(path)
) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    file.exists(path) &&
    !dir.exists(path)

  if (!valid_path || tolower(tools::file_ext(path)) != "xlsx") {
    cli::cli_abort("`path` deve apontar para um arquivo XLSX existente.")
  }

  if (
    !is.character(source_name) ||
    length(source_name) != 1L ||
    is.na(source_name) ||
    !nzchar(trimws(source_name))
  ) {
    cli::cli_abort("`source_name` deve ser um texto não vazio.")
  }

  specs <- cash_plan_sheet_specs()

  available_sheets <- readxl::excel_sheets(path)
  required_sheets <- vapply(specs, `[[`, character(1), "sheet")
  missing_sheets <- setdiff(required_sheets, available_sheets)

  if (length(missing_sheets) > 0L) {
    cli::cli_abort(
      "Abas obrigatórias ausentes: {.or {missing_sheets}}."
    )
  }

  raw <- lapply(
    specs,
    function(spec) cash_plan_read_sheet(path, spec)
  )

  parsed <- lapply(
    names(specs),
    function(name) {
      cash_plan_parse_table(
        raw = raw[[name]],
        schema = specs[[name]]$schema,
        source_file = source_name,
        source_sheet = specs[[name]]$sheet,
        kind = name
      )
    }
  )

  names(parsed) <- names(specs)

  shareholder_operations <- parsed$shareholder_operations$data
  investment_operations <- parsed$investment_operations$data
  cash_flows <- parsed$cash_flows$data

  formulas <- cash_plan_balance_formulas(path)

  formula_index <- match(
    cash_flows$source_row,
    formulas$source_row
  )

  cash_flows$cash_balance_formula <- formulas$expression[formula_index]

  cash_flows$cash_balance_is_formula <-
    !is.na(formula_index)

  cash_flows$cash_balance_is_manual <-
    !is.na(cash_flows$cumulative_cash_balance) &
    !cash_flows$cash_balance_is_formula

  issues <- do.call(
    rbind,
    lapply(parsed, `[[`, "issues")
  )

  missing_cache <- which(
    cash_flows$cash_balance_is_formula &
      is.na(cash_flows$cumulative_cash_balance)
  )

  if (length(missing_cache) > 0L) {
    issues <- rbind(
      issues,
      tibble::tibble(
        source_file = source_name,
        source_sheet = "Movimentações",
        source_row = cash_flows$source_row[missing_cache],
        field = "cumulative_cash_balance",
        issue_type = "missing_formula_cache",
        raw_value = NA_character_
      )
    )
  }

  issues <- issues[
    order(issues$source_sheet, issues$source_row, issues$field),
    ,
    drop = FALSE
  ]

  fund_names <- c(
    shareholder_operations$fund_name,
    investment_operations$portfolio_fund_name
  )

  fund_names <- fund_names[
    !is.na(fund_names) & nzchar(fund_names)
  ]

  metadata <- list(
    source_file = source_name,
    fund_name = if (length(fund_names) > 0L) {
      fund_names[[1L]]
    } else {
      NA_character_
    },
    n_shareholder_operations = nrow(shareholder_operations),
    n_investment_operations = nrow(investment_operations),
    n_cash_flow_dates = nrow(cash_flows),
    n_manual_cash_balances = sum(
      cash_flows$cash_balance_is_manual
    ),
    n_formula_cash_balances = sum(
      cash_flows$cash_balance_is_formula
    ),
    n_issues = nrow(issues)
  )

  structure(
    list(
      metadata = metadata,
      shareholder_operations = shareholder_operations,
      investment_operations = investment_operations,
      cash_flows = cash_flows,
      issues = issues,
      raw = raw
    ),
    class = "fund_cash_plan"
  )
}


cash_plan_sheet_specs <- function() {
  list(
    shareholder_operations = list(
      sheet = "Operações Cotistas",
      headers = c(
        "Id Operação", "Id.Cotista", "Cotista",
        "Id.Carteira", "Carteira", "Dt Operação",
        "Dt Registro", "Tipo Operação", "Dt Conversão",
        "Dt Liquidação", "Vl Bruto", "ValorIR",
        "ValorIOF", "Quantidade", "Vl Líquido",
        "Valores Colados"
      ),
      schema = c(
        operation_id = "id",
        investor_id = "id",
        investor_name = "text",
        fund_code = "id",
        fund_name = "text",
        operation_date = "date",
        registration_date = "date",
        operation_type = "text",
        conversion_date = "date",
        liquidation_date = "date",
        gross_value = "number",
        tax_value = "number",
        iof_value = "number",
        quantity = "number",
        net_value = "number",
        values_pasted = "text"
      )
    ),
    investment_operations = list(
      sheet = "Operações Fundos",
      headers = c(
        "Id Operação", "Cliente", "Carteira",
        "Dt Conversão", "Dt Liquidação", "Data",
        "Tipo", "Vl Bruto", "Quantidade",
        "Vl Líquido", "Valores Colados"
      ),
      schema = c(
        operation_id = "id",
        portfolio_fund_name = "text",
        asset_identifier = "text",
        conversion_date = "date",
        liquidation_date = "date",
        operation_date = "date",
        operation_type = "text",
        gross_value = "number",
        quantity = "number",
        net_value = "number",
        values_pasted = "text"
      )
    ),
    cash_flows = list(
      sheet = "Movimentações",
      headers = c(
        "Data",
        "Cotização Resgate Cotista",
        "Resgate Cotista Estimado",
        "Entradas de Resgates de Ativos Aplicados",
        "Saldo Dia",
        "Ajuste Futuros",
        "Novos Cotistas",
        "Outros",
        "Saldo Acumulado",
        "Aplicações Feitas (Ir Preenchendo)"
      ),
      schema = c(
        date = "date",
        shareholder_conversion_date = "date",
        estimated_shareholder_redemptions = "number",
        investment_redemption_inflows = "number",
        daily_net_balance = "number",
        futures_adjustment = "number",
        new_shareholders = "number",
        other_movements = "number",
        cumulative_cash_balance = "number",
        investment_applications = "number",
        estimated_nav = "number"
      )
    )
  )
}


cash_plan_read_sheet <- function(path, spec) {
  n_columns <- length(spec$schema)

  data <- readxl::read_excel(
    path = path,
    sheet = spec$sheet,
    range = readxl::cell_cols(c(1L, n_columns)),
    col_types = rep("list", n_columns),
    na = character(),
    trim_ws = FALSE,
    .name_repair = "minimal"
  )

  expected <- spec$headers
  actual <- head(names(data), length(expected))

  if (!identical(actual, expected)) {
    cli::cli_abort(
      "Layout inesperado na aba {.val {spec$sheet}}."
    )
  }

  names(data) <- names(spec$schema)

  tibble::add_column(
    data,
    source_row = as.integer(seq_len(nrow(data)) + 1L),
    .before = 1L
  )
}


cash_plan_has_value <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }

  if (is.character(x)) {
    return(nzchar(trimws(x, whitespace = "[\\h\\v]")))
  }

  TRUE
}


cash_plan_cell_text <- function(x) {
  if (!cash_plan_has_value(x)) {
    return(NA_character_)
  }

  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%Y-%m-%d"))
  }

  if (is.numeric(x)) {
    return(format(x, scientific = FALSE, trim = TRUE))
  }

  as.character(x)
}


cash_plan_parse_cell <- function(x, type) {
  if (!cash_plan_has_value(x)) {
    return(
      switch(
        type,
        date = as.Date(NA),
        number = NA_real_,
        id = NA_character_,
        text = NA_character_
      )
    )
  }

  if (type %in% c("id", "text")) {
    if (is.numeric(x) && !inherits(x, "POSIXt")) {
      if (!is.finite(x)) {
        return(NA_character_)
      }

      if (type == "id" && x != floor(x)) {
        return(NA_character_)
      }

      return(format(x, scientific = FALSE, trim = TRUE))
    }

    if (!is.character(x)) {
      return(NA_character_)
    }

    return(trimws(x, whitespace = "[\\h\\v]"))
  }

  if (type == "date") {
    if (inherits(x, "Date")) {
      return(as.Date(x))
    }

    if (inherits(x, "POSIXt")) {
      return(as.Date(x, tz = "UTC"))
    }

    if (is.numeric(x)) {
      valid_serial <- is.finite(x) &&
        x == floor(x) &&
        x >= 20000 &&
        x <= 80000

      if (valid_serial) {
        return(as.Date(x, origin = "1899-12-30"))
      }

      return(as.Date(NA))
    }

    if (!is.character(x)) {
      return(as.Date(NA))
    }

    value <- trimws(x)

    for (date_format in c("%Y-%m-%d", "%d/%m/%Y")) {
      parsed <- suppressWarnings(
        as.Date(value, format = date_format)
      )

      if (
        !is.na(parsed) &&
        identical(format(parsed, date_format), value)
      ) {
        return(parsed)
      }
    }

    return(as.Date(NA))
  }

  if (type == "number") {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      return(NA_real_)
    }

    if (is.numeric(x)) {
      return(if (is.finite(x)) as.numeric(x) else NA_real_)
    }

    if (!is.character(x)) {
      return(NA_real_)
    }

    value <- gsub("[[:space:]]", "", x, perl = TRUE)
    value <- gsub("\u00a0|\u202f", "", value)
    value <- sub("^R\\$", "", value)

    if (grepl(",", value, fixed = TRUE)) {
      value <- gsub(".", "", value, fixed = TRUE)
      value <- sub(",", ".", value, fixed = TRUE)
    }

    if (!grepl("^[+-]?[0-9]+(\\.[0-9]+)?$", value)) {
      return(NA_real_)
    }

    parsed <- suppressWarnings(as.numeric(value))

    return(if (is.finite(parsed)) parsed else NA_real_)
  }

  cli::cli_abort("Tipo de coluna desconhecido: {.val {type}}.")
}


cash_plan_keep_rows <- function(raw, kind) {
  fields <- switch(
    kind,
    shareholder_operations = c(
      "investor_name",
      "operation_date",
      "operation_type",
      "gross_value"
    ),
    investment_operations = c(
      "portfolio_fund_name",
      "asset_identifier",
      "operation_date",
      "gross_value"
    ),
    cash_flows = c(
      "date",
      "estimated_shareholder_redemptions",
      "investment_redemption_inflows",
      "cumulative_cash_balance"
    )
  )

  vapply(
    seq_len(nrow(raw)),
    function(i) {
      any(vapply(
        fields,
        function(field) {
          cash_plan_has_value(raw[[field]][[i]])
        },
        logical(1)
      ))
    },
    logical(1)
  )
}


cash_plan_parse_table <- function(
    raw,
    schema,
    source_file,
    source_sheet,
    kind
) {
  keep <- cash_plan_keep_rows(raw, kind)
  selected <- raw[keep, , drop = FALSE]

  data <- tibble::tibble(
    source_file = rep(source_file, nrow(selected)),
    source_sheet = rep(source_sheet, nrow(selected)),
    source_row = selected$source_row
  )

  issues <- tibble::tibble(
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    field = character(),
    issue_type = character(),
    raw_value = character()
  )

  for (field in names(schema)) {
    type <- schema[[field]]
    cells <- selected[[field]]

    parsed <- lapply(
      cells,
      cash_plan_parse_cell,
      type = type
    )

    values <- switch(
      type,
      date = as.Date(
        vapply(parsed, as.numeric, numeric(1)),
        origin = "1970-01-01"
      ),
      number = vapply(parsed, as.numeric, numeric(1)),
      id = vapply(parsed, as.character, character(1)),
      text = vapply(parsed, as.character, character(1))
    )

    data[[field]] <- values

    invalid <- which(
      vapply(cells, cash_plan_has_value, logical(1)) &
        is.na(values)
    )

    if (length(invalid) > 0L) {
      issues <- rbind(
        issues,
        tibble::tibble(
          source_file = source_file,
          source_sheet = source_sheet,
          source_row = selected$source_row[invalid],
          field = field,
          issue_type = paste0("invalid_", type),
          raw_value = vapply(
            cells[invalid],
            cash_plan_cell_text,
            character(1)
          )
        )
      )
    }
  }

  if (kind == "cash_flows") {
    missing_date <- which(is.na(data$date))

    if (length(missing_date) > 0L) {
      issues <- rbind(
        issues,
        tibble::tibble(
          source_file = source_file,
          source_sheet = source_sheet,
          source_row = data$source_row[missing_date],
          field = "date",
          issue_type = "missing_cash_flow_date",
          raw_value = NA_character_
        )
      )
    }
  }

  list(data = data, issues = issues)
}


cash_plan_read_zip_xml <- function(path, member) {
  connection <- unz(path, member, open = "rb")
  on.exit(close(connection), add = TRUE)

  xml2::read_xml(connection)
}


cash_plan_balance_formulas <- function(path) {
  workbook <- cash_plan_read_zip_xml(
    path,
    "xl/workbook.xml"
  )

  relationships <- cash_plan_read_zip_xml(
    path,
    "xl/_rels/workbook.xml.rels"
  )

  sheets <- xml2::xml_find_all(
    workbook,
    ".//*[local-name()='sheets']/*[local-name()='sheet']"
  )

  sheet_names <- xml2::xml_attr(sheets, "name")
  sheet_index <- match("Movimentações", sheet_names)

  if (is.na(sheet_index)) {
    cli::cli_abort("A aba Movimentações não foi encontrada.")
  }

  attributes <- xml2::xml_attrs(sheets[[sheet_index]])

  relationship_id <- unname(
    attributes[grepl("(^|:)id$", names(attributes))]
  )

  if (length(relationship_id) != 1L) {
    cli::cli_abort(
      "Não foi possível identificar a aba Movimentações no XLSX."
    )
  }

  links <- xml2::xml_find_all(
    relationships,
    ".//*[local-name()='Relationship']"
  )

  link_index <- match(
    relationship_id,
    xml2::xml_attr(links, "Id")
  )

  if (is.na(link_index)) {
    cli::cli_abort(
      "Não foi possível localizar o XML da aba Movimentações."
    )
  }

  target <- xml2::xml_attr(links[[link_index]], "Target")

  member <- if (startsWith(target, "/")) {
    substring(target, 2L)
  } else {
    paste0("xl/", target)
  }

  member <- gsub("\\", "/", member, fixed = TRUE)

  if (!member %in% utils::unzip(path, list = TRUE)$Name) {
    cli::cli_abort(
      "O XML da aba Movimentações não está presente no XLSX."
    )
  }

  worksheet <- cash_plan_read_zip_xml(path, member)

  cells <- xml2::xml_find_all(
    worksheet,
    paste0(
      ".//*[local-name()='sheetData']",
      "/*[local-name()='row']",
      "/*[local-name()='c'][*[local-name()='f']]"
    )
  )

  references <- xml2::xml_attr(cells, "r")
  selected <- grepl("^I[0-9]+$", references)

  cells <- cells[selected]
  references <- references[selected]

  formula_nodes <- xml2::xml_find_all(
    cells,
    "./*[local-name()='f']"
  )

  tibble::tibble(
    source_row = as.integer(sub("^I", "", references)),
    expression = xml2::xml_text(formula_nodes)
  )
}
