`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

make_unique_names <- function(nms) {
  nms <- as.character(nms)
  blank <- is.na(nms) | trimws(nms) == ""
  nms[blank] <- paste0("blank_", seq_len(sum(blank)))
  make.unique(nms, sep = "_")
}

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- toupper(trimws(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

extract_digits <- function(x) {
  gsub("\\D+", "", as.character(x %||% ""))
}

extract_fund_id <- function(x) {
  digits <- extract_digits(x)
  out <- ifelse(nchar(digits) >= 8, substr(digits, 1, 8), digits)
  out[is.na(out)] <- ""
  out
}

parse_amount <- function(x) {
  if (is.numeric(x)) {
    return(unname(as.numeric(x)))
  }

  unname(vapply(x, function(value) {
    if (is.na(value)) {
      return(NA_real_)
    }

    text <- trimws(as.character(value))
    if (text == "") {
      return(NA_real_)
    }

    is_negative <- grepl("^\\(.*\\)$", text) || grepl("^-", text)
    text <- gsub("[^0-9,.-]", "", text)

    if (grepl(",", text) && grepl("\\.", text)) {
      text <- gsub("\\.", "", text)
      text <- sub(",", ".", text, fixed = TRUE)
    } else if (grepl(",", text)) {
      text <- sub(",", ".", text, fixed = TRUE)
    }

    text <- gsub("[^0-9.-]", "", text)
    parsed <- suppressWarnings(as.numeric(text))
    if (is.na(parsed)) {
      return(NA_real_)
    }

    if (is_negative) -abs(parsed) else parsed
  }, numeric(1)))
}

format_money <- function(x) {
  ifelse(
    is.na(x),
    "",
    paste0(
      "R$ ",
      format(
        round(x, 2),
        big.mark = ".",
        decimal.mark = ",",
        nsmall = 2,
        trim = TRUE,
        scientific = FALSE
      )
    )
  )
}

format_units <- function(x) {
  ifelse(
    is.na(x),
    "",
    format(round(x, 8), big.mark = ".", decimal.mark = ",", nsmall = 8, trim = TRUE)
  )
}

format_date_text <- function(x) {
  x <- as.Date(x)
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

empty_positions <- function() {
  data.frame(
    investor_key = character(),
    investor_name = character(),
    fund_id = character(),
    fund_name = character(),
    units = numeric(),
    latest_cota = numeric(),
    latest_cota_date = as.Date(character()),
    position_value = numeric(),
    has_position = logical(),
    stringsAsFactors = FALSE
  )
}

empty_estimation_details <- function() {
  data.frame(
    source_file = character(),
    source_row = integer(),
    investor_name = character(),
    fund_id = character(),
    fund_name = character(),
    movement_code = character(),
    movement_type = character(),
    input_value = numeric(),
    available_units = numeric(),
    available_value = numeric(),
    latest_cota = numeric(),
    latest_cota_date = as.Date(character()),
    status = character(),
    note = character(),
    operation_date = as.Date(character()),
    registration_date = as.Date(character()),
    conversion_date = as.Date(character()),
    liquidation_date = as.Date(character()),
    estimated_gross_value = numeric(),
    exportable = logical(),
    stringsAsFactors = FALSE
  )
}

empty_transaction_export <- function() {
  data.frame(
    Cotista = character(),
    Id.Carteira = numeric(),
    Carteira = character(),
    `Dt OperaÃ§Ã£o` = as.Date(character()),
    `Dt Registro` = as.Date(character()),
    `Tipo OperaÃ§Ã£o` = character(),
    `Dt ConversÃ£o` = as.Date(character()),
    `Dt LiquidaÃ§Ã£o` = as.Date(character()),
    `Vl Bruto` = numeric(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

safe_read_txt <- function(path, original_name = basename(path)) {
  encodings <- c("UTF-8-BOM", "UTF-8", "Latin1")
  last_error <- NULL

  for (encoding in encodings) {
    result <- tryCatch(
      read.csv(
        path,
        sep = ";",
        quote = "\"",
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = encoding,
        na.strings = c("", "NA")
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(result)) {
      names(result) <- make_unique_names(names(result))
      result$`__source_file` <- original_name
      result$`__source_row` <- seq_len(nrow(result)) + 1
      return(result)
    }
  }

  stop("Could not read TXT file: ", conditionMessage(last_error), call. = FALSE)
}

read_txt_files <- function(files) {
  if (is.null(files) || nrow(files) == 0) {
    return(list(data = NULL, errors = data.frame()))
  }

  frames <- list()
  errors <- list()

  for (i in seq_len(nrow(files))) {
    original_name <- files$name[[i]]
    frame <- tryCatch(
      safe_read_txt(files$datapath[[i]], original_name),
      error = function(e) {
        errors[[length(errors) + 1]] <<- data.frame(
          file = original_name,
          message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
        NULL
      }
    )

    if (!is.null(frame)) {
      frames[[length(frames) + 1]] <- frame
    }
  }

  data <- if (length(frames) == 0) NULL else do.call(rbind, frames)
  error_data <- if (length(errors) == 0) data.frame() else do.call(rbind, errors)
  list(data = data, errors = error_data)
}

parse_input_orders <- function(txt_data) {
  if (is.null(txt_data) || nrow(txt_data) == 0) {
    return(data.frame())
  }

  required <- c("NOME", "PRODUTO", "TIPO_MOV")
  missing <- required[!required %in% names(txt_data)]
  if (length(missing) > 0) {
    stop("TXT files are missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  movement_code <- normalize_text(txt_data[["TIPO_MOV"]])
  movement_type <- ifelse(
    movement_code == "RT",
    "Resgate Total",
    ifelse(
      movement_code == "RP",
      "Resgate Bruto",
      ifelse(movement_code == "A", "Aplicação", NA_character_)
    )
  )

  out <- data.frame(
    source_file = as.character(txt_data$`__source_file` %||% ""),
    source_row = as.integer(txt_data$`__source_row` %||% NA_integer_),
    investor_name = as.character(txt_data[["NOME"]]),
    investor_key = normalize_text(txt_data[["NOME"]]),
    fund_cnpj = as.character(txt_data[["PRODUTO"]]),
    fund_id = extract_fund_id(txt_data[["PRODUTO"]]),
    movement_code = movement_code,
    movement_type = movement_type,
    input_value = parse_amount(if ("VALOR" %in% names(txt_data)) txt_data[["VALOR"]] else rep(NA, nrow(txt_data))),
    stringsAsFactors = FALSE
  )

  out$is_redemption <- out$movement_type %in% c("Resgate Total", "Resgate Bruto")
  out$usage_status <- ifelse(
    out$is_redemption,
    "Used in estimation",
    ifelse(out$movement_type == "Aplicação", "Ignored in estimation", "Unsupported movement")
  )
  out$is_subscription <- normalize_text(out$movement_type) == "APLICACAO"
  out$usage_status <- ifelse(out$is_redemption | out$is_subscription, "Used in estimation", "Unsupported movement")
  out
}

read_reference_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  required_sheets <- c("transaction_records", "cotas", "feriados", "redemption_period")
  missing_sheets <- required_sheets[!required_sheets %in% sheets]

  if (length(missing_sheets) > 0) {
    stop(
      "Workbook is missing required sheets: ",
      paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    transaction_records = as.data.frame(
      readxl::read_xlsx(path, sheet = "transaction_records", .name_repair = "minimal"),
      stringsAsFactors = FALSE
    ),
    cotas = as.data.frame(
      readxl::read_xlsx(path, sheet = "cotas", .name_repair = "minimal"),
      stringsAsFactors = FALSE
    ),
    feriados = as.data.frame(
      suppressWarnings(readxl::read_xlsx(path, sheet = "feriados", .name_repair = "minimal")),
      stringsAsFactors = FALSE
    ),
    redemption_period = as.data.frame(
      readxl::read_xlsx(path, sheet = "redemption_period", .name_repair = "minimal"),
      stringsAsFactors = FALSE
    ),
    subscription = if ("subscription" %in% sheets) {
      as.data.frame(
        readxl::read_xlsx(path, sheet = "subscription", .name_repair = "minimal"),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame()
    }
  )
}

prepare_holidays <- function(feriados_raw) {
  if (is.null(feriados_raw) || nrow(feriados_raw) == 0) {
    return(as.Date(character()))
  }

  names(feriados_raw) <- make_unique_names(names(feriados_raw))
  holiday_dates <- as.Date(feriados_raw[[1]])
  holiday_dates <- holiday_dates[!is.na(holiday_dates)]
  unique(sort(holiday_dates))
}

prepare_redemption_periods <- function(period_raw) {
  if (is.null(period_raw) || nrow(period_raw) == 0) {
    return(data.frame(
      fund_id = character(),
      conversion_days = integer(),
      payment_days = integer(),
      stringsAsFactors = FALSE
    ))
  }

  names(period_raw) <- make_unique_names(names(period_raw))
  data.frame(
    fund_id = extract_fund_id(period_raw[[1]]),
    conversion_days = suppressWarnings(as.integer(period_raw[["conversion"]])),
    payment_days = suppressWarnings(as.integer(period_raw[["payment"]])),
    stringsAsFactors = FALSE
  )
}

prepare_subscription_periods <- function(period_raw) {
  if (is.null(period_raw) || nrow(period_raw) == 0) {
    return(data.frame(
      fund_id = character(),
      conversion_days = integer(),
      liquidation_days = integer(),
      stringsAsFactors = FALSE
    ))
  }

  names(period_raw) <- make_unique_names(names(period_raw))
  data.frame(
    fund_id = extract_fund_id(period_raw[[1]]),
    conversion_days = suppressWarnings(as.integer(period_raw[["conversion"]])),
    liquidation_days = suppressWarnings(as.integer(period_raw[["liquidation"]])),
    stringsAsFactors = FALSE
  )
}

build_cota_lookup <- function(cotas_raw) {
  if (is.null(cotas_raw) || nrow(cotas_raw) == 0 || ncol(cotas_raw) < 2) {
    return(list())
  }

  names(cotas_raw) <- make_unique_names(names(cotas_raw))
  date_values <- as.Date(cotas_raw[[1]])
  lookup <- list()

  for (column_name in names(cotas_raw)[-1]) {
    fund_id <- extract_fund_id(column_name)
    unit_values <- suppressWarnings(as.numeric(cotas_raw[[column_name]]))
    keep <- !is.na(date_values) & !is.na(unit_values)
    if (fund_id != "" && any(keep)) {
      lookup[[fund_id]] <- data.frame(
        date = date_values[keep],
        unit_value = unit_values[keep],
        stringsAsFactors = FALSE
      )
    }
  }

  lookup
}

lookup_latest_cota <- function(cota_lookup, fund_id, reference_date) {
  empty <- list(value = NA_real_, date = as.Date(NA))

  fund_key <- extract_fund_id(fund_id)
  reference_date <- as.Date(reference_date)
  if (fund_key == "" || is.na(reference_date) || is.null(cota_lookup[[fund_key]])) {
    return(empty)
  }

  data <- cota_lookup[[fund_key]]
  pos <- findInterval(as.numeric(reference_date), as.numeric(data$date))
  if (pos < 1) {
    return(empty)
  }

  list(value = data$unit_value[[pos]], date = data$date[[pos]])
}

is_business_day <- function(date, holidays) {
  date <- as.Date(date)
  if (is.na(date)) {
    return(FALSE)
  }

  weekday <- as.POSIXlt(date)$wday
  !(weekday %in% c(0, 6)) && !(date %in% holidays)
}

next_business_day <- function(date, holidays) {
  date <- as.Date(date)
  while (!is_business_day(date, holidays)) {
    date <- date + 1
  }
  date
}

add_business_days <- function(date, days, holidays) {
  date <- as.Date(date)
  days <- as.integer(days %||% 0L)
  if (is.na(date) || is.na(days)) {
    return(as.Date(NA))
  }

  out <- date
  if (days <= 0) {
    return(next_business_day(out, holidays))
  }

  remaining <- days
  while (remaining > 0) {
    out <- out + 1
    if (is_business_day(out, holidays)) {
      remaining <- remaining - 1
    }
  }
  out
}

calculate_redemption_dates <- function(request_date, fund_id, periods, holidays) {
  request_date <- as.Date(request_date)
  fund_key <- extract_fund_id(fund_id)

  match_row <- periods[periods$fund_id == fund_key, , drop = FALSE]
  if (nrow(match_row) == 0) {
    return(list(
      operation_date = next_business_day(request_date, holidays),
      registration_date = next_business_day(request_date, holidays),
      conversion_date = as.Date(NA),
      liquidation_date = as.Date(NA),
      conversion_days = NA_integer_,
      payment_days = NA_integer_
    ))
  }

  operation_date <- next_business_day(request_date, holidays)
  conversion_days <- as.integer(match_row$conversion_days[[1]])
  payment_days <- as.integer(match_row$payment_days[[1]] %||% 2L)
  conversion_date <- next_business_day(operation_date + conversion_days, holidays)
  liquidation_date <- add_business_days(conversion_date, payment_days, holidays)

  list(
    operation_date = operation_date,
    registration_date = operation_date,
    conversion_date = conversion_date,
    liquidation_date = liquidation_date,
    conversion_days = conversion_days,
    payment_days = payment_days
  )
}

calculate_subscription_dates <- function(request_date, fund_id, periods, holidays) {
  request_date <- as.Date(request_date)
  fund_key <- extract_fund_id(fund_id)

  match_row <- periods[periods$fund_id == fund_key, , drop = FALSE]
  if (nrow(match_row) == 0) {
    return(list(
      operation_date = next_business_day(request_date, holidays),
      registration_date = next_business_day(request_date, holidays),
      conversion_date = as.Date(NA),
      liquidation_date = as.Date(NA),
      conversion_days = NA_integer_,
      liquidation_days = NA_integer_
    ))
  }

  operation_date <- next_business_day(request_date, holidays)
  conversion_days <- as.integer(match_row$conversion_days[[1]])
  liquidation_days <- as.integer(match_row$liquidation_days[[1]] %||% 0L)
  conversion_date <- next_business_day(operation_date + conversion_days, holidays)
  liquidation_date <- add_business_days(operation_date, liquidation_days, holidays)

  list(
    operation_date = operation_date,
    registration_date = operation_date,
    conversion_date = conversion_date,
    liquidation_date = liquidation_date,
    conversion_days = conversion_days,
    liquidation_days = liquidation_days
  )
}

prepare_reference_model <- function(path) {
  raw <- read_reference_workbook(path)

  tx <- raw$transaction_records
  names(tx) <- make_unique_names(names(tx))

  required_columns <- c(
    "Cotista",
    "Id.Carteira",
    "Carteira",
    "Dt Operação",
    "Dt Registro",
    "Tipo Operação",
    "Dt Conversão",
    "Dt Liquidação",
    "Vl Bruto"
  )
  missing_columns <- required_columns[!required_columns %in% names(tx)]
  if (length(missing_columns) > 0) {
    stop(
      "transaction_records is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  holidays <- prepare_holidays(raw$feriados)
  periods <- prepare_redemption_periods(raw$redemption_period)
  subscription_periods <- prepare_subscription_periods(raw$subscription)
  cota_lookup <- build_cota_lookup(raw$cotas)

  tx$investor_name <- as.character(tx[["Cotista"]])
  tx$investor_key <- normalize_text(tx$investor_name)
  tx$fund_id <- extract_fund_id(tx[["Id.Carteira"]])
  tx$fund_name <- as.character(tx[["Carteira"]])
  tx$operation_type <- as.character(tx[["Tipo Operação"]])
  tx$operation_key <- normalize_text(tx$operation_type)
  tx$operation_date <- as.Date(tx[["Dt Operação"]])
  tx$registration_date <- as.Date(tx[["Dt Registro"]])
  tx$conversion_date <- as.Date(tx[["Dt Conversão"]])
  tx$liquidation_date <- as.Date(tx[["Dt Liquidação"]])
  tx$gross_value <- suppressWarnings(as.numeric(tx[["Vl Bruto"]]))

  cota_on_conversion <- lapply(seq_len(nrow(tx)), function(i) {
    lookup_latest_cota(cota_lookup, tx$fund_id[[i]], tx$conversion_date[[i]])
  })

  tx$cota_on_conversion <- vapply(cota_on_conversion, function(x) x$value, numeric(1))
  tx$units_delta <- NA_real_

  application_rows <- tx$operation_key == "APLICACAO" &
    !is.na(tx$cota_on_conversion) &
    tx$cota_on_conversion > 0 &
    !is.na(tx$gross_value)
  redemption_rows <- tx$operation_key %in% c("RESGATE BRUTO", "RESGATE TOTAL") &
    !is.na(tx$cota_on_conversion) &
    tx$cota_on_conversion > 0 &
    !is.na(tx$gross_value)

  tx$units_delta[application_rows] <- tx$gross_value[application_rows] / tx$cota_on_conversion[application_rows]
  tx$units_delta[redemption_rows] <- -tx$gross_value[redemption_rows] / tx$cota_on_conversion[redemption_rows]
  tx$include_in_positions <- !is.na(tx$units_delta) & tx$operation_key != "COME COTAS"

  fund_catalog <- unique(tx[tx$fund_id != "", c("fund_id", "fund_name")])
  fund_catalog <- fund_catalog[order(fund_catalog$fund_id, fund_catalog$fund_name), , drop = FALSE]
  fund_catalog <- fund_catalog[!duplicated(fund_catalog$fund_id), , drop = FALSE]

  for (fund_id in names(cota_lookup)) {
    if (!(fund_id %in% fund_catalog$fund_id)) {
      fund_catalog <- rbind(
        fund_catalog,
        data.frame(fund_id = fund_id, fund_name = "", stringsAsFactors = FALSE)
      )
    }
  }

  fund_catalog <- fund_catalog[order(fund_catalog$fund_id), , drop = FALSE]

  list(
    transaction_records = tx,
    cota_lookup = cota_lookup,
    holidays = holidays,
    redemption_periods = periods,
    subscription_periods = subscription_periods,
    fund_catalog = fund_catalog
  )
}

compute_positions_snapshot <- function(model, as_of_date) {
  as_of_date <- as.Date(as_of_date)
  tx <- model$transaction_records

  active <- tx[
    tx$include_in_positions &
      !is.na(tx$conversion_date) &
      tx$conversion_date <= as_of_date &
      tx$investor_key != "" &
      tx$fund_id != "",
    c("investor_key", "investor_name", "fund_id", "fund_name", "units_delta"),
    drop = FALSE
  ]

  if (nrow(active) == 0) {
    return(empty_positions())
  }

  positions <- stats::aggregate(
    units_delta ~ investor_key + investor_name + fund_id + fund_name,
    data = active,
    FUN = sum
  )

  names(positions)[names(positions) == "units_delta"] <- "units"
  positions$units[abs(positions$units) < 1e-10] <- 0

  latest_cotas <- lapply(seq_len(nrow(positions)), function(i) {
    lookup_latest_cota(model$cota_lookup, positions$fund_id[[i]], as_of_date)
  })

  positions$latest_cota <- vapply(latest_cotas, function(x) x$value, numeric(1))
  positions$latest_cota_date <- as.Date(vapply(
    latest_cotas,
    function(x) if (is.na(x$date)) NA_character_ else as.character(x$date),
    character(1)
  ))
  positions$position_value <- ifelse(
    is.na(positions$latest_cota),
    NA_real_,
    positions$units * positions$latest_cota
  )
  positions$has_position <- !is.na(positions$position_value) &
    abs(positions$units) > 1e-8 &
    positions$position_value > 0.01

  positions[order(positions$fund_id, positions$investor_name), , drop = FALSE]
}

build_transaction_export <- function(details) {
  export_rows <- details[details$exportable, c(
    "investor_name",
    "fund_id",
    "fund_name",
    "operation_date",
    "registration_date",
    "movement_type",
    "conversion_date",
    "liquidation_date",
    "estimated_gross_value"
  ), drop = FALSE]

  if (nrow(export_rows) == 0) {
    return(empty_transaction_export())
  }

  names(export_rows) <- c(
    "Cotista",
    "Id.Carteira",
    "Carteira",
    "Dt OperaÃ§Ã£o",
    "Dt Registro",
    "Tipo OperaÃ§Ã£o",
    "Dt ConversÃ£o",
    "Dt LiquidaÃ§Ã£o",
    "Vl Bruto"
  )

  export_rows$`Id.Carteira` <- suppressWarnings(as.numeric(export_rows$`Id.Carteira`))
  export_rows[order(
    export_rows$`Dt OperaÃ§Ã£o`,
    export_rows$`Tipo OperaÃ§Ã£o`,
    export_rows$Carteira,
    export_rows$Cotista
  ), , drop = FALSE]
}

combine_transaction_exports <- function(...) {
  frames <- list(...)
  frames <- Filter(function(x) !is.null(x), frames)

  if (length(frames) == 0) {
    return(empty_transaction_export())
  }

  export_rows <- do.call(rbind, frames)
  if (nrow(export_rows) == 0) {
    return(empty_transaction_export())
  }

  export_rows[order(
    export_rows$`Dt OperaÃ§Ã£o`,
    export_rows$`Tipo OperaÃ§Ã£o`,
    export_rows$Carteira,
    export_rows$Cotista
  ), , drop = FALSE]
}

combine_estimation_details <- function(...) {
  frames <- list(...)
  frames <- Filter(function(x) !is.null(x), frames)

  if (length(frames) == 0) {
    return(empty_estimation_details())
  }

  details <- do.call(rbind, frames)
  if (nrow(details) == 0) {
    return(empty_estimation_details())
  }

  details[order(details$source_file, details$source_row, details$movement_type), , drop = FALSE]
}

estimate_redemptions <- function(input_orders, model, request_date, valuation_date = NULL, positions = NULL) {
  request_date <- as.Date(request_date)
  valuation_date <- as.Date(valuation_date %||% next_business_day(request_date, model$holidays))
  positions <- positions %||% compute_positions_snapshot(model, valuation_date)

  redemptions <- input_orders[input_orders$is_redemption, , drop = FALSE]
  if (nrow(redemptions) == 0) {
    return(list(
      valuation_date = valuation_date,
      details = empty_estimation_details(),
      export = empty_transaction_export(),
      positions = positions
    ))
  }

  fund_catalog <- model$fund_catalog
  names(fund_catalog) <- c("fund_id", "catalog_fund_name")
  details <- merge(redemptions, fund_catalog, by = "fund_id", all.x = TRUE)

  position_subset <- positions[, c(
    "investor_key",
    "fund_id",
    "investor_name",
    "fund_name",
    "units",
    "latest_cota",
    "latest_cota_date",
    "position_value",
    "has_position"
  )]
  names(position_subset)[3:4] <- c("position_investor_name", "position_fund_name")
  details <- merge(details, position_subset, by = c("investor_key", "fund_id"), all.x = TRUE)

  details$fund_name <- details$position_fund_name %||% ""
  missing_name <- is.na(details$fund_name) | trimws(details$fund_name) == ""
  details$fund_name[missing_name] <- details$catalog_fund_name[missing_name]
  details$fund_name[is.na(details$fund_name)] <- ""

  details$available_units <- ifelse(is.na(details$units), 0, details$units)
  details$available_value <- ifelse(is.na(details$position_value), 0, pmax(details$position_value, 0))

  details$status <- "ESTIMATED"
  details$note <- ""
  details$estimated_gross_value <- NA_real_
  details$operation_date <- as.Date(NA)
  details$registration_date <- as.Date(NA)
  details$conversion_date <- as.Date(NA)
  details$liquidation_date <- as.Date(NA)
  details$exportable <- FALSE

  for (i in seq_len(nrow(details))) {
    date_info <- calculate_redemption_dates(
      request_date = valuation_date,
      fund_id = details$fund_id[[i]],
      periods = model$redemption_periods,
      holidays = model$holidays
    )

    details$operation_date[[i]] <- date_info$operation_date
    details$registration_date[[i]] <- date_info$registration_date
    details$conversion_date[[i]] <- date_info$conversion_date
    details$liquidation_date[[i]] <- date_info$liquidation_date

    movement_type <- details$movement_type[[i]]
    available_value <- details$available_value[[i]]
    latest_cota <- details$latest_cota[[i]]
    latest_cota_date <- details$latest_cota_date[[i]]
    has_position <- isTRUE(details$has_position[[i]])
    input_value <- details$input_value[[i]]

    if (is.na(date_info$conversion_days)) {
      details$status[[i]] <- "MISSING REDEMPTION RULE"
      details$note[[i]] <- "Fund not found in redemption_period."
      next
    }

    if (is.na(latest_cota) || is.na(latest_cota_date)) {
      details$status[[i]] <- "MISSING COTA"
      details$note[[i]] <- "No unit value was found on or before the selected date."
      next
    }

    if (!has_position || available_value <= 0.01) {
      details$status[[i]] <- "NO POSITION"
      details$estimated_gross_value[[i]] <- 0
      details$note[[i]] <- "Investor has no estimated position for this fund on the selected date."
      next
    }

    if (movement_type == "Resgate Total") {
      details$estimated_gross_value[[i]] <- available_value
      details$status[[i]] <- "ESTIMATED"
      details$note[[i]] <- paste0(
        "Estimated using the latest cota available on ",
        format(latest_cota_date, "%Y-%m-%d"),
        "."
      )
      details$exportable[[i]] <- TRUE
      next
    }

    if (is.na(input_value) || input_value <= 0) {
      details$status[[i]] <- "INVALID VALUE"
      details$note[[i]] <- "Resgate Bruto requires a positive VALOR in the TXT file."
      next
    }

    details$estimated_gross_value[[i]] <- input_value

    if (input_value > available_value + 0.01) {
      details$status[[i]] <- "INSUFFICIENT POSITION"
      details$note[[i]] <- "Requested partial redemption is greater than the estimated available position."
      next
    }

    details$status[[i]] <- "ESTIMATED"
    details$note[[i]] <- paste0(
      "Requested partial redemption fits within the estimated position valued with the cota from ",
      format(latest_cota_date, "%Y-%m-%d"),
      "."
    )
    details$exportable[[i]] <- TRUE
  }

  details <- details[, c(
    "source_file",
    "source_row",
    "investor_name",
    "fund_id",
    "fund_name",
    "movement_code",
    "movement_type",
    "input_value",
    "available_units",
    "available_value",
    "latest_cota",
    "latest_cota_date",
    "status",
    "note",
    "operation_date",
    "registration_date",
    "conversion_date",
    "liquidation_date",
    "estimated_gross_value",
    "exportable"
  )]

  export_rows <- details[details$exportable, c(
    "investor_name",
    "fund_id",
    "fund_name",
    "operation_date",
    "registration_date",
    "movement_type",
    "conversion_date",
    "liquidation_date",
    "estimated_gross_value"
  ), drop = FALSE]

  names(export_rows) <- c(
    "Cotista",
    "Id.Carteira",
    "Carteira",
    "Dt Operação",
    "Dt Registro",
    "Tipo Operação",
    "Dt Conversão",
    "Dt Liquidação",
    "Vl Bruto"
  )

  export_rows$`Id.Carteira` <- suppressWarnings(as.numeric(export_rows$`Id.Carteira`))
  export_rows <- export_rows[order(export_rows$`Dt Operação`, export_rows$Carteira, export_rows$Cotista), , drop = FALSE]

  export_rows <- build_transaction_export(details)

  list(
    valuation_date = valuation_date,
    details = details,
    export = export_rows,
    positions = positions
  )
}

estimate_subscriptions <- function(input_orders, model, request_date, valuation_date = NULL) {
  request_date <- as.Date(request_date)
  valuation_date <- as.Date(valuation_date %||% next_business_day(request_date, model$holidays))

  subscriptions <- extract_subscriptions(input_orders)
  if (nrow(subscriptions) == 0) {
    return(list(
      valuation_date = valuation_date,
      details = empty_estimation_details(),
      export = empty_transaction_export()
    ))
  }

  fund_catalog <- model$fund_catalog
  names(fund_catalog) <- c("fund_id", "catalog_fund_name")
  details <- merge(subscriptions, fund_catalog, by = "fund_id", all.x = TRUE)

  details$fund_name <- details$catalog_fund_name %||% ""
  details$fund_name[is.na(details$fund_name)] <- ""
  details$available_units <- NA_real_
  details$available_value <- NA_real_
  details$latest_cota <- NA_real_
  details$latest_cota_date <- as.Date(NA)
  details$status <- "ESTIMATED"
  details$note <- ""
  details$estimated_gross_value <- NA_real_
  details$operation_date <- as.Date(NA)
  details$registration_date <- as.Date(NA)
  details$conversion_date <- as.Date(NA)
  details$liquidation_date <- as.Date(NA)
  details$exportable <- FALSE

  for (i in seq_len(nrow(details))) {
    date_info <- calculate_subscription_dates(
      request_date = valuation_date,
      fund_id = details$fund_id[[i]],
      periods = model$subscription_periods,
      holidays = model$holidays
    )

    details$operation_date[[i]] <- date_info$operation_date
    details$registration_date[[i]] <- date_info$registration_date
    details$conversion_date[[i]] <- date_info$conversion_date
    details$liquidation_date[[i]] <- date_info$liquidation_date

    input_value <- details$input_value[[i]]

    if (is.na(date_info$conversion_days)) {
      details$status[[i]] <- "MISSING SUBSCRIPTION RULE"
      details$note[[i]] <- "Fund not found in subscription."
      next
    }

    if (is.na(input_value) || input_value <= 0) {
      details$status[[i]] <- "INVALID VALUE"
      details$note[[i]] <- "AplicaÃ§Ã£o requires a positive VALOR in the TXT file."
      next
    }

    details$estimated_gross_value[[i]] <- input_value
    details$status[[i]] <- "ESTIMATED"
    details$note[[i]] <- "Estimated using the subscription sheet rule for this fund."
    details$exportable[[i]] <- TRUE
  }

  details <- details[, c(
    "source_file",
    "source_row",
    "investor_name",
    "fund_id",
    "fund_name",
    "movement_code",
    "movement_type",
    "input_value",
    "available_units",
    "available_value",
    "latest_cota",
    "latest_cota_date",
    "status",
    "note",
    "operation_date",
    "registration_date",
    "conversion_date",
    "liquidation_date",
    "estimated_gross_value",
    "exportable"
  )]

  export_rows <- build_transaction_export(details)

  list(
    valuation_date = valuation_date,
    details = details,
    export = export_rows
  )
}

estimate_transactions <- function(input_orders, model, request_date) {
  request_date <- as.Date(request_date)
  valuation_date <- next_business_day(request_date, model$holidays)
  positions <- compute_positions_snapshot(model, valuation_date)

  redemption_result <- estimate_redemptions(
    input_orders,
    model,
    request_date = valuation_date,
    valuation_date = valuation_date,
    positions = positions
  )
  subscription_result <- estimate_subscriptions(
    input_orders,
    model,
    request_date = valuation_date,
    valuation_date = valuation_date
  )

  list(
    valuation_date = valuation_date,
    details = combine_estimation_details(redemption_result$details, subscription_result$details),
    export = combine_transaction_exports(redemption_result$export, subscription_result$export),
    redemption_export = redemption_result$export,
    subscription_export = subscription_result$export,
    positions = positions
  )
}

display_input_orders <- function(input_orders) {
  if (is.null(input_orders) || nrow(input_orders) == 0) {
    return(data.frame())
  }

  out <- input_orders
  out$input_value <- format_money(out$input_value)
  out <- out[, c(
    "source_file",
    "source_row",
    "investor_name",
    "fund_cnpj",
    "fund_id",
    "movement_code",
    "movement_type",
    "input_value",
    "usage_status"
  )]

  names(out) <- c(
    "Source File",
    "Source Row",
    "Investor",
    "Fund CNPJ",
    "Fund ID",
    "Movement Code",
    "Movement Type",
    "Input Value",
    "Usage"
  )
  out
}

display_transaction_export <- function(export_rows) {
  if (is.null(export_rows) || nrow(export_rows) == 0) {
    return(data.frame())
  }

  out <- export_rows
  out$`Dt OperaÃ§Ã£o` <- format_date_text(out$`Dt OperaÃ§Ã£o`)
  out$`Dt Registro` <- format_date_text(out$`Dt Registro`)
  out$`Dt ConversÃ£o` <- format_date_text(out$`Dt ConversÃ£o`)
  out$`Dt LiquidaÃ§Ã£o` <- format_date_text(out$`Dt LiquidaÃ§Ã£o`)
  out$`Vl Bruto` <- format_money(out$`Vl Bruto`)
  out
}

extract_subscriptions <- function(input_orders) {
  if (is.null(input_orders)) {
    return(data.frame())
  }

  if (nrow(input_orders) == 0) {
    return(input_orders[FALSE, , drop = FALSE])
  }

  input_orders[input_orders$movement_type == "Aplicação", , drop = FALSE]
}

display_subscriptions <- function(export_rows) {
  display_transaction_export(export_rows)
}

display_estimation_details <- function(details) {
  if (is.null(details) || nrow(details) == 0) {
    return(data.frame())
  }

  out <- details
  out$input_value <- format_money(out$input_value)
  out$available_units <- format_units(out$available_units)
  out$available_value <- format_money(out$available_value)
  out$latest_cota <- ifelse(
    is.na(out$latest_cota),
    "",
    format(round(out$latest_cota, 8), decimal.mark = ",", nsmall = 8)
  )
  out$latest_cota_date <- format_date_text(out$latest_cota_date)
  out$operation_date <- format_date_text(out$operation_date)
  out$registration_date <- format_date_text(out$registration_date)
  out$conversion_date <- format_date_text(out$conversion_date)
  out$liquidation_date <- format_date_text(out$liquidation_date)
  out$estimated_gross_value <- format_money(out$estimated_gross_value)
  out$exportable <- ifelse(out$exportable, "Yes", "No")

  names(out) <- c(
    "Source File",
    "Source Row",
    "Investor",
    "Fund ID",
    "Fund",
    "Movement Code",
    "Movement Type",
    "Input Value",
    "Available Units",
    "Available Value",
    "Latest Cota",
    "Latest Cota Date",
    "Status",
    "Note",
    "Dt Operação",
    "Dt Registro",
    "Dt Conversão",
    "Dt Liquidação",
    "Estimated Vl Bruto",
    "Exportable"
  )
  out
}

display_positions <- function(positions) {
  if (is.null(positions) || nrow(positions) == 0) {
    return(data.frame())
  }

  out <- positions
  out$units <- format_units(out$units)
  out$latest_cota <- ifelse(
    is.na(out$latest_cota),
    "",
    format(round(out$latest_cota, 8), decimal.mark = ",", nsmall = 8)
  )
  out$latest_cota_date <- format_date_text(out$latest_cota_date)
  out$position_value <- format_money(out$position_value)
  out$has_position <- ifelse(out$has_position, "Yes", "No")
  names(out) <- c(
    "Investor Key",
    "Investor",
    "Fund ID",
    "Fund",
    "Units",
    "Latest Cota",
    "Latest Cota Date",
    "Position Value",
    "Has Position"
  )
  out
}

fund_choices <- function(model) {
  if (is.null(model) || is.null(model$fund_catalog) || nrow(model$fund_catalog) == 0) {
    return(c("All funds" = ""))
  }

  labels <- ifelse(
    trimws(model$fund_catalog$fund_name) == "",
    model$fund_catalog$fund_id,
    paste(model$fund_catalog$fund_id, model$fund_catalog$fund_name, sep = " - ")
  )

  c("All funds" = "", stats::setNames(model$fund_catalog$fund_id, labels))
}

filter_positions <- function(positions, investor_search = "", fund_id = "", positive_only = TRUE) {
  if (is.null(positions) || nrow(positions) == 0) {
    return(empty_positions())
  }

  out <- positions

  if (!is.null(investor_search) && trimws(investor_search) != "") {
    search_key <- normalize_text(investor_search)
    out <- out[grepl(search_key, out$investor_key, fixed = TRUE), , drop = FALSE]
  }

  if (!is.null(fund_id) && trimws(fund_id) != "") {
    out <- out[out$fund_id == extract_fund_id(fund_id), , drop = FALSE]
  }

  if (isTRUE(positive_only)) {
    out <- out[out$has_position, , drop = FALSE]
  }

  out[order(-out$position_value, out$investor_name), , drop = FALSE]
}

transaction_record_keys <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(character())
  }

  normalized <- as.data.frame(
    lapply(data, function(column) {
      if (inherits(column, "POSIXt")) {
        out <- format(as.Date(column), "%Y-%m-%d")
      } else if (inherits(column, "Date")) {
        out <- format(column, "%Y-%m-%d")
      } else if (is.numeric(column)) {
        out <- format(round(column, 8), scientific = FALSE, trim = TRUE)
      } else {
        out <- trimws(as.character(column))
      }

      out[is.na(out)] <- ""
      out
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  do.call(paste, c(normalized, sep = "\r"))
}

append_transaction_records_to_workbook <- function(path, transaction_rows, backup_dir = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!file.exists(path)) {
    stop("Workbook not found: ", path, call. = FALSE)
  }

  if (is.null(transaction_rows) || nrow(transaction_rows) == 0) {
    return(list(
      workbook_path = path,
      backup_path = NA_character_,
      inserted_rows = 0L,
      skipped_duplicate_rows = 0L,
      success = TRUE,
      message = "No generated transaction records to append."
    ))
  }

  existing_rows <- as.data.frame(
    readxl::read_xlsx(path, sheet = "transaction_records", .name_repair = "minimal"),
    stringsAsFactors = FALSE
  )

  rows_to_append <- transaction_rows
  skipped_duplicate_rows <- 0L

  if (nrow(existing_rows) > 0 && ncol(existing_rows) >= ncol(transaction_rows)) {
    existing_keys <- transaction_record_keys(existing_rows[, seq_len(ncol(transaction_rows)), drop = FALSE])
    new_keys <- transaction_record_keys(transaction_rows)
    duplicate_rows <- new_keys %in% existing_keys
    skipped_duplicate_rows <- sum(duplicate_rows)
    rows_to_append <- transaction_rows[!duplicate_rows, , drop = FALSE]
  }

  if (nrow(rows_to_append) == 0) {
    return(list(
      workbook_path = path,
      backup_path = NA_character_,
      inserted_rows = 0L,
      skipped_duplicate_rows = skipped_duplicate_rows,
      success = TRUE,
      message = paste0(
        "No new rows inserted. ",
        skipped_duplicate_rows,
        " duplicate row(s) skipped."
      )
    ))
  }

  if (is.null(backup_dir)) {
    backup_dir <- dirname(path)
  }

  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  backup_path <- file.path(
    backup_dir,
    paste0(
      tools::file_path_sans_ext(basename(path)),
      "_backup_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".xlsx"
    )
  )

  if (!file.copy(path, backup_path, overwrite = FALSE)) {
    stop(
      "Could not create workbook backup. Close the workbook in Excel and try again.",
      call. = FALSE
    )
  }

  workbook <- openxlsx::loadWorkbook(path)
  start_row <- nrow(existing_rows) + 2L

  openxlsx::writeData(
    workbook,
    sheet = "transaction_records",
    x = rows_to_append,
    startCol = 1,
    startRow = start_row,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)

  list(
    workbook_path = path,
    backup_path = backup_path,
    inserted_rows = nrow(rows_to_append),
    skipped_duplicate_rows = skipped_duplicate_rows,
    success = TRUE,
    message = paste0(
      nrow(rows_to_append),
      " row(s) inserted in ",
      basename(path),
      "."
    )
  )
}

write_redemption_report <- function(path, estimation, input_orders, read_errors = data.frame()) {
  wb <- openxlsx::createWorkbook()

  add_table_sheet <- function(sheet_name, data) {
    openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
    if (nrow(data) == 0) {
      data <- data.frame(Message = "No rows to show.", stringsAsFactors = FALSE)
    }
    openxlsx::writeDataTable(wb, sheet_name, data, tableStyle = "TableStyleMedium2")
    openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
    widths <- pmin(pmax(vapply(seq_along(data), function(i) {
      max(nchar(as.character(c(names(data)[[i]], head(data[[i]], 200)))), na.rm = TRUE) + 2
    }, numeric(1)), 10), 45)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_along(widths), widths = widths)
  }

  details <- estimation$details
  export_rows <- estimation$export
  positions_used <- estimation$positions
  subscriptions <- extract_subscriptions(input_orders)

  summary <- data.frame(
    Metric = c(
      "Generated at",
      "Request date used for valuation",
      "Redemption rows found in TXT files",
      "Subscription rows found in TXT files",
      "Generated rows exported",
      "Total generated value",
      "No position",
      "Insufficient position",
      "Missing cota or rule",
      "Unsupported rows ignored"
    ),
    Value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      format(estimation$valuation_date, "%Y-%m-%d"),
      sum(input_orders$is_redemption, na.rm = TRUE),
      nrow(subscriptions),
      nrow(export_rows),
      sum(export_rows$`Vl Bruto`, na.rm = TRUE),
      sum(details$status == "NO POSITION", na.rm = TRUE),
      sum(details$status == "INSUFFICIENT POSITION", na.rm = TRUE),
      sum(details$status %in% c("MISSING COTA", "MISSING REDEMPTION RULE", "MISSING SUBSCRIPTION RULE"), na.rm = TRUE),
      sum(!(input_orders$is_redemption | input_orders$is_subscription), na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )

  add_table_sheet("Summary", summary)
  add_table_sheet("transaction_records", export_rows)
  add_table_sheet("Estimation Details", details)
  add_table_sheet("Merged TXT Input", input_orders)
  add_table_sheet("Positions Snapshot", positions_used)

  if (!is.null(read_errors) && nrow(read_errors) > 0) {
    add_table_sheet("Read Errors", read_errors)
  }

  if (nrow(details) > 0) {
    sheet_name <- "Estimation Details"
    styles <- list(
      ESTIMATED = openxlsx::createStyle(fgFill = "#DCFCE7"),
      `NO POSITION` = openxlsx::createStyle(fgFill = "#FEF3C7"),
      `INSUFFICIENT POSITION` = openxlsx::createStyle(fgFill = "#FEE2E2"),
      `MISSING COTA` = openxlsx::createStyle(fgFill = "#DBEAFE"),
      `MISSING REDEMPTION RULE` = openxlsx::createStyle(fgFill = "#E5E7EB"),
      `MISSING SUBSCRIPTION RULE` = openxlsx::createStyle(fgFill = "#F3F4F6"),
      `INVALID VALUE` = openxlsx::createStyle(fgFill = "#FDE68A")
    )

    status_values <- details$status
    data_rows <- seq_len(nrow(details)) + 1
    for (status_name in names(styles)) {
      rows <- data_rows[status_values == status_name]
      if (length(rows) > 0) {
        openxlsx::addStyle(
          wb,
          sheet_name,
          styles[[status_name]],
          rows = rows,
          cols = seq_len(ncol(details)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
