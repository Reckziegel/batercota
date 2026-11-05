
#' Resolve a set of reports for one fund and reference date
#'
#' Finds reports using the persistent local library index.
#' Reports with multiple distinct hashes require an explicit selection.
#'
#' @param fund Fund key registered in Bater Cota.
#' @param reference_date A Date or an ISO date (YYYY-MM-DD).
#' @param report_types Unique report types to resolve.
#' @param selections Optional named character vector mapping report
#'   types to explicitly selected SHA-256 hashes.
#' @param library_path Directory containing the local report library.
#'
#' @return A `batercota_report_set` containing `status`, `reports`,
#'   and `candidates`. Each report has status `available`, `missing`,
#'   or `ambiguous`. The overall status is `ready`, `incomplete`,
#'   or `ambiguous`.
#' @export
resolve_report_set <- function(
    fund,
    reference_date,
    report_types = c(
      "daycoval_portfolio",
      "daycoval_position",
      "xp_statement"
    ),
    selections = character(),
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  bc_validate_text(fund, "fund")

  if (!fund %in% bc_fund_keys()) {
    cli::cli_abort(
      "Fundo não cadastrado: {.val {fund}}."
    )
  }

  reference_date <- bc_report_set_date(reference_date)

  allowed_types <- c(
    "daycoval_portfolio",
    "daycoval_position",
    "xp_statement"
  )

  valid_types <- is.character(report_types) &&
    length(report_types) > 0L &&
    !anyNA(report_types) &&
    !anyDuplicated(report_types) &&
    all(report_types %in% allowed_types)

  if (!valid_types) {
    cli::cli_abort(
      "`report_types` contém tipos inválidos ou duplicados."
    )
  }

  valid_selections <- is.character(selections) &&
    !anyNA(selections) &&
    (
      length(selections) == 0L ||
        (
          !is.null(names(selections)) &&
            !anyNA(names(selections)) &&
            all(nzchar(names(selections))) &&
            !anyDuplicated(names(selections)) &&
            all(names(selections) %in% report_types) &&
            all(grepl("^[a-f0-9]{64}$", selections))
        )
    )

  if (!valid_selections) {
    cli::cli_abort(
      "`selections` deve mapear tipos de relatório a hashes SHA-256."
    )
  }

  candidates <- list_reports(
    library_path = library_path,
    fund = fund
  )

  candidates <- candidates[
    !is.na(candidates$reference_date) &
      candidates$reference_date == reference_date &
      candidates$report_type %in% report_types,
    ,
    drop = FALSE
  ]

  candidates <- candidates |>
    dplyr::arrange(
      .data$report_type,
      dplyr::desc(.data$saved_at),
      .data$sha256
    )

  rows <- lapply(report_types, function(report_type) {
    matches <- candidates[
      candidates$report_type == report_type,
      ,
      drop = FALSE
    ]

    count <- nrow(matches)
    has_selection <- report_type %in% names(selections)

    if (has_selection) {
      selected_hash <- unname(selections[[report_type]])

      if (!selected_hash %in% matches$sha256) {
        cli::cli_abort(
          paste0(
            "O hash selecionado para {.val {report_type}} ",
            "não corresponde ao fundo, tipo e data solicitados."
          )
        )
      }
    } else {
      selected_hash <- NA_character_
    }

    if (count == 0L) {
      status <- "missing"
    } else if (count > 1L && !has_selection) {
      status <- "ambiguous"
    } else {
      status <- "available"

      if (!has_selection) {
        selected_hash <- matches$sha256[[1L]]
      }
    }

    tibble::tibble(
      report_type = report_type,
      status = status,
      n_candidates = count,
      sha256 = selected_hash
    )
  })

  reports <- dplyr::bind_rows(rows)

  status <- if (all(reports$status == "available")) {
    "ready"
  } else if (any(reports$status == "missing")) {
    "incomplete"
  } else {
    "ambiguous"
  }

  result <- list(
    fund_key = fund,
    reference_date = reference_date,
    status = status,
    reports = reports,
    candidates = candidates
  )

  class(result) <- c(
    "batercota_report_set",
    "list"
  )

  result
}



bc_report_set_date <- function(value) {
  if (inherits(value, "Date") &&
      length(value) == 1L &&
      !is.na(value)) {
    return(value)
  }

  valid_text <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
      value
    )

  if (valid_text) {
    parsed <- as.Date(
      value,
      format = "%Y-%m-%d"
    )

    if (!is.na(parsed) &&
        identical(format(parsed, "%Y-%m-%d"), value)) {
      return(parsed)
    }
  }

  cli::cli_abort(
    "`reference_date` deve ser uma data válida no formato YYYY-MM-DD."
  )
}
