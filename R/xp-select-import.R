
#' Extract the last Brazilian date from text
#'
#' @keywords internal
xp_select_text_date <- function(text) {
  matches <- regmatches(
    text,
    gregexpr(
      "[0-9]{2}/[0-9]{2}/[0-9]{4}",
      text
    )
  )

  values <- unlist(matches, use.names = FALSE)

  if (length(values) == 0L) {
    cli::cli_abort("Não foi possível identificar a data do relatório.")
  }

  date <- as.Date(
    utils::tail(values, 1L),
    format = "%d/%m/%Y"
  )

  if (is.na(date)) {
    cli::cli_abort("O relatório contém uma data inválida.")
  }

  date
}


#' Validate the fund identity
#'
#' @keywords internal
xp_select_check_identity <- function(value, expected) {
  if (
    length(value) == 0L ||
    is.na(value[[1L]]) ||
    !grepl(expected, value[[1L]], fixed = TRUE)
  ) {
    cli::cli_abort(
      "O relatório não pertence ao Linear Select."
    )
  }

  invisible(TRUE)
}


#' Detect an XP report by its contents
#'
#' @keywords internal
xp_select_detect_report <- function(path, source_name) {
  extension <- tolower(tools::file_ext(source_name))

  if (extension == "xml") {
    document <- xml2::read_xml(
      path,
      options = "NONET"
    )

    root <- xml2::xml_name(
      xml2::xml_root(document)
    )

    if (grepl("arquivoposicao_4_01", root, fixed = TRUE)) {
      header <- xml2::xml_find_first(
        document,
        ".//fundo/header"
      )

      cnpj <- xml2::xml_text(
        xml2::xml_find_first(header, "./cnpj")
      )

      xp_select_check_identity(cnpj, "57593831000131")

      date_text <- xml2::xml_text(
        xml2::xml_find_first(header, "./dtposicao")
      )

      date <- as.Date(date_text, format = "%Y%m%d")
      report_type <- "xp_position_xml"
    } else if (grepl("GalgoAssBalStmt", root, fixed = TRUE)) {
      fund_id <- xml2::xml_text(
        xml2::xml_find_first(
          document,
          paste0(
            ".//*[local-name()='BalForAcct']",
            "/*[local-name()='FinInstrmId']",
            "/*[local-name()='OthrId']",
            "/*[local-name()='Id']"
          )
        )
      )

      xp_select_check_identity(fund_id, "57593831000131")

      date_text <- xml2::xml_text(
        xml2::xml_find_first(
          document,
          paste0(
            ".//*[local-name()='StmtDtTm']",
            "/*[local-name()='Dt']"
          )
        )
      )

      date <- as.Date(date_text, format = "%Y-%m-%d")
      report_type <- "xp_position_xml5"
    } else {
      cli::cli_abort("Layout XML não reconhecido.")
    }

  } else if (extension == "csv") {
    data <- utils::read.csv2(
      path,
      fileEncoding = "UTF-8",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (nrow(data) == 0L) {
      cli::cli_abort("O CSV está vazio.")
    }

    if (all(c("Data_Operacao", "Codigo") %in% names(data))) {
      xp_select_check_identity(
        unique(trimws(data$Cliente)),
        "CUST4325"
      )

      dates <- as.Date(
        substr(data$Data_Operacao, 1L, 10L),
        format = "%Y-%m-%d"
      )

      if (anyNA(dates) || length(unique(dates)) != 1L) {
        cli::cli_abort("Datas inconsistentes na Carteira CSV.")
      }

      date <- unique(dates)
      report_type <- "xp_portfolio_csv"

    } else if (all(
      c("Cod", "Data", "Histórico", "Saldo") %in% names(data)
    )) {
      xp_select_check_identity(
        unique(trimws(data$Cod)),
        "CUST4325"
      )

      dates <- as.Date(
        data$Data,
        format = "%d/%m/%Y"
      )

      if (anyNA(dates)) {
        cli::cli_abort("Datas inválidas no Caixa CSV.")
      }

      date <- max(dates)
      report_type <- "xp_cash_csv"
    } else {
      cli::cli_abort("Layout CSV não reconhecido.")
    }

  } else if (extension == "xlsx") {
    sheets <- readxl::excel_sheets(path)

    if (
      length(sheets) != 1L ||
      !tolower(sheets[[1L]]) %in% c("carteira", "caixa")
    ) {
      cli::cli_abort("Layout XLSX não reconhecido.")
    }

    header <- readxl::read_excel(
      path,
      sheet = sheets[[1L]],
      range = "A1:B5",
      col_names = FALSE,
      col_types = "text"
    )

    xp_select_check_identity(
      as.character(header[[2L]][[5L]]),
      "CUST4325"
    )

    date <- xp_select_text_date(
      as.character(header[[2L]][[4L]])
    )

    report_type <- if (tolower(sheets[[1L]]) == "caixa") {
      "xp_cash_xlsx"
    } else {
      "xp_portfolio_xlsx"
    }

  } else if (extension == "pdf") {
    page <- pdftools::pdf_text(path)[[1L]]
    lines <- strsplit(page, "\n", fixed = TRUE)[[1L]]

    if (grepl("Extrato de Cotista", page, fixed = TRUE)) {
      xp_select_check_identity(page, "10850617")

      date_line <- grep(
        "Movimenta",
        lines,
        value = TRUE
      )

      report_type <- "xp_hub_statement"
    } else {
      xp_select_check_identity(page, "CUST4325")

      date_line <- grep(
        "Data da Posi",
        lines,
        value = TRUE
      )

      if (grepl("Demonstrativo de Caixa", page, fixed = TRUE)) {
        report_type <- "xp_cash_pdf"
      } else if (grepl("Carteira Diária", page, fixed = TRUE)) {
        report_type <- "xp_portfolio_pdf"
      } else {
        cli::cli_abort("Layout PDF não reconhecido.")
      }
    }

    date <- xp_select_text_date(date_line)

  } else {
    cli::cli_abort(
      "Extensão não suportada: {.val {extension}}."
    )
  }

  if (length(date) != 1L || is.na(date)) {
    cli::cli_abort("Data-base inválida no relatório.")
  }

  list(
    report_type = report_type,
    reference_date = date,
    extension = extension
  )
}


#' List the XP Linear Select report library
#'
#' @param library_path Root of the persistent report library.
#'
#' @export
list_xp_select_reports <- function(
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  root <- file.path(
    bc_library_root(library_path),
    "xp_select"
  )

  index_path <- file.path(root, "index.rds")

  empty <- tibble::tibble(
    sha256 = character(),
    source_name = character(),
    source_archive = character(),
    report_type = character(),
    reference_date = as.Date(character()),
    extension = character(),
    stored_path = character(),
    saved_at = as.POSIXct(character(), tz = "UTC")
  )

  if (!file.exists(index_path)) {
    return(empty)
  }

  index <- bc_read_rds(index_path)

  if (
    !is.data.frame(index) ||
    !identical(names(index), names(empty))
  ) {
    cli::cli_abort(
      "Índice da biblioteca XP inválido."
    )
  }

  index
}


#' Save a classified XP report by SHA-256
#'
#' @keywords internal
xp_select_store_report <- function(
    path,
    source_name,
    source_archive,
    library_path
) {
  info <- xp_select_detect_report(
    path = path,
    source_name = source_name
  )

  hash <- bc_sha256(path)

  index <- list_xp_select_reports(
    library_path = library_path
  )

  existing <- index[index$sha256 == hash, , drop = FALSE]

  if (nrow(existing) > 0L) {
    return(dplyr::mutate(
      existing,
      status = "duplicate",
      error = NA_character_
    ))
  }

  has_version <- any(
    index$report_type == info$report_type &
      index$reference_date == info$reference_date
  )

  root <- file.path(
    bc_library_root(library_path),
    "xp_select"
  )

  destination <- file.path(
    root,
    "objects",
    substr(hash, 1L, 2L),
    paste0(hash, ".", info$extension)
  )

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  bc_copy_original(
    source = path,
    destination = destination,
    hash = hash
  )

  record <- tibble::tibble(
    sha256 = hash,
    source_name = source_name,
    source_archive = source_archive,
    report_type = info$report_type,
    reference_date = info$reference_date,
    extension = info$extension,
    stored_path = destination,
    saved_at = Sys.time()
  )

  updated <- dplyr::bind_rows(index, record)

  bc_write_rds(
    object = updated,
    path = file.path(root, "index.rds"),
    replace = file.exists(file.path(root, "index.rds"))
  )

  dplyr::mutate(
    record,
    status = if (has_version) "new_version" else "stored",
    error = NA_character_
  )
}


#' Extract supported files from an XP ZIP safely
#'
#' @keywords internal
xp_select_extract_archive <- function(path, directory) {
  contents <- utils::unzip(path, list = TRUE)

  if (nrow(contents) > 150L) {
    cli::cli_abort("O ZIP contém arquivos demais.")
  }

  entries <- contents$Name

  unsafe <- grepl("^/|^[A-Za-z]:|\\\\", entries) |
    vapply(
      strsplit(entries, "/", fixed = TRUE),
      function(parts) any(parts %in% c("..", ".")),
      logical(1)
    )

  if (any(unsafe)) {
    cli::cli_abort(
      "O ZIP contém caminhos potencialmente inseguros."
    )
  }

  if (
    anyNA(contents$Length) ||
    any(contents$Length < 0) ||
    sum(contents$Length) > 150 * 1024^2
  ) {
    cli::cli_abort(
      "O ZIP excede o limite de 150 MB descompactados."
    )
  }

  selected <- entries[
    !grepl("/$", entries) &
      grepl("\\.(xml|csv|xlsx|pdf)$", entries, ignore.case = TRUE)
  ]

  if (length(selected) == 0L) {
    cli::cli_abort(
      "O ZIP não contém relatórios XP suportados."
    )
  }

  utils::unzip(
    zipfile = path,
    files = selected,
    exdir = directory
  )

  extracted <- file.path(directory, selected)

  root <- paste0(
    normalizePath(directory, winslash = "/", mustWork = TRUE),
    "/"
  )

  if (
    any(!file.exists(extracted)) ||
    any(dir.exists(extracted))
  ) {
    cli::cli_abort("Falha na extração dos arquivos do ZIP.")
  }

  normalized <- normalizePath(
    extracted,
    winslash = "/",
    mustWork = TRUE
  )

  if (any(!startsWith(normalized, root))) {
    cli::cli_abort("O ZIP contém caminhos fora da área temporária.")
  }

  extracted
}


#' Import and persist XP Linear Select reports
#'
#' Accepts ZIP archives and individual XML, CSV, XLSX or PDF files.
#' Files are classified by content and stored using SHA-256.
#'
#' @param paths One or more report paths, including ZIP archives.
#' @param source_names Original filenames, useful for Shiny uploads.
#' @param library_path Persistent report library directory.
#'
#' @return A tibble containing the import status of each file.
#'
#' @export
import_xp_select_reports <- function(
    paths,
    source_names = basename(paths),
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  valid <- is.character(paths) &&
    length(paths) > 0L &&
    !anyNA(paths) &&
    all(file.exists(paths)) &&
    !any(dir.exists(paths)) &&
    is.character(source_names) &&
    length(source_names) == length(paths) &&
    !anyNA(source_names) &&
    all(nzchar(source_names))

  if (!valid) {
    cli::cli_abort("Informe arquivos válidos para importação.")
  }

  temporary <- tempfile("xp-select-")
  dir.create(temporary, recursive = TRUE)

  on.exit(
    unlink(temporary, recursive = TRUE),
    add = TRUE
  )

  candidates <- purrr::map2_dfr(
    paths,
    source_names,
    function(path, name) {
      extension <- tolower(tools::file_ext(name))

      if (extension == "zip") {
        directory <- tempfile(
          pattern = "archive-",
          tmpdir = temporary
        )

        dir.create(directory)

        extracted <- xp_select_extract_archive(
          path = path,
          directory = directory
        )

        tibble::tibble(
          path = extracted,
          source_name = basename(extracted),
          source_archive = name
        )
      } else {
        tibble::tibble(
          path = path,
          source_name = name,
          source_archive = NA_character_
        )
      }
    }
  )

  purrr::pmap_dfr(
    candidates,
    function(path, source_name, source_archive) {
      tryCatch(
        xp_select_store_report(
          path = path,
          source_name = source_name,
          source_archive = source_archive,
          library_path = library_path
        ),
        error = function(error) {
          tibble::tibble(
            sha256 = NA_character_,
            source_name = source_name,
            source_archive = source_archive,
            report_type = NA_character_,
            reference_date = as.Date(NA),
            extension = tolower(tools::file_ext(source_name)),
            stored_path = NA_character_,
            saved_at = as.POSIXct(NA),
            status = "rejected",
            error = conditionMessage(error)
          )
        }
      )
    }
  )
}
