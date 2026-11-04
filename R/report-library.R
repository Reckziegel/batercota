

#' Save an original report in the local library
#'
#' Validates the report using its existing importer and stores the
#' original bytes under a SHA-256 content identifier.
#'
#' @param path Path to an original CSV, PDF, or XLSX report.
#' @param library_path Directory used for persistent storage.
#'
#' @return A one-row tibble with report metadata and a `status` column.
#' @export
save_report <- function(
    path,
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  bc_validate_file(path)

  hash <- bc_sha256(path)
  inspected <- bc_inspect_report(path)

  if (!identical(bc_sha256(path), hash)) {
    cli::cli_abort(
      "O arquivo mudou durante a validação."
    )
  }

  identity <- inspected$identity
  current <- list_reports(library_path)

  existing <- current[
    current$sha256 == hash,
    ,
    drop = FALSE
  ]

  root <- bc_library_root(library_path)

  if (nrow(existing) == 1L) {
    if (!bc_same_identity(existing, identity)) {
      cli::cli_abort(
        "O arquivo já existe, mas seus metadados são inconsistentes."
      )
    }

    destination <- bc_object_path(
      root,
      hash,
      existing$extension[[1L]]
    )

    if (!identical(bc_sha256(destination), hash)) {
      cli::cli_abort(
        "A cópia armazenada falhou na verificação SHA-256."
      )
    }

    return(dplyr::mutate(existing, status = "duplicate"))
  }

  extension <- tolower(tools::file_ext(path))

  destination <- bc_object_path(
    root,
    hash,
    extension
  )

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  bc_copy_original(path, destination, hash)

  record <- bc_make_record(
    path = destination,
    hash = hash,
    source_name = basename(path),
    identity = identity
  )

  bc_write_rds(
    record,
    bc_record_path(root, hash)
  )

  updated <- dplyr::bind_rows(current, record) |>
    bc_sort_index()

  bc_write_rds(
    updated,
    file.path(root, "index.rds"),
    replace = TRUE
  )

  dplyr::mutate(record, status = "stored")
}


#' List reports in the local library
#'
#' Uses a persistent catalog for fast queries. If files or metadata
#' records do not match the catalog, the index is rebuilt automatically.
#' Full content verification is performed by `verify_report_library()`.
#'
#' @param library_path Directory containing the local library.
#' @param fund Optional fund key used to filter reports.
#' @param report_type Optional report type used to filter reports.
#'
#' @return A tibble containing report metadata.
#' @export
list_reports <- function(
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    ),
    fund = NULL,
    report_type = NULL
) {
  root <- bc_library_root(library_path)
  bc_initialize_library(root)

  index <- bc_read_rds(
    file.path(root, "index.rds")
  )

  if (!bc_index_is_current(index, root)) {
    index <- bc_rebuild_index(root)
  }

  if (!is.null(fund)) {
    bc_validate_text(fund, "fund")

    index <- index[
      index$fund_key == fund,
      ,
      drop = FALSE
    ]
  }

  if (!is.null(report_type)) {
    bc_validate_text(report_type, "report_type")

    index <- index[
      index$report_type == report_type,
      ,
      drop = FALSE
    ]
  }

  index
}


#' Load a report from the local library
#'
#' Verifies the stored original using SHA-256 and optionally imports
#' it using the existing report reader.
#'
#' @param hash Full SHA-256 identifier returned by `save_report()`.
#' @param library_path Directory containing the local library.
#' @param parsed Whether to return the parsed S3 object. When `FALSE`,
#'   returns the local path to the original stored file.
#'
#' @return A parsed report or a local file path.
#' @export
load_report <- function(
    hash,
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    ),
    parsed = TRUE
) {
  bc_validate_text(hash, "hash")

  if (!grepl("^[a-f0-9]{64}$", hash)) {
    cli::cli_abort(
      "`hash` deve ser um SHA-256 completo."
    )
  }

  if (!is.logical(parsed) ||
      length(parsed) != 1L ||
      is.na(parsed)) {
    cli::cli_abort(
      "`parsed` deve ser TRUE ou FALSE."
    )
  }

  index <- list_reports(library_path)
  position <- match(hash, index$sha256)

  if (is.na(position)) {
    cli::cli_abort(
      "Relatório não encontrado na biblioteca."
    )
  }

  record <- index[position, , drop = FALSE]
  root <- bc_library_root(library_path)

  path <- bc_object_path(
    root,
    hash,
    record$extension[[1L]]
  )

  if (!identical(bc_sha256(path), hash)) {
    cli::cli_abort(
      "O arquivo original falhou na verificação SHA-256."
    )
  }

  if (!parsed) {
    return(
      normalizePath(
        path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  inspected <- bc_inspect_report(
    path,
    source_name = record$source_name[[1L]]
  )

  if (!bc_same_identity(record, inspected$identity)) {
    cli::cli_abort(
      "Os metadados do relatório não correspondem ao registro."
    )
  }

  inspected$report
}


#' Verify the integrity of the local report library
#'
#' Recalculates SHA-256 hashes for all stored originals and checks
#' whether their individual metadata records exist. This function
#' does not modify the library.
#'
#' @param library_path Directory containing the local library.
#'
#' @return A tibble with file paths, actual hashes, and audit status.
#' @export
verify_report_library <- function(
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  root <- bc_library_root(library_path)
  bc_initialize_library(root)

  objects <- bc_object_files(root)
  hashes <- bc_file_hashes(objects)
  record_hashes <- bc_record_hashes(root)

  rows <- lapply(seq_along(objects), function(i) {
    path <- objects[[i]]
    expected <- hashes[[i]]

    actual <- tryCatch(
      bc_sha256(path),
      error = function(error) NA_character_
    )

    status <- if (is.na(actual)) {
      "unreadable"
    } else if (!identical(actual, expected)) {
      "hash_mismatch"
    } else if (!expected %in% record_hashes) {
      "missing_metadata"
    } else {
      "ok"
    }

    tibble::tibble(
      sha256 = expected,
      file_path = path,
      actual_sha256 = actual,
      status = status
    )
  })

  missing <- setdiff(record_hashes, hashes)

  missing_rows <- tibble::tibble(
    sha256 = missing,
    file_path = NA_character_,
    actual_sha256 = NA_character_,
    status = rep("missing_object", length(missing))
  )

  if (length(rows) == 0L) {
    return(missing_rows)
  }

  dplyr::bind_rows(
    dplyr::bind_rows(rows),
    missing_rows
  )
}


#' Repair the local report library catalog
#'
#' Recalculates file hashes, reconstructs missing or invalid metadata
#' records, and recreates the central index. Original files are never
#' overwritten or deleted.
#'
#' @param library_path Directory containing the local library.
#'
#' @return The reconstructed report index.
#' @export
repair_report_library <- function(
    library_path = getOption(
      "batercota.report_library",
      file.path("data", "report_library")
    )
) {
  root <- bc_library_root(library_path)
  bc_initialize_library(root)

  bc_rebuild_index(root)
}


bc_validate_text <- function(value, field) {
  valid <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))

  if (!valid) {
    cli::cli_abort(
      "`{field}` deve ser um texto não vazio."
    )
  }

  invisible(TRUE)
}


bc_validate_file <- function(path) {
  bc_validate_text(path, "path")

  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort(
      "O arquivo informado não existe."
    )
  }

  extension <- tolower(tools::file_ext(path))

  if (!extension %in% c("csv", "pdf", "xlsx")) {
    cli::cli_abort(
      "Formato não suportado: {.val {extension}}."
    )
  }

  invisible(TRUE)
}


bc_library_root <- function(path) {
  bc_validate_text(path, "library_path")

  normalizePath(
    path.expand(path),
    winslash = "/",
    mustWork = FALSE
  )
}


bc_initialize_library <- function(root) {
  directories <- file.path(
    root,
    c("objects", "records")
  )

  for (directory in directories) {
    dir.create(
      directory,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!dir.exists(directory)) {
      cli::cli_abort(
        "Não foi possível criar o diretório {.path {directory}}."
      )
    }
  }

  invisible(root)
}


bc_sha256 <- function(path) {
  digest::digest(
    object = path,
    algo = "sha256",
    file = TRUE
  )
}


bc_object_path <- function(root, hash, extension) {
  file.path(
    root,
    "objects",
    substr(hash, 1L, 2L),
    paste0(hash, ".", extension)
  )
}


bc_record_path <- function(root, hash) {
  file.path(
    root,
    "records",
    paste0(hash, ".rds")
  )
}


bc_object_files <- function(root) {
  files <- list.files(
    file.path(root, "objects"),
    recursive = TRUE,
    full.names = TRUE
  )

  files[
    grepl(
      "^[a-f0-9]{64}\\.(csv|pdf|xlsx)$",
      basename(files)
    )
  ]
}


bc_record_files <- function(root) {
  list.files(
    file.path(root, "records"),
    pattern = "^[a-f0-9]{64}\\.rds$",
    full.names = TRUE
  )
}


bc_file_hashes <- function(paths) {
  sub(
    "\\.[^.]+$",
    "",
    basename(paths)
  )
}


bc_record_hashes <- function(root) {
  sub(
    "\\.rds$",
    "",
    basename(bc_record_files(root))
  )
}


bc_read_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    suppressWarnings(readRDS(path)),
    error = function(error) NULL
  )
}


bc_write_rds <- function(object, path, replace = FALSE) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary <- tempfile(
    pattern = ".pending-",
    tmpdir = dirname(path)
  )

  on.exit(unlink(temporary), add = TRUE)

  saveRDS(object, temporary, version = 3L)

  if (!replace && file.exists(path)) {
    cli::cli_abort(
      "O registro já existe e não será sobrescrito."
    )
  }

  backup <- NULL

  if (replace && file.exists(path)) {
    backup <- tempfile(
      pattern = ".backup-",
      tmpdir = dirname(path)
    )

    if (!file.rename(path, backup)) {
      cli::cli_abort(
        "Não foi possível preparar a atualização de {.path {path}}."
      )
    }

    on.exit({
      if (file.exists(backup) && !file.exists(path)) {
        file.rename(backup, path)
      }
    }, add = TRUE)
  }

  if (!file.rename(temporary, path)) {
    cli::cli_abort(
      "Não foi possível gravar {.path {path}}."
    )
  }

  if (!is.null(backup)) {
    unlink(backup)
  }

  invisible(path)
}


bc_copy_original <- function(source, destination, hash) {
  if (file.exists(destination)) {
    if (!identical(bc_sha256(destination), hash)) {
      cli::cli_abort(
        "Conflito com um arquivo já armazenado."
      )
    }

    return(invisible(destination))
  }

  temporary <- tempfile(
    pattern = ".incoming-",
    tmpdir = dirname(destination)
  )

  on.exit(unlink(temporary), add = TRUE)

  if (!file.copy(source, temporary, overwrite = FALSE)) {
    cli::cli_abort(
      "Não foi possível copiar o arquivo original."
    )
  }

  if (!identical(bc_sha256(temporary), hash)) {
    cli::cli_abort(
      "O conteúdo mudou durante a importação."
    )
  }

  if (!file.rename(temporary, destination)) {
    if (!file.exists(destination) ||
        !identical(bc_sha256(destination), hash)) {
      cli::cli_abort(
        "Não foi possível finalizar a cópia do arquivo."
      )
    }
  }

  invisible(destination)
}


bc_empty_index <- function() {
  tibble::tibble(
    sha256 = character(),
    source_name = character(),
    extension = character(),
    report_type = character(),
    fund_key = character(),
    fund_code = character(),
    fund_name = character(),
    scope = character(),
    period_start = as.Date(character()),
    period_end = as.Date(character()),
    reference_date = as.Date(character()),
    version_key = character(),
    bytes = numeric(),
    saved_at = as.POSIXct(character(), tz = "UTC")
  )
}


bc_make_record <- function(path, hash, source_name, identity) {
  version_key <- paste(
    identity$fund_code,
    identity$report_type,
    identity$scope,
    identity$period_start,
    identity$period_end,
    identity$reference_date,
    sep = "|"
  )

  tibble::tibble(
    sha256 = hash,
    source_name = source_name,
    extension = tolower(tools::file_ext(path)),
    report_type = identity$report_type,
    fund_key = identity$fund_key,
    fund_code = identity$fund_code,
    fund_name = identity$fund_name,
    scope = identity$scope,
    period_start = identity$period_start,
    period_end = identity$period_end,
    reference_date = identity$reference_date,
    version_key = version_key,
    bytes = as.numeric(file.info(path)$size),
    saved_at = Sys.time()
  )
}


bc_sort_index <- function(index) {
  index |>
    dplyr::arrange(
      .data$fund_key,
      .data$report_type,
      .data$period_start,
      .data$saved_at,
      .data$sha256
    )
}


bc_record_is_valid <- function(
    record,
    hash,
    extension,
    path
) {
  if (!is.data.frame(record) ||
      nrow(record) != 1L ||
      !identical(names(record), names(bc_empty_index()))) {
    return(FALSE)
  }

  identical(record$sha256[[1L]], hash) &&
    identical(record$extension[[1L]], extension) &&
    identical(
      as.numeric(record$bytes[[1L]]),
      as.numeric(file.info(path)$size)
    ) &&
    inherits(record$period_start, "Date") &&
    inherits(record$period_end, "Date") &&
    inherits(record$reference_date, "Date") &&
    inherits(record$saved_at, "POSIXct")
}


bc_index_is_current <- function(index, root) {
  if (!is.data.frame(index) ||
      !identical(names(index), names(bc_empty_index()))) {
    return(FALSE)
  }

  if (!inherits(index$period_start, "Date") ||
      !inherits(index$period_end, "Date") ||
      !inherits(index$reference_date, "Date") ||
      !inherits(index$saved_at, "POSIXct")) {
    return(FALSE)
  }

  hashes <- index$sha256
  extensions <- index$extension

  if (!is.character(hashes) ||
      !is.character(extensions) ||
      anyNA(hashes) ||
      anyNA(extensions) ||
      anyDuplicated(hashes) > 0L ||
      any(!grepl("^[a-f0-9]{64}$", hashes)) ||
      any(!extensions %in% c("csv", "pdf", "xlsx"))) {
    return(FALSE)
  }

  objects <- bc_object_files(root)
  object_hashes <- bc_file_hashes(objects)
  record_hashes <- bc_record_hashes(root)

  if (anyDuplicated(object_hashes) > 0L ||
      !setequal(hashes, object_hashes) ||
      !setequal(hashes, record_hashes)) {
    return(FALSE)
  }

  expected_paths <- bc_object_path(
    root,
    hashes,
    extensions
  )

  if (!setequal(expected_paths, objects) ||
      any(!file.exists(expected_paths))) {
    return(FALSE)
  }

  if (length(expected_paths) > 0L) {
    sizes <- as.numeric(file.info(expected_paths)$size)

    if (anyNA(sizes) ||
        !identical(sizes, as.numeric(index$bytes))) {
      return(FALSE)
    }
  }

  TRUE
}


bc_rebuild_index <- function(root) {
  objects <- bc_object_files(root)
  hashes <- bc_file_hashes(objects)
  record_hashes <- bc_record_hashes(root)

  if (anyDuplicated(hashes) > 0L) {
    cli::cli_abort(
      "A biblioteca contém objetos com hashes duplicados."
    )
  }

  missing_objects <- setdiff(record_hashes, hashes)

  if (length(missing_objects) > 0L) {
    cli::cli_abort(
      "Existem registros sem os arquivos originais correspondentes."
    )
  }

  rows <- lapply(seq_along(objects), function(i) {
    object_path <- objects[[i]]
    hash <- hashes[[i]]

    if (!identical(bc_sha256(object_path), hash)) {
      cli::cli_abort(
        "Integridade comprometida no objeto {.val {hash}}."
      )
    }

    extension <- tolower(tools::file_ext(object_path))
    record_path <- bc_record_path(root, hash)

    record <- bc_read_rds(record_path)

    if (!bc_record_is_valid(
      record,
      hash,
      extension,
      object_path
    )) {
      inspected <- bc_inspect_report(object_path)

      record <- bc_make_record(
        path = object_path,
        hash = hash,
        source_name = basename(object_path),
        identity = inspected$identity
      )

      bc_write_rds(
        record,
        record_path,
        replace = file.exists(record_path)
      )
    }

    record
  })

  index <- if (length(rows) == 0L) {
    bc_empty_index()
  } else {
    dplyr::bind_rows(rows) |>
      bc_sort_index()
  }

  bc_write_rds(
    index,
    file.path(root, "index.rds"),
    replace = TRUE
  )

  index
}


bc_same_identity <- function(record, identity) {
  fields <- c(
    "report_type",
    "fund_key",
    "fund_code",
    "fund_name",
    "scope",
    "period_start",
    "period_end",
    "reference_date"
  )

  if (!all(fields %in% names(record))) {
    return(FALSE)
  }

  all(vapply(fields, function(field) {
    identical(
      record[[field]][[1L]],
      identity[[field]]
    )
  }, logical(1)))
}


bc_fund_keys <- function() {
  c("linear", "reta_alocacao", "long_term")
}


bc_fund_reference <- function(key) {
  reference <- batercota_fund_reference(key)

  list(
    fund_key = key,
    fund_code = reference$portfolio_code,
    fund_name = reference$name
  )
}


bc_resolve_fund <- function(identifier, field) {
  bc_validate_text(identifier, "identifier")

  keys <- bc_fund_keys()

  matches <- keys[vapply(keys, function(key) {
    reference <- batercota_fund_reference(key)

    identical(
      as.character(reference[[field]]),
      identifier
    )
  }, logical(1))]

  if (length(matches) != 1L) {
    cli::cli_abort(
      "Identificador de fundo não configurado: {.val {identifier}}."
    )
  }

  bc_fund_reference(matches[[1L]])
}


bc_normalize_fund_name <- function(value) {
  value <- iconv(
    as.character(value),
    from = "",
    to = "ASCII//TRANSLIT"
  )

  value <- tolower(value)

  gsub("[^a-z0-9]", "", value)
}


bc_match_fund_names <- function(names) {
  names <- unique(
    names[!is.na(names) & nzchar(names)]
  )

  normalized <- bc_normalize_fund_name(names)
  keys <- bc_fund_keys()

  keys[vapply(keys, function(key) {
    reference <- batercota_fund_reference(key)

    bc_normalize_fund_name(reference$name) %in% normalized
  }, logical(1))]
}



bc_cash_plan_fund <- function(report) {
  shareholder_operations <- report$shareholder_operations
  investment_operations <- report$investment_operations

  fund_codes <- unique(
    trimws(as.character(shareholder_operations$fund_code))
  )

  fund_codes <- fund_codes[
    !is.na(fund_codes) & nzchar(fund_codes)
  ]

  fund_names <- unique(c(
    as.character(shareholder_operations$fund_name),
    as.character(investment_operations$portfolio_fund_name),
    as.character(report$metadata$fund_name)
  ))

  fund_names <- trimws(fund_names)
  fund_names <- fund_names[
    !is.na(fund_names) & nzchar(fund_names)
  ]

  if (length(fund_codes) != 1L || length(fund_names) == 0L) {
    cli::cli_abort(
      "A planilha deve identificar um único fundo."
    )
  }




  aliases <- tibble::tribble(
    ~fund_key, ~fund_code, ~fund_name,
    "linear",
    "60629171",
    paste(
      "LINEAR FUNDO DE INVESTIMENTO MULTIMERCADO",
      "CRÉDITO PRIVADO RESPONSABILIDADE LIMITADA"
    ),
    "long_term",
    "56365598",
    "LONG TERM LB FUNDO DE INVESTIMENTO MULTIMERCADO",
    "reta_alocacao",
    "54236091",
    "RETA ALOCAÇÃO FUNDO DE INVESTIMENTO EM COTAS MULTIMERCADO"
  )




  keys <- bc_fund_keys()

  matches <- keys[vapply(keys, function(key) {
    reference <- batercota_fund_reference(key)

    accepted_codes <- c(
      reference$portfolio_code,
      aliases$fund_code[aliases$fund_key == key]
    )

    fund_codes[[1L]] %in% accepted_codes
  }, logical(1))]

  if (length(matches) != 1L) {
    cli::cli_abort(
      "Código de fundo não cadastrado: {.val {fund_codes[[1L]]}}."
    )
  }

  fund_key <- matches[[1L]]
  reference <- batercota_fund_reference(fund_key)

  accepted_names <- c(
    reference$name,
    aliases$fund_name[aliases$fund_key == fund_key]
  )

  normalized_names <- bc_normalize_fund_name(fund_names)
  normalized_accepted <- bc_normalize_fund_name(accepted_names)

  if (!all(normalized_names %in% normalized_accepted)) {
    cli::cli_abort(
      "A planilha contém nomes de fundos inconsistentes
       com o código identificado."
    )
  }

  bc_fund_reference(fund_key)
}



bc_operations_fund <- function(report) {
  operations <- report$operations

  codes <- unique(
    as.character(operations$portfolio_fund_cnpj)
  )

  codes <- codes[!is.na(codes) & nzchar(codes)]

  names <- unique(
    as.character(operations$portfolio_fund_name)
  )

  names <- names[!is.na(names) & nzchar(names)]

  if (length(codes) == 0L ||
      length(names) == 0L) {
    cli::cli_abort(
      "A planilha não identifica seus fundos."
    )
  }

  if (length(codes) > 1L) {
    return(list(
      fund_key = "multiple",
      fund_code = "multiple",
      fund_name = "Múltiplos fundos",
      scope = paste(sort(codes), collapse = ";")
    ))
  }

  matched <- bc_match_fund_names(names)

  if (length(matched) > 1L) {
    cli::cli_abort(
      "Nomes de fundos inconsistentes na planilha."
    )
  }

  if (length(matched) == 1L) {
    fund <- bc_fund_reference(matched[[1L]])
  } else {
    fund <- list(
      fund_key = "external",
      fund_code = codes[[1L]],
      fund_name = names[[1L]]
    )
  }

  fund$scope <- codes[[1L]]

  fund
}


bc_detect_report_type <- function(path) {
  extension <- tolower(tools::file_ext(path))

  if (extension == "pdf") {
    return("xp_statement")
  }

  if (extension == "xlsx") {
    sheets <- readxl::excel_sheets(path)

    cash_plan_sheets <- c(
      "Operações Cotistas",
      "Operações Fundos",
      "Movimentações"
    )

    if (all(cash_plan_sheets %in% sheets)) {
      return("fund_cash_plan")
    }

    return("fund_operations")
  }

  records <- read_daycoval_records(path)
  types <- unique(records$record_type)

  layouts <- c(
    daycoval_portfolio = "LayCrtDiaDespRef",
    daycoval_position = "P1030",
    daycoval_daily_movement = "P0115_QuebraPag",
    daycoval_cash_statement = "P1051"
  )

  detected <- names(layouts)[layouts %in% types]

  if (length(detected) != 1L) {
    cli::cli_abort(
      "O CSV não identifica um único tipo de relatório suportado."
    )
  }

  detected[[1L]]
}


bc_read_report <- function(
    path,
    report_type,
    source_name
) {
  switch(
    report_type,
    daycoval_portfolio = read_daycoval_portfolio(
      path,
      source_name = source_name
    ),
    daycoval_position = read_daycoval_position(
      path,
      source_name = source_name
    ),
    daycoval_daily_movement = read_daycoval_daily_movement(
      path,
      source_name = source_name
    ),
    daycoval_cash_statement = read_daycoval_cash_statement(
      path,
      source_name = source_name
    ),
    xp_statement = read_xp_statement(
      path,
      source_name = source_name
    ),
    fund_cash_plan = read_fund_cash_plan(
      path,
      source_name = source_name
    ),
    fund_operations = read_fund_operations(
      path,
      source_name = source_name
    ),
    cli::cli_abort(
      "Tipo de relatório não suportado."
    )
  )
}


bc_inspect_report <- function(
    path,
    source_name = basename(path)
) {
  bc_validate_file(path)

  report_type <- bc_detect_report_type(path)

  report <- bc_read_report(
    path = path,
    report_type = report_type,
    source_name = source_name
  )

  metadata <- report$metadata
  scope <- "single"

  if (report_type %in% c(
    "daycoval_portfolio",
    "daycoval_position"
  )) {
    fund <- bc_resolve_fund(
      metadata$codigo_carteira,
      "portfolio_code"
    )

    start <- metadata$data_posicao
    end <- start
    reference <- start
  } else if (report_type == "daycoval_daily_movement") {
    fund <- bc_resolve_fund(
      metadata$codigo_carteira,
      "portfolio_code"
    )

    start <- metadata$data_inicial
    end <- metadata$data_final
    reference <- end
  } else if (report_type == "daycoval_cash_statement") {
    fund <- bc_resolve_fund(
      metadata$codigo_carteira,
      "portfolio_code"
    )

    start <- metadata$opening_date
    end <- metadata$closing_date
    reference <- end
  } else if (report_type == "xp_statement") {
    fund <- bc_resolve_fund(
      metadata$account_number,
      "xp_account"
    )

    start <- metadata$movement_start
    end <- metadata$movement_end
    reference <- metadata$position_date
  } else if (report_type == "fund_cash_plan") {
    fund <- bc_cash_plan_fund(report)

    dates <- report$cash_flows$date
    dates <- dates[!is.na(dates)]

    if (length(dates) == 0L) {
      cli::cli_abort(
        "A planilha não contém datas de fluxo válidas."
      )
    }

    start <- min(dates)
    end <- max(dates)
    reference <- end
  } else {
    fund <- bc_operations_fund(report)

    scope <- fund$scope
    fund$scope <- NULL

    start <- metadata$operation_start
    end <- metadata$operation_end
    reference <- end
  }

  valid_dates <- all(vapply(
    list(start, end, reference),
    function(value) {
      inherits(value, "Date") &&
        length(value) == 1L &&
        !is.na(value)
    },
    logical(1)
  ))

  if (!valid_dates || start > end) {
    cli::cli_abort(
      "O relatório não possui um período válido."
    )
  }

  identity <- c(
    list(report_type = report_type),
    fund,
    list(
      scope = scope,
      period_start = start,
      period_end = end,
      reference_date = reference
    )
  )

  list(
    report = report,
    identity = identity
  )
}
