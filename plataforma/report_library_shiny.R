
# Shiny integration for the local report library.

bc_library_project_root <- function(start = getwd()) {
  directory <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    is_project <- file.exists(
      file.path(directory, "DESCRIPTION")
    ) &&
      dir.exists(file.path(directory, "plataforma"))

    if (is_project) {
      return(directory)
    }

    parent <- dirname(directory)

    if (identical(parent, directory)) {
      cli::cli_abort(
        "Não foi possível identificar a raiz do projeto Bater Cota."
      )
    }

    directory <- parent
  }
}


bc_library_shiny_path <- function() {
  configured <- getOption(
    "batercota.report_library",
    file.path("data", "report_library")
  )

  if (!is.character(configured) ||
      length(configured) != 1L ||
      is.na(configured) ||
      !nzchar(trimws(configured))) {
    cli::cli_abort(
      "O caminho da biblioteca local é inválido."
    )
  }

  is_absolute <- startsWith(configured, "/") ||
    startsWith(configured, "~") ||
    startsWith(configured, "\\\\") ||
    grepl("^[A-Za-z]:[/\\\\]", configured)

  if (!is_absolute) {
    configured <- file.path(
      bc_library_project_root(),
      configured
    )
  }

  normalizePath(
    path.expand(configured),
    winslash = "/",
    mustWork = FALSE
  )
}


bc_library_choices <- function(fund, report_type) {
  reports <- batercota::list_reports(
    library_path = bc_library_shiny_path(),
    fund = fund,
    report_type = report_type
  )

  if (nrow(reports) == 0L) {
    return(c("Nenhum relatório disponível" = ""))
  }

  reports <- reports[
    order(
      reports$reference_date,
      reports$saved_at,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  period <- ifelse(
    reports$period_start == reports$period_end,
    format(reports$period_start, "%d/%m/%Y"),
    paste0(
      format(reports$period_start, "%d/%m/%Y"),
      " a ",
      format(reports$period_end, "%d/%m/%Y")
    )
  )

  labels <- paste0(
    period,
    " | ",
    reports$source_name,
    " | ",
    substr(reports$sha256, 1L, 10L)
  )

  stats::setNames(reports$sha256, labels)
}


bc_library_picker_ui <- function(
    path_id,
    ns = identity
) {
  shiny::tagList(
    shiny::selectInput(
      inputId = ns(paste0(path_id, "_library_hash")),
      label = "Ou selecione da biblioteca local",
      choices = c("Nenhum relatório disponível" = ""),
      selected = ""
    ),
    shiny::actionButton(
      inputId = ns(paste0(path_id, "_library_use")),
      label = "Usar relatório salvo",
      class = "btn-outline-secondary",
      width = "100%"
    ),
    shiny::br()
  )
}


bc_library_bind_pickers <- function(
    input,
    session,
    selected_fund,
    revision,
    specs
) {
  if (is.null(names(specs)) ||
      any(!nzchar(names(specs)))) {
    cli::cli_abort(
      "`specs` deve possuir identificadores de entrada."
    )
  }

  for (path_id in names(specs)) {
    local({
      current_id <- path_id
      current_type <- unname(specs[[current_id]])

      select_id <- paste0(
        current_id,
        "_library_hash"
      )

      button_id <- paste0(
        current_id,
        "_library_use"
      )

      shiny::observe({
        revision()

        fund <- selected_fund()
        shiny::req(fund)

        choices <- tryCatch(
          bc_library_choices(fund, current_type),
          error = function(error) {
            shiny::showNotification(
              conditionMessage(error),
              type = "warning",
              duration = NULL
            )

            c("Biblioteca indisponível" = "")
          }
        )

        shiny::updateSelectInput(
          session = session,
          inputId = select_id,
          choices = choices,
          selected = ""
        )
      })

      shiny::observeEvent(
        input[[button_id]],
        {
          hash <- input[[select_id]]

          if (is.null(hash) ||
              length(hash) != 1L ||
              !nzchar(hash)) {
            shiny::showNotification(
              "Selecione um relatório da biblioteca.",
              type = "warning"
            )

            return(invisible(NULL))
          }

          tryCatch(
            {
              library_path <- bc_library_shiny_path()

              available <- batercota::list_reports(
                library_path = library_path,
                fund = selected_fund(),
                report_type = current_type
              )

              if (!hash %in% available$sha256) {
                cli::cli_abort(
                  "O relatório não pertence ao fundo e tipo selecionados."
                )
              }

              path <- batercota::load_report(
                hash = hash,
                library_path = library_path,
                parsed = FALSE
              )

              shiny::updateTextInput(
                session = session,
                inputId = current_id,
                value = path
              )

              shiny::showNotification(
                "Relatório recuperado da biblioteca.",
                type = "message"
              )
            },
            error = function(error) {
              shiny::showNotification(
                conditionMessage(error),
                type = "error",
                duration = NULL
              )
            }
          )
        },
        ignoreInit = TRUE
      )
    })
  }

  invisible(NULL)
}


bc_library_find_stored_report <- function(
    path,
    library_path = bc_library_shiny_path()
) {
  normalized <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  reports <- batercota::list_reports(
    library_path = library_path
  )

  objects_root <- normalizePath(
    file.path(library_path, "objects"),
    winslash = "/",
    mustWork = TRUE
  )

  if (!startsWith(
    normalized,
    paste0(objects_root, "/")
  )) {
    return(NULL)
  }

  hash <- sub(
    "\\.[^.]+$",
    "",
    basename(normalized)
  )

  matches <- reports[
    reports$sha256 == hash,
    ,
    drop = FALSE
  ]

  if (nrow(matches) != 1L) {
    cli::cli_abort(
      "O arquivo informado não está registrado na biblioteca."
    )
  }

  expected <- normalizePath(
    file.path(
      objects_root,
      substr(hash, 1L, 2L),
      paste0(hash, ".", matches$extension[[1L]])
    ),
    winslash = "/",
    mustWork = TRUE
  )

  if (!identical(normalized, expected)) {
    cli::cli_abort(
      "O caminho do documento não corresponde ao índice."
    )
  }

  matches
}


bc_library_read <- function(
    path,
    report_type,
    library_path = bc_library_shiny_path()
) {
  record <- bc_library_find_stored_report(
    path = path,
    library_path = library_path
  )

  if (!is.null(record)) {
    if (!identical(
      record$report_type[[1L]],
      report_type
    )) {
      cli::cli_abort(
        "O documento salvo possui tipo incompatível."
      )
    }

    return(
      batercota::load_report(
        hash = record$sha256[[1L]],
        library_path = library_path
      )
    )
  }

  switch(
    report_type,
    daycoval_portfolio = batercota::read_daycoval_portfolio(
      path = path
    ),
    daycoval_position = batercota::read_daycoval_position(
      path = path
    ),
    daycoval_daily_movement =
      batercota::read_daycoval_daily_movement(
        path = path
      ),
    daycoval_cash_statement =
      batercota::read_daycoval_cash_statement(
        path = path
      ),
    xp_statement = batercota::read_xp_statement(
      path = path
    ),
    fund_cash_plan = batercota::read_fund_cash_plan(
      path = path
    ),
    fund_operations = batercota::read_fund_operations(
      path = path
    ),
    cli::cli_abort(
      "Tipo de relatório não suportado: {.val {report_type}}."
    )
  )
}


bc_library_save_paths <- function(
    paths,
    library_path = bc_library_shiny_path()
) {
  paths <- unique(
    unlist(paths, use.names = FALSE)
  )

  paths <- paths[
    !is.na(paths) & nzchar(paths)
  ]

  if (length(paths) == 0L) {
    return(tibble::tibble(
      source_path = character(),
      status = character(),
      sha256 = character(),
      error = character()
    ))
  }

  results <- lapply(paths, function(path) {
    saved <- tryCatch(
      batercota::save_report(
        path = path,
        library_path = library_path
      ),
      error = function(error) error
    )

    if (inherits(saved, "error")) {
      return(tibble::tibble(
        source_path = path,
        status = "error",
        sha256 = NA_character_,
        error = conditionMessage(saved)
      ))
    }

    tibble::tibble(
      source_path = path,
      status = saved$status[[1L]],
      sha256 = saved$sha256[[1L]],
      error = NA_character_
    )
  })

  dplyr::bind_rows(results)
}


bc_library_register_imports <- function(
    paths,
    revision
) {
  result <- bc_library_save_paths(paths)

  failures <- result[
    result$status == "error",
    ,
    drop = FALSE
  ]

  if (nrow(failures) > 0L) {
    details <- paste(
      paste0(
        basename(failures$source_path),
        ": ",
        failures$error
      ),
      collapse = "\n"
    )

    shiny::showNotification(
      paste(
        "Conciliação concluída, mas houve falha ao salvar:",
        details,
        sep = "\n"
      ),
      type = "warning",
      duration = NULL
    )
  }

  if (any(result$status %in% c("stored", "duplicate"))) {
    revision(shiny::isolate(revision()) + 1L)
  }

  invisible(result)
}
