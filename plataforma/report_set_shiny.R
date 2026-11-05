
# Date-based report selection for the Bater Cota Shiny app.

bc_report_set_field_labels <- function() {
  c(
    daycoval_portfolio = "Carteira Diária",
    daycoval_position = "Posição C",
    xp_statement = "Extrato XP"
  )
}


bc_report_set_validate_fields <- function(report_fields) {
  valid <- is.character(report_fields) &&
    length(report_fields) > 0L &&
    !is.null(names(report_fields)) &&
    !anyNA(report_fields) &&
    !anyNA(names(report_fields)) &&
    all(nzchar(names(report_fields))) &&
    !anyDuplicated(names(report_fields)) &&
    !anyDuplicated(report_fields) &&
    all(report_fields %in% names(bc_report_set_field_labels()))

  if (!valid) {
    cli::cli_abort(
      "`report_fields` deve mapear campos a tipos de relatório únicos."
    )
  }

  invisible(TRUE)
}


bc_report_set_verified_paths <- function(
    report_set,
    report_fields,
    library_path = bc_library_shiny_path()
) {
  bc_report_set_validate_fields(report_fields)

  if (!inherits(report_set, "batercota_report_set")) {
    cli::cli_abort(
      "`report_set` deve ser um conjunto de relatórios resolvido."
    )
  }

  if (!setequal(
    unname(report_fields),
    report_set$reports$report_type
  )) {
    cli::cli_abort(
      "Os tipos solicitados não correspondem ao conjunto resolvido."
    )
  }

  paths <- vapply(names(report_fields), function(field) {
    report_type <- unname(report_fields[[field]])

    row <- report_set$reports[
      report_set$reports$report_type == report_type,
      ,
      drop = FALSE
    ]

    hash <- row$sha256[[1L]]

    if (is.na(hash)) {
      return(NA_character_)
    }

    batercota::load_report(
      hash = hash,
      library_path = library_path,
      parsed = FALSE
    )
  }, character(1))

  paths
}


bc_report_set_picker_ui <- function(
    id,
    title = "Localizar relatórios por data"
) {
  ns <- shiny::NS(id)

  shiny::wellPanel(
    shiny::h5(title),

    shiny::dateInput(
      inputId = ns("reference_date"),
      label = "Data-base",
      value = Sys.Date(),
      format = "dd/mm/yyyy"
    ),

    shiny::actionButton(
      inputId = ns("search"),
      label = "Buscar na biblioteca",
      width = "100%"
    ),

    shiny::br(),

    shiny::uiOutput(
      outputId = ns("availability")
    ),

    shiny::uiOutput(
      outputId = ns("versions")
    ),

    shiny::actionButton(
      inputId = ns("apply"),
      label = "Preencher relatórios encontrados",
      class = "btn-outline-primary",
      width = "100%"
    )
  )
}


bc_report_set_picker_server <- function(
    id,
    selected_fund,
    report_fields,
    revision,
    target_session = NULL,
    on_apply = NULL
) {
  bc_report_set_validate_fields(report_fields)

  shiny::moduleServer(id, function(input, output, session) {
    report_set_state <- shiny::reactiveVal(NULL)
    report_set_error <- shiny::reactiveVal(NULL)

    if (is.null(target_session)) {
      target_session <- session
    }

    shiny::observeEvent(
      list(
        selected_fund(),
        input$reference_date,
        revision()
      ),
      {
        report_set_state(NULL)
      },
      ignoreInit = TRUE
    )


    shiny::observeEvent(input$search, {
      report_set_state(NULL)
      report_set_error(NULL)

      result <- tryCatch(
        batercota::resolve_report_set(
          fund = selected_fund(),
          reference_date = input$reference_date,
          report_types = unname(report_fields),
          library_path = bc_library_shiny_path()
        ),
        error = function(error) error
      )

      if (inherits(result, "error")) {
        message <- conditionMessage(result)

        report_set_error(message)

        shiny::showNotification(
          message,
          type = "error",
          duration = NULL
        )

        return(invisible(NULL))
      }

      report_set_state(result)
    })


    output$availability <- shiny::renderUI({
      result <- report_set_state()

      if (is.null(result)) {
        return(NULL)
      }

      labels <- bc_report_set_field_labels()

      rows <- lapply(seq_len(nrow(result$reports)), function(i) {
        report <- result$reports[i, , drop = FALSE]

        status_text <- switch(
          report$status[[1L]],
          available = "Disponível",
          missing = "Não encontrado",
          ambiguous = paste0(
            report$n_candidates[[1L]],
            " versões: escolha uma"
          )
        )

        shiny::tags$li(
          shiny::strong(
            unname(labels[[report$report_type[[1L]]]])
          ),
          ": ",
          status_text
        )
      })

      shiny::tagList(
        shiny::tags$p(
          "Data-base: ",
          format(result$reference_date, "%d/%m/%Y")
        ),
        shiny::tags$ul(rows)
      )
    })

    output$versions <- shiny::renderUI({
      result <- report_set_state()

      if (is.null(result)) {
        return(NULL)
      }

      ambiguous_types <- result$reports$report_type[
        result$reports$status == "ambiguous"
      ]

      if (length(ambiguous_types) == 0L) {
        return(NULL)
      }

      labels <- bc_report_set_field_labels()

      controls <- lapply(ambiguous_types, function(report_type) {
        matches <- result$candidates[
          result$candidates$report_type == report_type,
          ,
          drop = FALSE
        ]

        option_labels <- paste0(
          format(matches$saved_at, "%d/%m/%Y %H:%M"),
          " | ",
          matches$source_name,
          " | SHA ",
          substr(matches$sha256, 1L, 10L)
        )

        choices <- stats::setNames(
          matches$sha256,
          option_labels
        )

        shiny::selectInput(
          inputId = session$ns(
            paste0("choice_", report_type)
          ),
          label = paste(
            "Escolha a versão:",
            unname(labels[[report_type]])
          ),
          choices = c(
            "Selecione uma versão" = "",
            choices
          ),
          selected = ""
        )
      })

      shiny::tagList(controls)
    })

    shiny::observeEvent(input$apply, {
      current <- report_set_state()

      if (is.null(current)) {
        shiny::showNotification(
          "Escolha uma data e busque os relatórios primeiro.",
          type = "warning"
        )

        return(invisible(NULL))
      }

      selections <- character()

      ambiguous_types <- current$reports$report_type[
        current$reports$status == "ambiguous"
      ]

      for (report_type in ambiguous_types) {
        selected <- input[[paste0("choice_", report_type)]]

        if (
          is.character(selected) &&
          length(selected) == 1L &&
          !is.na(selected) &&
          nzchar(selected)
        ) {
          selections[report_type] <- selected
        }
      }

      resolved <- tryCatch(
        batercota::resolve_report_set(
          fund = selected_fund(),
          reference_date = current$reference_date,
          report_types = unname(report_fields),
          selections = selections,
          library_path = bc_library_shiny_path()
        ),
        error = function(error) error
      )

      if (inherits(resolved, "error")) {
        shiny::showNotification(
          conditionMessage(resolved),
          type = "error",
          duration = NULL
        )

        return(invisible(NULL))
      }

      paths <- tryCatch(
        bc_report_set_verified_paths(
          report_set = resolved,
          report_fields = report_fields
        ),
        error = function(error) error
      )

      if (inherits(paths, "error")) {
        shiny::showNotification(
          conditionMessage(paths),
          type = "error",
          duration = NULL
        )

        return(invisible(NULL))
      }

      for (field in names(paths)) {
        value <- paths[[field]]

        shiny::updateTextInput(
          session = target_session,
          inputId = field,
          value = if (is.na(value)) "" else value
        )
      }

      if (is.function(on_apply)) {
        on_apply()
      }

      available <- sum(!is.na(paths))

      if (available == length(paths)) {
        message <- paste(
          "Todos os relatórios foram preenchidos.",
          "Clique em Conciliar para executar a análise."
        )
      } else {
        message <- paste0(
          available,
          " de ",
          length(paths),
          " relatórios preenchidos. ",
          "Complete os campos pendentes antes de conciliar."
        )
      }

      shiny::showNotification(
        message,
        type = if (available == length(paths)) {
          "message"
        } else {
          "warning"
        },
        duration = 8
      )
    })

    invisible(report_set_state)
  })
}
