
# Bater Cota - Linear Select
# Administrador: XP

options(shiny.maxRequestSize = 200 * 1024^2)

xp_app_project_root <- function() {
  candidates <- c(
    getwd(),
    dirname(getwd())
  )

  valid <- vapply(
    candidates,
    function(path) {
      file.exists(file.path(path, "DESCRIPTION")) &&
        dir.exists(file.path(path, "R")) &&
        dir.exists(file.path(path, "plataforma"))
    },
    logical(1)
  )

  if (!any(valid)) {
    stop(
      "Abra o projeto batercota antes de executar o aplicativo.",
      call. = FALSE
    )
  }

  normalizePath(
    candidates[which(valid)[[1L]]],
    winslash = "/",
    mustWork = TRUE
  )
}


project_root <- xp_app_project_root()

source(
  file.path(
    project_root,
    "plataforma",
    "mod_linear_select_ativos.R"
  ),
  local = TRUE
)

source(
  file.path(
    project_root,
    "plataforma",
    "mod_linear_select_cota.R"
  ),
  local = TRUE
)

source(
  file.path(
    project_root,
    "plataforma",
    "mod_linear_select_fluxos.R"
  ),
  local = TRUE
)

library_path <- file.path(
  project_root,
  "data",
  "report_library"
)

package_namespace <- asNamespace("batercota")

required_functions <- c(
  "import_xp_select_reports",
  "list_xp_select_reports"
)

missing_functions <- required_functions[
  !vapply(
    required_functions,
    exists,
    logical(1),
    envir = package_namespace,
    inherits = FALSE
  )
]

if (length(missing_functions) > 0L) {
  stop(
    "Funções XP não carregadas: ",
    paste(missing_functions, collapse = ", "),
    ". Execute devtools::load_all('.') novamente.",
    call. = FALSE
  )
}

import_reports <- get(
  "import_xp_select_reports",
  envir = package_namespace
)

list_saved_reports <- get(
  "list_xp_select_reports",
  envir = package_namespace
)


xp_app_table <- function(data, page_length = 15L) {
  DT::datatable(
    data,
    rownames = FALSE,
    class = "stripe hover compact",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = TRUE,
      order = list(),
      language = list(
        search = "Buscar:",
        lengthMenu = "Mostrar _MENU_ registros",
        info = "_START_ a _END_ de _TOTAL_ registros",
        infoEmpty = "Nenhum registro",
        zeroRecords = "Nenhum registro encontrado",
        paginate = list(
          previous = "Anterior",
          `next` = "Próximo"
        )
      )
    )
  )
}


xp_app_report_label <- function(report_type) {
  labels <- c(
    xp_position_xml = "XML de posição",
    xp_position_xml5 = "XML5",
    xp_portfolio_csv = "Carteira CSV",
    xp_portfolio_xlsx = "Carteira XLSX",
    xp_portfolio_pdf = "Carteira PDF",
    xp_cash_csv = "Caixa CSV",
    xp_cash_xlsx = "Caixa XLSX",
    xp_cash_pdf = "Caixa PDF",
    xp_hub_statement = "Extrato XP Hub"
  )

  result <- unname(labels[report_type])

  result[is.na(result)] <- report_type[is.na(result)]

  result
}


ui <- shiny::navbarPage(
  title = "Bater Cota - Linear Select",

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  shiny::tabPanel(
    "Importação",
    shiny::br(),

    shiny::h3("Importação automática de relatórios"),

    shiny::p(
      paste(
        "Importe o ZIP disponibilizado pela XP Administradora.",
        "O aplicativo identificará e armazenará os relatórios",
        "automaticamente na biblioteca local."
      )
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,

        shiny::actionButton(
          inputId = "load_library",
          label = "Carregar biblioteca existente",
          icon = shiny::icon("database"),
          class = "btn-outline-primary",
          width = "100%"
        ),

        shiny::hr(),

        shiny::fileInput(
          inputId = "reports",
          label = "Selecionar ZIP ou relatórios",
          multiple = TRUE,
          accept = c(
            ".zip",
            ".xml",
            ".csv",
            ".xlsx",
            ".pdf"
          ),
          buttonLabel = "Selecionar arquivos",
          placeholder = "Nenhum arquivo selecionado"
        ),


        shiny::hr(),

        shiny::h5("Importar diretamente do computador"),

        shiny::textInput(
          inputId = "zip_path",
          label = "Caminho completo do ZIP",
          value = "",
          placeholder = "C:/Users/USUARIO/Downloads/arquivo.zip",
          width = "100%"
        ),

        shiny::textInput(
          inputId = "hub_path",
          label = "Caminho do extrato XP Hub (PDF)",
          value = "",
          placeholder = "C:/Users/USUARIO/Downloads/Extrato_arquivo.pdf",
          width = "100%"
        ),


        shiny::actionButton(
          inputId = "import_zip_path",
          label = "Importar relatórios XP e Hub",
          icon = shiny::icon("folder-open"),
          class = "btn-primary",
          width = "100%"
        ),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "O arquivo será lido diretamente do disco local.",
            "Não é necessário utilizar a janela de seleção de arquivos."
          )
        ),


        shiny::tags$small(
          class = "text-muted",
          paste(
            "Também é possível importar o extrato da XP Hub",
            "separadamente ou junto com o ZIP."
          )
        ),

        shiny::hr(),

        shiny::uiOutput("import_summary"),

        shiny::hr(),

        shiny::h5("Biblioteca local"),

        shiny::textOutput("library_location")
      ),

      shiny::mainPanel(
        shiny::h4("Resultado da última importação"),

        DT::DTOutput("import_table"),

        shiny::br(),

        shiny::h4("Datas disponíveis"),

        DT::DTOutput("dates_table")
      )
    )
  ),

  xp_select_assets_ui("assets"),
  xp_select_cota_ui("cota"),
  xp_select_fluxos_ui("fluxos"),

  shiny::tabPanel(
    "Biblioteca",
    shiny::br(),

    shiny::h3("Relatórios armazenados"),

    shiny::p(
      paste(
        "Consulte os relatórios por data-base.",
        "Arquivos já importados permanecem disponíveis",
        "para futuras conciliações."
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,

        shiny::selectInput(
          inputId = "reference_date",
          label = "Data-base",
          choices = character()
        )
      ),

      shiny::column(
        width = 4,

        shiny::actionButton(
          inputId = "refresh_library",
          label = "Atualizar biblioteca",
          icon = shiny::icon("rotate")
        )
      )
    ),

    shiny::uiOutput("date_status"),

    DT::DTOutput("library_table")
  )
)


server <- function(input, output, session) {
  import_result <- shiny::reactiveVal(NULL)

  library_revision <- shiny::reactiveVal(NULL)


  refresh_library <- function() {
    current <- shiny::isolate(library_revision())

    next_revision <- if (is.null(current)) {
      1L
    } else {
      current + 1L
    }

    library_revision(next_revision)

    invisible(next_revision)
  }


  saved_reports <- shiny::reactive({
    shiny::req(
      !is.null(library_revision())
    )

    list_saved_reports(
      library_path = library_path
    )
  })


  xp_select_assets_server(
    id = "assets",
    saved_reports = saved_reports,
    library_path = library_path
  )

  xp_select_cota_server(
    id = "cota",
    saved_reports = saved_reports,
    library_path = library_path
  )

  xp_select_fluxos_server(
    id = "fluxos",
    saved_reports = saved_reports,
    library_path = library_path
  )


  shiny::observeEvent(input$reports, {
    shiny::req(input$reports)

    files <- input$reports

    result <- tryCatch(
      import_reports(
        paths = files$datapath,
        source_names = files$name,
        library_path = library_path
      ),
      error = function(error) error
    )

    if (inherits(result, "error")) {
      shiny::showNotification(
        conditionMessage(result),
        type = "error",
        duration = NULL
      )

      return(invisible(NULL))
    }

    import_result(result)

    refresh_library()

    shiny::observeEvent(
      input$load_library,
      {
        refresh_library()

        shiny::showNotification(
          "Biblioteca carregada com sucesso.",
          type = "message",
          duration = 4
        )
      },
      ignoreInit = TRUE
    )

    rejected <- sum(result$status == "rejected")

    if (rejected > 0L) {
      shiny::showNotification(
        paste(
          rejected,
          "arquivo(s) rejeitado(s). Consulte os detalhes."
        ),
        type = "warning",
        duration = 10
      )
    } else {
      shiny::showNotification(
        "Importação concluída. Biblioteca atualizada.",
        type = "message",
        duration = 5
      )
    }
  })



  shiny::observeEvent(input$import_zip_path, {
    paths <- c(
      zip = trimws(input$zip_path),
      hub = trimws(input$hub_path)
    )

    paths <- gsub('^"|"$', "", paths)
    paths <- paths[nzchar(paths)]

    if (length(paths) == 0L) {
      shiny::showNotification(
        "Informe o caminho do ZIP ou do extrato XP Hub.",
        type = "warning"
      )

      return(invisible(NULL))
    }

    missing_files <- paths[
      !file.exists(paths) | dir.exists(paths)
    ]

    if (length(missing_files) > 0L) {
      shiny::showNotification(
        paste(
          "Arquivo não encontrado:",
          paste(missing_files, collapse = "; ")
        ),
        type = "error",
        duration = NULL
      )

      return(invisible(NULL))
    }

    expected_extensions <- c(
      zip = "zip",
      hub = "pdf"
    )

    invalid_extensions <- names(paths)[
      tolower(tools::file_ext(paths)) !=
        expected_extensions[names(paths)]
    ]

    if (length(invalid_extensions) > 0L) {
      shiny::showNotification(
        paste(
          "Formato inválido para:",
          paste(invalid_extensions, collapse = ", ")
        ),
        type = "error"
      )

      return(invisible(NULL))
    }

    result <- tryCatch(
      import_reports(
        paths = unname(paths),
        source_names = basename(unname(paths)),
        library_path = library_path
      ),
      error = function(error) error
    )

    if (inherits(result, "error")) {
      shiny::showNotification(
        conditionMessage(result),
        type = "error",
        duration = NULL
      )

      return(invisible(NULL))
    }

    import_result(result)
    refresh_library()

    successful <- sum(
      result$status %in% c(
        "stored",
        "duplicate",
        "new_version"
      )
    )

    rejected <- sum(result$status == "rejected")

    if ("hub" %in% names(paths)) {
      hub_record <- result |>
        dplyr::filter(
          .data$source_name == basename(paths[["hub"]]),
          is.na(.data$source_archive)
        )

      valid_hub <- nrow(hub_record) == 1L &&
        identical(
          hub_record$report_type[[1L]],
          "xp_hub_statement"
        ) &&
        hub_record$status[[1L]] %in% c(
          "stored",
          "duplicate",
          "new_version"
        )

      if (!valid_hub) {
        shiny::showNotification(
          paste(
            "O PDF informado não foi reconhecido como",
            "um extrato XP Hub válido.",
            "Consulte o resultado da importação."
          ),
          type = "warning",
          duration = 10
        )
      }
    }

    if (rejected > 0L) {
      shiny::showNotification(
        paste(
          rejected,
          "arquivo(s) rejeitado(s).",
          "Consulte a tabela de importação."
        ),
        type = "warning",
        duration = 10
      )
    } else {
      shiny::showNotification(
        paste(
          "Importação concluída:",
          successful,
          "arquivo(s) identificados."
        ),
        type = "message",
        duration = 5
      )
    }
  })



  shiny::observeEvent(input$refresh_library, {
    refresh_library()
  })


  output$import_summary <- shiny::renderUI({
    result <- import_result()

    if (is.null(result)) {
      return(
        shiny::p(
          "Selecione um ZIP para iniciar a importação."
        )
      )
    }

    counts <- table(result$status)

    count_status <- function(status) {
      count <- counts[status]

      if (length(count) == 0L || is.na(count)) {
        return(0L)
      }

      as.integer(count)
    }

    shiny::tagList(
      shiny::h5("Resumo da importação"),

      shiny::tags$p(
        shiny::strong("Arquivos processados: "),
        nrow(result)
      ),

      shiny::tags$p(
        shiny::strong("Novos: "),
        count_status("stored")
      ),

      shiny::tags$p(
        shiny::strong("Novas versões: "),
        count_status("new_version")
      ),

      shiny::tags$p(
        shiny::strong("Duplicados: "),
        count_status("duplicate")
      ),

      shiny::tags$p(
        shiny::strong("Rejeitados: "),
        count_status("rejected")
      )
    )
  })


  output$import_table <- DT::renderDT({
    result <- import_result()

    if (is.null(result)) {
      return(xp_app_table(
        tibble::tibble(
          Informação = "Nenhum arquivo importado nesta sessão."
        )
      ))
    }

    result |>
      dplyr::transmute(
        Arquivo = .data$source_name,
        Tipo = xp_app_report_label(.data$report_type),
        `Data-base` = .data$reference_date,
        Situação = .data$status,
        Erro = .data$error
      ) |>
      xp_app_table()
  }, server = FALSE)


  output$dates_table <- DT::renderDT({
    reports <- saved_reports()

    if (nrow(reports) == 0L) {
      return(xp_app_table(
        tibble::tibble(
          Informação = "Biblioteca vazia."
        )
      ))
    }

    reports |>
      dplyr::group_by(.data$reference_date) |>
      dplyr::summarise(
        Relatórios = dplyr::n(),
        Tipos = dplyr::n_distinct(.data$report_type),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$reference_date)
      ) |>
      dplyr::rename(
        `Data-base` = .data$reference_date
      ) |>
      xp_app_table()
  }, server = FALSE)


  output$library_location <- shiny::renderText({
    file.path(library_path, "xp_select")
  })


  shiny::observeEvent(saved_reports(), {
    reports <- saved_reports()

    dates <- sort(
      unique(reports$reference_date),
      decreasing = TRUE
    )

    choices <- as.character(dates)

    current <- shiny::isolate(input$reference_date)

    selected <- if (
      !is.null(current) &&
      current %in% choices
    ) {
      current
    } else if (length(choices) > 0L) {
      choices[[1L]]
    } else {
      character()
    }

    shiny::updateSelectInput(
      session = session,
      inputId = "reference_date",
      choices = choices,
      selected = selected
    )
  })


  selected_reports <- shiny::reactive({
    shiny::req(input$reference_date)

    saved_reports() |>
      dplyr::filter(
        .data$reference_date ==
          as.Date(input$reference_date)
      )
  })


  output$date_status <- shiny::renderUI({
    reports <- selected_reports()

    if (nrow(reports) == 0L) {
      return(
        shiny::p("Nenhum relatório disponível.")
      )
    }

    versions <- reports |>
      dplyr::count(
        .data$report_type,
        name = "n_versions"
      )

    duplicated_types <- sum(
      versions$n_versions > 1L
    )

    shiny::tagList(
      shiny::p(
        shiny::strong("Arquivos disponíveis: "),
        nrow(reports)
      ),

      shiny::p(
        shiny::strong("Tipos de relatório: "),
        dplyr::n_distinct(reports$report_type)
      ),

      if (duplicated_types > 0L) {
        shiny::tags$p(
          class = "text-warning",
          paste(
            duplicated_types,
            "tipo(s) possuem versões diferentes.",
            "A escolha da versão será feita explicitamente",
            "na etapa de conciliação."
          )
        )
      }
    )
  })


  output$library_table <- DT::renderDT({
    reports <- selected_reports()

    reports |>
      dplyr::arrange(
        .data$report_type,
        dplyr::desc(.data$saved_at)
      ) |>
      dplyr::transmute(
        Tipo = xp_app_report_label(.data$report_type),
        Arquivo = .data$source_name,
        `Data-base` = .data$reference_date,
        Hash = substr(.data$sha256, 1L, 16L),
        `Salvo em` = format(
          .data$saved_at,
          "%d/%m/%Y %H:%M:%S"
        )
      ) |>
      xp_app_table()
  }, server = FALSE)
}


shiny::shinyApp(
  ui = ui,
  server = server
)

