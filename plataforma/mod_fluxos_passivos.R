
# plataforma/mod_fluxos_passivos.R

source_files <- vapply(
  sys.frames(),
  function(frame) {
    if (is.null(frame$ofile)) {
      return(NA_character_)
    }

    as.character(frame$ofile)
  },
  character(1)
)

source_files <- source_files[!is.na(source_files)]

module_file <- source_files[
  basename(source_files) == "mod_fluxos_passivos.R"
]

if (length(module_file) == 0L) {
  stop(
    "Não foi possível identificar o caminho do módulo.",
    call. = FALSE
  )
}

module_dir <- dirname(
  normalizePath(
    utils::tail(module_file, 1L),
    winslash = "/",
    mustWork = TRUE
  )
)

source(
  file.path(module_dir, "report_library_shiny.R"),
  local = TRUE
)

diagnostic_path <- file.path(
  module_dir,
  "mod_fluxos_passivos_diagnostico.R"
)

if (!file.exists(diagnostic_path)) {
  stop(
    "Arquivo do módulo de diagnóstico não encontrado: ",
    diagnostic_path,
    call. = FALSE
  )
}

source(diagnostic_path, local = TRUE)



bc_fluxos_report_path <- function(path, label, extension) {
  valid_path <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(trimws(path))

  if (!valid_path) {
    cli::cli_abort("Informe o caminho de: {label}.")
  }

  path <- gsub('"', "", trimws(path), fixed = TRUE)

  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("Arquivo não encontrado: {.path {path}}.")
  }

  if (tolower(tools::file_ext(path)) != extension) {
    cli::cli_abort("Formato inválido para {label}.")
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}


bc_fluxos_default_cash_plan_path <- function(
    fund,
    directory = module_dir
) {
  filenames <- c(
    linear = "fundos - Movimentações Previstas LINEAR.xlsx",
    long_term = "fundos - Movimentações Previstas Long Term LB.xlsx",
    reta_alocacao = "fundos - Movimentações Previstas Reta Alocação.xlsx"
  )

  if (
    !is.character(fund) ||
    length(fund) != 1L ||
    is.na(fund) ||
    !fund %in% names(filenames)
  ) {
    return("")
  }

  file.path(directory, unname(filenames[[fund]]))
}



bc_fluxos_validate_reports <- function(
    cash_plan,
    daily_movement,
    cash_statement,
    opening_position,
    fund
) {
  reports <- list(
    cash_plan = cash_plan,
    daily_movement = daily_movement,
    cash_statement = cash_statement,
    opening_position = opening_position
  )

  expected_classes <- c(
    "fund_cash_plan",
    "daycoval_daily_movement",
    "daycoval_cash_statement",
    "daycoval_position"
  )

  valid_classes <- vapply(
    seq_along(reports),
    function(i) inherits(reports[[i]], expected_classes[[i]]),
    logical(1)
  )

  if (!all(valid_classes)) {
    cli::cli_abort("Os documentos possuem classes incompatíveis.")
  }

  reference <- batercota:::batercota_fund_reference(fund)

  portfolio_codes <- c(
    daily_movement$metadata$codigo_carteira,
    cash_statement$metadata$codigo_carteira,
    opening_position$metadata$codigo_carteira
  )

  if (
    length(portfolio_codes) != 3L ||
    anyNA(portfolio_codes) ||
    !all(as.character(portfolio_codes) == reference$portfolio_code)
  ) {
    cli::cli_abort(
      "Os relatórios Daycoval não pertencem ao fundo selecionado."
    )
  }

  expected_fund_code <- substr(
    gsub("[^0-9]", "", opening_position$metadata$cnpj),
    1L,
    8L
  )

  internal_codes <- unique(
    cash_plan$shareholder_operations$fund_code[
      !is.na(cash_plan$shareholder_operations$fund_code)
    ]
  )

  if (
    length(internal_codes) != 1L ||
    !identical(internal_codes, expected_fund_code)
  ) {
    cli::cli_abort(
      "A planilha da gestora não corresponde ao fundo selecionado."
    )
  }

  opening_date <- opening_position$metadata$data_posicao
  ending_date <- daily_movement$metadata$data_final

  valid_dates <- inherits(opening_date, "Date") &&
    inherits(ending_date, "Date") &&
    length(opening_date) == 1L &&
    length(ending_date) == 1L &&
    !is.na(opening_date) &&
    !is.na(ending_date) &&
    opening_date < ending_date

  if (!valid_dates) {
    cli::cli_abort(
      "A Posição C inicial deve ser anterior ao fim do Movimento Diário."
    )
  }

  cash_opening <- cash_statement$metadata$opening_date
  cash_ending <- cash_statement$metadata$closing_date

  if (
    anyNA(c(cash_opening, cash_ending)) ||
    cash_opening > opening_date ||
    cash_ending < ending_date
  ) {
    cli::cli_abort(
      "O Demonstrativo de Caixa não cobre o período analisado."
    )
  }

  list(
    fund = fund,
    fund_name = reference$name,
    portfolio_code = reference$portfolio_code,
    opening_date = opening_date,
    ending_date = ending_date,
    cash_plan = cash_plan,
    daily_movement = daily_movement,
    cash_statement = cash_statement,
    opening_position = opening_position
  )
}


bc_fluxos_available_checkpoints <- function(
    reports,
    current_reports
) {
  if (
    is.null(current_reports) ||
    !identical(current_reports$fund, reports$fund) ||
    !inherits(current_reports$position, "daycoval_position")
  ) {
    return(list())
  }

  position <- current_reports$position
  position_date <- position$metadata$data_posicao
  position_code <- as.character(
    position$metadata$codigo_carteira
  )

  valid_position <- inherits(position_date, "Date") &&
    length(position_date) == 1L &&
    !is.na(position_date) &&
    identical(position_code, reports$portfolio_code) &&
    position_date > reports$opening_date &&
    position_date <= reports$ending_date

  if (!valid_position) {
    return(list())
  }

  list(position)
}


bc_fluxos_money <- function(value) {
  if (
    !is.numeric(value) ||
    length(value) != 1L ||
    !is.finite(value)
  ) {
    return("Não disponível")
  }

  formatter <- scales::label_dollar(
    accuracy = 0.01,
    prefix = "R$ ",
    big.mark = ".",
    decimal.mark = ","
  )

  formatter(value)
}


bc_fluxos_status <- function(values, labels) {
  result <- unname(labels[values])
  missing <- is.na(result) & !is.na(values)

  result[missing] <- values[missing]

  result
}


bc_fluxos_daily_table <- function(result) {
  payment_labels <- c(
    verified = "Confirmado",
    no_settlement = "Sem pagamento",
    requires_review = "Revisar"
  )

  checkpoint_labels <- c(
    consistent = "Consistente",
    not_checked = "Sem Posição C",
    requires_review = "Revisar"
  )

  manager_labels <- c(
    within_tolerance = "Dentro da tolerância",
    requires_review = "Revisar",
    incomplete_internal = "Dados incompletos"
  )

  data <- result$timeline |>
    dplyr::transmute(
      Data = format(.data$date, "%d/%m/%Y"),
      `Passivo inicial` = .data$daycoval_opening,
      `Novas cotizações` = .data$daycoval_conversions,
      `Cotizações gestora` = .data$manager_conversions,
      `Baixas contábeis` = .data$daycoval_settlements,
      `Pagamentos confirmados` =
        .data$verified_cash_settlements,
      `Pagamentos previstos` =
        .data$manager_scheduled_payments,
      `Passivo reconstruído` = .data$daycoval_closing,
      `Projeção ancorada` =
        .data$manager_anchored_closing,
      Diferença = .data$closing_difference,
      `Posição C reportada` = .data$reported_closing,
      `Conferência contábil` = bc_fluxos_status(
        .data$checkpoint_status,
        checkpoint_labels
      ),
      `Confirmação financeira` = bc_fluxos_status(
        .data$payment_status,
        payment_labels
      ),
      `Conciliação gestora` = bc_fluxos_status(
        .data$manager_status,
        manager_labels
      )
    )

  money_columns <- names(data)[vapply(
    data,
    is.numeric,
    logical(1)
  )]

  DT::datatable(
    data,
    rownames = FALSE,
    selection = "single",
    class = "stripe hover compact",
    options = list(
      pageLength = 10L,
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
          "next" = "Próximo"
        )
      )
    )
  ) |>
    DT::formatCurrency(
      columns = money_columns,
      currency = "R$ ",
      digits = 2,
      mark = ".",
      dec.mark = ","
    )
}


bc_fluxos_issues_table <- function(issues) {
  data <- issues |>
    dplyr::transmute(
      Data = format(.data$date, "%d/%m/%Y"),
      Pendência = .data$issue_type,
      Referência = .data$reference,
      Arquivo = .data$source_file,
      Linha = .data$source_line
    )

  DT::datatable(
    data,
    rownames = FALSE,
    class = "stripe hover compact",
    options = list(
      pageLength = 10L,
      scrollX = TRUE,
      order = list(),
      language = list(
        search = "Buscar:",
        zeroRecords = "Nenhuma pendência encontrada",
        emptyTable = "Nenhuma pendência encontrada"
      )
    )
  )
}


mod_fluxos_passivos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),
    shiny::h3("Fluxos e Passivos"),

    shiny::p(
      "Conciliação das cotizações, pagamentos e",
      "obrigações do fundo perante os cotistas."
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::uiOutput(ns("fund_info")),

        shiny::textInput(
          ns("cash_plan_path"),
          "Gestora - Movimentações Previstas (XLSX)",
          placeholder = "Selecionado automaticamente pelo fundo"
        )
        ,
        bc_library_picker_ui(
          "cash_plan_path",
          ns = ns
        ),

        shiny::textInput(
          ns("daily_movement_path"),
          "Daycoval - Movimento Diário (CSV)",
          placeholder = "Cole o caminho completo"
        ),
        bc_library_picker_ui(
          "daily_movement_path",
          ns = ns
        ),

        shiny::textInput(
          ns("cash_statement_path"),
          "Daycoval - Demonstrativo de Caixa (CSV)",
          placeholder = "Cole o caminho completo"
        ),
        bc_library_picker_ui(
          "cash_statement_path",
          ns = ns
        ),


        shiny::hr(),

        shiny::checkboxInput(
          ns("reuse_position"),
          "Reutilizar a Posição C da página Ativos",
          value = FALSE
        ),

        shiny::conditionalPanel(
          condition = "!input.reuse_position",
          ns = ns,

          shiny::textInput(
            ns("opening_position_path"),
            "Daycoval - Posição C inicial (CSV)",
            placeholder = "Cole o caminho completo"
          ),
          bc_library_picker_ui(
            "opening_position_path",
            ns = ns
          )
        ),



        shiny::uiOutput(ns("reuse_info")),

        shiny::actionButton(
          ns("import_reports"),
          "Importar e conciliar passivo",
          class = "btn-primary",
          width = "100%"
        ),

        shiny::br(),
        shiny::br(),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "A Posição C inicial fornece o saldo de",
            "fechamento utilizado como âncora.",
            "Movimentos dessa data não são reaplicados."
          )
        )
      ),

      shiny::mainPanel(
        width = 9,

        shiny::uiOutput(ns("status")),
        shiny::uiOutput(ns("summary")),

        shiny::h4("Evolução diária do passivo"),
        DT::DTOutput(ns("daily_table")),

        shiny::br(),

        shiny::tags$p(
          class = "text-muted",
          style = "font-size: 12px;",
          paste(
            "Passivo reconstruído: evolução contábil",
            "a partir da Posição C inicial.",
            "Projeção ancorada: evolução prevista pela",
            "gestora sobre o mesmo saldo inicial.",
            "Campos sem valor não representam zero."
          )
        ),

        shiny::br(),
        mod_fluxos_diagnostico_ui(ns("diagnostico")),
        shiny::br(),

        shiny::h4("Pendências para investigação"),
        shiny::uiOutput(ns("issues_summary")),
        DT::DTOutput(ns("issues_table"))
      )
    )
  )
}



mod_fluxos_passivos_server <- function(
    id,
    selected_fund,
    current_reports,
    library_revision = shiny::reactiveVal(0L),
    library_enabled = FALSE
) {

  shiny::moduleServer(id, function(input, output, session) {
    imported <- shiny::reactiveVal(NULL)
    liability_timeline <- shiny::reactiveVal(NULL)
    import_error <- shiny::reactiveVal(NULL)


    shiny::observeEvent(selected_fund(), {
      fund <- selected_fund()

      path <- bc_fluxos_default_cash_plan_path(
        fund = fund
      )

      shiny::updateTextInput(
        session = session,
        inputId = "cash_plan_path",
        value = path
      )
    })


    if (isTRUE(library_enabled)) {
      bc_library_bind_pickers(
        input = input,
        session = session,
        selected_fund = selected_fund,
        revision = library_revision,
        specs = c(
          cash_plan_path = "fund_cash_plan",
          daily_movement_path = "daycoval_daily_movement",
          cash_statement_path = "daycoval_cash_statement",
          opening_position_path = "daycoval_position"
        )
      )
    }

    diagnostic <- mod_fluxos_diagnostico_server(
      id = "diagnostico",
      reconciliation = shiny::reactive(liability_timeline()),
      reports = shiny::reactive(imported()),
      selected_row = shiny::reactive(
        input$daily_table_rows_selected
      )
    )

    shiny::observeEvent(
      list(
        selected_fund(),
        input$cash_plan_path,
        input$daily_movement_path,
        input$cash_statement_path,
        input$opening_position_path,
        input$reuse_position,
        current_reports()
      ),
      {
        imported(NULL)
        liability_timeline(NULL)
        import_error(NULL)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$import_reports, {
      imported(NULL)
      liability_timeline(NULL)
      import_error(NULL)

      tryCatch({
          fund <- selected_fund()

          cash_plan_path <- bc_fluxos_report_path(
            input$cash_plan_path,
            "Movimentações Previstas",
            "xlsx"
          )

          movement_path <- bc_fluxos_report_path(
            input$daily_movement_path,
            "Movimento Diário",
            "csv"
          )

          cash_path <- bc_fluxos_report_path(
            input$cash_statement_path,
            "Demonstrativo de Caixa",
            "csv"
          )

          cash_plan <- bc_library_read(
            path = cash_plan_path,
            report_type = "fund_cash_plan"
          )

          daily_movement <- bc_library_read(
            path = movement_path,
            report_type = "daycoval_daily_movement"
          )

          cash_statement <- bc_library_read(
            path = cash_path,
            report_type = "daycoval_cash_statement"
          )

          position_path <- NULL

          if (isTRUE(input$reuse_position)) {
            available <- current_reports()

            if (
              is.null(available) ||
              !identical(available$fund, fund) ||
              !inherits(available$position, "daycoval_position")
            ) {
              cli::cli_abort(
                "Não existe uma Posição C compatível na página Ativos."
              )
            }

            opening_position <- available$position
          } else {
            position_path <- bc_fluxos_report_path(
              input$opening_position_path,
              "Posição C inicial",
              "csv"
            )

            opening_position <- bc_library_read(
              path = position_path,
              report_type = "daycoval_position"
            )
          }

          reports <- bc_fluxos_validate_reports(
            cash_plan = cash_plan,
            daily_movement = daily_movement,
            cash_statement = cash_statement,
            opening_position = opening_position,
            fund = fund
          )

          imported(reports)

          checkpoints <- bc_fluxos_available_checkpoints(
            reports,
            current_reports()
          )

          result <- batercota::build_shareholder_liability_timeline(
            cash_plan = reports$cash_plan,
            daily_movement = reports$daily_movement,
            cash_statement = reports$cash_statement,
            opening_position = reports$opening_position,
            positions = checkpoints
          )

          liability_timeline(result)

          if (isTRUE(library_enabled)) {
            bc_library_register_imports(
              paths = c(
                cash_plan_path,
                movement_path,
                cash_path,
                position_path
              ),
              revision = library_revision
            )
          }
        },
        error = function(error) {
          import_error(conditionMessage(error))
        }
      )
    })

    output$fund_info <- shiny::renderUI({
      fund <- selected_fund()
      shiny::req(fund)

      reference <- batercota:::batercota_fund_reference(fund)

      shiny::tags$p(
        shiny::strong("Fundo selecionado: "),
        reference$name,
        shiny::tags$br(),
        shiny::tags$small(
          class = "text-muted",
          paste("Carteira Daycoval:", reference$portfolio_code)
        )
      )
    })

    output$reuse_info <- shiny::renderUI({
      if (!isTRUE(input$reuse_position)) {
        return(NULL)
      }

      available <- current_reports()

      if (
        is.null(available) ||
        !identical(available$fund, selected_fund()) ||
        !inherits(available$position, "daycoval_position")
      ) {
        return(shiny::div(
          class = "alert alert-warning",
          "Não há uma Posição C disponível para este fundo."
        ))
      }

      position_date <- available$position$metadata$data_posicao

      shiny::tags$p(
        class = "text-muted",
        "Posição disponível: ",
        format(position_date, "%d/%m/%Y")
      )
    })

    output$status <- shiny::renderUI({
      error <- import_error()

      if (!is.null(error)) {
        return(shiny::div(
          class = "alert alert-danger",
          error
        ))
      }

      result <- liability_timeline()

      if (!is.null(result)) {
        return(shiny::div(
          class = "alert alert-success",
          "Relatórios importados e passivo reconstruído."
        ))
      }

      if (!is.null(imported())) {
        return(shiny::div(
          class = "alert alert-warning",
          "Documentos importados, mas sem conciliação disponível."
        ))
      }

      shiny::div(
        class = "alert alert-info",
        "Informe os documentos para iniciar a conciliação."
      )
    })

    output$summary <- shiny::renderUI({
      result <- liability_timeline()
      shiny::req(result)

      metadata <- result$metadata
      timeline <- result$timeline

      latest <- timeline[nrow(timeline), , drop = FALSE]

      confirmed_payments <- sum(
        timeline$verified_cash_settlements
      )

      n_checkpoints <- sum(
        !is.na(timeline$reported_closing)
      )

      shiny::tagList(
        shiny::h4("Resumo do passivo"),

        shiny::tags$p(
          shiny::strong("Carteira: "),
          metadata$codigo_carteira,
          " | Período: ",
          format(metadata$opening_date, "%d/%m/%Y"),
          " a ",
          format(metadata$ending_date, "%d/%m/%Y")
        ),

        shiny::tags$p(
          shiny::strong("Passivo reconstruído: "),
          bc_fluxos_money(latest$daycoval_closing),
          "  |  ",
          shiny::strong("Projeção ancorada da gestora: "),
          bc_fluxos_money(latest$manager_anchored_closing)
        ),

        shiny::tags$p(
          shiny::strong("Diferença financeira: "),
          bc_fluxos_money(latest$closing_difference),
          "  |  ",
          shiny::strong("Pagamentos confirmados no período: "),
          bc_fluxos_money(confirmed_payments)
        ),

        shiny::tags$p(
          shiny::strong("Posição C de conferência: "),
          n_checkpoints,
          " data(s)",
          "  |  ",
          shiny::strong("Pendências: "),
          nrow(result$issues)
        ),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "O passivo reconstruído é um saldo calculado",
            "a partir dos lançamentos contábeis.",
            "Sua confirmação independente requer uma",
            "Posição C de conferência."
          )
        ),

        shiny::tags$hr()
      )
    })

    output$daily_table <- DT::renderDT({
      result <- liability_timeline()
      shiny::req(result)

      bc_fluxos_daily_table(result)
    }, server = FALSE)

    output$issues_summary <- shiny::renderUI({
      result <- liability_timeline()
      shiny::req(result)

      if (nrow(result$issues) == 0L) {
        return(shiny::tags$p(
          class = "text-muted",
          "Nenhuma pendência registrada."
        ))
      }

      shiny::tags$p(
        nrow(result$issues),
        "registro(s) para investigação.",
        "Diferenças entre estimativas não representam",
        "necessariamente erros operacionais."
      )
    })

    output$issues_table <- DT::renderDT({
      result <- liability_timeline()
      shiny::req(result)

      bc_fluxos_issues_table(result$issues)
    }, server = FALSE)

    shiny::reactive({
      list(
        reports = imported(),
        reconciliation = liability_timeline(),
        error = import_error()
      )
    })
  })
}

