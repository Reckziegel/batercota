
options(shiny.maxRequestSize = 50 * 1024^2)


reconciliation_table <- function(data) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    class = "stripe hover compact",
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      autoWidth = TRUE,
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
  )
}



reconciliation_metric <- function(label, value) {
  shiny::div(
    class = "bc-metric",
    shiny::div(class = "bc-metric-label", label),
    shiny::div(class = "bc-metric-value", value)
  )
}


local_report_path <- function(path, label) {
  if (
    is.null(path) ||
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(trimws(path))
  ) {
    stop(
      paste("Informe o caminho de:", label),
      call. = FALSE
    )
  }

  path <- trimws(path)
  path <- gsub('"', "", path, fixed = TRUE)

  if (!file.exists(path) || dir.exists(path)) {
    stop(
      paste("Arquivo não encontrado:", path),
      call. = FALSE
    )
  }

  path
}



ui <- shiny::fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body {
        background: #f7f8fa;
      }

      .container-fluid {
        max-width: 1600px;
      }

      .bc-header {
        margin: 22px 0;
      }

      .bc-subtitle {
        color: #667085;
        margin-bottom: 16px;
      }

      .bc-metrics {
        display: grid;
        grid-template-columns: repeat(
          auto-fit,
          minmax(155px, 1fr)
        );
        gap: 12px;
        margin-bottom: 20px;
      }

      .bc-metric {
        background: white;
        border: 1px solid #e4e7ec;
        border-radius: 10px;
        padding: 16px;
      }

      .bc-metric-label {
        font-size: 12px;
        color: #667085;
      }

      .bc-metric-value {
        font-size: 26px;
        font-weight: 700;
        margin-top: 5px;
      }

      .bc-note {
        color: #667085;
        font-size: 13px;
        margin: 12px 0;
      }

      .bc-panel {
        background: white;
        border: 1px solid #e4e7ec;
        border-radius: 10px;
        padding: 16px;
        margin-top: 16px;
      }

      .dataTables_wrapper {
        font-size: 12px;
      }

      table.dataTable thead th {
        white-space: nowrap;
      }
    "))
  ),
  shiny::div(
    class = "bc-header",
    shiny::h2("Bater Cota"),
    shiny::div(
      class = "bc-subtitle",
      "Conciliação de posições entre XP e Daycoval"
    )
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Relatórios"),
      shiny::selectInput(
        "fund",
        "Fundo",
        choices = c(
          "Long Term LB FIM" = "long_term",
          "Linear FIM CP RL" = "linear"
        )
      ),

      shiny::numericInput(
        "financial_tolerance",
        "Tolerância financeira por ativo (R$)",
        value = 100,
        min = 0,
        step = 10,
        width = "100%"
      ),

      shiny::textInput(
        "portfolio_path",
        "Daycoval — Carteira Diária (CSV)",
        placeholder = "Cole o caminho completo do CSV",
        width = "100%"
      ),

      shiny::textInput(
        "position_path",
        "Daycoval — Posição de Carteira C (CSV)",
        placeholder = "Cole o caminho completo do CSV",
        width = "100%"
      ),

      shiny::textInput(
        "xp_path",
        "XP — Extrato consolidado (PDF)",
        placeholder = "Cole o caminho completo do PDF",
        width = "100%"
      ),

      shiny::tags$hr(),

      shiny::textInput(
        "operations_path",
        "Operações (Excel, opcional)",
        placeholder = "Cole o caminho completo do XLSX",
        width = "100%"
      ),

      shiny::div(
        class = "bc-note",
        paste(
          "Para comparar datas, carregue primeiro a posição",
          "anterior e depois a posição mais recente do mesmo fundo.",
          "A planilha de operações permite associar movimentações."
        )
      ),
      shiny::actionButton(
        "analyze",
        "Conciliar ativos",
        class = "btn-primary",
        width = "100%"
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput("status"),
      shiny::uiOutput("metrics"),
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Posições",
          shiny::div(
            class = "bc-panel",
            shiny::p(
              "Comparação das quantidades e valores financeiros",
              "informados pelas duas fontes."
            ),
            DT::DTOutput("positions_table")
          )
        ),
        shiny::tabPanel(
          "Exceções",
          shiny::div(
            class = "bc-panel",
            shiny::p(
              "Inclui posições não associadas, inconsistências",
              "internas e exceções previamente conhecidas."
            ),
            DT::DTOutput("issues_table")
          )
        ),
        shiny::tabPanel(
          "Evolução",
          shiny::div(
            class = "bc-panel",
            shiny::uiOutput("temporal_status"),
            DT::DTOutput("changes_table")
          )
        ),
        shiny::tabPanel(
          "Operações",
          shiny::div(
            class = "bc-panel",
            shiny::p(
              "Operações associadas à variação entre as datas.",
              "Uma associação não comprova reconciliação quantitativa."
            ),
            DT::DTOutput("operations_table")
          )
        ),
        shiny::tabPanel(
          "Daycoval interno",
          shiny::div(
            class = "bc-panel",
            shiny::uiOutput("internal_summary"),
            DT::DTOutput("internal_table")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  current <- shiny::reactiveVal(NULL)
  previous <- shiny::reactiveVal(NULL)
  operations_state <- shiny::reactiveVal(NULL)
  temporal <- shiny::reactiveVal(NULL)

  read_error <- shiny::reactiveVal(NULL)
  temporal_error <- shiny::reactiveVal(NULL)

  shiny::observeEvent(
    input$analyze,
    {
      tryCatch(
        {

          portfolio_path <- local_report_path(
            input$portfolio_path,
            "Carteira Diária"
          )

          position_path <- local_report_path(
            input$position_path,
            "Posição de Carteira C"
          )

          xp_path <- local_report_path(
            input$xp_path,
            "Extrato XP"
          )

          portfolio <- batercota::read_daycoval_portfolio(
            path = portfolio_path,
            source_name = basename(portfolio_path)
          )

          position <- batercota::read_daycoval_position(
            path = position_path,
            source_name = basename(position_path)
          )

          xp <- batercota::read_xp_statement(
            path = xp_path,
            source_name = basename(xp_path)
          )


          view <- batercota:::build_reconciliation_screen(
            portfolio = portfolio,
            position = position,
            xp = xp,
            fund = input$fund,
            financial_tolerance = input$financial_tolerance
          )


          new_operations <- operations_state()

          if (nzchar(trimws(input$operations_path))) {
            operations_path <- local_report_path(
              input$operations_path,
              "Planilha de operações"
            )

            new_operations <- batercota::read_fund_operations(
              path = operations_path,
              source_name = basename(operations_path)
            )
          }


          snapshot <- list(
            fund = input$fund,
            date = view$metadata$position_date,
            position = position,
            view = view
          )

          old <- current()

          if (
            !is.null(old) &&
            identical(old$fund, snapshot$fund) &&
            old$date < snapshot$date
          ) {
            new_previous <- old
          } else if (
            !is.null(old) &&
            identical(old$fund, snapshot$fund) &&
            identical(old$date, snapshot$date)
          ) {
            new_previous <- previous()
          } else {
            new_previous <- NULL
          }

          new_temporal <- NULL
          new_temporal_error <- NULL

          if (
            !is.null(new_previous) &&
            !is.null(new_operations)
          ) {
            new_temporal <- tryCatch(
              batercota::reconcile_position_changes(
                previous = new_previous$view$external,
                current = snapshot$view$external,
                operations = new_operations,
                previous_position = new_previous$position,
                current_position = snapshot$position
              ),
              error = function(error) {
                new_temporal_error <<- conditionMessage(error)
                NULL
              }
            )
          }

          previous(new_previous)
          current(snapshot)
          operations_state(new_operations)
          temporal(new_temporal)

          read_error(NULL)
          temporal_error(new_temporal_error)
        },
        error = function(error) {
          read_error(conditionMessage(error))
        }
      )
    },
    ignoreInit = TRUE
  )

  output$status <- shiny::renderUI({
    message <- read_error()

    if (!is.null(message)) {
      return(
        shiny::div(
          class = "alert alert-danger",
          message
        )
      )
    }

    snapshot <- current()

    if (is.null(snapshot)) {
      return(
        shiny::div(
          class = "alert alert-info",
          "Selecione o fundo e carregue os relatórios para começar."
        )
      )
    }

    shiny::div(
      class = "alert alert-light",
      shiny::strong(snapshot$view$metadata$fund_name),
      " — Data-base: ",
      format(snapshot$date, "%d/%m/%Y")
    )
  })


  output$metrics <- shiny::renderUI({
    snapshot <- current()
    shiny::req(snapshot)

    positions <- snapshot$view$positions
    summary <- snapshot$view$external$summary

    financial_status <- positions$status

    shiny::div(
      class = "bc-metrics",
      reconciliation_metric(
        "Ativos associados",
        nrow(positions)
      ),
      reconciliation_metric(
        "Conformes",
        sum(financial_status %in% c("OK", "Tolerada"))
      ),
      reconciliation_metric(
        "Diferenças toleradas",
        sum(financial_status == "Tolerada")
      ),
      reconciliation_metric(
        "Divergentes",
        sum(financial_status == "Divergente")
      ),
      reconciliation_metric(
        "Não associados",
        summary$unmatched_xp[[1L]] +
          summary$unmatched_daycoval[[1L]]
      ),
      reconciliation_metric(
        "Exceções conhecidas",
        summary$expected_daycoval_only[[1L]]
      ),
      reconciliation_metric(
        "Pendências internas",
        nrow(snapshot$view$internal$issues)
      )
    )
  })


  output$positions_table <- DT::renderDT({
    snapshot <- current()
    shiny::req(snapshot)

    data <- snapshot$view$positions |>
      dplyr::transmute(
        Ativo = .data$xp_asset_name,
        Identificador = .data$daycoval_title,
        `Qtd XP` = .data$xp_quantity,
        `Qtd Daycoval` = .data$daycoval_quantity,
        `Dif. cotas` = .data$quantity_difference,
        `Valor XP` = .data$xp_market_value,
        `Valor Daycoval` = .data$daycoval_market_value,
        `Dif. financeira` = .data$market_value_difference,
        Situação = .data$status
      )

    reconciliation_table(data)
  })

  output$issues_table <- DT::renderDT({
    snapshot <- current()
    shiny::req(snapshot)

    data <- snapshot$view$issues |>
      dplyr::transmute(
        Origem = .data$origin,
        Tipo = .data$issue_type,
        Ativo = .data$asset_identifier,
        Arquivo = .data$source_file,
        Linha = .data$source_line
      )

    reconciliation_table(data)
  })

  output$internal_summary <- shiny::renderUI({
    snapshot <- current()
    shiny::req(snapshot)

    summary <- snapshot$view$internal$summary

    shiny::p(
      "Ativos associados: ", summary$matched[[1L]],
      " | Iguais: ", summary$matched_equal[[1L]],
      " | Diferentes: ", summary$matched_different[[1L]],
      " | Pendências: ", nrow(snapshot$view$internal$issues)
    )
  })

  output$internal_table <- DT::renderDT({
    snapshot <- current()
    shiny::req(snapshot)

    data <- snapshot$view$internal$checks |>
      dplyr::transmute(
        Associação = .data$match_id,
        Campo = .data$field,
        Daycoval_Carteira = .data$portfolio_value,
        Daycoval_Posicao_C = .data$position_value,
        Diferença = .data$difference,
        Situação = .data$status
      )

    reconciliation_table(data)
  })

  output$temporal_status <- shiny::renderUI({
    snapshot <- current()
    shiny::req(snapshot)

    if (is.null(previous())) {
      return(shiny::p(
        "Carregue uma data posterior do mesmo fundo",
        "para visualizar a evolução das posições."
      ))
    }

    if (!is.null(temporal_error())) {
      return(
        shiny::div(
          class = "alert alert-warning",
          temporal_error()
        )
      )
    }

    if (is.null(operations_state())) {
      return(shiny::p(
        "A comparação temporal está disponível quando",
        "a planilha de operações for carregada."
      ))
    }

    shiny::p(
      "Período: ",
      format(previous()$date, "%d/%m/%Y"),
      " até ",
      format(snapshot$date, "%d/%m/%Y")
    )
  })

  output$changes_table <- DT::renderDT({
    result <- temporal()

    shiny::req(result)

    data <- result$changes |>
      dplyr::transmute(
        Identificador = .data$daycoval_title,
        Situação = dplyr::recode(
          .data$change_type,
          held = "Mantido",
          entered = "Entrada",
          exited = "Saída",
          unverified_entry = "Entrada não confirmada",
          unverified_exit = "Saída não confirmada"
        ),
        `Var. cotas XP` = .data$xp_quantity_change,
        `Var. cotas Daycoval` =
          .data$daycoval_quantity_change,
        `Var. financeira XP` =
          .data$xp_market_value_change,
        `Var. financeira Daycoval` =
          .data$daycoval_market_value_change,
        Operações = .data$n_conversions,
        Avaliação = .data$operation_status
      )

    reconciliation_table(data)
  })

  output$operations_table <- DT::renderDT({
    result <- temporal()

    shiny::req(result)

    data <- result$links |>
      dplyr::transmute(
        Identificador = .data$daycoval_title,
        Operação = .data$operation_type,
        Solicitação = .data$operation_date,
        Cotização = .data$conversion_date,
        Liquidação = .data$liquidation_date,
        Valor = .data$value,
        Quantidade = .data$quantity,
        `Quantidade assinada` = .data$signed_quantity,
        Arquivo = .data$source_file,
        Linha = .data$source_row
      )

    reconciliation_table(data)
  })
}


shiny::shinyApp(
  ui = ui,
  server = server
)

