
# Bater Cota - Shiny application
#
# Run from the project root:
# devtools::load_all(".")
# shiny::runApp("plataforma/app_reconciliation.R")

bc_report_path <- function(path, label, extensions) {
  if (
    !is.character(path) ||
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(trimws(path))
  ) {
    stop(
      paste("Informe o caminho de:", label),
      call. = FALSE
    )
  }

  path <- gsub('"', "", trimws(path), fixed = TRUE)

  if (!file.exists(path) || dir.exists(path)) {
    stop(
      paste("Arquivo não encontrado:", path),
      call. = FALSE
    )
  }

  if (!(tolower(tools::file_ext(path)) %in% extensions)) {
    stop(
      paste("Formato de arquivo inválido:", label),
      call. = FALSE
    )
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}


bc_money <- scales::label_dollar(
  accuracy = 0.01,
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

bc_percent <- scales::label_number(
  accuracy = 0.000001,
  big.mark = ".",
  decimal.mark = ","
)


bc_table <- function(data, page_length = 20L) {
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
          "next" = "Próximo"
        )
      )
    )
  )
}


bc_positions_table <- function(positions) {
  data <- positions |>
    dplyr::transmute(
      Fundo = .data$xp_asset_name,
      Identificador = .data$daycoval_title,
      `Cotas XP` = .data$xp_quantity,
      `Cotas Daycoval` = .data$daycoval_quantity,
      `Dif. cotas` = .data$quantity_difference,
      `Financeiro XP` = .data$xp_market_value,
      `Financeiro Daycoval` = .data$daycoval_market_value,
      `Dif. financeira` = .data$market_value_difference,
      Situação = .data$status
    )

  bc_table(data, page_length = 25L) |>
    DT::formatRound(
      columns = c(
        "Cotas XP",
        "Cotas Daycoval",
        "Dif. cotas"
      ),
      digits = 4,
      mark = ".",
      dec.mark = ","
    ) |>
    DT::formatCurrency(
      columns = c(
        "Financeiro XP",
        "Financeiro Daycoval",
        "Dif. financeira"
      ),
      currency = "R$ ",
      digits = 2,
      mark = ".",
      dec.mark = ","
    )
}


bc_component_name <- function(x) {
  labels <- c(
    funds = "Fundos investidos",
    fixed_income = "Renda fixa",
    equities = "Ações, ETFs e BDRs",
    futures = "Futuros",
    accounting_expenses = "Despesas contábeis"
  )

  result <- unname(labels[x])
  result[is.na(result)] <- x[is.na(result)]

  result
}


bc_return_components_table <- function(components) {
  data <- components |>
    dplyr::transmute(
      Componente = bc_component_name(.data$component),
      `Resultado (R$)` = .data$estimated_result,
      `Contribuição (p.p.)` = .data$contribution_pp,
      `Posições/contas` = .data$n_positions,
      `Não estimadas` = .data$n_unestimated
    )

  bc_table(data, page_length = 10L) |>
    DT::formatCurrency(
      columns = "Resultado (R$)",
      currency = "R$ ",
      digits = 2,
      mark = ".",
      dec.mark = ","
    ) |>
    DT::formatRound(
      columns = "Contribuição (p.p.)",
      digits = 6,
      mark = ".",
      dec.mark = ","
    )
}


bc_return_detail_rows <- function(reconciliation) {
  instruments <- reconciliation$details$instruments$details
  expenses <- reconciliation$details$accounting_expenses$effects

  instrument_rows <- function(data, component, name_column) {
    tibble::tibble(
      Classe = component,
      Ativo = data[[name_column]],
      `Resultado (R$)` = data$estimated_result,
      `Contribuição (p.p.)` = data$contribution_pp,
      Situação = data$status
    )
  }

  previous_nav <- reconciliation$summary$previous_nav[[1L]]

  dplyr::bind_rows(
    instrument_rows(
      instruments$funds$contributions,
      "Fundos investidos",
      "title"
    ),
    instrument_rows(
      instruments$fixed_income$contributions,
      "Renda fixa",
      "title"
    ),
    instrument_rows(
      instruments$equities$contributions,
      "Ações, ETFs e BDRs",
      "title"
    ),
    instrument_rows(
      instruments$futures$contributions,
      "Futuros",
      "contract"
    ),
    tibble::tibble(
      Classe = "Despesas contábeis",
      Ativo = dplyr::coalesce(
        expenses$group_name_current,
        expenses$group_name_previous
      ),
      `Resultado (R$)` = expenses$estimated_effect,
      `Contribuição (p.p.)` =
        100 * expenses$estimated_effect / previous_nav,
      Situação = expenses$status
    )
  )
}


bc_return_details_table <- function(reconciliation) {
  data <- bc_return_detail_rows(reconciliation)

  bc_table(data, page_length = 25L) |>
    DT::formatCurrency(
      columns = "Resultado (R$)",
      currency = "R$ ",
      digits = 2,
      mark = ".",
      dec.mark = ","
    ) |>
    DT::formatRound(
      columns = "Contribuição (p.p.)",
      digits = 6,
      mark = ".",
      dec.mark = ","
    )
}


bc_return_issues_table <- function(reconciliation, flows) {
  issues <- dplyr::bind_rows(
    reconciliation$issues,
    flows$issues |>
      dplyr::transmute(
        component = "shareholders",
        issue_type = .data$issue_type,
        identifier = NA_character_,
        title = "Movimentação de cotistas"
      )
  )

  data <- issues |>
    dplyr::transmute(
      Componente = .data$component,
      Pendência = .data$issue_type,
      Identificador = .data$identifier,
      Descrição = .data$title
    )

  bc_table(data, page_length = 25L)
}


ui <- shiny::navbarPage(
  title = "Bater Cota",
  id = "page",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  shiny::tabPanel(
    "Ativos",
    shiny::br(),
    shiny::h3("Conciliação de ativos"),
    shiny::p(
      "Comparação das posições financeiras entre XP e Daycoval."
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::selectInput(
          "fund",
          "Fundo",
          choices = c(
            "Long Term LB FIM" = "long_term",
            "Linear FIM CP RL" = "linear",
            "Reta Alocação FIF" = "reta_alocacao"
          )
        ),

        shiny::textInput(
          "portfolio_path",
          "Daycoval - Carteira Diária (CSV)",
          placeholder = "Cole o caminho completo"
        ),

        shiny::textInput(
          "position_path",
          "Daycoval - Posição C (CSV)",
          placeholder = "Cole o caminho completo"
        ),

        shiny::textInput(
          "xp_path",
          "XP - Extrato (PDF)",
          placeholder = "Cole o caminho completo"
        ),

        shiny::numericInput(
          "financial_tolerance",
          "Tolerância por ativo (R$)",
          value = 100,
          min = 0,
          step = 10
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
        shiny::uiOutput("summary"),

        DT::DTOutput("positions_table"),

        shiny::br(),

        shiny::uiOutput("issues"),

        shiny::p(
          class = "text-muted",
          style = "font-size: 12px;",
          paste(
            "Quantidades exibidas com quatro casas decimais.",
            "Valores financeiros exibidos em reais.",
            "Os valores originais permanecem preservados."
          )
        )
      )
    )
  ),

  shiny::tabPanel(
    "Variação da cota",
    shiny::br(),
    shiny::h3("Conciliação da variação da cota"),
    shiny::p(
      "Compara a rentabilidade estimada pelos componentes",
      "da carteira com a variação divulgada pelo Daycoval."
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::uiOutput("return_current_source"),

        shiny::hr(),

        shiny::h5("Relatórios anteriores"),

        shiny::textInput(
          "previous_portfolio_path",
          "Daycoval - Carteira Diária anterior (CSV)",
          placeholder = "Cole o caminho completo"
        ),

        shiny::textInput(
          "previous_position_path",
          "Daycoval - Posição C anterior (CSV)",
          placeholder = "Cole o caminho completo"
        ),

        shiny::numericInput(
          "return_tolerance",
          "Tolerância da rentabilidade (p.p.)",
          value = 0.05,
          min = 0,
          step = 0.01
        ),

        shiny::actionButton(
          "analyze_return",
          "Conciliar cota",
          class = "btn-primary",
          width = "100%"
        ),

        shiny::br(),
        shiny::br(),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "A conferência pressupõe que os relatórios",
            "selecionados representem o intervalo desejado.",
            "A sequência das datas é responsabilidade do usuário."
          )
        )
      ),

      shiny::mainPanel(
        width = 9,

        shiny::uiOutput("return_status"),
        shiny::uiOutput("return_summary"),

        shiny::h4("Contribuição por componente"),
        DT::DTOutput("return_components_table"),

        shiny::br(),

        shiny::h4("Detalhamento por ativo e conta"),
        DT::DTOutput("return_details_table"),

        shiny::br(),

        shiny::h4("Movimentação de cotistas"),
        shiny::uiOutput("return_flows"),

        shiny::br(),

        shiny::h4("Pendências para análise"),
        shiny::uiOutput("return_issues_summary"),
        DT::DTOutput("return_issues_table"),

        shiny::br(),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "Os resultados são aproximações.",
            "Movimentações não explicadas, proventos,",
            "operações intradiárias e ajustes contábeis",
            "podem afetar a reconciliação.",
            "Uma diferença dentro da tolerância não",
            "implica conciliação econômica integral."
          )
        )
      )
    )
  ),

  shiny::tabPanel(
    "Aplicações e resgates",
    shiny::br(),
    shiny::h3("Aplicações e resgates"),
    shiny::p(
      "Em desenvolvimento.",
      "Esta página conciliará as movimentações,",
      "as datas de cotização e as liquidações financeiras."
    )
  )
)


server <- function(input, output, session) {
  analysis <- shiny::reactiveVal(NULL)
  read_error <- shiny::reactiveVal(NULL)

  # Shared current-date reports.
  current_reports <- shiny::reactiveVal(NULL)

  # Return reconciliation is independent of the assets screen.
  return_analysis <- shiny::reactiveVal(NULL)
  return_error <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$analyze, {
    analysis(NULL)
    read_error(NULL)

    current_reports(NULL)
    return_analysis(NULL)
    return_error(NULL)

    tryCatch(
      {
        portfolio_path <- bc_report_path(
          input$portfolio_path,
          "Carteira Diária",
          "csv"
        )

        position_path <- bc_report_path(
          input$position_path,
          "Posição C",
          "csv"
        )

        xp_path <- bc_report_path(
          input$xp_path,
          "Extrato XP",
          "pdf"
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

        current_reports(list(
          fund = input$fund,
          portfolio = portfolio,
          position = position
        ))

        analysis(view)
      },
      error = function(error) {
        read_error(conditionMessage(error))
      }
    )
  })

  # Existing assets screen.

  output$status <- shiny::renderUI({
    error <- read_error()

    if (!is.null(error)) {
      return(shiny::div(
        class = "alert alert-danger",
        error
      ))
    }

    view <- analysis()

    if (is.null(view)) {
      return(shiny::p(
        class = "text-muted",
        "Informe os três relatórios para iniciar."
      ))
    }

    shiny::tags$p(
      shiny::strong(view$metadata$fund_name),
      " | Data-base: ",
      format(view$metadata$position_date, "%d/%m/%Y")
    )
  })

  output$summary <- shiny::renderUI({
    view <- analysis()
    shiny::req(view)

    integer_label <- scales::label_number(
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ","
    )

    statuses <- view$positions$status
    summary <- view$external$summary

    conforming <- sum(statuses %in% c("OK", "Tolerada"))
    divergent <- sum(statuses == "Divergente")
    not_evaluated <- sum(statuses == "Não avaliado")

    unmatched <- summary$unmatched_xp[[1L]] +
      summary$unmatched_daycoval[[1L]]

    shiny::tags$p(
      shiny::strong("Ativos: "),
      integer_label(nrow(view$positions)),
      "  |  ",
      shiny::strong("Conformes: "),
      integer_label(conforming),
      "  |  ",
      shiny::strong("Divergentes: "),
      integer_label(divergent),
      "  |  ",
      shiny::strong("Não associados: "),
      integer_label(unmatched),
      "  |  ",
      shiny::strong("Não avaliados: "),
      integer_label(not_evaluated),
      shiny::tags$br(),
      shiny::tags$small(
        class = "text-muted",
        "Tolerância financeira por ativo: ",
        bc_money(view$metadata$financial_tolerance)
      )
    )
  })

  output$positions_table <- DT::renderDT({
    view <- analysis()
    shiny::req(view)

    bc_positions_table(view$positions)
  }, server = FALSE)

  output$issues <- shiny::renderUI({
    view <- analysis()
    shiny::req(view)

    external_issues <- view$external$issues

    unmatched <- external_issues |>
      dplyr::filter(
        .data$issue_type %in% c(
          "unmatched_xp",
          "unmatched_daycoval"
        )
      )

    internal_count <- nrow(view$internal$issues)

    messages <- list()

    if (nrow(unmatched) > 0L) {
      messages <- c(
        messages,
        list(shiny::tags$p(
          shiny::strong("Ativos não associados: "),
          paste(
            unmatched$asset_identifier,
            collapse = "; "
          )
        ))
      )
    }

    if (internal_count > 0L) {
      messages <- c(
        messages,
        list(shiny::tags$p(
          shiny::strong("Pendências Daycoval interno: "),
          internal_count
        ))
      )
    }

    if (length(messages) == 0L) {
      return(NULL)
    }

    shiny::div(
      class = "alert alert-warning",
      messages
    )
  })

  # Return reconciliation.

  output$return_current_source <- shiny::renderUI({
    reports <- current_reports()

    if (is.null(reports)) {
      return(shiny::div(
        class = "alert alert-warning",
        "Importe primeiro os relatórios atuais na página Ativos."
      ))
    }

    metadata <- reports$position$metadata

    shiny::tags$div(
      shiny::tags$strong("Dados atuais disponíveis"),
      shiny::tags$p(
        metadata$nome_curto,
        shiny::tags$br(),
        "Data: ",
        format(metadata$data_posicao, "%d/%m/%Y")
      ),
      shiny::tags$small(
        class = "text-muted",
        "Carteira Diária e Posição C reutilizadas."
      )
    )
  })

  shiny::observeEvent(input$analyze_return, {
    return_analysis(NULL)
    return_error(NULL)

    tryCatch(
      {
        reports <- current_reports()

        if (is.null(reports)) {
          stop(
            "Importe primeiro os relatórios atuais na página Ativos.",
            call. = FALSE
          )
        }

        previous_portfolio_path <- bc_report_path(
          input$previous_portfolio_path,
          "Carteira Diária anterior",
          "csv"
        )

        previous_position_path <- bc_report_path(
          input$previous_position_path,
          "Posição C anterior",
          "csv"
        )

        previous_portfolio <-
          batercota::read_daycoval_portfolio(
            path = previous_portfolio_path,
            source_name = basename(previous_portfolio_path)
          )

        previous_position <-
          batercota::read_daycoval_position(
            path = previous_position_path,
            source_name = basename(previous_position_path)
          )

        reconciliation <- batercota::reconcile_portfolio_return(
          previous_position = previous_position,
          current_position = reports$position,
          previous_portfolio = previous_portfolio,
          current_portfolio = reports$portfolio,
          tolerance_pp = input$return_tolerance
        )

        flows <- batercota::diagnose_shareholder_flows(
          previous_position = previous_position,
          current_position = reports$position,
          previous_portfolio = previous_portfolio,
          current_portfolio = reports$portfolio
        )

        return_analysis(list(
          reconciliation = reconciliation,
          flows = flows
        ))
      },
      error = function(error) {
        return_error(conditionMessage(error))
      }
    )
  })

  output$return_status <- shiny::renderUI({
    error <- return_error()

    if (!is.null(error)) {
      return(shiny::div(
        class = "alert alert-danger",
        error
      ))
    }

    result <- return_analysis()

    if (is.null(result)) {
      return(shiny::p(
        class = "text-muted",
        "Informe os relatórios anteriores e clique em Conciliar cota."
      ))
    }

    metadata <- result$reconciliation$metadata

    shiny::tags$p(
      shiny::strong("Fundo: "),
      metadata$codigo_carteira,
      " | Período: ",
      format(metadata$previous_date, "%d/%m/%Y"),
      " a ",
      format(metadata$current_date, "%d/%m/%Y")
    )
  })

  output$return_summary <- shiny::renderUI({
    result <- return_analysis()
    shiny::req(result)

    summary <- result$reconciliation$summary

    status <- switch(
      summary$comparison_status[[1L]],
      within_tolerance = "Dentro da tolerância",
      outside_tolerance = "Fora da tolerância",
      not_evaluated = "Não avaliado",
      "Não avaliado"
    )

    shiny::tagList(
      shiny::tags$h4("Resumo da rentabilidade"),

      shiny::tags$p(
        shiny::strong("Rentabilidade estimada: "),
        bc_percent(summary$estimated_return_percent),
        "%",
        "  |  ",
        shiny::strong("Rentabilidade Daycoval: "),
        bc_percent(summary$reported_return_percent),
        "%"
      ),

      shiny::tags$p(
        shiny::strong("Diferença: "),
        bc_percent(summary$difference_pp),
        " p.p.",
        "  |  ",
        shiny::strong("Tolerância: "),
        bc_percent(summary$tolerance_pp),
        " p.p.",
        "  |  ",
        shiny::strong("Resultado: "),
        status
      ),

      shiny::tags$p(
        shiny::strong("Resultado financeiro estimado: "),
        bc_money(summary$estimated_result),
        "  |  ",
        shiny::strong("Residual: "),
        bc_money(summary$residual_brl)
      ),

      shiny::tags$p(
        shiny::strong("Movimentações pendentes: "),
        summary$n_pending_operations,
        "  |  ",
        shiny::strong("Posições não estimadas: "),
        summary$n_unestimated,
        "  |  ",
        shiny::strong("Modelo: "),
        "Cobertura parcial"
      ),

      shiny::tags$hr()
    )
  })

  output$return_components_table <- DT::renderDT({
    result <- return_analysis()
    shiny::req(result)

    bc_return_components_table(
      result$reconciliation$components
    )
  }, server = FALSE)

  output$return_details_table <- DT::renderDT({
    result <- return_analysis()
    shiny::req(result)

    bc_return_details_table(
      result$reconciliation
    )
  }, server = FALSE)

  output$return_flows <- shiny::renderUI({
    result <- return_analysis()
    shiny::req(result)

    summary <- result$flows$summary

    shiny::tagList(
      shiny::tags$p(
        shiny::strong("Fluxo implícito pelas cotas: "),
        bc_money(summary$implied_net_flow),
        "  |  ",
        shiny::strong("Fluxo informado: "),
        bc_money(summary$reported_net_flow)
      ),

      shiny::tags$p(
        shiny::strong("Diferença dos fluxos: "),
        bc_money(summary$flow_difference),
        "  |  ",
        shiny::strong("Efeito contábil 24005: "),
        bc_money(summary$quota_adjustment_effect)
      ),

      shiny::tags$p(
        shiny::strong("Diagnóstico: "),
        summary$status
      )
    )
  })

  output$return_issues_summary <- shiny::renderUI({
    result <- return_analysis()
    shiny::req(result)

    n_issues <- nrow(result$reconciliation$issues) +
      nrow(result$flows$issues)

    if (n_issues == 0L) {
      return(shiny::tags$p(
        class = "text-muted",
        "Nenhuma pendência registrada."
      ))
    }

    shiny::tags$p(
      n_issues,
      "registro(s) para acompanhamento.",
      "Uma alteração de quantidade não significa",
      "necessariamente divergência operacional."
    )
  })

  output$return_issues_table <- DT::renderDT({
    result <- return_analysis()
    shiny::req(result)

    bc_return_issues_table(
      result$reconciliation,
      result$flows
    )
  }, server = FALSE)
}


shiny::shinyApp(
  ui = ui,
  server = server
)


