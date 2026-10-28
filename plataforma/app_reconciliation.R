
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

  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
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

  DT::datatable(
    data,
    rownames = FALSE,
    class = "stripe hover compact",
    options = list(
      pageLength = 25,
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
            "Linear FIM CP RL" = "linear"
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
    shiny::h3("Variação projetada da cota"),
    shiny::p(
      "Em desenvolvimento.",
      "Esta página utilizará as posições anteriores,",
      "os componentes patrimoniais e os fluxos do fundo",
      "para conferir a variação da cota."
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

  shiny::observeEvent(input$analyze, {
    analysis(NULL)
    read_error(NULL)

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

        analysis(view)
      },
      error = function(error) {
        read_error(conditionMessage(error))
      }
    )
  })

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

    money_label <- scales::label_dollar(
      accuracy = 0.01,
      prefix = "R$ ",
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
        money_label(view$metadata$financial_tolerance)
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
}


shiny::shinyApp(
  ui = ui,
  server = server
)


