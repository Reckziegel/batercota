
# plataforma/mod_fluxos_passivos_diagnostico.R

bc_fluxos_day_details <- function(reconciliation, reports, date) {
  if (!inherits(reconciliation, "shareholder_liability_timeline")) {
    cli::cli_abort("A conciliação de passivos é inválida.")
  }

  if (
    !is.list(reports) ||
    !inherits(reports$cash_statement, "daycoval_cash_statement")
  ) {
    cli::cli_abort("O Demonstrativo de Caixa não está disponível.")
  }

  if (
    !inherits(date, "Date") ||
    length(date) != 1L ||
    is.na(date)
  ) {
    cli::cli_abort("A data selecionada é inválida.")
  }

  daily <- reconciliation$timeline |>
    dplyr::filter(.data$date == .env$date)

  if (nrow(daily) != 1L) {
    cli::cli_abort("A data não pertence ao período conciliado.")
  }

  manager <- reconciliation$manager_operations |>
    dplyr::filter(
      (
        !is.na(.data$conversion_date) &
          .data$conversion_date == .env$date
      ) |
        (
          !is.na(.data$liquidation_date) &
            .data$liquidation_date == .env$date
        )
    ) |>
    dplyr::mutate(
      event = dplyr::case_when(
        .data$conversion_date == .env$date &
          .data$liquidation_date == .env$date ~
          "Cotização e liquidação",
        .data$conversion_date == .env$date ~ "Cotização",
        .data$liquidation_date == .env$date ~ "Liquidação",
        TRUE ~ "Não identificado"
      )
    ) |>
    dplyr::transmute(
      Etapa = .data$event,
      `ID operação` = .data$operation_id,
      Cotista = .data$investor_name,
      Operação = .data$operation_type,
      `Data cotização` = .data$conversion_date,
      `Data liquidação` = .data$liquidation_date,
      `Valor previsto` = .data$gross_value,
      Arquivo = .data$source_file,
      Aba = .data$source_sheet,
      Linha = .data$source_row
    )

  accounting <- reconciliation$accounting_entries |>
    dplyr::filter(.data$data_mov == .env$date) |>
    dplyr::transmute(
      Referência = .data$reg,
      `Referência financeira` = .data$reg_cp,
      Histórico = .data$historico,
      Operação = .data$operacao,
      Débito = .data$debito,
      Crédito = .data$credito,
      Arquivo = .data$source_file,
      Linha = .data$source_line
    )

  verification_labels <- c(
    verified = "Confirmado",
    missing_cash_movement = "Pagamento não localizado",
    ambiguous_cash_reference = "Referência ambígua",
    date_mismatch = "Data divergente",
    amount_mismatch = "Valor divergente",
    unexpected_cash_counterpart = "Contrapartida inesperada"
  )

  daily_links <- reconciliation$settlement_links |>
    dplyr::filter(.data$data_mov == .env$date)

  links <- daily_links |>
    dplyr::transmute(
      `Referência contábil` = .data$reg,
      `Referência financeira` = .data$reg_cp,
      `Baixa contábil` = .data$debito,
      `Data caixa` = .data$cash_date,
      `Valor no caixa` = .data$cash_credit,
      Contrapartida = .data$cash_counterpart,
      Confirmação = unname(
        .env$verification_labels[.data$verification]
      ),
      `Arquivo contábil` = .data$source_file,
      `Linha contábil` = .data$source_line,
      `Linha caixa` = .data$cash_source_line
    )

  verified_references <- daily_links$reg_cp[
    daily_links$verification == "verified"
  ]

  cash <- reports$cash_statement$movements |>
    dplyr::filter(
      .data$dt == .env$date,
      .data$tit_cp == "RESGA"
    ) |>
    dplyr::transmute(
      Referência = .data$reg,
      Histórico = .data$hist,
      `Valor pago` = .data$vl_cred,
      Confirmação = dplyr::if_else(
        .data$reg %in% .env$verified_references,
        "Confirmado",
        "Não confirmado"
      ),
      Arquivo = .data$source_file,
      Linha = .data$source_line
    )

  issues <- reconciliation$issues |>
    dplyr::filter(.data$date == .env$date) |>
    dplyr::transmute(
      Pendência = .data$issue_type,
      Referência = .data$reference,
      Arquivo = .data$source_file,
      Linha = .data$source_line
    )

  list(
    date = date,
    daily = daily,
    manager = manager,
    accounting = accounting,
    links = links,
    cash = cash,
    issues = issues
  )
}


bc_fluxos_details_table <- function(
    data,
    money_columns = character()
) {
  table <- DT::datatable(
    data,
    rownames = FALSE,
    selection = "none",
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
        emptyTable = "Nenhum registro para esta data",
        paginate = list(
          previous = "Anterior",
          "next" = "Próximo"
        )
      )
    )
  )

  if (length(money_columns) == 0L) {
    return(table)
  }

  DT::formatCurrency(
    table,
    columns = money_columns,
    currency = "R$ ",
    digits = 2,
    mark = ".",
    dec.mark = ","
  )
}


mod_fluxos_diagnostico_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Diagnóstico detalhado por data"),

    shiny::p(
      class = "text-muted",
      "Selecione uma linha na tabela de evolução diária",
      "para investigar os registros daquela data."
    ),

    shiny::uiOutput(ns("selected_day_info")),

    shiny::tabsetPanel(
      id = ns("diagnostic_tabs"),

      shiny::tabPanel(
        "Operações da gestora",
        shiny::br(),
        DT::DTOutput(ns("manager_table"))
      ),

      shiny::tabPanel(
        "Contabilidade",
        shiny::br(),
        DT::DTOutput(ns("accounting_table"))
      ),

      shiny::tabPanel(
        "Vínculos financeiros",
        shiny::br(),
        DT::DTOutput(ns("links_table"))
      ),

      shiny::tabPanel(
        "Pagamentos",
        shiny::br(),
        DT::DTOutput(ns("cash_table"))
      ),

      shiny::tabPanel(
        "Pendências da data",
        shiny::br(),
        DT::DTOutput(ns("issues_table"))
      )
    )
  )
}


mod_fluxos_diagnostico_server <- function(
    id,
    reconciliation,
    reports,
    selected_row
) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_date <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      reconciliation(),
      {
        result <- reconciliation()

        if (
          is.null(result) ||
          nrow(result$timeline) == 0L
        ) {
          selected_date(NULL)
        } else {
          selected_date(
            utils::tail(result$timeline$date, 1L)
          )
        }
      },
      ignoreNULL = FALSE
    )

    shiny::observeEvent(selected_row(), {
      result <- reconciliation()
      row_index <- selected_row()

      if (
        is.null(result) ||
        length(row_index) != 1L ||
        is.na(row_index) ||
        row_index < 1L ||
        row_index > nrow(result$timeline)
      ) {
        return(invisible(NULL))
      }

      selected_date(result$timeline$date[[row_index]])
    })

    details <- shiny::reactive({
      result <- reconciliation()
      source_reports <- reports()
      date <- selected_date()

      shiny::req(result, source_reports, date)

      bc_fluxos_day_details(
        reconciliation = result,
        reports = source_reports,
        date = date
      )
    })

    output$selected_day_info <- shiny::renderUI({
      if (is.null(reconciliation())) {
        return(shiny::p(
          class = "text-muted",
          "Importe e concilie os relatórios para iniciar."
        ))
      }

      data <- details()
      daily <- data$daily

      money <- scales::label_dollar(
        accuracy = 0.01,
        prefix = "R$ ",
        big.mark = ".",
        decimal.mark = ","
      )

      format_money <- function(value) {
        if (!is.finite(value)) {
          return("Não disponível")
        }

        money(value)
      }

      shiny::tagList(
        shiny::tags$p(
          shiny::strong("Data selecionada: "),
          format(data$date, "%d/%m/%Y")
        ),

        shiny::tags$p(
          shiny::strong("Cotizações Daycoval: "),
          format_money(daily$daycoval_conversions),
          "  |  ",
          shiny::strong("Pagamentos confirmados: "),
          format_money(daily$verified_cash_settlements)
        ),

        shiny::tags$p(
          shiny::strong("Passivo reconstruído: "),
          format_money(daily$daycoval_closing),
          "  |  ",
          shiny::strong("Diferença contra gestora: "),
          format_money(daily$closing_difference)
        ),

        shiny::tags$p(
          class = "text-muted",
          paste(
            "Os registros abaixo preservam identificadores",
            "e informações de origem dos documentos."
          )
        )
      )
    })

    output$manager_table <- DT::renderDT({
      bc_fluxos_details_table(
        details()$manager,
        money_columns = "Valor previsto"
      )
    }, server = FALSE)

    output$accounting_table <- DT::renderDT({
      bc_fluxos_details_table(
        details()$accounting,
        money_columns = c("Débito", "Crédito")
      )
    }, server = FALSE)

    output$links_table <- DT::renderDT({
      bc_fluxos_details_table(
        details()$links,
        money_columns = c(
          "Baixa contábil",
          "Valor no caixa"
        )
      )
    }, server = FALSE)

    output$cash_table <- DT::renderDT({
      bc_fluxos_details_table(
        details()$cash,
        money_columns = "Valor pago"
      )
    }, server = FALSE)

    output$issues_table <- DT::renderDT({
      bc_fluxos_details_table(details()$issues)
    }, server = FALSE)

    shiny::reactive(details())
  })
}
