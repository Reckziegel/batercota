
# Fluxos e Passivos - Linear Select / XP

xp_fluxos_category_label <- function(category) {
  labels <- c(
    shareholder_redemption = "Resgate de cotista",
    shareholder_subscription = "Aplicação de cotista",
    cash_sweep_redemption = "Resgate do fundo de zeragem",
    cash_sweep_subscription = "Aplicação no fundo de zeragem",
    investee_fund_redemption = "Resgate de fundo investido",
    investee_fund_subscription = "Aplicação em fundo investido",
    investee_fund_adjustment = "Ajuste de operação em fundo",
    futures_adjustment = "Ajuste de futuros",
    fee_rebate = "Rebate de taxa",
    operating_expense = "Despesa operacional",
    brokerage = "Corretagem e emolumentos",
    unclassified = "Não classificado"
  )

  result <- unname(labels[category])
  result[is.na(result)] <- category[is.na(result)]

  result
}


xp_fluxos_money <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return("Não disponível")
  }

  sign <- if (value < 0) "-" else ""

  paste0(
    sign,
    "R$ ",
    formatC(
      abs(value),
      format = "f",
      digits = 2,
      big.mark = ".",
      decimal.mark = ","
    )
  )
}


xp_fluxos_table <- function(data, page_length = 20L) {
  DT::datatable(
    data,
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      order = list()
    )
  )
}


xp_fluxos_version_choices <- function(reports, report_type, date) {
  rows <- reports |>
    dplyr::filter(
      .data$report_type == .env$report_type,
      .data$reference_date == .env$date
    ) |>
    dplyr::arrange(dplyr::desc(.data$saved_at))

  stats::setNames(
    rows$sha256,
    paste0(
      rows$source_name,
      " [",
      substr(rows$sha256, 1L, 10L),
      "]"
    )
  )
}


xp_select_fluxos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Fluxos e Passivos",
    shiny::br(),

    shiny::h3("Fluxos e Passivos - Linear Select"),

    shiny::p(
      paste(
        "Conferência das movimentações financeiras",
        "e dos saldos de caixa da XP."
      )
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::selectInput(
          ns("date"),
          "Data-base",
          choices = character()
        ),

        shiny::selectInput(
          ns("current_hash"),
          "Demonstrativo de Caixa",
          choices = character()
        ),

        shiny::h5("Comparação com relatório anterior"),

        shiny::textOutput(ns("previous_date")),

        shiny::selectInput(
          ns("previous_hash"),
          "Versão anterior",
          choices = character()
        ),

        shiny::checkboxInput(
          ns("only_selected_day"),
          "Mostrar apenas movimentações da data-base",
          value = TRUE
        ),

        shiny::actionButton(
          ns("analyze"),
          "Conferir fluxos",
          class = "btn-primary",
          width = "100%"
        ),

        shiny::hr(),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "O XLSX de caixa é recuperado automaticamente",
            "da biblioteca local.",
            "Se houver versões diferentes,",
            "selecione explicitamente a desejada."
          )
        )
      ),

      shiny::mainPanel(
        shiny::h4("Resumo financeiro"),
        shiny::uiOutput(ns("summary")),

        shiny::hr(),

        shiny::h4("Saldos diários"),
        DT::DTOutput(ns("daily_balances")),

        shiny::br(),

        shiny::h4("Fluxos por categoria"),
        DT::DTOutput(ns("categories")),

        shiny::br(),

        shiny::h4("Movimentações financeiras"),
        DT::DTOutput(ns("transactions")),

        shiny::br(),

        shiny::h4("Lançamentos para revisão"),
        DT::DTOutput(ns("review")),

        shiny::br(),

        shiny::h4("Alterações no histórico da XP"),
        shiny::uiOutput(ns("revision_summary")),
        DT::DTOutput(ns("revisions"))
      )
    )
  )
}


xp_select_fluxos_server <- function(
    id,
    saved_reports,
    library_path
) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)

    available_reports <- shiny::reactive({
      saved_reports() |>
        dplyr::filter(
          .data$report_type == "xp_cash_xlsx"
        )
    })

    shiny::observeEvent(available_reports(), {
      dates <- available_reports() |>
        dplyr::pull(.data$reference_date) |>
        unique() |>
        sort(decreasing = TRUE)

      choices <- stats::setNames(
        as.character(dates),
        format(dates, "%d/%m/%Y")
      )

      current <- shiny::isolate(input$date)

      selected <- if (
        !is.null(current) &&
        current %in% unname(choices)
      ) {
        current
      } else if (length(choices) > 0L) {
        unname(choices[[1L]])
      } else {
        character()
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "date",
        choices = choices,
        selected = selected
      )
    })

    previous_date <- shiny::reactive({
      shiny::req(input$date)

      dates <- unique(
        available_reports()$reference_date
      )

      candidates <- dates[
        dates < as.Date(input$date)
      ]

      if (length(candidates) == 0L) {
        return(as.Date(NA))
      }

      max(candidates)
    })

    output$previous_date <- shiny::renderText({
      date <- previous_date()

      if (is.na(date)) {
        return("Nenhum relatório anterior disponível.")
      }

      paste(
        "Última data disponível:",
        format(date, "%d/%m/%Y")
      )
    })

    shiny::observeEvent(
      list(input$date, available_reports()),
      {
        shiny::req(input$date)

        reports <- available_reports()
        current_date <- as.Date(input$date)

        current_choices <- xp_fluxos_version_choices(
          reports = reports,
          report_type = "xp_cash_xlsx",
          date = current_date
        )

        current_selected <- if (
          length(current_choices) == 1L
        ) {
          unname(current_choices[[1L]])
        } else {
          ""
        }

        shiny::updateSelectInput(
          session,
          "current_hash",
          choices = c(
            "Selecione uma versão" = "",
            current_choices
          ),
          selected = current_selected
        )

        old_date <- previous_date()

        if (is.na(old_date)) {
          previous_choices <- character()
        } else {
          previous_choices <- xp_fluxos_version_choices(
            reports = reports,
            report_type = "xp_cash_xlsx",
            date = old_date
          )
        }

        previous_selected <- if (
          length(previous_choices) == 1L
        ) {
          unname(previous_choices[[1L]])
        } else {
          ""
        }

        shiny::updateSelectInput(
          session,
          "previous_hash",
          choices = c(
            "Nenhuma versão selecionada" = "",
            previous_choices
          ),
          selected = previous_selected
        )

        result(NULL)
      }
    )

    shiny::observeEvent(input$analyze, {
      shiny::req(input$date)

      analysis <- tryCatch(
        {
          reports <- available_reports()
          current_date <- as.Date(input$date)
          old_date <- previous_date()

          current_rows <- reports |>
            dplyr::filter(
              .data$reference_date == current_date
            )

          if (
            is.null(input$current_hash) ||
            !input$current_hash %in% current_rows$sha256
          ) {
            stop(
              "Selecione uma versão válida do caixa atual.",
              call. = FALSE
            )
          }

          current_path <- xp_assets_verified_path(
            reports = reports,
            hash = input$current_hash,
            library_path = library_path
          )

          current <- batercota::read_xp_select_cash(
            current_path
          )

          if (current$metadata$period_end != current_date) {
            stop(
              "O XLSX atual não corresponde à data selecionada.",
              call. = FALSE
            )
          }

          previous <- NULL
          revisions <- NULL

          if (!is.na(old_date)) {
            previous_rows <- reports |>
              dplyr::filter(
                .data$reference_date == old_date
              )

            if (
              is.null(input$previous_hash) ||
              !input$previous_hash %in% previous_rows$sha256
            ) {
              stop(
                "Selecione uma versão válida do caixa anterior.",
                call. = FALSE
              )
            }

            previous_path <- xp_assets_verified_path(
              reports = reports,
              hash = input$previous_hash,
              library_path = library_path
            )

            previous <- batercota::read_xp_select_cash(
              previous_path
            )

            if (previous$metadata$period_end != old_date) {
              stop(
                "O XLSX anterior não corresponde à data selecionada.",
                call. = FALSE
              )
            }

            revisions <- batercota::compare_xp_select_cash_versions(
              previous = previous,
              current = current
            )
          }

          list(
            current = current,
            previous = previous,
            revisions = revisions,
            reference_date = current_date
          )
        },
        error = function(error) error
      )

      if (inherits(analysis, "error")) {
        result(NULL)

        shiny::showNotification(
          conditionMessage(analysis),
          type = "error",
          duration = NULL
        )

        return(invisible(NULL))
      }

      result(analysis)

      n_divergences <- sum(
        analysis$current$daily_balances$status != "OK"
      )

      if (n_divergences > 0L) {
        shiny::showNotification(
          paste(
            n_divergences,
            "saldo(s) diário(s) divergente(s)."
          ),
          type = "warning",
          duration = 10
        )
      } else {
        shiny::showNotification(
          "Saldos diários conciliados.",
          type = "message",
          duration = 5
        )
      }
    })

    displayed_transactions <- shiny::reactive({
      analysis <- result()
      shiny::req(analysis)

      transactions <- analysis$current$transactions

      if (isTRUE(input$only_selected_day)) {
        transactions <- transactions |>
          dplyr::filter(
            .data$date == analysis$reference_date
          )
      }

      transactions
    })

    output$summary <- shiny::renderUI({
      analysis <- result()

      if (is.null(analysis)) {
        return(
          shiny::p(
            "Selecione a data-base e clique em Conferir fluxos."
          )
        )
      }

      metadata <- analysis$current$metadata
      balances <- analysis$current$daily_balances

      max_difference <- max(
        abs(balances$difference)
      )

      shiny::tagList(
        shiny::tags$p(
          shiny::strong("Período do relatório: "),
          format(metadata$period_start, "%d/%m/%Y"),
          " a ",
          format(metadata$period_end, "%d/%m/%Y")
        ),

        shiny::tags$p(
          shiny::strong("Saldo inicial: "),
          xp_fluxos_money(metadata$opening_balance)
        ),

        shiny::tags$p(
          shiny::strong("Saldo final: "),
          xp_fluxos_money(metadata$closing_balance)
        ),

        shiny::tags$p(
          shiny::strong("Movimentações no período: "),
          metadata$n_transactions
        ),

        shiny::tags$p(
          shiny::strong("Dias conciliados: "),
          metadata$n_days
        ),

        shiny::tags$p(
          shiny::strong("Diferença máxima nos saldos: "),
          xp_fluxos_money(max_difference)
        ),

        shiny::tags$p(
          shiny::strong("Movimentações para revisão: "),
          sum(analysis$current$transactions$review)
        )
      )
    })

    output$daily_balances <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      analysis$current$daily_balances |>
        dplyr::transmute(
          Data = .data$date,
          Abertura = .data$opening_balance,
          Entradas = .data$inflows,
          Saídas = .data$outflows,
          Calculado = .data$calculated_closing,
          Informado = .data$reported_closing,
          Diferença = .data$difference,
          Movimentações = .data$n_transactions,
          Situação = .data$status
        ) |>
        xp_fluxos_table(page_length = 25L) |>
        DT::formatCurrency(
          columns = c(
            "Abertura",
            "Entradas",
            "Saídas",
            "Calculado",
            "Informado",
            "Diferença"
          ),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$categories <- DT::renderDT({
      transactions <- displayed_transactions()

      transactions |>
        dplyr::group_by(.data$category) |>
        dplyr::summarise(
          Movimentações = dplyr::n(),
          Entradas = sum(.data$inflow),
          Saídas = sum(.data$outflow),
          Líquido = sum(.data$amount),
          Revisões = sum(.data$review),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          category = xp_fluxos_category_label(.data$category)
        ) |>
        dplyr::rename(
          Categoria = .data$category
        ) |>
        xp_fluxos_table() |>
        DT::formatCurrency(
          columns = c("Entradas", "Saídas", "Líquido"),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$transactions <- DT::renderDT({
      displayed_transactions() |>
        dplyr::transmute(
          Data = .data$date,
          Histórico = .data$description,
          Categoria = xp_fluxos_category_label(.data$category),
          Código = .data$asset_code,
          Entradas = .data$inflow,
          Saídas = .data$outflow,
          Líquido = .data$amount,
          Revisar = dplyr::if_else(
            .data$review,
            "Sim",
            ""
          )
        ) |>
        xp_fluxos_table() |>
        DT::formatCurrency(
          columns = c("Entradas", "Saídas", "Líquido"),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$review <- DT::renderDT({
      displayed_transactions() |>
        dplyr::filter(.data$review) |>
        dplyr::transmute(
          Data = .data$date,
          Histórico = .data$description,
          Categoria = xp_fluxos_category_label(.data$category),
          Valor = .data$amount
        ) |>
        xp_fluxos_table() |>
        DT::formatCurrency(
          columns = "Valor",
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$revision_summary <- shiny::renderUI({
      analysis <- result()
      shiny::req(analysis)

      if (is.null(analysis$previous)) {
        return(shiny::p(
          "Nenhum relatório anterior disponível para comparação."
        ))
      }

      revisions <- analysis$revisions

      shiny::tags$p(
        shiny::strong("Alterações identificadas: "),
        nrow(revisions),
        " | ",
        shiny::strong("Impacto líquido: "),
        xp_fluxos_money(sum(revisions$financial_effect))
      )
    })

    output$revisions <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      if (is.null(analysis$revisions)) {
        return(xp_fluxos_table(
          tibble::tibble(
            Informação = "Comparação não disponível."
          )
        ))
      }

      analysis$revisions |>
        dplyr::transmute(
          Data = .data$date,
          Histórico = .data$description,
          Entradas = .data$inflow,
          Saídas = .data$outflow,
          Ocorrências = .data$count_change,
          `Impacto financeiro` = .data$financial_effect,
          Situação = .data$status
        ) |>
        xp_fluxos_table() |>
        DT::formatCurrency(
          columns = c(
            "Entradas",
            "Saídas",
            "Impacto financeiro"
          ),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)
  })
}
