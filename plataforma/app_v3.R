local_lib <- file.path(getwd(), "r-lib")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

required_packages <- c("shiny", "DT", "openxlsx", "readxl")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run install_packages.R first.",
    call. = FALSE
  )
}

library(shiny)
library(DT)

source("redemption_estimation_logic.R", local = TRUE)

reference_workbook_filename <- "positions_plus_transaction_records.xlsx"

reference_workbook_path <- function() {
  normalizePath(
    file.path(getwd(), reference_workbook_filename),
    winslash = "/",
    mustWork = FALSE
  )
}

compact_table <- function(data, filter = "top", page_length = 25, empty_message = "No rows to show.") {
  if (is.null(data) || nrow(data) == 0) {
    data <- data.frame(Message = empty_message, stringsAsFactors = FALSE)
  }

  datatable(
    data,
    rownames = FALSE,
    filter = filter,
    class = "compact stripe hover",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = TRUE
    )
  )
}

filtered_gross_total <- function(data, row_indexes) {
  if (is.null(data) || nrow(data) == 0 || !"Vl Bruto" %in% names(data)) {
    return(0)
  }

  if (!is.null(row_indexes)) {
    row_indexes <- row_indexes[row_indexes >= 1 & row_indexes <= nrow(data)]
    data <- data[row_indexes, , drop = FALSE]
  }

  sum(data$`Vl Bruto`, na.rm = TRUE)
}

gross_total_line <- function(label, data, row_indexes) {
  div(
    class = "table-total-line",
    span(class = "table-total-label", label),
    span(class = "table-total-value", format_money(filtered_gross_total(data, row_indexes)))
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f7f5; color: #222; }
      .container-fluid { max-width: 1480px; }
      .app-title { margin: 18px 0 8px; font-weight: 700; }
      .subtitle { color: #5f6368; margin-bottom: 18px; }
      .panel, .well { border-radius: 8px; border: 1px solid #ddd8cf; box-shadow: none; }
      .well { background: #fffdf9; }
      .metric-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 12px; margin: 6px 0 16px; }
      .metric { background: #ffffff; border: 1px solid #ddd8cf; border-radius: 8px; padding: 12px 14px; min-height: 76px; }
      .metric-label { font-size: 12px; color: #626866; text-transform: uppercase; }
      .metric-value { font-size: 22px; font-weight: 700; margin-top: 4px; }
      .alert { border-radius: 8px; }
      .section-note { color: #555; margin-bottom: 12px; }
      .table-total-line { display: flex; justify-content: space-between; align-items: center; gap: 12px; margin: 0 0 10px; padding: 8px 10px; border-top: 1px solid #ddd8cf; border-bottom: 1px solid #ddd8cf; background: #fffdf9; font-size: 12px; }
      .table-total-label { color: #626866; font-weight: 700; text-transform: uppercase; }
      .table-total-value { font-size: 14px; font-weight: 700; white-space: nowrap; }
      .dataTables_wrapper { font-size: 12px; }
      table.dataTable.compact thead th,
      table.dataTable.compact thead td { padding: 6px 8px !important; font-size: 11px; white-space: nowrap; }
      table.dataTable.compact tbody th,
      table.dataTable.compact tbody td { padding: 4px 8px !important; font-size: 11px; line-height: 1.15; vertical-align: top; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select { font-size: 11px; min-height: 28px; }
      table.dataTable thead .sorting:after,
      table.dataTable thead .sorting_asc:after,
      table.dataTable thead .sorting_desc:after { top: 6px; }
      @media (max-width: 1050px) { .metric-row { grid-template-columns: repeat(2, minmax(120px, 1fr)); } }
      @media (max-width: 560px) { .metric-row { grid-template-columns: 1fr; } }
    "))
  ),
  div(class = "app-title", h2("Passive Orders Transaction Estimator")),
  div(
    class = "subtitle",
    "Upload one or more TXT files, choose the request date, estimate redemptions and subscriptions, and update the project workbook."
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("orders_files", "TXT order files", accept = c(".txt"), multiple = TRUE),
      uiOutput("reference_status"),
      dateInput("request_date", "Request date", value = Sys.Date(), format = "yyyy-mm-dd"),
      tags$hr(),
      div(
        class = "section-note",
        "The reference workbook is loaded from the project folder."
      ),
      actionButton("update_reference_workbook", "Update Base Workbook", class = "btn-primary"),
      tags$br(),
      tags$br(),
      downloadButton("download_report", "Export XLSX")
    ),
    mainPanel(
      uiOutput("messages"),
      uiOutput("update_status"),
      uiOutput("metrics"),
      tabsetPanel(
        tabPanel("Redemptions", br(), uiOutput("redemptions_total"), DTOutput("estimated_table")),
        tabPanel("Estimation Details", br(), DTOutput("details_table")),
        tabPanel("Merged TXT Input", br(), DTOutput("input_table")),
        tabPanel("Subscriptions", br(), uiOutput("subscriptions_total"), DTOutput("subscriptions_table")),
        tabPanel(
          "Position Consultation",
          br(),
          fluidRow(
            column(4, dateInput("consultation_date", "Position date", value = Sys.Date(), format = "yyyy-mm-dd")),
            column(4, selectInput("consultation_fund", "Fund", choices = c("All funds" = ""))),
            column(4, textInput("consultation_investor", "Investor contains", value = ""))
          ),
          checkboxInput("consultation_positive_only", "Only show investors with position", value = TRUE),
          DTOutput("consultation_table")
        ),
        tabPanel("Read Errors", br(), DTOutput("errors_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  reference_refresh <- reactiveVal(0)
  update_result <- reactiveVal(NULL)

  orders_read <- reactive({
    req(input$orders_files)
    read_txt_files(input$orders_files)
  })

  orders_parsed <- reactive({
    req(orders_read()$data)
    tryCatch(
      list(data = parse_input_orders(orders_read()$data), error = NULL),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
  })

  reference_read <- reactive({
    reference_refresh()
    path <- reference_workbook_path()
    tryCatch(
      {
        if (!file.exists(path)) {
          stop("Workbook not found: ", path, call. = FALSE)
        }

        list(data = prepare_reference_model(path), path = path, error = NULL)
      },
      error = function(e) list(data = NULL, path = path, error = conditionMessage(e))
    )
  })

  estimation_result <- reactive({
    req(reference_read()$data, orders_parsed()$data, input$request_date)
    tryCatch(
      list(
        data = estimate_transactions(orders_parsed()$data, reference_read()$data, input$request_date),
        error = NULL
      ),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
  })

  consultation_positions <- reactive({
    req(reference_read()$data, input$consultation_date)
    tryCatch(
      list(
        data = filter_positions(
          compute_positions_snapshot(reference_read()$data, input$consultation_date),
          investor_search = input$consultation_investor,
          fund_id = input$consultation_fund,
          positive_only = isTRUE(input$consultation_positive_only)
        ),
        error = NULL
      ),
      error = function(e) list(data = NULL, error = conditionMessage(e))
    )
  })

  observe({
    if (!is.null(reference_read()$data)) {
      updateSelectInput(session, "consultation_fund", choices = fund_choices(reference_read()$data))
    }
  })

  output$reference_status <- renderUI({
    reference <- reference_read()

    if (!is.null(reference$error)) {
      return(div(
        class = "alert alert-danger",
        strong("Workbook: "),
        reference$error
      ))
    }

    div(
      class = "alert alert-success",
      strong("Workbook loaded: "),
      basename(reference$path)
    )
  })

  output$messages <- renderUI({
    messages <- list()
    reference <- reference_read()
    parsed <- NULL

    if (is.null(input$orders_files)) {
      messages[[length(messages) + 1]] <- div(
        class = "alert alert-info",
        "Upload at least one TXT file to start the estimation."
      )
    }

    if (!is.null(input$orders_files)) {
      orders <- orders_read()

      if (!is.null(orders$data)) {
        parsed <- orders_parsed()
      }
    }

    if (!is.null(parsed$error)) {
      messages[[length(messages) + 1]] <- div(
        class = "alert alert-danger",
        strong("TXT parsing error: "),
        parsed$error
      )
    }

    if (!is.null(reference$error)) {
      messages[[length(messages) + 1]] <- div(
        class = "alert alert-danger",
        strong("Workbook error: "),
        reference$error
      )
    }

    if (!is.null(input$orders_files) && !is.null(reference$data) && !is.null(parsed$data)) {
      estimation <- estimation_result()
    } else {
      estimation <- NULL
    }

    if (!is.null(estimation$error)) {
      messages[[length(messages) + 1]] <- div(
        class = "alert alert-danger",
        strong("Estimation error: "),
        estimation$error
      )
    }

    if (!is.null(reference$data)) {
      adjusted_date <- next_business_day(as.Date(input$request_date), reference$data$holidays)
      if (!isTRUE(as.Date(input$request_date) == adjusted_date)) {
        messages[[length(messages) + 1]] <- div(
          class = "alert alert-warning",
          paste0(
            "The selected request date is not a Brazilian business day. The app is using ",
            format(adjusted_date, "%Y-%m-%d"),
            " for valuation and export dates."
          )
        )
      }
    }

    if (!is.null(input$orders_files)) {
      errors <- orders_read()$errors
      if (nrow(errors) > 0) {
        messages[[length(messages) + 1]] <- div(
          class = "alert alert-warning",
          strong("Some TXT files could not be read."),
          tags$ul(lapply(seq_len(nrow(errors)), function(i) {
            tags$li(paste(errors$file[[i]], "-", errors$message[[i]]))
          }))
        )
      }
    }

    tagList(messages)
  })

  observeEvent(input$update_reference_workbook, {
    req(estimation_result()$data)

    result <- tryCatch(
      append_transaction_records_to_workbook(
        path = reference_workbook_path(),
        transaction_rows = estimation_result()$data$export,
        backup_dir = file.path(getwd(), "outputs", "workbook_backups")
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )

    if (is.null(result$success)) {
      result$success <- TRUE
    }

    update_result(result)

    if (isTRUE(result$success)) {
      if (isTRUE(result$inserted_rows > 0)) {
        reference_refresh(reference_refresh() + 1)
      }

      notification_type <- if (isTRUE(result$inserted_rows > 0)) "message" else "warning"
      showNotification(result$message, type = notification_type, duration = 8)
    } else {
      showNotification(result$message, type = "error", duration = 10)
    }
  })

  output$update_status <- renderUI({
    result <- update_result()

    if (is.null(result)) {
      return(NULL)
    }

    if (!isTRUE(result$success)) {
      return(div(
        class = "alert alert-danger",
        strong("Workbook update failed: "),
        result$message
      ))
    }

    if (isTRUE(result$inserted_rows > 0)) {
      return(div(
        class = "alert alert-success",
        strong("Workbook updated: "),
        paste0(result$inserted_rows, " row(s) inserted."),
        if (isTRUE(result$skipped_duplicate_rows > 0)) {
          paste0(" ", result$skipped_duplicate_rows, " duplicate row(s) skipped.")
        },
        tags$br(),
        small(paste("Backup:", result$backup_path))
      ))
    }

    div(
      class = "alert alert-warning",
      strong("Workbook not changed: "),
      result$message
    )
  })

  output$metrics <- renderUI({
    req(estimation_result()$data, orders_parsed()$data)

    details <- estimation_result()$data$details
    export_rows <- estimation_result()$data$export
    input_orders <- orders_parsed()$data

    metric <- function(label, value) {
      div(class = "metric", div(class = "metric-label", label), div(class = "metric-value", value))
    }

    div(
      class = "metric-row",
      metric("TXT Rows", nrow(input_orders)),
      metric("Redemptions", sum(input_orders$is_redemption, na.rm = TRUE)),
      metric("Subscriptions", sum(input_orders$is_subscription, na.rm = TRUE)),
      metric("Generated Rows", nrow(export_rows)),
      metric("No Position", sum(details$status == "NO POSITION", na.rm = TRUE)),
      metric("Generated Total", format_money(sum(export_rows$`Vl Bruto`, na.rm = TRUE)))
    )
  })

  output$redemptions_total <- renderUI({
    req(estimation_result()$data)
    gross_total_line(
      "Total Vl Bruto (current filter)",
      estimation_result()$data$redemption_export,
      input$estimated_table_rows_all
    )
  })

  output$estimated_table <- renderDT({
    req(estimation_result()$data)
    data <- estimation_result()$data$redemption_export
    return(compact_table(
      display_transaction_export(data),
      empty_message = "No exportable redemption rows were generated."
    ))
    if (nrow(data) > 0) {
      display <- data
      display$`Dt Operação` <- format_date_text(display$`Dt Operação`)
      display$`Dt Registro` <- format_date_text(display$`Dt Registro`)
      display$`Dt Conversão` <- format_date_text(display$`Dt Conversão`)
      display$`Dt Liquidação` <- format_date_text(display$`Dt Liquidação`)
      display$`Vl Bruto` <- format_money(display$`Vl Bruto`)
    } else {
      display <- data.frame(Message = "No exportable redemption rows were generated.", stringsAsFactors = FALSE)
    }

    if (nrow(data) > 0) {
      display <- display_transaction_export(data)
    }

    compact_table(display)
  })

  output$details_table <- renderDT({
    req(estimation_result()$data)
    data <- display_estimation_details(estimation_result()$data$details)
    table <- compact_table(data)

    if ("Status" %in% names(data)) {
      formatStyle(
        table,
        "Status",
        target = "row",
        backgroundColor = styleEqual(
          c(
            "ESTIMATED",
            "NO POSITION",
            "INSUFFICIENT POSITION",
            "MISSING COTA",
            "MISSING REDEMPTION RULE",
            "MISSING SUBSCRIPTION RULE",
            "INVALID VALUE"
          ),
          c("#F0FDF4", "#FFFBEB", "#FEF2F2", "#EFF6FF", "#F3F4F6", "#F3F4F6", "#FEF3C7")
        )
      )
    } else {
      table
    }
  })

  output$input_table <- renderDT({
    req(orders_parsed()$data)
    compact_table(
      display_input_orders(orders_parsed()$data),
      empty_message = "No TXT rows were parsed."
    )
  })

  output$subscriptions_total <- renderUI({
    req(estimation_result()$data)
    gross_total_line(
      "Total Vl Bruto (current filter)",
      estimation_result()$data$subscription_export,
      input$subscriptions_table_rows_all
    )
  })

  output$subscriptions_table <- renderDT({
    req(estimation_result()$data)
    compact_table(
      display_transaction_export(estimation_result()$data$subscription_export),
      empty_message = "No exportable subscriptions/applications were generated."
    )
  })

  output$consultation_table <- renderDT({
    req(consultation_positions()$data)
    compact_table(
      display_positions(consultation_positions()$data),
      empty_message = "No positions match the current filters."
    )
  })

  output$errors_table <- renderDT({
    errors <- if (!is.null(input$orders_files)) orders_read()$errors else data.frame()
    compact_table(errors, filter = "none")
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("redemption_estimate_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(estimation_result()$data, orders_parsed()$data)
      errors <- if (!is.null(input$orders_files)) orders_read()$errors else data.frame()
      write_redemption_report(file, estimation_result()$data, orders_parsed()$data, errors)
    }
  )
}

shinyApp(ui, server)
