
# Variação da cota - Linear Select / XP

xp_cota_money <- function(value) {
  if (is.na(value)) {
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


xp_cota_number <- function(value, field) {
  result <- suppressWarnings(as.numeric(value))

  if (anyNA(result) || any(!is.finite(result))) {
    stop(
      paste("Valores numéricos inválidos em", field),
      call. = FALSE
    )
  }

  result
}


xp_cota_read_xml <- function(path, reference_date) {
  document <- xml2::read_xml(path, options = "NONET")

  header <- xml2::xml_find_first(
    document,
    "/arquivoposicao_4_01/fundo/header"
  )

  if (inherits(header, "xml_missing")) {
    stop("Cabeçalho XP não encontrado.", call. = FALSE)
  }

  field <- function(name) {
    node <- xml2::xml_find_first(
      header,
      paste0("./", name)
    )

    if (inherits(node, "xml_missing")) {
      stop(
        paste("Campo ausente no XML:", name),
        call. = FALSE
      )
    }

    xml2::xml_text(node)
  }

  if (field("cnpj") != "57593831000131") {
    stop("O XML não pertence ao Linear Select.", call. = FALSE)
  }

  date <- as.Date(
    field("dtposicao"),
    format = "%Y%m%d"
  )

  if (is.na(date) || date != reference_date) {
    stop("Data-base inconsistente no XML.", call. = FALSE)
  }

  list(
    date = date,
    nav = xp_cota_number(field("patliq"), "patliq"),
    unit_value = xp_cota_number(field("valorcota"), "valorcota"),
    shares = xp_cota_number(field("quantidade"), "quantidade")
  )
}


xp_cota_read_portfolio <- function(path, reference_date) {
  raw <- utils::read.csv2(
    path,
    fileEncoding = "UTF-8",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  required <- c(
    "Cliente", "Data_Operacao", "Codigo", "Fundo",
    "Papel", "Descricao", "Quantidade_Total",
    "Cotacao", "Valor_Atual", "Valor_Bruto",
    "Valor_3", "Ajuste_Valorizacao", "Patrimonio"
  )

  if (!all(required %in% names(raw)) || nrow(raw) == 0L) {
    stop("Layout da Carteira CSV inválido.", call. = FALSE)
  }

  if (!all(trimws(raw$Cliente) == "CUST4325")) {
    stop("A carteira não pertence ao Linear Select.", call. = FALSE)
  }

  dates <- as.Date(substr(raw$Data_Operacao, 1L, 10L))

  if (
    anyNA(dates) ||
    length(unique(dates)) != 1L ||
    unique(dates) != reference_date
  ) {
    stop("Data-base inconsistente na Carteira CSV.", call. = FALSE)
  }

  nonempty <- function(value) {
    !is.na(value) & nzchar(trimws(as.character(value)))
  }

  code <- trimws(as.character(raw$Codigo))
  paper <- trimws(as.character(raw$Papel))
  description <- ifelse(
    is.na(raw$Descricao),
    "",
    trimws(raw$Descricao)
  )

  is_fund <- nonempty(raw$Fundo)
  is_security <- nonempty(raw$Codigo)
  is_bond <- is_security & !is_fund & nonempty(raw$Papel)
  is_future <- is_security & !is_fund & !is_bond
  is_cash <- grepl("^Saldo em Tesouraria", description)
  is_account <- nonempty(raw$Descricao) & !is_cash

  future_codes <- code[is_future]

  if (
    any(!grepl("^(DI1|DDI|DOL|WDO|IND|WIN)", future_codes))
  ) {
    stop(
      "Foram encontrados ativos não classificados na Carteira CSV.",
      call. = FALSE
    )
  }

  funds <- tibble::tibble(
    asset_code = code[is_fund],
    asset_name = as.character(raw$Fundo[is_fund]),
    quantity = xp_cota_number(
      raw$Quantidade_Total[is_fund],
      "Quantidade_Total"
    ),
    unit_price = xp_cota_number(
      raw$Cotacao[is_fund],
      "Cotacao"
    ),
    value = xp_cota_number(
      raw$Valor_Atual[is_fund],
      "Valor_Atual"
    )
  )

  if (
    nrow(funds) == 0L ||
    anyDuplicated(funds$asset_code) ||
    any(funds$quantity < 0) ||
    any(funds$unit_price <= 0)
  ) {
    stop("Posições inválidas nos fundos investidos.", call. = FALSE)
  }

  bonds <- tibble::tibble(
    asset_code = code[is_bond],
    asset_name = paper[is_bond],
    value = xp_cota_number(
      raw$Valor_Bruto[is_bond],
      "Valor_Bruto"
    )
  )

  account_names <- description[is_account] |>
    gsub(
      pattern = "\\s*\\[[^]]*\\]",
      replacement = "",
      x = _
    ) |>
    trimws()

  accounts <- tibble::tibble(
    account = account_names,
    value = xp_cota_number(
      raw$Valor_3[is_account],
      "Valor_3"
    )
  ) |>
    dplyr::group_by(.data$account) |>
    dplyr::summarise(
      value = sum(.data$value),
      .groups = "drop"
    )

  cash <- sum(
    xp_cota_number(raw$Valor_3[is_cash], "Tesouraria")
  )

  nav_values <- unique(
    xp_cota_number(
      raw$Patrimonio[
        !is.na(raw$Patrimonio) & raw$Patrimonio != 0
      ],
      "Patrimonio"
    )
  )

  if (length(nav_values) != 1L) {
    stop("Patrimônio ausente ou ambíguo no CSV.", call. = FALSE)
  }

  future_adjustment <- sum(
    xp_cota_number(
      raw$Ajuste_Valorizacao[is_future],
      "Ajuste_Valorizacao"
    )
  )

  provisioned_adjustment <- accounts |>
    dplyr::filter(
      grepl("^Ajuste de Futuros", .data$account)
    ) |>
    dplyr::summarise(value = sum(.data$value)) |>
    dplyr::pull(.data$value)

  if (abs(future_adjustment - provisioned_adjustment) > 0.01) {
    stop(
      "O ajuste de futuros diverge das contas a pagar/receber.",
      call. = FALSE
    )
  }

  list(
    nav = nav_values[[1L]],
    funds = funds,
    bonds = bonds,
    accounts = accounts,
    cash = cash,
    future_adjustment = future_adjustment
  )
}


xp_cota_fund_changes <- function(previous, current) {
  previous <- previous |>
    dplyr::transmute(
      asset_code = .data$asset_code,
      name_previous = .data$asset_name,
      quantity_previous = .data$quantity,
      price_previous = .data$unit_price,
      value_previous = .data$value
    )

  current <- current |>
    dplyr::transmute(
      asset_code = .data$asset_code,
      name_current = .data$asset_name,
      quantity_current = .data$quantity,
      price_current = .data$unit_price,
      value_current = .data$value
    )

  dplyr::full_join(
    previous,
    current,
    by = "asset_code"
  ) |>
    dplyr::mutate(
      asset = dplyr::coalesce(
        .data$name_current,
        .data$name_previous
      ),
      matched = !is.na(.data$price_previous) &
        !is.na(.data$price_current),
      quantity_previous = dplyr::coalesce(
        .data$quantity_previous,
        0
      ),
      quantity_current = dplyr::coalesce(
        .data$quantity_current,
        0
      ),
      value_previous = dplyr::coalesce(
        .data$value_previous,
        0
      ),
      value_current = dplyr::coalesce(
        .data$value_current,
        0
      ),
      quantity_change = .data$quantity_current -
        .data$quantity_previous,
      value_change = .data$value_current -
        .data$value_previous,
      price_effect = dplyr::if_else(
        .data$matched,
        .data$quantity_previous *
          (.data$price_current - .data$price_previous),
        0
      ),
      movement_effect = dplyr::if_else(
        .data$matched,
        .data$quantity_change * .data$price_current,
        .data$value_change
      ),
      rounding_effect = .data$value_change -
        .data$price_effect -
        .data$movement_effect,
      classification = dplyr::case_when(
        is.na(.data$name_previous) ~ "Nova posição",
        is.na(.data$name_current) ~ "Posição encerrada",
        abs(.data$quantity_change) > 0.0001 ~ "Movimentação",
        TRUE ~ "Mantida"
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(abs(.data$value_change))
    )
}


xp_cota_account_changes <- function(previous, current) {
  previous <- previous |>
    dplyr::rename(
      value_previous = .data$value
    )

  current <- current |>
    dplyr::rename(
      value_current = .data$value
    )

  dplyr::full_join(
    previous,
    current,
    by = "account"
  ) |>
    dplyr::mutate(
      value_previous = dplyr::coalesce(
        .data$value_previous,
        0
      ),
      value_current = dplyr::coalesce(
        .data$value_current,
        0
      ),
      value_change = .data$value_current -
        .data$value_previous
    ) |>
    dplyr::arrange(
      dplyr::desc(abs(.data$value_change))
    )
}


xp_cota_calculate <- function(
    previous_date,
    current_date,
    previous_xml_path,
    current_xml_path,
    previous_portfolio_path,
    current_portfolio_path
) {
  previous_xml <- xp_cota_read_xml(
    previous_xml_path,
    previous_date
  )

  current_xml <- xp_cota_read_xml(
    current_xml_path,
    current_date
  )

  previous <- xp_cota_read_portfolio(
    previous_portfolio_path,
    previous_date
  )

  current <- xp_cota_read_portfolio(
    current_portfolio_path,
    current_date
  )

  if (
    abs(previous$nav - previous_xml$nav) > 0.01 ||
    abs(current$nav - current_xml$nav) > 0.01
  ) {
    stop(
      "O patrimônio do CSV não coincide com o XML.",
      call. = FALSE
    )
  }

  components <- c(
    "Fundos investidos",
    "Títulos públicos",
    "Contas a pagar/receber",
    "Tesouraria"
  )

  previous_values <- c(
    sum(previous$funds$value),
    sum(previous$bonds$value),
    sum(previous$accounts$value),
    previous$cash
  )

  current_values <- c(
    sum(current$funds$value),
    sum(current$bonds$value),
    sum(current$accounts$value),
    current$cash
  )

  previous_residual <- previous_xml$nav - sum(previous_values)
  current_residual <- current_xml$nav - sum(current_values)

  bridge <- tibble::tibble(
    component = c(components, "Resíduo contábil"),
    previous_value = c(previous_values, previous_residual),
    current_value = c(current_values, current_residual)
  ) |>
    dplyr::mutate(
      value_change = .data$current_value -
        .data$previous_value
    )

  nav_change <- current_xml$nav - previous_xml$nav

  reconciliation_gap <- sum(bridge$value_change) - nav_change

  same_shares <- abs(
    current_xml$shares - previous_xml$shares
  ) < 0.000001

  published_return <- 100 * (
    current_xml$unit_value / previous_xml$unit_value - 1
  )

  return_from_nav <- if (same_shares) {
    100 * nav_change / previous_xml$nav
  } else {
    NA_real_
  }

  structure(
    list(
      previous_date = previous_date,
      current_date = current_date,
      previous_xml = previous_xml,
      current_xml = current_xml,
      previous_residual = previous_residual,
      current_residual = current_residual,
      nav_change = nav_change,
      reconciliation_gap = reconciliation_gap,
      published_return = published_return,
      return_from_nav = return_from_nav,
      same_shares = same_shares,
      bridge = bridge,
      funds = xp_cota_fund_changes(
        previous$funds,
        current$funds
      ),
      accounts = xp_cota_account_changes(
        previous$accounts,
        current$accounts
      )
    ),
    class = "xp_select_cota_reconciliation"
  )
}


xp_select_cota_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Variação da Cota",
    shiny::br(),
    shiny::h3("Replicação da variação da cota"),

    shiny::p(
      paste(
        "Selecione a posição atual.",
        "O aplicativo recuperará automaticamente",
        "a posição anterior da biblioteca."
      )
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        shiny::selectInput(
          ns("current_date"),
          "Data atual",
          choices = character()
        ),

        shiny::strong("Posição anterior"),
        shiny::textOutput(ns("previous_date")),

        shiny::uiOutput(ns("versions")),

        shiny::br(),

        shiny::actionButton(
          ns("calculate"),
          "Calcular variação",
          class = "btn-primary",
          width = "100%"
        )
      ),

      shiny::mainPanel(
        shiny::h4("Resumo da rentabilidade"),
        shiny::uiOutput(ns("summary")),

        shiny::hr(),

        shiny::h4("Decomposição do patrimônio"),
        DT::DTOutput(ns("bridge")),

        shiny::br(),

        shiny::h4("Variação dos fundos investidos"),
        DT::DTOutput(ns("funds")),

        shiny::br(),

        shiny::h4("Contas a pagar e receber"),
        DT::DTOutput(ns("accounts"))
      )
    )
  )
}


xp_select_cota_server <- function(
    id,
    saved_reports,
    library_path
) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)

    complete_dates <- shiny::reactive({
      saved_reports() |>
        dplyr::filter(
          .data$report_type %in% c(
            "xp_position_xml",
            "xp_portfolio_csv"
          )
        ) |>
        dplyr::distinct(
          .data$reference_date,
          .data$report_type
        ) |>
        dplyr::count(
          .data$reference_date,
          name = "n_types"
        ) |>
        dplyr::filter(.data$n_types == 2L) |>
        dplyr::pull(.data$reference_date) |>
        sort()
    })

    shiny::observeEvent(complete_dates(), {
      choices <- as.character(
        rev(complete_dates())
      )

      current <- shiny::isolate(input$current_date)

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
        session,
        "current_date",
        choices = choices,
        selected = selected
      )
    })

    previous_date <- shiny::reactive({
      shiny::req(input$current_date)

      dates <- complete_dates()
      current <- as.Date(input$current_date)

      candidates <- dates[dates < current]

      if (length(candidates) == 0L) {
        return(as.Date(NA))
      }

      max(candidates)
    })

    output$previous_date <- shiny::renderText({
      date <- previous_date()

      if (is.na(date)) {
        return("Nenhuma posição anterior completa.")
      }

      format(date, "%d/%m/%Y")
    })

    required_reports <- shiny::reactive({
      previous <- previous_date()
      shiny::req(!is.na(previous))

      tibble::tibble(
        input_id = c(
          "previous_xml",
          "previous_portfolio",
          "current_xml",
          "current_portfolio"
        ),
        report_type = c(
          "xp_position_xml",
          "xp_portfolio_csv",
          "xp_position_xml",
          "xp_portfolio_csv"
        ),
        reference_date = c(
          previous,
          previous,
          as.Date(input$current_date),
          as.Date(input$current_date)
        ),
        label = c(
          "XML anterior",
          "Carteira anterior",
          "XML atual",
          "Carteira atual"
        )
      )
    })

    output$versions <- shiny::renderUI({
      specs <- required_reports()
      reports <- saved_reports()

      controls <- lapply(seq_len(nrow(specs)), function(i) {
        spec <- specs[i, ]

        rows <- reports |>
          dplyr::filter(
            .data$report_type == spec$report_type,
            .data$reference_date == spec$reference_date
          )

        if (nrow(rows) <= 1L) {
          return(NULL)
        }

        choices <- stats::setNames(
          rows$sha256,
          paste0(
            rows$source_name,
            " [",
            substr(rows$sha256, 1L, 10L),
            "]"
          )
        )

        shiny::selectInput(
          inputId = session$ns(spec$input_id),
          label = spec$label,
          choices = c("Selecione a versão" = "", choices),
          selected = ""
        )
      })

      shiny::tagList(controls)
    })

    shiny::observeEvent(
      list(input$current_date, saved_reports()),
      {
        result(NULL)
      }
    )

    shiny::observeEvent(input$calculate, {
      analysis <- tryCatch(
        {
          specs <- required_reports()
          reports <- saved_reports()

          paths <- vapply(seq_len(nrow(specs)), function(i) {
            spec <- specs[i, ]

            rows <- reports |>
              dplyr::filter(
                .data$report_type == spec$report_type,
                .data$reference_date == spec$reference_date
              )

            if (nrow(rows) == 0L) {
              stop(
                paste("Relatório ausente:", spec$label),
                call. = FALSE
              )
            }

            hash <- if (nrow(rows) == 1L) {
              rows$sha256[[1L]]
            } else {
              input[[spec$input_id]]
            }

            if (
              is.null(hash) ||
              !nzchar(hash) ||
              !hash %in% rows$sha256
            ) {
              stop(
                paste("Selecione uma versão para", spec$label),
                call. = FALSE
              )
            }

            xp_assets_verified_path(
              reports = reports,
              hash = hash,
              library_path = library_path
            )
          }, character(1))

          names(paths) <- specs$input_id

          xp_cota_calculate(
            previous_date = previous_date(),
            current_date = as.Date(input$current_date),
            previous_xml_path = paths[["previous_xml"]],
            current_xml_path = paths[["current_xml"]],
            previous_portfolio_path = paths[["previous_portfolio"]],
            current_portfolio_path = paths[["current_portfolio"]]
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
    })

    output$summary <- shiny::renderUI({
      analysis <- result()

      if (is.null(analysis)) {
        return(shiny::p(
          "Selecione a data e clique em Calcular variação."
        ))
      }

      shiny::tagList(
        shiny::tags$p(
          shiny::strong("Período: "),
          format(analysis$previous_date, "%d/%m/%Y"),
          " a ",
          format(analysis$current_date, "%d/%m/%Y")
        ),

        shiny::tags$p(
          shiny::strong("Cota anterior: "),
          formatC(
            analysis$previous_xml$unit_value,
            digits = 9,
            format = "f",
            decimal.mark = ","
          )
        ),

        shiny::tags$p(
          shiny::strong("Cota atual: "),
          formatC(
            analysis$current_xml$unit_value,
            digits = 9,
            format = "f",
            decimal.mark = ","
          )
        ),

        shiny::tags$h4(
          style = "color: #198754;",
          paste0(
            "Rentabilidade: ",
            formatC(
              analysis$published_return,
              format = "f",
              digits = 6,
              decimal.mark = ","
            ),
            "%"
          )
        ),

        shiny::tags$p(
          shiny::strong("Variação do PL: "),
          xp_cota_money(analysis$nav_change)
        ),

        shiny::tags$p(
          shiny::strong("Diferença da reconciliação: "),
          xp_cota_money(analysis$reconciliation_gap)
        ),

        shiny::tags$p(
          shiny::strong("Retorno calculado pelo PL: "),
          if (analysis$same_shares) {
            paste0(
              formatC(
                analysis$return_from_nav,
                digits = 6,
                format = "f",
                decimal.mark = ","
              ),
              "%"
            )
          } else {
            "Não calculado: houve mudança no número de cotas."
          }
        ),

        shiny::tags$p(
          shiny::strong("Resíduo contábil anterior: "),
          xp_cota_money(analysis$previous_residual),
          " | ",
          shiny::strong("Atual: "),
          xp_cota_money(analysis$current_residual)
        ),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "A decomposição utiliza posições contábeis.",
            "Movimentações de cotas de fundos investidos",
            "não são tratadas como rentabilidade.",
            "O saldo de ajustes futuros já está incluído",
            "nas contas a pagar e receber."
          )
        )
      )
    })

    output$bridge <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      analysis$bridge |>
        dplyr::transmute(
          Componente = .data$component,
          Anterior = .data$previous_value,
          Atual = .data$current_value,
          Variação = .data$value_change
        ) |>
        DT::datatable(
          rownames = FALSE,
          options = list(
            dom = "t",
            scrollX = TRUE,
            ordering = FALSE
          )
        ) |>
        DT::formatCurrency(
          columns = c("Anterior", "Atual", "Variação"),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$funds <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      data <- analysis$funds |>
        dplyr::transmute(
          Ativo = .data$asset,
          Código = .data$asset_code,
          `Qtd. anterior` = .data$quantity_previous,
          `Qtd. atual` = .data$quantity_current,
          `Variação quantidade` = .data$quantity_change,
          `Efeito preço` = .data$price_effect,
          `Efeito movimentação` = .data$movement_effect,
          `Ajuste arredondamento` = .data$rounding_effect,
          `Variação financeira` = .data$value_change,
          Situação = .data$classification
        )

      DT::datatable(
        data,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          order = list()
        )
      ) |>
        DT::formatRound(
          columns = c(
            "Qtd. anterior",
            "Qtd. atual",
            "Variação quantidade"
          ),
          digits = 4,
          mark = ".",
          dec.mark = ","
        ) |>
        DT::formatCurrency(
          columns = c(
            "Efeito preço",
            "Efeito movimentação",
            "Ajuste arredondamento",
            "Variação financeira"
          ),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)

    output$accounts <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      analysis$accounts |>
        dplyr::transmute(
          Conta = .data$account,
          Anterior = .data$value_previous,
          Atual = .data$value_current,
          Variação = .data$value_change
        ) |>
        DT::datatable(
          rownames = FALSE,
          options = list(
            pageLength = 20,
            scrollX = TRUE
          )
        ) |>
        DT::formatCurrency(
          columns = c("Anterior", "Atual", "Variação"),
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        )
    }, server = FALSE)
  })
}
