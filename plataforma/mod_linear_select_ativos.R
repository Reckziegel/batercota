
# Conciliação de ativos - Linear Select / XP

xp_assets_price_key <- function(price) {
  if (anyNA(price) || any(!is.finite(price)) || any(price <= 0)) {
    stop("Foram identificados preços de cota inválidos.", call. = FALSE)
  }

  sprintf("%.8f", price)
}


xp_assets_validate_keys <- function(data, source) {
  if (anyDuplicated(data$price_key)) {
    stop(
      paste(
        "Existem preços de cota duplicados em",
        source,
        "- será necessário utilizar o cadastro de identificadores."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


xp_assets_read_portfolio <- function(path, reference_date) {
  raw <- utils::read.csv2(
    path,
    fileEncoding = "UTF-8",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  required <- c(
    "Cliente",
    "Data_Operacao",
    "Codigo",
    "Fundo",
    "Quantidade_Total",
    "Cotacao",
    "Valor_Atual",
    "Patrimonio"
  )

  if (!all(required %in% names(raw))) {
    stop("A Carteira CSV não possui as colunas esperadas.", call. = FALSE)
  }

  if (!all(trimws(raw$Cliente) == "CUST4325")) {
    stop("A Carteira CSV não corresponde ao Linear Select.", call. = FALSE)
  }

  dates <- as.Date(substr(raw$Data_Operacao, 1L, 10L))

  if (
    anyNA(dates) ||
    length(unique(dates)) != 1L ||
    unique(dates) != reference_date
  ) {
    stop("A data da Carteira CSV não corresponde à seleção.", call. = FALSE)
  }

  positions <- raw |>
    dplyr::filter(
      !is.na(.data$Fundo),
      nzchar(trimws(.data$Fundo))
    ) |>
    dplyr::transmute(
      asset_code = trimws(as.character(.data$Codigo)),
      asset_name = trimws(as.character(.data$Fundo)),
      quantity_adm = as.numeric(.data$Quantidade_Total),
      unit_price = as.numeric(.data$Cotacao),
      value_adm = as.numeric(.data$Valor_Atual)
    ) |>
    dplyr::mutate(
      price_key = xp_assets_price_key(.data$unit_price)
    )

  if (
    nrow(positions) == 0L ||
    anyNA(positions) ||
    anyDuplicated(positions$asset_code) ||
    any(!is.finite(positions$quantity_adm)) ||
    any(!is.finite(positions$value_adm))
  ) {
    stop("As posições da Carteira CSV são inválidas.", call. = FALSE)
  }

  xp_assets_validate_keys(positions, "Carteira CSV")

  nav <- unique(
    as.numeric(raw$Patrimonio)[
      !is.na(raw$Patrimonio) & raw$Patrimonio != 0
    ]
  )

  if (length(nav) != 1L || !is.finite(nav)) {
    stop("Patrimônio inválido na Carteira CSV.", call. = FALSE)
  }

  list(
    nav = nav,
    positions = positions
  )
}


xp_assets_read_xml <- function(path, reference_date) {
  document <- xml2::read_xml(path, options = "NONET")

  header <- xml2::xml_find_first(
    document,
    "/arquivoposicao_4_01/fundo/header"
  )

  if (inherits(header, "xml_missing")) {
    stop("Cabeçalho XML da XP não encontrado.", call. = FALSE)
  }

  field <- function(node, name) {
    xml2::xml_text(
      xml2::xml_find_first(node, paste0("./", name))
    )
  }

  if (field(header, "cnpj") != "57593831000131") {
    stop("O XML não corresponde ao Linear Select.", call. = FALSE)
  }

  position_date <- as.Date(
    field(header, "dtposicao"),
    format = "%Y%m%d"
  )

  if (is.na(position_date) || position_date != reference_date) {
    stop("A data do XML não corresponde à seleção.", call. = FALSE)
  }

  nodes <- xml2::xml_find_all(
    document,
    "/arquivoposicao_4_01/fundo/cotas"
  )

  if (length(nodes) == 0L) {
    stop("O XML não contém fundos investidos.", call. = FALSE)
  }

  positions <- purrr::map_dfr(
    nodes,
    function(node) {
      quantity <- as.numeric(field(node, "qtdisponivel"))
      unit_price <- as.numeric(field(node, "puposicao"))

      tibble::tibble(
        cnpj = field(node, "cnpjfundo"),
        quantity_xml = quantity,
        value_xml = round(quantity * unit_price, 2),
        price_key = xp_assets_price_key(unit_price)
      )
    }
  )

  if (
    anyNA(positions) ||
    anyDuplicated(positions$cnpj) ||
    any(!is.finite(positions$quantity_xml))
  ) {
    stop("As posições do XML são inválidas.", call. = FALSE)
  }

  xp_assets_validate_keys(positions, "XML de posição")

  nav <- as.numeric(field(header, "patliq"))

  if (!is.finite(nav)) {
    stop("Patrimônio inválido no XML.", call. = FALSE)
  }

  list(
    nav = nav,
    unit_value = as.numeric(field(header, "valorcota")),
    shares_outstanding = as.numeric(field(header, "quantidade")),
    positions = positions
  )
}



xp_assets_read_hub <- function(path, reference_date) {
  statement <- batercota::read_xp_statement(path)

  if (statement$metadata$account_number != "10850617") {
    stop(
      "O extrato Hub não corresponde ao Linear Select.",
      call. = FALSE
    )
  }

  if (statement$metadata$position_date != reference_date) {
    stop(
      "A data do extrato Hub não corresponde à seleção.",
      call. = FALSE
    )
  }

  positions <- statement$positions |>
    dplyr::transmute(
      asset_name_hub = .data$asset_name,
      unit_price_hub = .data$unit_price,
      quantity_hub = .data$quantity,
      value_hub = .data$gross_value
    )

  valid <- all(is.finite(positions$unit_price_hub)) &&
    all(is.finite(positions$quantity_hub)) &&
    all(is.finite(positions$value_hub)) &&
    all(positions$unit_price_hub >= 0) &&
    all(positions$quantity_hub >= 0) &&
    all(positions$value_hub >= 0)

  if (!valid) {
    stop(
      "O extrato Hub contém posições financeiras inválidas.",
      call. = FALSE
    )
  }

  positions$price_key <- NA_character_

  valid_prices <- positions$unit_price_hub > 0

  positions$price_key[valid_prices] <- xp_assets_price_key(
    positions$unit_price_hub[valid_prices]
  )

  matched_keys <- positions$price_key[
    !is.na(positions$price_key)
  ]

  if (anyDuplicated(matched_keys)) {
    stop(
      "O extrato Hub contém preços duplicados.",
      call. = FALSE
    )
  }

  list(
    total = statement$metadata$total_gross,
    positions = positions
  )
}


xp_assets_match_hub <- function(
    hub_positions,
    portfolio_positions,
    quantity_tolerance = 0.02,
    value_tolerance = 0.05
) {
  zero_price_rows <- which(
    is.na(hub_positions$price_key) &
      hub_positions$unit_price_hub == 0
  )

  for (i in zero_price_rows) {
    available <- !portfolio_positions$price_key %in%
      hub_positions$price_key

    candidates <- which(
      available &
        abs(
          portfolio_positions$quantity_adm -
            hub_positions$quantity_hub[[i]]
        ) <= quantity_tolerance &
        abs(
          portfolio_positions$value_adm -
            hub_positions$value_hub[[i]]
        ) <= value_tolerance
    )

    if (length(candidates) == 1L) {
      hub_positions$price_key[[i]] <-
        portfolio_positions$price_key[[candidates]]
    }
  }

  matched_keys <- hub_positions$price_key[
    !is.na(hub_positions$price_key)
  ]

  if (anyDuplicated(matched_keys)) {
    stop(
      "Associação ambígua entre Carteira e Hub.",
      call. = FALSE
    )
  }

  hub_positions
}



xp_assets_empty_hub <- function() {
  tibble::tibble(
    asset_name_hub = character(),
    quantity_hub = double(),
    value_hub = double(),
    price_key = character()
  )
}


xp_assets_verified_path <- function(
    reports,
    hash,
    library_path
) {
  record <- reports |>
    dplyr::filter(.data$sha256 == .env$hash)

  if (nrow(record) != 1L) {
    stop("Relatório não encontrado na biblioteca.", call. = FALSE)
  }

  root <- normalizePath(
    file.path(library_path, "xp_select", "objects"),
    winslash = "/",
    mustWork = TRUE
  )

  path <- normalizePath(
    record$stored_path[[1L]],
    winslash = "/",
    mustWork = TRUE
  )

  if (!startsWith(path, paste0(root, "/"))) {
    stop("Caminho armazenado fora da biblioteca.", call. = FALSE)
  }

  current_hash <- digest::digest(
    path,
    algo = "sha256",
    file = TRUE
  )

  if (!identical(current_hash, hash)) {
    stop("Falha na verificação SHA-256.", call. = FALSE)
  }

  path
}


xp_assets_reconcile <- function(
    reference_date,
    xml_path,
    portfolio_path,
    hub_path = NULL,
    tolerance = 50
) {
  xml <- xp_assets_read_xml(xml_path, reference_date)
  portfolio <- xp_assets_read_portfolio(portfolio_path, reference_date)

  hub <- if (is.null(hub_path)) {
    NULL
  } else {
    xp_assets_read_hub(hub_path, reference_date)
  }

  hub_positions <- if (is.null(hub)) {
    xp_assets_empty_hub()
  } else {
    xp_assets_match_hub(
      hub_positions = hub$positions,
      portfolio_positions = portfolio$positions
    )
  }

  comparison <- portfolio$positions |>
    dplyr::full_join(
      xml$positions,
      by = "price_key"
    ) |>
    dplyr::full_join(
      hub_positions,
      by = "price_key",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      difference_xml = .data$value_adm - .data$value_xml,
      difference_hub = .data$value_adm - .data$value_hub,
      status_xml = dplyr::case_when(
        is.na(.data$asset_code) & is.na(.data$cnpj) ~
          "Somente Hub",
        is.na(.data$asset_code) ~ "Somente XML",
        is.na(.data$cnpj) ~ "Somente carteira",
        abs(.data$quantity_adm - .data$quantity_xml) > 0.0001 ~
          "Quantidade divergente",
        abs(.data$difference_xml) > tolerance ~
          "Valor divergente",
        TRUE ~ "OK"
      )
    )

  if (is.null(hub)) {
    comparison$status_hub <- "Não avaliado"
  } else {
    comparison <- comparison |>
      dplyr::mutate(

        status_hub = dplyr::case_when(
          is.na(.data$asset_code) & .data$unit_price_hub == 0 ~ "Cota zero sem associação",
          is.na(.data$asset_code) ~ "Somente Hub",
          is.na(.data$asset_name_hub) & .data$asset_code == "CUST2451" ~ "Fundo de zeragem",
          is.na(.data$asset_name_hub) & abs(.data$value_adm) <= 0.01 ~ "Posição residual",
          is.na(.data$asset_name_hub) ~ "Ausente no Hub",
          .data$unit_price_hub == 0 ~ "Cota zerada no Hub",
          abs(.data$difference_hub) <= tolerance ~ "OK",
          TRUE ~ "Divergente"
        )
      )
  }

  comparison <- comparison |>
    dplyr::mutate(
      asset = dplyr::coalesce(
        .data$asset_name_hub,
        .data$asset_name
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(abs(.data$difference_hub)),
      .data$asset
    )

  administrator_hub_total <- portfolio$positions |>
    dplyr::filter(.data$asset_code != "CUST2451") |>
    dplyr::summarise(
      total = sum(.data$value_adm)
    ) |>
    dplyr::pull(.data$total)

  hub_total <- if (is.null(hub)) NA_real_ else hub$total

  summary <- tibble::tibble(
    reference_date = reference_date,
    nav_xml = xml$nav,
    nav_csv = portfolio$nav,
    nav_difference = portfolio$nav - xml$nav,
    unit_value = xml$unit_value,
    shares_outstanding = xml$shares_outstanding,
    n_funds_administrator = nrow(portfolio$positions),
    n_funds_xml = nrow(xml$positions),
    n_funds_hub = if (is.null(hub)) {
      NA_integer_
    } else {
      nrow(hub$positions)
    },
    administrator_hub_total = administrator_hub_total,
    hub_total = hub_total,
    hub_difference = administrator_hub_total - hub_total
  )

  structure(
    list(
      summary = summary,
      positions = comparison
    ),
    class = "xp_select_assets_reconciliation"
  )
}


xp_select_assets_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "Ativos",
    shiny::br(),
    shiny::h3("Conciliação de ativos - Linear Select"),
    shiny::p(
      "Comparação da Carteira XP com o XML e o Extrato XP Hub."
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
          ns("xml"),
          "XML de posição",
          choices = character()
        ),

        shiny::selectInput(
          ns("portfolio"),
          "Carteira Diária CSV",
          choices = character()
        ),

        shiny::selectInput(
          ns("hub"),
          "Extrato XP Hub",
          choices = character()
        ),

        shiny::numericInput(
          ns("tolerance"),
          "Tolerância por posição (R$)",
          value = 50,
          min = 0,
          step = 10
        ),

        shiny::actionButton(
          ns("analyze"),
          "Conferir ativos",
          icon = shiny::icon("check"),
          class = "btn-primary",
          width = "100%"
        ),

        shiny::br(),
        shiny::br(),

        shiny::tags$small(
          class = "text-muted",
          paste(
            "Os documentos são recuperados da biblioteca.",
            "Se houver mais de uma versão, selecione a desejada."
          )
        )
      ),

      shiny::mainPanel(
        shiny::h4("Resumo da conciliação"),
        shiny::uiOutput(ns("summary")),

        shiny::hr(),

        shiny::h4("Conferência por ativo"),
        DT::DTOutput(ns("positions"))
      )
    )
  )
}


xp_select_assets_server <- function(
    id,
    saved_reports,
    library_path
) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(saved_reports(), {
      reports <- saved_reports()

      dates <- sort(
        unique(reports$reference_date),
        decreasing = TRUE
      )

      choices <- as.character(dates)

      current <- shiny::isolate(input$date)

      selected <- if (!is.null(current) && current %in% choices) {
        current
      } else if (length(choices) > 0L) {
        choices[[1L]]
      } else {
        character()
      }

      shiny::updateSelectInput(
        session,
        "date",
        choices = choices,
        selected = selected
      )
    })

    shiny::observeEvent(
      list(input$date, saved_reports()),
      {
        shiny::req(input$date)

        reports <- saved_reports() |>
          dplyr::filter(
            .data$reference_date == as.Date(input$date)
          )

        update_report_choice <- function(id, report_type, optional = FALSE) {
          rows <- reports |>
            dplyr::filter(.data$report_type == .env$report_type)

          choices <- stats::setNames(
            rows$sha256,
            paste0(
              rows$source_name,
              " [",
              substr(rows$sha256, 1L, 10L),
              "]"
            )
          )

          default <- if (length(choices) == 1L) {
            unname(choices[[1L]])
          } else {
            ""
          }

          first_choice <- if (optional) {
            c("Não utilizar extrato" = "")
          } else {
            c("Selecione uma versão" = "")
          }

          shiny::updateSelectInput(
            session,
            id,
            choices = c(first_choice, choices),
            selected = default
          )
        }

        update_report_choice("xml", "xp_position_xml")
        update_report_choice("portfolio", "xp_portfolio_csv")
        update_report_choice("hub", "xp_hub_statement", optional = TRUE)

        result(NULL)
      }
    )

    shiny::observeEvent(input$analyze, {
      shiny::req(input$date)

      if (
        is.null(input$xml) ||
        !nzchar(input$xml) ||
        is.null(input$portfolio) ||
        !nzchar(input$portfolio)
      ) {
        shiny::showNotification(
          "Selecione o XML e a Carteira CSV.",
          type = "warning"
        )

        return(invisible(NULL))
      }

      reports <- saved_reports()

      analysis <- tryCatch(
        {
          xml_path <- xp_assets_verified_path(
            reports,
            input$xml,
            library_path
          )

          portfolio_path <- xp_assets_verified_path(
            reports,
            input$portfolio,
            library_path
          )

          hub_path <- if (
            !is.null(input$hub) &&
            nzchar(input$hub)
          ) {
            xp_assets_verified_path(
              reports,
              input$hub,
              library_path
            )
          } else {
            NULL
          }

          xp_assets_reconcile(
            reference_date = as.Date(input$date),
            xml_path = xml_path,
            portfolio_path = portfolio_path,
            hub_path = hub_path,
            tolerance = input$tolerance
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
        return(shiny::p("Selecione a data e clique em Conferir ativos."))
      }

      info <- analysis$summary[1L, ]

      money <- function(value) {
        if (is.na(value)) {
          return("Não disponível")
        }

        paste0(
          "R$ ",
          format(
            round(value, 2),
            big.mark = ".",
            decimal.mark = ",",
            nsmall = 2,
            scientific = FALSE
          )
        )
      }

      shiny::tagList(
        shiny::tags$p(
          shiny::strong("Patrimônio XML: "),
          money(info$nav_xml)
        ),
        shiny::tags$p(
          shiny::strong("Diferença de PL CSV × XML: "),
          money(info$nav_difference)
        ),
        shiny::tags$p(
          shiny::strong("Cota publicada: "),
          format(info$unit_value, digits = 10)
        ),
        shiny::tags$p(
          shiny::strong("Fundos na carteira: "),
          info$n_funds_administrator
        ),
        shiny::tags$p(
          shiny::strong("Carteira sem fundo de zeragem: "),
          money(info$administrator_hub_total)
        ),
        shiny::tags$p(
          shiny::strong("Posição XP Hub: "),
          money(info$hub_total)
        ),
        shiny::tags$p(
          shiny::strong("Diferença Carteira × Hub: "),
          money(info$hub_difference)
        )
      )
    })



    output$positions <- DT::renderDT({
      analysis <- result()
      shiny::req(analysis)

      data <- analysis$positions |>
        dplyr::transmute(
          Ativo = .data$asset,
          Código = .data$asset_code,
          CNPJ = stringr::str_replace(
            .data$cnpj,
            "^(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})$",
            "\\1.\\2.\\3/\\4-\\5"
          ),
          `Quantidade carteira` = .data$quantity_adm,
          `Quantidade XML` = .data$quantity_xml,
          `Valor carteira` = .data$value_adm,
          `Valor Hub` = .data$value_hub,
          `Diferença Hub` = .data$difference_hub,
          `Status XML` = .data$status_xml,
          `Status Hub` = .data$status_hub
        )

      quantity_columns <- c(
        "Quantidade carteira",
        "Quantidade XML"
      )

      financial_columns <- c(
        "Valor carteira",
        "Valor Hub",
        "Diferença Hub"
      )

      DT::datatable(
        data,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          order = list()
        )
      ) |>
        DT::formatRound(
          columns = quantity_columns,
          digits = 4,
          mark = ".",
          dec.mark = ","
        ) |>
        DT::formatCurrency(
          columns = financial_columns,
          currency = "R$ ",
          digits = 2,
          mark = ".",
          dec.mark = ","
        ) |>
        DT::formatStyle(
          columns = c(quantity_columns, financial_columns),
          textAlign = "right"
        )
    }, server = FALSE)
  })
}
