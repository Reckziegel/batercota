
test_that("imports all six real Daycoval portfolios", {
  fixture_dir <- testthat::test_path("fixtures", "daycoval")

  expected <- tibble::tribble(
    ~codigo_carteira, ~data_posicao, ~n_entries,
    ~patrimonio_liquido, ~total_ativos,
    "13711539", "2026-10-16", 30L, 30822092.91, 30896065.58,
    "13711539", "2026-10-19", 31L, 30740004.70, 30904913.72,
    "13386778", "2026-10-16", 27L, 9966347.81, 10235838.93,
    "13386778", "2026-10-19", 27L, 9914725.55, 10164001.22,
    "13386735", "2026-10-16", 37L, 2409809.35, 2418201.55,
    "13386735", "2026-10-19", 39L, 2418254.80, 2428375.33
  ) |>
    dplyr::mutate(
      data_posicao = as.Date(data_posicao)
    ) |>
    dplyr::arrange(codigo_carteira, data_posicao)

  files <- list.files(
    path = fixture_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  expect_length(files, nrow(expected))

  if (length(files) != nrow(expected)) {
    return(invisible(NULL))
  }

  portfolios <- purrr::map(
    files,
    read_daycoval_portfolio
  )

  actual <- purrr::map2_dfr(
    files,
    portfolios,
    function(file, portfolio) {
      metadata <- portfolio$metadata

      tibble::tibble(
        codigo_carteira = metadata$codigo_carteira,
        data_posicao = metadata$data_posicao,
        n_entries = nrow(portfolio$entries),
        patrimonio_liquido = metadata$patrimonio_liquido,
        total_ativos = metadata$total_ativos
      )
    }
  ) |>
    dplyr::arrange(codigo_carteira, data_posicao)

  expect_equal(actual, expected)

  expect_equal(
    nrow(dplyr::distinct(
      actual,
      codigo_carteira,
      data_posicao
    )),
    6L
  )

  purrr::walk2(
    files,
    portfolios,
    function(file, portfolio) {
      metadata <- portfolio$metadata

      expect_s3_class(portfolio, "daycoval_portfolio")

      expect_named(
        portfolio,
        c("metadata", "entries", "details", "raw")
      )

      expect_identical(
        metadata$arquivo_origem,
        basename(file)
      )

      expect_equal(nrow(portfolio$details), 3L)

      expect_true(
        all(
          portfolio$entries$codigo_crt ==
            metadata$codigo_carteira
        )
      )

      expect_true(
        all(
          portfolio$entries$data_emis ==
            metadata$data_posicao
        )
      )

      expect_true(
        all(
          portfolio$entries$source_line %in%
            portfolio$raw$source_line
        )
      )

      expect_true(
        all(
          portfolio$details$source_line %in%
            portfolio$raw$source_line
        )
      )

      patliq <- portfolio$entries |>
        dplyr::filter(
          stringr::str_trim(.data$titulo) == "PATLIQ",
          .data$grp_n1 == "20000",
          .data$grp_n2 == "26000",
          .data$grp_n3 == "26000"
        )

      expect_equal(nrow(patliq), 1L)

      expect_equal(
        patliq$vl_mrc,
        metadata$patrimonio_liquido
      )

      benchmark <- parse_daycoval_layout(
        portfolio$raw,
        record_type = "RentabBenkmark"
      )

      expect_equal(nrow(benchmark), 1L)

      expect_equal(
        sum(portfolio$details$link_status == "linked"),
        3L
      )

      expect_false(
        any(portfolio$details$link_status == "unlinked")
      )

      expect_true(
        all(
          portfolio$details$entry_source_line %in%
            portfolio$entries$source_line
        )
      )

      benchmark <- portfolio$metadata$benchmark

      expect_s3_class(benchmark, "tbl_df")
      expect_equal(nrow(benchmark), 1L)

      expect_identical(
        benchmark$indice,
        "CDI"
      )

      expect_type(benchmark$dia, "double")
      expect_type(benchmark$mes, "double")
      expect_type(benchmark$ano, "double")

      expect_true(
        all(benchmark$source_line %in% portfolio$raw$source_line)
      )


      numeric_fields <- c(
        "qt",
        "qt_bloq",
        "qt_disp",
        "pu_cst",
        "vl_cst",
        "pu_mrc",
        "vl_mrc",
        "mrc_bloq",
        "vl_atv_pl",
        "n_cotas",
        "vl_cotas",
        "valor_total_atv",
        "dia",
        "mes",
        "ano",
        "doze_meses",
        "vinte_qt_meses",
        "inicio",
        "n_cotas2",
        "vl_cotas2",
        "dia2",
        "mes2",
        "ano2",
        "doze_meses2",
        "vinte_qt_meses2",
        "inicio2",
        "n_cotista2",
        "vl_taxa",
        "vl_cota_sem_amrtz",
        "vl_cota_ant_amrtz"
      )

      integer_fields <- c(
        "n_cotista",
        "dec_pu",
        "dec_num_cota",
        "dec_val_cota",
        "dec_qtd"
      )

      logical_fields <- c(
        "tit_in465",
        "b_preco_extn",
        "b_calc_preco_intn"
      )

      date_fields <- c(
        "data_emis",
        "data_venc",
        "data_aplic"
      )

      expect_equal(ncol(portfolio$entries), 70L)

      expect_true(
        all(vapply(
          portfolio$entries[numeric_fields],
          is.double,
          logical(1)
        ))
      )

      expect_true(
        all(vapply(
          portfolio$entries[integer_fields],
          is.integer,
          logical(1)
        ))
      )

      expect_true(
        all(vapply(
          portfolio$entries[logical_fields],
          is.logical,
          logical(1)
        ))
      )

      expect_true(
        all(vapply(
          portfolio$entries[date_fields],
          inherits,
          logical(1),
          what = "Date"
        ))
      )

      expect_identical(
        portfolio$metadata$numero_cotistas,
        portfolio$entries$n_cotista[[1L]]
      )

      expect_equal(
        nrow(portfolio$raw),
        nrow(portfolio$entries) +
          nrow(portfolio$details) +
          nrow(portfolio$metadata$benchmark) +
          4L
      )

    }
  )
})


