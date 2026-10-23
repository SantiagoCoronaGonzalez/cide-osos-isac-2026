# ============================================================
# mod_ranking_historico.R — CIDE OSOS
# Modulo nuevo — Ranking del modelo vs. la vida real
#
# Eliges temporada y posicion. A la izquierda, el ranking que daba
# el modelo esa temporada. A la derecha, lo que paso en la realidad:
#   - la misma temporada: % de minutos (lo que decidio el entrenador)
#     y OBV de StatsBomb (un modelo de valor independiente)
#   - la temporada siguiente: donde jugo, si subio o bajo de nivel de
#     club, cuantos minutos tuvo y cuanto rindio
#
# Arriba, tres correlaciones resumen: ¿que ordena mejor el
# rendimiento de la temporada siguiente, el modelo, los minutos o
# el OBV?
# ============================================================

rankingHistPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("¿El ranking del modelo se parece a lo que pasó después?")),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(
          column(3, selectInput(ns("temporada"), "Temporada", choices = NULL, width = "100%")),
          column(3, selectInput(ns("sub"), "Posición", width = "100%",
                                choices = setNames(SUBS_ORDEN, SUBS_ETIQ[SUBS_ORDEN]))),
          column(2, numericInput(ns("min_min"), "Minutos mínimos", value = MIN_MIN_RANKING, min = 0, step = 90,
                                 width = "100%")),
          column(4, selectInput(ns("orden"), "Ordenar por", width = "100%",
                                choices = c("Modelo (z con contracción)" = "modelo",
                                            "Modelo (z compuesto)" = "z",
                                            "Minutos (lo que decidió el entrenador)" = "min",
                                            "OBV de StatsBomb" = "obv"))))),
    uiOutput(ns("resumen")),
    br(),
    DTOutput(ns("tabla")),
    br(),
    div(style = "background:#F3F5F8; border-left:3px solid #B8862B; padding:12px 16px;
                 font-size:0.84em; color:#0E2240; line-height:1.6;",
        tags$strong("Cómo leerlo. "),
        paste0("Los z se estandarizan dentro de la posición y la temporada. Δ nivel = nivel del club ",
               "de la temporada siguiente menos el de esta (10 puntos = una desviación estándar entre ",
               "clubes). 'Sin registro' = no jugó en Liga MX la temporada siguiente (venta al extranjero, ",
               "retiro o lesión larga). Las correlaciones usan solo a quienes jugaron los minutos ",
               "mínimos en ambas temporadas."))
  )
}

rankingHistPageServer <- function(id, perfiles_hist) {
  moduleServer(id, function(input, output, session) {
    temporadas <- sort(unique(perfiles_hist$temporada))

    observe({
      updateSelectInput(session, "temporada", choices = rev(temporadas),
                        selected = rev(temporadas[temporadas != max(temporadas)])[1])
    })

    # Una fila por jugador y temporada (la del club con mas minutos)
    por_jug <- perfiles_hist %>%
      dplyr::group_by(player_id, temporada) %>%
      dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    datos <- reactive({
      req(input$temporada, input$sub)
      t  <- input$temporada
      ts <- temporadas[match(t, temporadas) + 1]
      cur <- por_jug %>%
        dplyr::filter(temporada == t, subgrupo == input$sub, minutos_total >= input$min_min) %>%
        dplyr::mutate(
          rank_modelo = rank(-z_compuesto_fiable, ties.method = "min"),
          rank_min    = rank(-pct_minutos, ties.method = "min"),
          rank_obv    = rank(-obv_p90_res, ties.method = "min", na.last = "keep"))
      if (nrow(cur) == 0) return(list(d = cur, ts = ts))

      sig <- if (!is.na(ts)) {
        por_jug %>% dplyr::filter(temporada == ts) %>%
          dplyr::transmute(player_id, club_sig = club, min_sig = minutos_total,
                           pct_min_sig = pct_minutos, z_sig = z_compuesto,
                           nivel_sig = cal_comp_media)
      } else NULL

      d <- if (!is.null(sig)) dplyr::left_join(cur, sig, by = "player_id") else
        cur %>% dplyr::mutate(club_sig = NA_character_, min_sig = NA_real_, pct_min_sig = NA_real_,
                              z_sig = NA_real_, nivel_sig = NA_real_)
      d <- d %>% dplyr::mutate(
        delta_nivel = nivel_sig - cal_comp_media,
        que_paso = dplyr::case_when(
          is.na(ts) ~ "Temporada en curso",
          is.na(club_sig) ~ "Sin registro en Liga MX",
          club_sig != club ~ paste0("Fichó por ", club_sig),
          TRUE ~ "Siguió en el club"))
      list(d = d, ts = ts)
    })

    output$resumen <- renderUI({
      x <- datos(); d <- x$d
      if (nrow(d) == 0) return(div(class = "alert alert-info", "Sin jugadores con esos filtros."))
      if (is.na(x$ts)) return(div(class = "alert alert-secondary",
                                  "Es la temporada más reciente: todavía no hay 'después' para comparar."))
      v <- d %>% dplyr::filter(!is.na(z_sig), min_sig >= input$min_min)
      if (nrow(v) < 8) return(div(class = "alert alert-secondary",
                                  paste0("Solo ", nrow(v), " jugadores con minutos suficientes en ambas temporadas.")))
      rho <- function(a) suppressWarnings(cor(a, v$z_sig, method = "spearman", use = "complete.obs"))
      r_mod <- rho(v$z_compuesto_fiable); r_min <- rho(v$pct_minutos); r_obv <- rho(v$obv_p90_res)

      # Del top 10 de cada criterio, cuantos quedaron en el tercio superior al ano siguiente
      corte_sup <- quantile(v$z_sig, 2 / 3, na.rm = TRUE)
      top_en <- function(col) {
        top <- d %>% dplyr::arrange(dplyr::desc(.data[[col]])) %>% dplyr::slice_head(n = 10) %>%
          dplyr::filter(!is.na(z_sig), min_sig >= input$min_min)
        if (nrow(top) == 0) return("s/d")
        paste0(sum(top$z_sig >= corte_sup), " de ", nrow(top))
      }
      tarjeta <- function(titulo, valor, sub, destacado = FALSE) {
        tags$td(style = paste0("padding:14px 16px; vertical-align:top; border-left:1px solid #DCE1E8;",
                               if (destacado) " background:#FBF3DF;" else ""),
                tags$div(style = "font-size:0.82em; color:#46566C;",
                         titulo),
                tags$div(style = "font-size:1.6em; font-weight:700; color:#0E2240; margin:4px 0;", valor),
                tags$div(style = "font-size:0.8em; color:#46566C;", sub))
      }
      f <- function(r) if (is.na(r)) "s/d" else sprintf("%.2f", r)
      div(style = "border:1px solid #DCE1E8; background:#FFFFFF;",
          tags$div(style = "padding:10px 16px 0 16px; font-size:0.86em; color:#0E2240;",
                   tags$strong(paste0("¿Qué ordenaba mejor el rendimiento de ", x$ts,
                                      if (isTRUE(any(perfiles_hist$en_curso[perfiles_hist$temporada == x$ts])))
                                        " (temporada en curso, muestra parcial)" else "", "?")),
                   paste0(" Correlación de Spearman con el z compuesto de la temporada siguiente (n = ",
                          nrow(v), ").")),
          tags$table(style = "width:100%; border-collapse:collapse;", tags$tr(
            tarjeta("Modelo CIDE OSOS", f(r_mod), paste0("Top 10 que siguió arriba: ", top_en("z_compuesto_fiable")), TRUE),
            tarjeta("Minutos (entrenador)", f(r_min), paste0("Top 10 que siguió arriba: ", top_en("pct_minutos"))),
            tarjeta("OBV StatsBomb", f(r_obv), paste0("Top 10 que siguió arriba: ", top_en("obv_p90_res"))))))
    })

    output$tabla <- renderDT({
      d <- datos()$d
      req(nrow(d) > 0)
      col <- switch(input$orden, modelo = "z_compuesto_fiable", z = "z_compuesto",
                    min = "pct_minutos", obv = "obv_p90_res")
      df <- d %>%
        dplyr::arrange(dplyr::desc(.data[[col]])) %>%
        dplyr::transmute(
          `# modelo` = rank_modelo, Jugador = nombre_display(jugador_id), Club = club,
          Edad = round(edad), Min. = minutos_total,
          `Z fiable` = round(z_compuesto_fiable, 2), Percentil = percentil,
          `# minutos` = rank_min, `# OBV` = rank_obv,
          `Qué pasó después` = que_paso,
          `Δ nivel club` = round(delta_nivel, 1),
          `% min. siguiente` = round(pct_min_sig), `Z siguiente` = round(z_sig, 2))
      datatable(df, rownames = FALSE, class = "compact stripe",
                options = list(dom = "tp", pageLength = 25, scrollX = TRUE, ordering = TRUE)) %>%
        formatStyle("Z fiable", fontWeight = "bold", color = "#0E2240") %>%
        formatStyle("Z siguiente", fontWeight = "bold",
                    color = styleInterval(c(-0.0001, 0.0001), c("#B23A3A", "#46566C", "#1E7A5A"))) %>%
        formatStyle("Δ nivel club",
                    color = styleInterval(c(-2, 2), c("#B23A3A", "#46566C", "#1E7A5A")))
    })
  })
}
