# ============================================================
# mod_perfil.R — Módulos 1 + 2
# Selector global + tarjeta jugador + radar + z-scores + clutch
# ============================================================

# ── UI del selector (vive en el header del navbarPage) ─────────
selectorJugadorUI <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "display:flex; gap:12px; align-items:flex-end; flex-wrap:wrap;",
    tags$div(style = "width:130px;",
      selectInput(ns("liga"), "Liga", choices = NULL, width = "100%")
    ),
    tags$div(style = "width:160px;",
      selectInput(ns("club"), "Club", choices = NULL, width = "100%")
    ),
    tags$div(style = "width:110px;",
      selectInput(ns("grupo"), "Posición", choices = NULL, width = "100%")
    ),
    tags$div(style = "width:320px;",
      selectizeInput(ns("jugador_sel"), "Jugador", choices = NULL, width = "100%",
                     options = list(placeholder = "Escribe para buscar…"))
    )
  )
}

# ── Server del selector — devuelve reactive con jugador_id ────
selectorJugadorServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {

    observe({
      ligas <- sort(unique(perfiles$liga))
      updateSelectInput(session, "liga", choices = c("Todas" = "", ligas))
    })

    df_liga <- reactive({
      if (is.null(input$liga) || input$liga == "") return(perfiles)
      filter(perfiles, liga == input$liga)
    })

    observe({
      clubes <- sort(unique(df_liga()$club))
      updateSelectInput(session, "club", choices = c("Todos" = "", clubes))
    })

    df_club <- reactive({
      d <- df_liga()
      if (!is.null(input$club) && input$club != "") d <- filter(d, club == input$club)
      d
    })

    observe({
      grupos <- sort(unique(df_club()$grupo_posicion))
      updateSelectInput(session, "grupo", choices = c("Todas" = "", grupos))
    })

    df_filtrado <- reactive({
      d <- df_club()
      if (!is.null(input$grupo) && input$grupo != "") d <- filter(d, grupo_posicion == input$grupo)
      d
    })

    observe({
      d <- df_filtrado()
      # Etiqueta: "JAlvarez — Atlético Madrid (La Liga)"
      etiqueta <- paste0(nombre_display(d$jugador_id),
                         " — ", d$club, " (", d$liga, ")")
      choices <- setNames(d$jugador_id, etiqueta)
      updateSelectizeInput(session, "jugador_sel", choices = choices, server = TRUE)
    })

    reactive({ input$jugador_sel })
  })
}

# ── UI de la página Perfil ─────────────────────────────────────
perfilPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("titulo")),
    uiOutput(ns("aviso_minutos")),
    fluidRow(
      column(5,
        h4("Radar de índices"),
        plotlyOutput(ns("radar"), height = "400px")
      ),
      column(7,
        h4("Z-scores e índices"),
        DTOutput(ns("tabla_indices")),
             tags$h4("Balón parado (informativo)"),
             tags$p(style = "color:#46566C; font-size:13.5px;",
                    "Por 90 minutos, sin ajuste. No entra a la calificación."),
             DTOutput(ns("tabla_bp")),
        br(),
        h4("Contexto de temporada"),
        uiOutput(ns("tarjeta_contexto")),
        br(),
        h4("Clutch index (xG vs nivel de rival)"),
        uiOutput(ns("clutch_card"))
      )
    ),
    br(),
    fluidRow(
      column(12,
        h4("Métricas ajustadas vs crudas (top métricas por índice)"),
        DTOutput(ns("tabla_metricas"))
      )
    )
  )
}

# ── Server de la página Perfil ─────────────────────────────────
perfilPageServer <- function(id, perfiles, jugador_id_sel) {
  moduleServer(id, function(input, output, session) {

    jugador_row <- reactive({
      jid <- jugador_id_sel()
      if (is.null(jid) || jid == "") return(NULL)
      filas <- filter(perfiles, jugador_id == jid)
      if (nrow(filas) == 0) return(NULL)
      # Si hay varias filas (transferencia en temporada), tomar la de mayor minutos
      slice_max(filas, minutos_total, n = 1, with_ties = FALSE)
    })

    # Aviso si no cumple mínimo de minutos
    output$aviso_minutos <- renderUI({
      j <- jugador_row()
      if (is.null(j)) {
        return(div(class="alert alert-info",
                   "Selecciona liga → club → posición → jugador para ver el perfil."))
      }
      if (!isTRUE(j$cumple_minimo)) {
        div(class="alert alert-warning",
            icon("exclamation-triangle"),
            paste0(" ", nombre_display(j$jugador_id), " tiene ", j$minutos_total,
                   " minutos — menos del mínimo de ", MIN_MINUTOS_UI,
                   ". El perfil puede no ser representativo."))
      }
    })

    # Radar de los 5 índices
    output$radar <- renderPlotly({
      j <- jugador_row()
      if (is.null(j)) return(NULL)

      vals <- as.numeric(unlist(j[1, DIMS_INDICES]))
      etiqs <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
      # Cerrar polígono
      vals_c  <- c(vals,  vals[1])
      etiqs_c <- c(etiqs, etiqs[1])

      plot_ly(
        type = "scatterpolar", mode = "lines+markers",
        r = vals_c, theta = etiqs_c,
        fill = "toself", name = nombre_display(j$jugador_id),
        line = list(color = "#2E6DA4", width = 2),
        fillcolor = "rgba(46,109,164,0.18)",
        marker = list(color = "#2E6DA4", size = 6),
        hovertemplate = "<b>%{theta}</b><br>Z-score: %{r:.2f}<extra></extra>"
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(min(-2.5, min(vals, na.rm=TRUE) - 0.3),
                        max(2.5,  max(vals, na.rm=TRUE) + 0.3)),
              tickfont = list(size = 10)
            ),
            angularaxis = list(tickfont = list(size = 11))
          ),
          showlegend = FALSE,
          margin = list(t = 20, b = 20, l = 40, r = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })

    output$titulo <- renderUI({
      j <- jugador_row(); req(j)
      encabezado(j$player_name, descripcion_jugador(j))
    })

    output$tabla_bp <- renderDT({
      j <- jugador_row(); req(j)
      d <- tabla_balon_parado(j, perfiles)
      validate(need(!is.null(d), "Sin eventos para esta temporada."))
      datatable(d, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE), class = "compact")
    })

    # Tabla de z-scores e índices con semáforo
    output$tabla_indices <- renderDT({
      j <- jugador_row()
      if (is.null(j)) return(NULL)

      # Verificar que existan las columnas de índices
      dims_disponibles <- DIMS_INDICES[DIMS_INDICES %in% names(j)]
      if (length(dims_disponibles) == 0) return(NULL)

      vals <- as.numeric(unlist(j[1, dims_disponibles]))
      # Peso de cada indice en la posicion del jugador y su aporte al
      # z compuesto (antes de re-estandarizarlo). Explica de donde sale
      # el lugar de un jugador en el ranking. El revulsivo es informativo.
      pesos <- setNames(pesos_de(j$subgrupo[1]), DIMS_INDICES)[dims_disponibles]
      aporte <- pesos * ifelse(is.na(vals), 0, vals)
      z_rev <- if (DIM_REV %in% names(j)) j[[DIM_REV]][1] else NA_real_
      sin_senal <- "revulsivo_con_senal" %in% names(j) && isFALSE(as.logical(j$revulsivo_con_senal[1]))
      n_ent <- if ("entradas_cambio" %in% names(j)) j$entradas_cambio[1] else 0
      df_t <- data.frame(
        `Índice`  = c(unname(ETIQUETAS_INDICES[dims_disponibles]), "Revulsivo"),
        `Z-score` = round(c(vals, if (sin_senal) NA else z_rev), 2),
        Percentil = c(paste0(round(pnorm(vals) * 100), "º"),
                      if (is.na(z_rev)) paste0("s/d (", n_ent, " entradas)")
                      else if (sin_senal) paste0("s/d (", n_ent, " entradas, sin señal)")
                      else paste0(round(pnorm(z_rev) * 100), "º")),
        Peso      = c(paste0(round(100 * pesos), "%"), "Informativo"),
        Aporte    = c(round(aporte, 2), NA),
        `% del total` = c(paste0(round(100 * aporte / sum(abs(aporte))), "%"), ""),
        check.names = FALSE
      )

      datatable(df_t, rownames = FALSE,
                options = list(dom = "t", paging = FALSE, ordering = FALSE),
                class = "compact stripe") %>%
        formatStyle("Z-score",
          backgroundColor = styleInterval(
            c(-0.675, 0.675),
            c("#F6E3E3", "#F3F5F8", "#E1ECF6")
          )
        )
    })

    # Tarjeta de contexto de temporada
    output$tarjeta_contexto <- renderUI({
      j <- jugador_row()
      if (is.null(j)) return(NULL)
      div(class = "well well-sm",
        fluidRow(
          column(3, strong("Minutos"), br(), round(j$minutos_total),
                 if (!is.null(j$pct_min_subgrupo) && !is.na(j$pct_min_subgrupo) && j$pct_min_subgrupo < 70)
                   tags$div(style = "font-size:0.8em; color:#B8862B;",
                            paste0("Solo ", j$pct_min_subgrupo, "% como ", tolower(j$subgrupo_lab),
                                   ": perfil versátil"))),
          column(3, strong("Partidos"), br(), j$n_partidos),
          column(3, strong("Disponibilidad"), br(), paste0(j$disponibilidad, "%")),
          column(3, strong("Percentil"), br(),
                 tags$span(style="font-size:1.4em; font-weight:bold; color:#2E6DA4;",
                           paste0(j$percentil, "º")))
        ),
        hr(style="margin:8px 0;"),
        fluidRow(
          column(3, strong("Posesión media"), br(), paste0(round(j$posesion_media, 1), "%")),
          column(3, strong("PPDA media"),      br(), round(j$ppda_media, 2)),
          column(3, strong("Cal. compañeros"), br(), round(j$cal_comp_media, 1)),
          column(3, strong("Cal. rival media"),br(), round(j$cal_rival_media, 1))
        ),
        hr(style="margin:8px 0;"),
        fluidRow(
          column(4, strong("Goles"), br(), j$goles_tot),
          column(4, strong("Asistencias"), br(), j$asistencias_tot),
          column(4, strong("xA total"), br(), round(j$xa_tot, 2))
        ),
        hr(style="margin:8px 0;"),
        fluidRow(
          column(3, strong("Edad"), br(),
                 if (is.na(j$edad)) "—" else paste0(j$edad, " años")),
          column(3, strong("Nacionalidad"), br(),
                 if (is.na(j$nacionalidad)) "—" else j$nacionalidad),
          column(3, strong("Convocatorias"), br(),
                 paste0(j$convocatorias, " de ", j$partidos_tenencia,
                        " (", j$pct_convocatorias, "%)")),
          column(3, strong("Salidas por lesión"), br(),
                 paste0(j$salidas_lesion, "  ·  ", j$partidos_baja_lesion, " partidos de baja"))
        ),
        hr(style="margin:8px 0;"),
        fluidRow(
          column(6, strong("Fiabilidad de la muestra"), br(),
                 paste0(round(100 * j$fiabilidad), "%  ",
                        "(con ", j$m0_contraccion, " min el índice es mitad señal, mitad ruido)")),
          column(6, strong("Z compuesto con contracción"), br(),
                 sprintf("%+.2f  (sin contracción: %+.2f)", j$z_compuesto_fiable, j$z_compuesto))
        )
      )
    })

    # Clutch index
    output$clutch_card <- renderUI({
      j <- jugador_row()
      if (is.null(j)) return(NULL)

      xg_top    <- j$xg_vs_top20
      xg_bot    <- j$xg_vs_bottom20
      n_top     <- j$n_partidos_niv_top20
      n_bot     <- j$n_partidos_niv_bottom20
      clutch    <- j$clutch_index
      reducida  <- isTRUE(j$muestra_reducida_top20)

      color_clutch <- if (!is.na(clutch) && clutch > 0) "#2E6DA4" else "#B23A3A"

      div(class = "well well-sm",
        fluidRow(
          column(4,
            strong("xG p90 vs TOP 20%"),
            br(),
            if(is.na(xg_top)) "—" else round(xg_top, 3),
            br(),
            tags$small(paste0("(", ifelse(is.na(n_top), 0, n_top), " partidos)"))
          ),
          column(4,
            strong("xG p90 vs BOT 20%"),
            br(),
            if(is.na(xg_bot)) "—" else round(xg_bot, 3),
            br(),
            tags$small(paste0("(", ifelse(is.na(n_bot), 0, n_bot), " partidos)"))
          ),
          column(4,
            strong("Clutch index"),
            br(),
            tags$span(
              style = paste0("font-size:1.3em; font-weight:bold; color:", color_clutch, ";"),
              if(is.na(clutch)) "—" else round(clutch, 3)
            )
          )
        ),
        if (reducida) {
          div(class = "alert alert-warning", style = "margin:8px 0 0 0; padding:6px;",
              "⚠ Menos de 4 partidos vs equipos top de su liga — interpretar con cautela.")
        }
      )
    })

    # Tabla de métricas ajustadas vs crudas
    output$tabla_metricas <- renderDT({
      j <- jugador_row()
      if (is.null(j)) return(NULL)

      metricas_display <- list(
        "xG sin penaltis p90"     = c("xg_p90_aj",              "xg_p90_crudo"),
        "xA p90"                  = c("xa_p90_aj",              "xa_p90_crudo"),
        "Tiros p90"               = c("tiros_p90_aj",           "tiros_p90_crudo"),
        "Progresiones al últ. tercio p90" = c("progresiones_p90_aj", "progresiones_p90_crudo"),
        "Regates exitosos p90"    = c("regates_p90_aj",         "regates_p90_crudo"),
        "Presiones p90"           = c("presiones_p90_aj",       "presiones_p90_crudo"),
        "Intercepciones p90"      = c("intercep_p90_aj",        "intercep_p90_crudo"),
        "Acc. def. sobre lo esperado p90" = c("acc_def_sobre_esp_p90_aj", "acc_def_sobre_esp_p90_crudo"),
        "Recuperaciones campo rival p90"  = c("recup_campo_rival_p90_aj", "recup_campo_rival_p90_crudo"),
        "Pases p90"               = c("pases_p90_aj",           "pases_p90_crudo"),
        "OBV p90 (validación)"    = c("obv_p90_aj",             "obv_p90_crudo")
      )

      df_t <- purrr::imap_dfr(metricas_display, function(cols, nombre) {
        aj   <- if (cols[1] %in% names(j)) round(as.numeric(j[[cols[1]]]), 3) else NA
        crudo <- if (cols[2] %in% names(j)) round(as.numeric(j[[cols[2]]]), 3) else NA
        delta <- if (!is.na(aj) && !is.na(crudo)) round(aj - crudo, 3) else NA
        data.frame(Métrica = nombre, Ajustado = aj, Crudo = crudo, Delta = delta,
                   check.names = FALSE)
      })

      datatable(df_t, rownames = FALSE,
                options = list(dom = "t", paging = FALSE, ordering = FALSE),
                class = "compact stripe") %>%
        formatStyle("Delta",
          color = styleInterval(c(-0.001, 0.001), c("#B23A3A", "#666", "#2E6DA4")),
          fontWeight = "bold"
        )
    })

  })
}
