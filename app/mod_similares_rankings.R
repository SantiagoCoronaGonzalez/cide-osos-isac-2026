# ============================================================
# mod_similares_rankings.R
# Módulo 3 — Gemelos estadísticos (Mahalanobis)
# Módulo 4 — Rankings (local y global)
# ============================================================

# ── FUNCIONES DE CÁLCULO (compartidas) ───────────────────────

# Calcula distancia Mahalanobis de un jugador contra toda una población.
# Devuelve el df con columnas extra: dist_maha y similitud_pct
calcular_similares <- function(focal, poblacion, dims = DIMS_INDICES, top_n = 10) {
  
  # 1. Quitar filas con NA en cualquier dimensión
  pob_completa <- poblacion %>%
    filter(if_all(all_of(dims), ~ !is.na(.x)))
  
  if (nrow(pob_completa) < length(dims) + 2) return(NULL)
  
  mat <- as.matrix(pob_completa[, dims])
  
  # 2. Matriz de covarianza de la población de referencia
  S <- tryCatch(cov(mat), error = function(e) { NULL })
  if (is.null(S)) return(NULL)
  
  # Verificar que la matriz es invertible
  S_inv <- tryCatch(solve(S), error = function(e) { NULL })
  if (is.null(S_inv)) return(NULL)
  
  # 3. Vector del jugador focal
  focal_completo <- focal %>%
    filter(if_all(all_of(dims), ~ !is.na(.x))) %>%
    slice(1)
  if (nrow(focal_completo) == 0) return(NULL)
  
  vec_focal <- as.numeric(unlist(focal_completo[1, dims]))
  
  # 4. Distancia Mahalanobis de cada jugador de la población al focal
  # mahalanobis(x, center, cov, inverted) — usamos inverted=TRUE porque
  # ya tenemos S_inv directamente, evitando que R la recalcule internamente
  distancias <- tryCatch(
    mahalanobis(mat, center = vec_focal, cov = S_inv, inverted = TRUE),
    error = function(e) { NULL }
  )
  if (is.null(distancias)) return(NULL)
  
  pob_completa$dist_maha <- distancias
  
  # 5. Excluir al propio jugador focal
  resultado <- pob_completa %>%
    filter(jugador_id != focal_completo$jugador_id) %>%
    arrange(dist_maha) %>%
    slice_head(n = top_n)
  
  # 6. Convertir distancia en % de similitud (0-100)
  # dist_maha sigue una χ² con k grados de libertad (k = número de dims)
  # Usamos pchisq para obtener el percentil y lo invertimos:
  # similitud = 100 * (1 - pchisq(dist, df=k))
  # Un jugador idéntico tiene dist=0 → similitud=100%
  # Un jugador a 2σ en todas las dims → similitud cercana a 0%
  k <- length(dims)
  resultado$similitud_pct <- round(100 * (1 - pchisq(resultado$dist_maha, df = k)), 1)
  resultado$similitud_pct <- pmax(0, pmin(100, resultado$similitud_pct))
  
  resultado
}

# ── UI — Módulo 3 Similares ───────────────────────────────────
similaresPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado("Jugadores con un perfil parecido",
               "Similitud entre los cinco índices dentro de la misma posición. Haz clic en una fila para superponer su radar."),
    fluidRow(
      column(12,
             div(style = "display:flex; gap:12px; align-items:flex-end; margin-bottom:16px;",
                 div(style = "width:100px;",
                     numericInput(ns("top_n"), "Mostrar top", value = 10, min = 5, max = 20, step = 5)
                 ),
                 div(style = "width:180px;",
                     checkboxInput(ns("solo_cumple_minimo"), "Solo jugadores con minutos suficientes",
                                   value = TRUE)
                 )
             )
      )
    ),
    fluidRow(
      column(6,
             h4("Gemelos en su liga"),
             uiOutput(ns("aviso_liga")),
             DTOutput(ns("tabla_liga")),
             br(),
             plotlyOutput(ns("radar_liga"), height = "380px")
      ),
      column(6,
             h4("Gemelos históricos (todas las temporadas)"),
             uiOutput(ns("aviso_global")),
             DTOutput(ns("tabla_global")),
             br(),
             plotlyOutput(ns("radar_global"), height = "380px")
      )
    )
  )
}

# ── Server — Módulo 3 Similares ──────────────────────────────
similaresPageServer <- function(id, perfiles, jugador_id_sel, perfiles_hist = perfiles) {
  moduleServer(id, function(input, output, session) {
    
    focal <- reactive({
      jid <- jugador_id_sel()
      if (is.null(jid) || jid == "") return(NULL)
      perfiles %>% filter(jugador_id == jid) %>% slice(1)
    })
    
    # Población filtrada por minutos si se pide
    poblacion_base <- reactive({
      if (isTRUE(input$solo_cumple_minimo)) {
        filter(perfiles, cumple_minimo)
      } else {
        perfiles
      }
    })
    
    poblacion_hist <- reactive({
      if (isTRUE(input$solo_cumple_minimo)) filter(perfiles_hist, cumple_minimo)
      else perfiles_hist
    })

    # Similares en la temporada actual, MISMO SUBGRUPO. Antes se usaba
    # el grupo (DEF/MID/ATT) y un lateral podia salir "gemelo" de un
    # central: los z-scores se estandarizan por subgrupo, asi que
    # comparar entre subgrupos no tiene sentido.
    similares_liga <- reactive({
      j <- focal()
      if (is.null(j)) return(NULL)
      pob <- poblacion_base() %>% filter(subgrupo == j$subgrupo)
      calcular_similares(j, pob, top_n = input$top_n)
    })

    # Similares historicos: cada fila es un jugador en una temporada.
    # Se excluyen las otras temporadas del propio jugador.
    similares_global <- reactive({
      j <- focal()
      if (is.null(j)) return(NULL)
      pob <- poblacion_hist() %>%
        filter(subgrupo == j$subgrupo, player_id != j$player_id)
      calcular_similares(j, pob, top_n = input$top_n)
    })
    
    # ── Helper: tabla formateada de similares ─────────────────
    hacer_tabla <- function(similares, focal_row) {
      if (is.null(similares) || nrow(similares) == 0) return(NULL)
      
      df <- similares %>%
        transmute(
          Jugador    = nombre_display(jugador_id),
          Club       = club,
          Temporada  = temporada,
          Similitud  = paste0(similitud_pct, "%"),
          Ofensivo   = round(z_idx_ofensivo,    2),
          Creación   = round(z_idx_creacion,     2),
          Defensivo  = round(z_idx_defensivo,    2),
          Progresión = round(z_idx_progresion,   2),
          Consistencia = round(z_idx_consistencia, 2),
          Percentil  = paste0(percentil, "º")
        )
      
      datatable(df, rownames = FALSE, selection = "single",
                options = list(
                  dom = "t", paging = FALSE, ordering = TRUE,
                  scrollX = TRUE
                ),
                class = "compact stripe") %>%
        formatStyle("Similitud",
                    fontWeight = "bold", color = "#2E6DA4"
        )
    }
    
    # ── Helper: radar superpuesto focal + seleccionado ────────
    hacer_radar_superpuesto <- function(similares, focal_row, idx_sel) {
      if (is.null(similares) || nrow(similares) == 0 || is.null(focal_row)) return(NULL)
      
      # Tomar el jugador del índice seleccionado (o el primero si no hay selección)
      fila_sim <- if (!is.null(idx_sel) && length(idx_sel) > 0 && idx_sel <= nrow(similares)) {
        similares[idx_sel, ]
      } else {
        similares[1, ]
      }
      
      etiqs <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
      vals_focal <- as.numeric(unlist(focal_row[1, DIMS_INDICES]))
      vals_sim   <- as.numeric(unlist(fila_sim[1, DIMS_INDICES]))
      
      # Cerrar polígono
      etiqs_c    <- c(etiqs, etiqs[1])
      vals_f_c   <- c(vals_focal, vals_focal[1])
      vals_s_c   <- c(vals_sim,   vals_sim[1])
      
      plot_ly() %>%
        add_trace(
          type = "scatterpolar", mode = "lines+markers",
          r = vals_f_c, theta = etiqs_c, fill = "toself",
          name = nombre_display(focal_row$jugador_id),
          line = list(color = "#2E6DA4", width = 2),
          fillcolor = "rgba(46,109,164,0.16)",
          marker = list(color = "#2E6DA4", size = 5),
          hovertemplate = "<b>%{theta}</b><br>Z: %{r:.2f}<extra></extra>"
        ) %>%
        add_trace(
          type = "scatterpolar", mode = "lines+markers",
          r = vals_s_c, theta = etiqs_c, fill = "toself",
          name = nombre_display(fila_sim$jugador_id),
          line = list(color = "#B8862B", width = 2),
          fillcolor = "rgba(184,134,43,0.18)",
          marker = list(color = "#B8862B", size = 5),
          hovertemplate = "<b>%{theta}</b><br>Z: %{r:.2f}<extra></extra>"
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(
                min(-2.5, min(c(vals_focal, vals_sim), na.rm=TRUE) - 0.3),
                max(2.5,  max(c(vals_focal, vals_sim), na.rm=TRUE) + 0.3)
              )
            )
          ),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.15),
          margin = list(t = 20, b = 40, l = 40, r = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    }
    
    # ── Outputs: avisos ───────────────────────────────────────
    output$aviso_liga <- renderUI({
      j <- focal()
      if (is.null(j)) return(div(class="alert alert-info", "Selecciona un jugador."))
      s <- similares_liga()
      if (is.null(s) || nrow(s) == 0)
        return(div(class="alert alert-warning",
                   "Población insuficiente en esta liga para calcular similares."))
      div(class="alert alert-success", style="padding:6px 10px;",
          paste0("Comparando vs ", tolower(j$subgrupo_lab), "es de ", j$liga, " ", j$temporada,
                 " — haz clic en una fila para superponer el radar."))
    })
    
    output$aviso_global <- renderUI({
      j <- focal()
      if (is.null(j)) return(NULL)
      s <- similares_global()
      if (is.null(s) || nrow(s) == 0)
        return(div(class="alert alert-warning", "Población insuficiente para similares globales."))
      div(class="alert alert-info", style="padding:6px 10px;",
          paste0("Comparando vs ", tolower(j$subgrupo_lab), "es de todas las temporadas ",
                 "(cada fila es un jugador en una temporada)."))
    })
    
    # ── Outputs: tablas ───────────────────────────────────────
    output$tabla_liga <- renderDT({
      hacer_tabla(similares_liga(), focal())
    })
    
    output$tabla_global <- renderDT({
      hacer_tabla(similares_global(), focal())
    })
    
    # ── Outputs: radares superpuestos ─────────────────────────
    output$radar_liga <- renderPlotly({
      hacer_radar_superpuesto(
        similares_liga(), focal(),
        input$tabla_liga_rows_selected
      )
    })
    
    output$radar_global <- renderPlotly({
      hacer_radar_superpuesto(
        similares_global(), focal(),
        input$tabla_global_rows_selected
      )
    })
    
  })
}


# ── UI — Módulo 4 Rankings ───────────────────────────────────
rankingsPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado("Ranking por posición",
               "Ordenado por el z fiable: el índice compuesto contraído hacia la media según los minutos jugados."),
    fluidRow(
      column(12,
             div(style = "display:flex; gap:12px; align-items:flex-end; margin-bottom:16px;",
                 div(style = "width:170px;",
                     numericInput(ns("min_min"), "Minutos mínimos", value = MIN_MIN_RANKING,
                                  min = 0, step = 90)),
                 div(style = "color:#46566C; font-size:0.84em; padding-bottom:18px; max-width:620px;",
                     paste0("Orden por z compuesto con contracción por minutos (el que se validó en el ",
                            "backtest). El percentil es contra la población de referencia del subgrupo."))
             )
      )
    ),
    fluidRow(
      column(6,
             h4(uiOutput(ns("titulo_local"))),
             uiOutput(ns("badge_local")),
             br(),
             DTOutput(ns("tabla_local"))
      ),
      column(6,
             h4("Ranking histórico (todas las temporadas)"),
             uiOutput(ns("badge_global")),
             br(),
             DTOutput(ns("tabla_global"))
      )
    )
  )
}

# ── Server — Módulo 4 Rankings ───────────────────────────────
rankingsPageServer <- function(id, perfiles, jugador_id_sel, perfiles_hist = perfiles) {
  moduleServer(id, function(input, output, session) {
    
    focal <- reactive({
      jid <- jugador_id_sel()
      if (is.null(jid) || jid == "") return(NULL)
      perfiles %>% filter(jugador_id == jid) %>% slice(1)
    })
    
    min_rank <- reactive({ m <- input$min_min; if (is.null(m) || is.na(m)) MIN_MIN_RANKING else m })
    poblacion_base <- reactive({ filter(perfiles, minutos_total >= min_rank()) })
    
    # ── Helper: construir tabla de ranking ────────────────────
    hacer_ranking <- function(pob, focal_row) {
      if (is.null(focal_row) || nrow(pob) == 0) return(NULL)
      
      pob_pos <- pob %>%
        filter(subgrupo == focal_row$subgrupo) %>%
        arrange(desc(z_compuesto_fiable)) %>%
        mutate(
          es_focal  = jugador_id == focal_row$jugador_id & temporada == focal_row$temporada,
          Pos       = row_number(),
          Jugador   = nombre_display(jugador_id),
          Perfil    = subgrupo_lab,
          Club      = club,
          Temporada = temporada,
          `Min.`    = round(minutos_total),
          `Z fiable` = round(z_compuesto_fiable, 2),
          Percentil = paste0(percentil, "º"),
          Ofensivo  = round(z_idx_ofensivo,    2),
          Creación  = round(z_idx_creacion,     2),
          Defensivo = round(z_idx_defensivo,    2),
          Progresión= round(z_idx_progresion,   2),
          Consistencia = round(z_idx_consistencia, 2),
          Revulsivo = if (DIM_REV %in% names(.)) round(.data[[DIM_REV]], 2) else NA_real_
        ) %>%
        select(Pos, Jugador, Perfil, Club, Temporada, `Min.`, `Z fiable`, Percentil,
               Ofensivo, Creación, Defensivo, Progresión, Consistencia, Revulsivo, es_focal)
      
      # Fila del jugador focal para resaltar
      pos_focal <- which(pob_pos$es_focal)[1]
      fuera <- is.na(pos_focal)
      pob_pos <- pob_pos %>% select(-es_focal)
      fila_js <- if (fuera) "" else
        paste0("  $(this.api().row(", pos_focal - 1, ").node()).css('background-color','#E1ECF6').css('font-weight','bold');")
      
      dt <- datatable(
        pob_pos, rownames = FALSE,
        options = list(
          dom = "tp", pageLength = 20, ordering = TRUE,
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            fila_js,
            "}"
          )
        ),
        class = "compact stripe"
      ) %>%
        formatStyle("Percentil", fontWeight = "bold", color = "#2E6DA4")
      
      list(tabla = dt, pos = pos_focal, total = nrow(pob_pos), fuera = fuera,
           min_focal = round(focal_row$minutos_total))
    }
    
    ranking_local  <- reactive({
      j <- focal()
      if (is.null(j)) return(NULL)
      pob <- poblacion_base() %>% filter(liga == j$liga)
      hacer_ranking(pob, j)
    })
    
    ranking_global <- reactive({
      j <- focal()
      if (is.null(j)) return(NULL)
      hacer_ranking(filter(perfiles_hist, minutos_total >= min_rank()), j)
    })
    
    # ── Títulos dinámicos ─────────────────────────────────────
    output$titulo_local <- renderUI({
      j <- focal()
      if (is.null(j)) return("Ranking local")
      paste0("Ranking de ", tolower(j$subgrupo_lab), "es — ", j$liga, " ", j$temporada)
    })
    
    # ── Badges de posición ────────────────────────────────────
    hacer_badge <- function(r) {
      if (is.null(r)) return(NULL)
      if (isTRUE(r$fuera))
        return(div(class = "alert alert-warning", style = "padding:6px 12px; font-size:0.9em;",
                   paste0("El jugador seleccionado tiene ", r$min_focal, " minutos, por debajo del mínimo de ",
                          min_rank(), ": no entra a este ranking. Baja el mínimo para verlo.")))
      fila_kpi(tarjeta_kpi("Lugar del jugador seleccionado", paste0(r$pos, " de ", r$total),
                           paste0("está en el ", round(r$pos / r$total * 100), "% superior")))
    }
    
    output$badge_local  <- renderUI({ hacer_badge(ranking_local()) })
    output$badge_global <- renderUI({ hacer_badge(ranking_global()) })
    
    # ── Tablas ────────────────────────────────────────────────
    output$tabla_local <- renderDT({
      r <- ranking_local()
      if (is.null(r)) return(NULL)
      r$tabla
    })
    
    output$tabla_global <- renderDT({
      r <- ranking_global()
      if (is.null(r)) return(NULL)
      r$tabla
    })
    
  })
}