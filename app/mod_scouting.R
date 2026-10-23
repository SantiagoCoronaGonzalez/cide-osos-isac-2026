# ============================================================
# mod_scouting.R
# Modulo 11 — Scouting (inverso de Clubes)
#
# Fijas un club y la app te devuelve, por cada perfil posicional
# que elijas, los mejores candidatos del mercado segun las mismas
# tres lecturas del modulo de Clubes:
#
#   Afinidad  — el jugador viene de un contexto parecido al del club
#   Necesidad — el club es debil en ese perfil y el jugador fuerte
#   Estilo    — el perfil del jugador es del tipo que el club aprovecha
#
# Reutiliza DIMS_FIT y la paleta definidos en mod_clubes_simulador.R
# (ese archivo se carga antes en app.R). Aqui se implementan las
# versiones "un club contra muchos jugadores".
# ============================================================

# Paleta (por si este archivo se carga aislado)
if (!exists("fic_navy")) {
  fic_navy <- "#0E2240"; fic_navy_soft <- "#1B3A63"; fic_gold <- "#B8862B"
  fic_cream <- "#F3F5F8"; fic_gray <- "#46566C"; fic_gray_light <- "#DCE1E8"
  fic_green <- "#1E7A5A"; fic_red <- "#B23A3A"; fic_white <- "#FFFFFF"
}
if (!exists("DIMS_FIT")) {
  DIMS_FIT <- c("z_idx_ofensivo","z_idx_creacion","z_idx_defensivo",
                "z_idx_progresion","z_idx_consistencia")
}
if (!exists("SUBS_ORDEN")) {
  SUBS_ORDEN <- c("CB","FB","DM","CM","AM","W","ST")
  SUBS_ETIQ  <- c(CB = "Centrales", FB = "Laterales", DM = "Pivotes",
                  CM = "Interiores", AM = "Mediapuntas",
                  W  = "Extremos",  ST = "Delanteros centro")
}

# ── Estilo de UN club (una fila de estilo_clubes) ─────────────
estilo_de_club <- function(estilos, club) {
  fila <- estilos[estilos$club == club, , drop = FALSE]
  if (nrow(fila) == 0) return(NULL)
  fila[1, ]
}

# ── Necesidad del club en un subgrupo (vector de 5 huecos) ────
# hueco_k = max(0, -z_promedio_del_club_en_ese_indice)
huecos_club_subgrupo <- function(perfiles, club, subgrupo) {
  plantel <- perfiles %>%
    dplyr::filter(club == !!club, subgrupo == !!subgrupo, cumple_minimo)
  if (nrow(plantel) == 0) {
    # sin jugadores de ese perfil: hueco maximo en todo
    return(setNames(rep(1.0, length(DIMS_FIT)), DIMS_FIT))
  }
  z_club <- vapply(DIMS_FIT,
                   function(d) mean(plantel[[d]], na.rm = TRUE),
                   numeric(1))
  z_club[is.na(z_club)] <- 0
  out <- pmax(0, -z_club)
  setNames(as.numeric(out), DIMS_FIT)
}

# ── A) Afinidad: contexto del jugador vs el club fijo ─────────
afinidad_jugadores <- function(cand, club_row, estilos) {
  # normalizacion con los sigma del conjunto de clubes
  sd_pos  <- sd(estilos$posesion_club, na.rm = TRUE); sd_pos  <- ifelse(is.na(sd_pos)||sd_pos==0,1,sd_pos)
  sd_ppda <- sd(estilos$ppda_club,     na.rm = TRUE); sd_ppda <- ifelse(is.na(sd_ppda)||sd_ppda==0,1,sd_ppda)
  
  d_pos  <- (cand$posesion_media - club_row$posesion_club) / sd_pos
  d_ppda <- (cand$ppda_media     - club_row$ppda_club)     / sd_ppda
  dist   <- sqrt(pmax(0, d_pos^2 + d_ppda^2))
  pmax(0, pmin(100, round(100 * exp(-dist / sqrt(2)), 1)))
}

# ── B) Necesidad: fortaleza del jugador ponderada por hueco ───
# Suma columna a columna en vez de %*% para evitar cualquier
# incompatibilidad de dimensiones o de tipo en la matriz.
necesidad_jugadores <- function(cand, huecos) {
  # Alinear huecos a DIMS_FIT, longitud fija 5, rellenando con 0
  h <- setNames(rep(0, length(DIMS_FIT)), DIMS_FIT)
  if (!is.null(names(huecos))) {
    comunes <- intersect(names(huecos), DIMS_FIT)
    if (length(comunes) > 0) h[comunes] <- as.numeric(huecos[comunes])
  } else if (length(huecos) > 0) {
    m <- min(length(huecos), length(DIMS_FIT))
    h[seq_len(m)] <- as.numeric(huecos[seq_len(m)])
  }
  
  n <- nrow(cand)
  score <- numeric(n)
  for (k in seq_along(DIMS_FIT)) {
    col <- suppressWarnings(as.numeric(cand[[DIMS_FIT[k]]]))
    col[is.na(col)] <- 0
    score <- score + pmax(0, col) * h[k]
  }
  round(pmax(0, pmin(100, 100 * (1 - exp(-score / 1.5)))), 1)
}

# ── C) Estilo: perfil deseado del club vs perfil del jugador ──
estilo_jugadores <- function(cand, club_row, estilos) {
  zc <- function(x, v) {
    s <- sd(x, na.rm = TRUE); if (is.na(s)||s==0) return(0)
    (v - mean(x, na.rm = TRUE)) / s
  }
  z_pos   <- zc(estilos$posesion_club, club_row$posesion_club)
  z_ppda  <- zc(estilos$ppda_club,     club_row$ppda_club)
  # Juego directo: team_match_directness de StatsBomb
  z_largo <- if ("directo_club" %in% names(estilos) &&
                 !all(is.na(estilos$directo_club)))
    zc(estilos$directo_club, club_row$directo_club) else 0
  if (is.na(z_largo)) z_largo <- 0
  
  deseo <- c(
    z_idx_ofensivo     =  0.35 * z_largo + 0.15 * (-z_pos),
    z_idx_creacion     =  0.45 * z_pos   - 0.20 * z_largo,
    z_idx_defensivo    =  0.40 * (-z_ppda),
    z_idx_progresion   =  0.45 * z_pos,
    z_idx_consistencia =  0
  )
  pos <- pmax(0, deseo)
  if (sum(pos) == 0) return(rep(50, nrow(cand)))
  w <- pos / sum(pos)
  
  n <- nrow(cand)
  match_z <- numeric(n)
  for (k in seq_along(DIMS_FIT)) {
    col <- suppressWarnings(as.numeric(cand[[DIMS_FIT[k]]]))
    col[is.na(col)] <- 0
    match_z <- match_z + col * w[k]
  }
  round(pmax(0, pmin(100, pnorm(match_z) * 100)), 1)
}

# ── Orquestador: candidatos para (club, subgrupo) ─────────────
scouting_subgrupo <- function(perfiles, estilos, club, subgrupo,
                              excluir_liga = FALSE, top_n = 10) {
  club_row <- estilo_de_club(estilos, club)
  if (is.null(club_row)) return(NULL)
  liga_club <- club_row$liga
  
  cand <- perfiles %>%
    dplyr::filter(subgrupo == !!subgrupo, cumple_minimo, club != !!club)
  if (isTRUE(excluir_liga)) cand <- cand %>% dplyr::filter(liga != liga_club)
  if (nrow(cand) == 0) return(NULL)
  
  huecos <- huecos_club_subgrupo(perfiles, club, subgrupo)
  
  cand$afinidad_pct  <- afinidad_jugadores(cand, club_row, estilos)
  cand$necesidad_pct <- necesidad_jugadores(cand, huecos)
  cand$estilo_pct    <- estilo_jugadores(cand, club_row, estilos)
  cand$fit_medio     <- round(rowMeans(
    cbind(cand$afinidad_pct, cand$necesidad_pct, cand$estilo_pct), na.rm = TRUE), 1)
  
  cand %>%
    dplyr::arrange(dplyr::desc(fit_medio)) %>%
    dplyr::slice_head(n = top_n)
}

# ── UI ────────────────────────────────────────────────────────
scoutingPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("Candidatos por posición"),
        tags$p(paste0("Fija un club y elige los perfiles que quieras reforzar. ",
                      "La app rankea el mercado con las mismas tres lecturas del ",
                      "módulo de Clubes: afinidad, necesidad y estilo."))
    ),
    
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(
          column(3, selectInput(ns("liga"), "Liga del club",
                                choices = NULL, width = "100%")),
          column(3, selectInput(ns("club"), "Club a reforzar",
                                choices = NULL, width = "100%")),
          column(2, numericInput(ns("top_n"), "Top por perfil",
                                 value = 8, min = 3, max = 20, step = 1, width = "100%")),
          column(2, numericInput(ns("edad_max"), "Edad máxima",
                                 value = 40, min = 16, max = 40, step = 1, width = "100%")),
          column(2,
                 tags$label("Mercado", style = "font-weight:700; font-size:0.85em;"),
                 tags$div(style = "padding-top:4px;",
                          checkboxInput(ns("solo_extranjeros"),
                                        "Solo extranjeros", value = FALSE)))
        ),
        tags$hr(style = "margin:10px 0;"),
        tags$label("Perfiles a reforzar",
                   style = "font-weight:700; font-size:0.85em; display:block; margin-bottom:6px;"),
        uiOutput(ns("chks_subgrupos"))
    ),
    
    uiOutput(ns("ficha_club")),
    br(),
    uiOutput(ns("resultados")),
    br(), br()
  )
}

# ── Server ────────────────────────────────────────────────────
scoutingPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    estilos <- reactive({
      ec <- cargar_estilo_clubes()
      if (is.null(ec)) {
        perfiles %>% dplyr::group_by(club, liga) %>%
          dplyr::summarise(posesion_club = mean(posesion_media, na.rm=TRUE),
                           ppda_club = mean(ppda_media, na.rm=TRUE),
                           directo_club = NA_real_, n_partidos_eq = dplyr::n(),
                           .groups="drop") %>%
          dplyr::filter(!is.na(posesion_club))
      } else ec
    })
    
    observe({
      updateSelectInput(session, "liga", choices = sort(unique(perfiles$liga)))
    })
    observe({
      req(input$liga)
      clubes <- perfiles %>% dplyr::filter(liga == input$liga) %>%
        dplyr::pull(club) %>% unique() %>% sort()
      updateSelectInput(session, "club", choices = clubes)
    })
    
    # Casillas de subgrupo — marcadas por defecto donde el club tiene jugadores
    output$chks_subgrupos <- renderUI({
      req(input$club)
      subs_club <- perfiles %>%
        dplyr::filter(club == input$club, cumple_minimo) %>%
        dplyr::pull(subgrupo) %>% unique()
      div(style = "display:flex; flex-wrap:wrap; gap:16px;",
          lapply(SUBS_ORDEN, function(sg) {
            tags$label(style = "font-weight:400; font-size:0.9em; cursor:pointer;
                              display:flex; align-items:center; gap:5px; margin:0;",
                       tags$input(type = "checkbox", class = "sg-check",
                                  id = ns(paste0("sg_", sg)),
                                  checked = if (sg %in% subs_club) "checked" else NULL,
                                  onclick = sprintf("Shiny.setInputValue('%s', document.getElementById('%s').checked, {priority:'event'})",
                                                    ns("sg_toggle"), ns(paste0("sg_", sg)))),
                       SUBS_ETIQ[sg])
          })
      )
    })
    
    # Lee el estado de las 7 casillas
    subs_sel <- reactive({
      req(input$club)
      sel <- SUBS_ORDEN[vapply(SUBS_ORDEN,
                               function(sg) isTRUE(input[[paste0("sg_", sg)]]), logical(1))]
      if (length(sel) == 0) {
        # default: los subgrupos presentes en el club
        sel <- perfiles %>% dplyr::filter(club == input$club, cumple_minimo) %>%
          dplyr::pull(subgrupo) %>% unique()
      }
      SUBS_ORDEN[SUBS_ORDEN %in% sel]
    })
    
    # ── Ficha del club: perfil de necesidad por subgrupo ──────
    output$ficha_club <- renderUI({
      req(input$club)
      est <- estilo_de_club(estilos(), input$club)
      if (is.null(est)) return(NULL)
      
      # z medio del club por subgrupo (para pintar donde flojea)
      resumen <- lapply(SUBS_ORDEN, function(sg) {
        pl <- perfiles %>% dplyr::filter(club == input$club, subgrupo == sg, cumple_minimo)
        z  <- if (nrow(pl) == 0) NA_real_ else mean(pl$z_compuesto, na.rm = TRUE)
        n  <- nrow(pl)
        list(sg = sg, z = z, n = n)
      })
      
      celda <- function(r) {
        col <- if (is.na(r$z)) "#B23A3A"
        else if (r$z < -0.3) "#B8862B"
        else if (r$z > 0.3) "#1E7A5A" else "#46566C"
        txt <- if (is.na(r$z)) "sin jugador" else sprintf("%+.2f", r$z)
        tags$td(style = "padding:11px 8px; text-align:center; vertical-align:top;
                         border-left:1px solid #DCE1E8;",
                tags$div(style = "font-size:0.8em; color:#46566C; margin-bottom:5px; line-height:1.2;",
                         SUBS_ETIQ[r$sg]),
                tags$div(style = paste0("font-size:1.15em; font-weight:700; color:", col, ";"), txt),
                tags$div(style = "font-size:0.72em; color:#9AA0A6; margin-top:2px;",
                         paste0("n=", r$n))
        )
      }
      
      div(style = "border:1px solid #DCE1E8; background:#FFFFFF;",
          tags$div(style = "background:#0E2240; color:#FFFFFF; padding:12px 18px;
                          display:flex; justify-content:space-between; align-items:baseline;",
                   tags$span(style = "font-weight:700; font-size:1.15em;", input$club),
                   tags$span(style = "font-size:0.85em; color:#B8862B;",
                             paste0("Posesión ", round(est$posesion_club,1), "%  ·  PPDA ",
                                    round(est$ppda_club,1)))),
          tags$div(style = "font-size:0.8em; color:#46566C; padding:8px 16px 0 16px;",
                   "Nivel medio del plantel por perfil (z compuesto)"),
          tags$table(style = "width:100%; border-collapse:collapse;",
                     tags$tr(lapply(resumen, celda)))
      )
    })
    
    # ── Resultados: un bloque de tabla por subgrupo ───────────
    output$resultados <- renderUI({
      req(input$club)
      subs <- subs_sel()
      if (length(subs) == 0)
        return(div(class = "alert alert-info", "Marca al menos un perfil."))
      tagList(lapply(subs, function(sg) {
        tagList(
          tags$h4(SUBS_ETIQ[sg],
                  style = "font-weight:700; color:#0E2240; margin:26px 0 4px 0;
                           border-bottom:2px solid #B8862B; padding-bottom:4px;"),
          DTOutput(ns(paste0("tab_", sg)))
        )
      }))
    })
    
    observe({
      req(input$club)
      subs <- subs_sel()
      for (sg in subs) {
        local({
          sg_l <- sg
          output[[paste0("tab_", sg_l)]] <- renderDT({
            pool <- perfiles %>%
              dplyr::filter(is.na(edad) | edad <= input$edad_max)
            if (isTRUE(input$solo_extranjeros)) pool <- dplyr::filter(pool, extranjero)
            # Las referencias del club (huecos) se calculan con la plantilla completa
            pool <- dplyr::bind_rows(pool, dplyr::filter(perfiles, club == input$club)) %>%
              dplyr::distinct(jugador_id, club, .keep_all = TRUE)
            cand <- scouting_subgrupo(pool, estilos(), input$club, sg_l,
                                      excluir_liga = FALSE,
                                      top_n = input$top_n)
            if (is.null(cand) || nrow(cand) == 0)
              return(datatable(data.frame(Aviso = "Sin candidatos"),
                               rownames = FALSE, options = list(dom = "t")))
            
            df <- cand %>%
              dplyr::transmute(
                Jugador   = nombre_display(jugador_id),
                Club      = club,
                Edad      = round(edad, 0),
                Nac.      = nacionalidad,
                `Min.`    = minutos_total,
                Afinidad  = round(afinidad_pct, 0),
                Necesidad = round(necesidad_pct, 0),
                Estilo    = round(estilo_pct, 0),
                Promedio  = round(fit_medio, 0),
                `Perc.`   = paste0(percentil, "\u00ba")
              )
            
            datatable(df, rownames = FALSE,
                      options = list(dom = "t", paging = FALSE, ordering = TRUE,
                                     scrollX = TRUE,
                                     columnDefs = list(list(className = "dt-center",
                                                            targets = 5:8))),
                      class = "compact stripe") %>%
              formatStyle("Afinidad",
                          background = styleColorBar(c(0,100), "#B9C7D6"),
                          backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                          backgroundPosition = "center") %>%
              formatStyle("Necesidad",
                          background = styleColorBar(c(0,100), "#EAD7A8"),
                          backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                          backgroundPosition = "center", fontWeight = "bold") %>%
              formatStyle("Estilo",
                          background = styleColorBar(c(0,100), "#A9D2C2"),
                          backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                          backgroundPosition = "center") %>%
              formatStyle("Promedio", fontWeight = "bold", color = "#0E2240")
          })
        })
      }
    })
    
  })
}