# ============================================================
# mod_clubes.R — CIDE OSOS
# Modulo 5 — Compatibilidad del jugador con los clubes de Liga MX
#
# Tres lecturas independientes, sin colapsarlas en un numero:
#   Afinidad  — que tan parecido es el sistema del club (posesion y
#               PPDA) al contexto donde juega hoy. Riesgo de adaptacion.
#   Necesidad — el club esta debil justo donde el jugador es fuerte,
#               dentro del MISMO subgrupo. Es la lectura accionable.
#   Estilo    — el perfil del jugador es del tipo que ese club
#               aprovecha (posesion -> creacion/progresion,
#               presion alta -> defensivo, juego directo -> ofensivo).
#
# Cambios frente a la version anterior:
#   - Se elimina el simulador entre ligas: con una sola liga no aplica.
#   - La afinidad usa solo posesion y PPDA. La version anterior
#     rellenaba intensidad / longitud de pase con el promedio de los
#     clubes y metia una distancia constante que aplastaba a todos.
#   - "Juego directo" sale de team_match_directness de StatsBomb.
# ============================================================

DIMS_FIT <- c("z_idx_ofensivo", "z_idx_creacion", "z_idx_defensivo",
              "z_idx_progresion", "z_idx_consistencia")

# --- A) Afinidad ----------------------------------------------
fit_afinidad <- function(focal, estilo_clubes) {
  sd_pos  <- sd(estilo_clubes$posesion_club, na.rm = TRUE)
  sd_ppda <- sd(estilo_clubes$ppda_club,     na.rm = TRUE)
  if (is.na(sd_pos)  || sd_pos  == 0) sd_pos  <- 1
  if (is.na(sd_ppda) || sd_ppda == 0) sd_ppda <- 1
  afinidad_contexto(focal$posesion_media[1], focal$ppda_media[1],
                    estilo_clubes$posesion_club, estilo_clubes$ppda_club,
                    sd_pos, sd_ppda)
}

# --- B) Necesidad ---------------------------------------------
perfil_subgrupo_clubes <- function(perfiles, subgrupo) {
  perfiles %>%
    filter(subgrupo == !!subgrupo, cumple_minimo) %>%
    group_by(club) %>%
    summarise(across(all_of(DIMS_FIT), ~ mean(.x, na.rm = TRUE)),
              n_sub = n(), .groups = "drop")
}

fit_necesidad <- function(focal, perf_sub, clubes) {
  z_jug <- as.numeric(focal[1, DIMS_FIT])
  vapply(clubes, function(cl) {
    fila <- perf_sub[perf_sub$club == cl, , drop = FALSE]
    z_club <- if (nrow(fila) == 0) rep(-1, length(DIMS_FIT))   # sin jugadores: hueco total
              else as.numeric(fila[1, DIMS_FIT])
    necesidad_club(z_jug, z_club)
  }, numeric(1))
}

# --- C) Estilo ------------------------------------------------
fit_estilo <- function(focal, estilo_clubes) {
  z_jug <- as.numeric(focal[1, DIMS_FIT]); names(z_jug) <- DIMS_FIT
  zc <- function(x) {
    s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x, na.rm = TRUE)) / s
  }
  z_pos  <- zc(estilo_clubes$posesion_club)
  z_ppda <- zc(estilo_clubes$ppda_club)       # alto = presiona poco
  z_dir  <- if (!all(is.na(estilo_clubes$directo_club))) zc(estilo_clubes$directo_club)
            else rep(0, nrow(estilo_clubes))
  z_dir[is.na(z_dir)] <- 0

  vapply(seq_len(nrow(estilo_clubes)), function(i) {
    deseo <- c(
      z_idx_ofensivo     = 0.35 * z_dir[i] + 0.15 * (-z_pos[i]),
      z_idx_creacion     = 0.45 * z_pos[i] - 0.20 * z_dir[i],
      z_idx_defensivo    = 0.40 * (-z_ppda[i]),
      z_idx_progresion   = 0.45 * z_pos[i],
      z_idx_consistencia = 0
    )
    pos <- pmax(0, deseo)
    if (sum(pos) == 0) return(50)
    w <- pos / sum(pos)
    round(pnorm(sum(w * z_jug[names(deseo)], na.rm = TRUE)) * 100, 1)
  }, numeric(1))
}

calcular_fit_clubes <- function(focal, estilo_clubes, perfiles, top_n = 18) {
  if (is.null(focal) || nrow(focal) == 0) return(NULL)
  if (is.null(estilo_clubes) || nrow(estilo_clubes) == 0) return(NULL)
  base <- estilo_clubes %>% filter(club != focal$club[1])
  if (nrow(base) == 0) return(NULL)

  base$afinidad_pct <- fit_afinidad(focal, base)
  base$estilo_pct   <- fit_estilo(focal, base)
  base$encaje_pct   <- fit_necesidad(focal, perfil_subgrupo_clubes(perfiles, focal$subgrupo[1]),
                                     base$club)
  base %>%
    mutate(fit_medio = (afinidad_pct + encaje_pct + estilo_pct) / 3) %>%
    arrange(desc(fit_medio)) %>%
    slice_head(n = top_n)
}

# ── UI ────────────────────────────────────────────────────────
clubesPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado("Clubes compatibles con el jugador",
               "Dónde encajaría el jugador seleccionado: afinidad de estilo, necesidad en su posición y nivel del plantel."),
    fluidRow(column(12, h4("Dependencia de contexto"), uiOutput(ns("tarjeta_dependencia")))),
    br(),
    fluidRow(column(12,
      h4(uiOutput(ns("titulo_liga"))),
      uiOutput(ns("aviso_liga")),
      DTOutput(ns("tabla_liga"))
    ))
  )
}

# ── Server ────────────────────────────────────────────────────
clubesPageServer <- function(id, perfiles, jugador_id_sel) {
  moduleServer(id, function(input, output, session) {

    focal <- reactive({
      jid <- jugador_id_sel()
      if (is.null(jid) || jid == "") return(NULL)
      perfiles %>% filter(jugador_id == jid) %>%
        slice_max(minutos_total, n = 1, with_ties = FALSE)
    })

    estilo_clubes <- reactive({ cargar_estilo_clubes() })

    fit_liga <- reactive({
      j <- focal()
      if (is.null(j)) return(NULL)
      calcular_fit_clubes(j, estilo_clubes(), perfiles)
    })

    output$tarjeta_dependencia <- renderUI({
      j <- focal()
      if (is.null(j)) return(div(class = "alert alert-info",
                                 "Selecciona un jugador para ver su dependencia de contexto."))
      clutch   <- j$clutch_index[1]
      reducida <- isTRUE(j$muestra_reducida_top20[1])
      cal_comp <- round(j$cal_comp_media[1], 1)
      dep_label <- if (!is.na(cal_comp)) {
        if (cal_comp >= 78) list(txt = "Contexto favorecedor", col = "#B8862B")
        else if (cal_comp >= 65) list(txt = "Contexto neutral", col = "#2E6DA4")
        else list(txt = "Contexto exigente", col = "#2E6DA4")
      } else NULL

      div(class = "well well-sm",
        tags$table(style = "width:100%; margin-bottom:8px;", tags$tr(
          tags$td(style = "width:25%; padding:4px;", tags$strong("Posesión media"), tags$br(),
                  paste0(round(j$posesion_media[1], 1), "%")),
          tags$td(style = "width:25%; padding:4px;", tags$strong("PPDA media"), tags$br(),
                  round(j$ppda_media[1], 2)),
          tags$td(style = "width:25%; padding:4px;", tags$strong("Nivel de su equipo"), tags$br(), cal_comp),
          tags$td(style = "width:25%; padding:4px;", tags$strong("Nivel medio del rival"), tags$br(),
                  round(j$cal_rival_media[1], 1)))),
        tags$hr(style = "margin:6px 0;"),
        tags$table(style = "width:100%;", tags$tr(
          tags$td(style = "width:33%; padding:4px;", tags$strong("Clutch index"), tags$br(),
                  if (!is.na(clutch)) tags$span(style = paste0("font-weight:bold; font-size:1.2em; color:",
                                                              if (clutch > 0) "#2E6DA4" else "#B23A3A", ";"),
                                               round(clutch, 3)) else "—"),
          tags$td(style = "width:33%; padding:4px;", tags$strong("Partidos vs top 20%"), tags$br(),
                  if (!is.na(j$n_partidos_niv_top20[1])) j$n_partidos_niv_top20[1] else "—",
                  if (reducida) tags$span(style = "color:#B8862B; margin-left:6px;", "⚠ muestra pequeña")),
          tags$td(style = "width:33%; padding:4px;", tags$strong("Perfil de contexto"), tags$br(),
                  if (!is.null(dep_label)) tags$span(
                    style = paste0("background:", dep_label$col,
                                   "; color:white; padding:2px 10px; border-radius:10px;"),
                    dep_label$txt) else "—"))),
        tags$p(style = "font-size:0.8em; color:#46566C; margin:8px 0 0 0;",
               "Nivel de equipo: diferencial de xG sin penaltis por partido, escala con media 70 ",
               "y 10 puntos por desviación estándar dentro del torneo.")
      )
    })

    output$titulo_liga <- renderUI({
      j <- focal()
      paste0("Clubes compatibles — Liga MX ", if (!is.null(j)) j$temporada[1] else "")
    })

    output$aviso_liga <- renderUI({
      j <- focal()
      if (is.null(j)) return(NULL)
      div(class = "alert alert-secondary", style = "padding:8px 12px; font-size:0.84em; line-height:1.5;",
          tags$strong("Tres lecturas de compatibilidad, sin mezclar: "), tags$br(),
          tags$b(style = "color:#1B3A63;", "Afinidad"),
          " — qué tan parecido es el sistema del club al contexto donde jugó (riesgo de adaptación). ", tags$br(),
          tags$b(style = "color:#B8862B;", "Necesidad"),
          " — el club es débil justo donde el jugador (", j$subgrupo_lab[1],
          ") es fuerte. Es la lectura más accionable. Necesidad 0 no significa que no haya hueco en ",
          "el segundo puesto: para eso está el módulo de Fichajes.", tags$br(),
          tags$b(style = "color:#1E7A5A;", "Estilo"),
          " — el perfil del jugador es del tipo que ese club sabe aprovechar.")
    })

    output$tabla_liga <- renderDT({
      fit <- fit_liga()
      if (is.null(fit) || nrow(fit) == 0) return(NULL)
      df <- fit %>% transmute(
        Club = club,
        Afinidad = round(afinidad_pct, 0), Necesidad = round(encaje_pct, 0),
        Estilo = round(estilo_pct, 0), Promedio = round(fit_medio, 0),
        `Posesión` = paste0(round(posesion_club, 1), "%"), PPDA = round(ppda_club, 2),
        Nivel = round(nivel_club, 0))
      datatable(df, rownames = FALSE,
                options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE,
                               columnDefs = list(list(className = "dt-center", targets = 1:4))),
                class = "compact stripe") %>%
        formatStyle("Afinidad", background = styleColorBar(c(0, 100), "#B9C7D6"),
                    backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Necesidad", background = styleColorBar(c(0, 100), "#EAD7A8"),
                    backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center", fontWeight = "bold") %>%
        formatStyle("Estilo", background = styleColorBar(c(0, 100), "#A9D2C2"),
                    backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Promedio", fontWeight = "bold", color = "#0E2240")
    })
  })
}
