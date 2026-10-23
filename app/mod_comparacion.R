# ============================================================
# mod_comparacion.R
# Modulo 12 — Comparacion de jugadores
#
# Dos jugadores CUALESQUIERA de toda la base (distinto club, liga,
# lo que sea), cada uno con su cascada liga -> club -> jugador.
#
# Dos bloques, ambos visibles:
#   BLOQUE 1 — comparacion actual: radar + barras + tabla con los
#              z-score tal como rinde cada uno en su contexto.
#   BLOQUE 2 — comparacion proyectada a un club destino: se
#              proyecta a AMBOS a ese club (re-escalado por liga +
#              ajuste por estilo, misma logica que fichajes) y se
#              enfrentan en igualdad de contexto. Aparece cuando
#              eliges club destino; si no, muestra un aviso.
#
# Reutiliza proyectar_fichaje() y estilo_por_club() de mod_fichajes.R
# (cargado antes en app.R) para no duplicar la matematica.
# ============================================================

library(ggplot2)

if (!exists("cmp_navy")) {
  cmp_navy  <- "#0E2240"; cmp_gold <- "#B8862B"; cmp_gray <- "#46566C"
  cmp_gray_l <- "#DCE1E8"; cmp_cream <- "#F3F5F8"; cmp_white <- "#FFFFFF"
  cmp_azul  <- "#1B3A63"
}

# ── Radar superpuesto de dos vectores de indices ──────────────
radar_dos <- function(va, vb, nombre_a, nombre_b) {
  etiqs <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
  etiqs_c <- c(etiqs, etiqs[1])
  va_c <- c(va, va[1]); vb_c <- c(vb, vb[1])
  rango <- max(2.5, max(abs(c(va, vb)), na.rm = TRUE) + 0.4)
  
  plot_ly() %>%
    add_trace(type = "scatterpolar", mode = "lines+markers",
              r = va_c, theta = etiqs_c, name = nombre_a,
              line = list(color = cmp_navy, width = 2.5),
              fill = "toself", fillcolor = "rgba(14,34,64,0.12)",
              marker = list(color = cmp_navy, size = 7),
              hovertemplate = "<b>%{theta}</b><br>Z: %{r:.2f}<extra></extra>") %>%
    add_trace(type = "scatterpolar", mode = "lines+markers",
              r = vb_c, theta = etiqs_c, name = nombre_b,
              line = list(color = cmp_gold, width = 2.5),
              fill = "toself", fillcolor = "rgba(184,134,43,0.18)",
              marker = list(color = cmp_gold, size = 7),
              hovertemplate = "<b>%{theta}</b><br>Z: %{r:.2f}<extra></extra>") %>%
    layout(
      polar = list(bgcolor = cmp_white,
                   radialaxis = list(visible = TRUE, range = c(-rango, rango),
                                     gridcolor = cmp_gray_l, linecolor = cmp_gray_l,
                                     tickfont = list(size = 10, color = cmp_gray),
                                     tickvals = c(-2,-1,0,1,2)),
                   angularaxis = list(gridcolor = cmp_gray_l, linecolor = cmp_gray_l,
                                      tickfont = list(size = 12, color = cmp_navy))),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.08, x = 0,
                    font = list(size = 12, color = cmp_navy)),
      margin = list(t = 30, b = 60, l = 60, r = 60),
      paper_bgcolor = cmp_white, plot_bgcolor = cmp_white)
}

# ── Barras enfrentadas de dos vectores ────────────────────────
barras_dos <- function(va, vb, nombre_a, nombre_b, subtitulo) {
  etiqs <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
  df <- data.frame(
    indice = factor(rep(etiqs, 2), levels = rev(etiqs)),
    jug    = c(rep(nombre_a, length(etiqs)), rep(nombre_b, length(etiqs))),
    z      = c(va, vb))
  df$jug <- factor(df$jug, levels = c(nombre_a, nombre_b))
  rango <- max(2.2, max(abs(c(va, vb)), na.rm = TRUE) + 0.5)
  
  ggplot(df, aes(x = z, y = indice, fill = jug)) +
    geom_vline(xintercept = 0, color = cmp_gray_l, linewidth = 0.5) +
    geom_col(position = position_dodge(width = 0.62), width = 0.55) +
    geom_text(aes(label = sprintf("%+.2f", z),
                  hjust = ifelse(z >= 0, -0.15, 1.15)),
              position = position_dodge(width = 0.62),
              size = 3.2, color = cmp_navy) +
    scale_fill_manual(values = setNames(c(cmp_navy, cmp_gold), c(nombre_a, nombre_b))) +
    scale_x_continuous(limits = c(-rango, rango)) +
    labs(title = "Z-score por índice", subtitle = subtitulo,
         x = "Z-score", y = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title          = element_text(size = 14, face = "bold", color = cmp_navy,
                                         margin = margin(b = 4)),
      plot.subtitle       = element_text(size = 10, color = cmp_gray,
                                         margin = margin(b = 14)),
      plot.title.position = "plot",
      axis.text.y         = element_text(size = 12, color = cmp_navy, face = "bold"),
      axis.text.x         = element_text(size = 9, color = cmp_azul),
      axis.ticks          = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.major.x  = element_line(color = cmp_gray_l, linewidth = 0.3),
      panel.grid.minor    = element_blank(),
      panel.background    = element_rect(fill = cmp_white, color = NA),
      plot.background     = element_rect(fill = cmp_white, color = NA),
      legend.position     = "top", legend.justification = "left",
      legend.text         = element_text(size = 11, color = cmp_navy),
      plot.margin         = margin(16, 24, 12, 16))
}

# ── Tabla de diferencias (devuelve un div de Shiny) ───────────
tabla_diferencias <- function(va, vb, nombre_a, nombre_b) {
  etq <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
  filas <- lapply(seq_along(etq), function(k) {
    d <- va[k] - vb[k]
    gana <- if (abs(d) < 0.05) "=" else if (d > 0) nombre_a else nombre_b
    col_g <- if (gana == nombre_a) cmp_navy else if (gana == nombre_b) cmp_gold else cmp_gray
    tags$tr(
      tags$td(style="padding:8px 14px; color:#0E2240;", etq[k]),
      tags$td(style="padding:8px 14px; text-align:right; color:#0E2240;", sprintf("%+.2f", va[k])),
      tags$td(style="padding:8px 14px; text-align:right; color:#0E2240;", sprintf("%+.2f", vb[k])),
      tags$td(style=paste0("padding:8px 14px; text-align:right; font-weight:700; color:", col_g, ";"),
              paste0(sprintf("%+.2f", d), "  ", gana)))
  })
  div(style = "border:1px solid #DCE1E8;",
      tags$table(style = "width:100%; border-collapse:collapse;",
                 tags$thead(tags$tr(style = "background:#0E2240; color:#FFFFFF;",
                                    tags$th(style="padding:9px 14px; text-align:left; font-size:0.82em;", "Indice"),
                                    tags$th(style="padding:9px 14px; text-align:right; font-size:0.82em;", nombre_a),
                                    tags$th(style="padding:9px 14px; text-align:right; font-size:0.82em;", nombre_b),
                                    tags$th(style="padding:9px 14px; text-align:right; font-size:0.82em;", "Diferencia"))),
                 tags$tbody(filas)))
}

# ── Cascada liga -> club -> jugador (helper de UI) ────────────
selector_jugador <- function(ns, sufijo, etiqueta, color) {
  div(style = paste0("flex:1; border-top:3px solid ", color, "; padding-top:10px;"),
      tags$div(style = paste0("font-weight:700; color:", color,
                              "; margin-bottom:8px; font-size:1.05em;"), etiqueta),
      selectInput(ns(paste0("liga_", sufijo)), "Liga", choices = NULL, width = "100%"),
      selectInput(ns(paste0("club_", sufijo)), "Club", choices = NULL, width = "100%"),
      selectizeInput(ns(paste0("jug_", sufijo)), "Jugador", choices = NULL, width = "100%",
                     options = list(placeholder = "Escribe para buscar...")))
}

# ── UI ────────────────────────────────────────────────────────
comparacionPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("Duelo de jugadores"),
        tags$p(paste0("Compara dos jugadores cualesquiera de la base. Opcionalmente, ",
                      "proyecta a ambos a un mismo club destino para enfrentarlos en ",
                      "igualdad de contexto."))
    ),
    
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        div(style = "display:flex; gap:24px;",
            selector_jugador(ns, "a", "Jugador A", cmp_navy),
            selector_jugador(ns, "b", "Jugador B", cmp_gold)),
        tags$hr(style = "margin:14px 0;"),
        div(style = "display:flex; gap:16px; align-items:flex-end;",
            tags$div(style = "font-size:0.85em; color:#46566C; padding-bottom:8px;
                          font-weight:600; white-space:nowrap;",
                     "Proyectar ambos a un club:"),
            div(style = "width:220px;",
                selectInput(ns("liga_dest"), "Liga destino",
                            choices = NULL, width = "100%")),
            div(style = "width:240px;",
                selectInput(ns("club_dest"), "Club destino",
                            choices = NULL, width = "100%")))
    ),
    
    uiOutput(ns("aviso")),
    uiOutput(ns("cabeceras")),
    
    # BLOQUE 1 — actual
    tags$h3("Comparación actual",
            style = "font-weight:700; color:#0E2240; margin:26px 0 4px 0;"),
    tags$p("Cada jugador con sus z-score reales, estandarizados dentro de su propio subgrupo.",
           style = "color:#46566C; font-size:0.86em; margin-bottom:14px;"),
    fluidRow(
      column(5, plotlyOutput(ns("radar_act"), height = "430px")),
      column(7, plotOutput(ns("barras_act"),  height = "430px"))),
    br(),
    uiOutput(ns("tabla_act")),
    
    tags$hr(style = "border-color:#DCE1E8; margin:34px 0;"),
    
    # BLOQUE 2 — proyectado
    tags$h3("Comparación proyectada al club destino",
            style = "font-weight:700; color:#0E2240; margin:8px 0 4px 0;"),
    uiOutput(ns("sub_proy")),
    uiOutput(ns("bloque_proy")),
    br(), br()
  )
}

# ── Server ────────────────────────────────────────────────────
comparacionPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    estilos <- reactive({ estilo_por_club(perfiles) })
    
    # ── Cascadas de ambos jugadores ───────────────────────────
    cascada <- function(sufijo) {
      observe({
        updateSelectInput(session, paste0("liga_", sufijo),
                          choices = sort(unique(perfiles$liga)))
      })
      observe({
        req(input[[paste0("liga_", sufijo)]])
        clubes <- perfiles %>%
          dplyr::filter(liga == input[[paste0("liga_", sufijo)]]) %>%
          dplyr::pull(club) %>% unique() %>% sort()
        updateSelectInput(session, paste0("club_", sufijo), choices = clubes)
      })
      observe({
        req(input[[paste0("club_", sufijo)]])
        d <- perfiles %>%
          dplyr::filter(club == input[[paste0("club_", sufijo)]], cumple_minimo) %>%
          dplyr::arrange(dplyr::desc(z_compuesto_fiable))
        if (nrow(d) == 0) {
          updateSelectizeInput(session, paste0("jug_", sufijo),
                               choices = character(0), server = TRUE)
          return()
        }
        etiqueta <- paste0(nombre_display(d$jugador_id), "  (", d$subgrupo_lab, ")")
        choices  <- setNames(as.character(d$jugador_id), etiqueta)
        updateSelectizeInput(session, paste0("jug_", sufijo),
                             choices = choices, server = TRUE)
      })
    }
    cascada("a"); cascada("b")
    
    # ── Cascada del club destino ──────────────────────────────
    observe({
      updateSelectInput(session, "liga_dest",
                        choices = c("(elige liga)" = "", sort(unique(perfiles$liga))))
    })
    observe({
      ld <- input$liga_dest
      if (is.null(ld) || ld == "") {
        updateSelectInput(session, "club_dest", choices = c("(elige club)" = ""))
        return()
      }
      clubes <- perfiles %>% dplyr::filter(liga == ld) %>%
        dplyr::pull(club) %>% unique() %>% sort()
      updateSelectInput(session, "club_dest",
                        choices = c("(elige club)" = "", clubes))
    })
    
    ja <- reactive({
      jid <- input$jug_a
      if (is.null(jid) || jid == "") return(NULL)
      r <- perfiles %>% dplyr::filter(jugador_id == jid) %>%
        dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE)
      if (nrow(r) == 0) NULL else r
    })
    jb <- reactive({
      jid <- input$jug_b
      if (is.null(jid) || jid == "") return(NULL)
      r <- perfiles %>% dplyr::filter(jugador_id == jid) %>%
        dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE)
      if (nrow(r) == 0) NULL else r
    })
    
    validos <- reactive({
      a <- ja(); b <- jb()
      !is.null(a) && !is.null(b) && nrow(a) > 0 && nrow(b) > 0 &&
        a$jugador_id[1] != b$jugador_id[1]
    })
    
    # ── Aviso ─────────────────────────────────────────────────
    output$aviso <- renderUI({
      a <- ja(); b <- jb()
      if (is.null(a) || is.null(b) || nrow(a) == 0 || nrow(b) == 0) {
        return(div(class = "alert alert-info",
                   "Elige un jugador en cada columna para comparar."))
      }
      if (a$jugador_id[1] == b$jugador_id[1])
        return(div(class = "alert alert-warning", "Selecciona dos jugadores distintos."))
      if (a$subgrupo[1] != b$subgrupo[1])
        return(div(class = "alert alert-info", style = "font-size:0.88em;",
                   paste0("Perfiles distintos: ", nombre_display(a$jugador_id), " es ",
                          tolower(a$subgrupo_lab[1]), " y ", nombre_display(b$jugador_id), " es ",
                          tolower(b$subgrupo_lab[1]),
                          ". Cada z-score se calcula contra su propio subgrupo, asi que ",
                          "la comparación es de percentil relativo, no de volumen bruto.")))
      NULL
    })
    
    # ── Cabeceras ─────────────────────────────────────────────
    output$cabeceras <- renderUI({
      if (!validos()) return(NULL)
      a <- ja(); b <- jb()
      tarjeta <- function(j, color) {
        div(style = "flex:1; border:1px solid #DCE1E8;",
            tags$div(style = paste0("background:", color, "; color:#FFFFFF; padding:10px 16px;"),
                     tags$span(style = "font-weight:700; font-size:1.1em;",
                               nombre_display(j$jugador_id))),
            tags$table(style = "width:100%; font-size:0.86em;",
                       tags$tr(tags$td(style="color:#46566C; padding:6px 16px;", "Club / Liga"),
                               tags$td(style="text-align:right; padding:6px 16px; color:#0E2240;",
                                       paste0(j$club, " · ", j$liga))),
                       tags$tr(tags$td(style="color:#46566C; padding:6px 16px;", "Perfil"),
                               tags$td(style="text-align:right; padding:6px 16px; color:#0E2240;", j$subgrupo_lab)),
                       tags$tr(tags$td(style="color:#46566C; padding:6px 16px;", "Percentil compuesto"),
                               tags$td(style="text-align:right; padding:6px 16px; font-weight:700; color:#0E2240;",
                                       paste0(j$percentil, "\u00ba"))),
                       tags$tr(tags$td(style="color:#46566C; padding:6px 16px;", "Min · Goles · Asist."),
                               tags$td(style="text-align:right; padding:6px 16px; color:#0E2240;",
                                       paste0(j$minutos_total, " · ", j$goles_tot, " · ", j$asistencias_tot)))))
      }
      div(style = "display:flex; gap:14px; margin-top:6px;",
          tarjeta(a, cmp_navy), tarjeta(b, cmp_gold))
    })
    
    # ── BLOQUE 1: actual ──────────────────────────────────────
    va_act <- reactive({ req(validos()); as.numeric(ja()[1, DIMS_INDICES]) })
    vb_act <- reactive({ req(validos()); as.numeric(jb()[1, DIMS_INDICES]) })
    
    output$radar_act <- renderPlotly({
      req(validos())
      radar_dos(va_act(), vb_act(),
                nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id))
    })
    output$barras_act <- renderPlot({
      req(validos())
      barras_dos(va_act(), vb_act(),
                 nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id),
                 "Estandarizado dentro del subgrupo de cada jugador")
    }, res = 96)
    output$tabla_act <- renderUI({
      req(validos())
      tabla_diferencias(va_act(), vb_act(),
                        nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id))
    })
    
    # ── BLOQUE 2: proyeccion a club destino ───────────────────
    proy_a <- reactive({
      req(validos(), input$club_dest != "")
      proyectar_fichaje(ja(), input$club_dest, perfiles, estilos())
    })
    proy_b <- reactive({
      req(validos(), input$club_dest != "")
      proyectar_fichaje(jb(), input$club_dest, perfiles, estilos())
    })
    
    output$sub_proy <- renderUI({
      if (!validos()) return(NULL)
      if (is.null(input$club_dest) || input$club_dest == "")
        return(tags$p("Elige una liga y un club destino arriba para ver como quedaria el duelo alli.",
                      style = "color:#46566C; font-size:0.86em; margin-bottom:14px;"))
      tags$p(paste0("Ambos jugadores proyectados a ", input$club_dest,
                    ". Cada uno se estandariza contra su propio subgrupo en la liga ",
                    "destino y se ajusta por el estilo del club (posesión y presión)."),
             style = "color:#46566C; font-size:0.86em; margin-bottom:14px;")
    })
    
    output$bloque_proy <- renderUI({
      if (!validos()) return(NULL)
      if (is.null(input$club_dest) || input$club_dest == "") return(NULL)
      pa <- proy_a(); pb <- proy_b()
      if (is.null(pa) || is.null(pb))
        return(div(class = "alert alert-warning",
                   "Poblacion insuficiente en el club destino para proyectar a alguno de los dos."))
      tagList(
        fluidRow(
          column(5, plotlyOutput(ns("radar_proy"), height = "430px")),
          column(7, plotOutput(ns("barras_proy"),  height = "430px"))),
        br(),
        uiOutput(ns("tabla_proy")),
        br(),
        div(style = "background:#F3F5F8; border-left:3px solid #B8862B;
                     padding:12px 16px; font-size:0.83em; color:#0E2240; line-height:1.6;",
            tags$strong("Nota. "),
            paste0("La proyección re-escala los índices contra la liga destino y aplica ",
                   "un ajuste por estilo acotado a +-0.5 sigma. Es una heuristica para ",
                   "ordenar, no un modelo estimado. Si los dos jugadores son de subgrupos ",
                   "distintos, cada uno se compara contra su propia poblacion, no entre si ",
                   "en volumen bruto."))
      )
    })
    
    va_proy <- reactive({ req(proy_a()); as.numeric(proy_a()$z_estilo) })
    vb_proy <- reactive({ req(proy_b()); as.numeric(proy_b()$z_estilo) })
    
    output$radar_proy <- renderPlotly({
      req(proy_a(), proy_b())
      radar_dos(va_proy(), vb_proy(),
                nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id))
    })
    output$barras_proy <- renderPlot({
      req(proy_a(), proy_b())
      barras_dos(va_proy(), vb_proy(),
                 nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id),
                 paste0("Proyectado a ", input$club_dest))
    }, res = 96)
    output$tabla_proy <- renderUI({
      req(proy_a(), proy_b())
      tabla_diferencias(va_proy(), vb_proy(),
                        nombre_display(ja()$jugador_id), nombre_display(jb()$jugador_id))
    })
    
  })
}