# ============================================================
# mod_visualizaciones.R
# Modulo 9 — Exhibits de calidad consultoria
#
# Estructura de cada exhibit (formato MGI / McKinsey):
#   - Titulo = el hallazgo, no la etiqueta
#   - Subtitulo = que se grafica + unidades
#   - Grafica con etiquetas de dato directas
#   - Nota + fuente al pie
#   - Firma CIDE OSOS
#
#   E1 — Radar de indices (plotly)
#   E2 — Campanas independientes, una por indice
#   E3 — Ajuste contextual, escala libre por metrica
#   E4 — Posicion en la distribucion
#   E5 — Rendimiento por nivel de rival
#   E6 — Tabla de contexto
# ============================================================

library(ggplot2)

# ── Paleta Navy & Gold ────────────────────────────────────────
fia_navy       <- "#0E2240"
fia_navy_mid   <- "#1B3A63"
fia_navy_soft  <- "#1B3A63"
fia_gold       <- "#B8862B"
fia_gold_light <- "#F0D080"
fia_gold_pale  <- "#FAF0D0"
fia_cream      <- "#F3F5F8"
fia_white      <- "#FFFFFF"
fia_red        <- "#B23A3A"
fia_gray       <- "#46566C"
fia_gray_light <- "#DCE1E8"
fia_amber      <- "#B8862B"

FIRMA <- FIRMA_OSOS   # viene de R/funciones_modelo.R

# Envuelve titulos y subtitulos para que no se corten
env <- function(x, ancho = 58) paste(strwrap(x, width = ancho), collapse = "\n")

# ── Tema exhibit ──────────────────────────────────────────────
tema_exhibit <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      text                  = element_text(color = fia_navy),
      plot.title            = element_text(size = base + 4, face = "bold",
                                           color = fia_navy, lineheight = 1.15,
                                           margin = margin(b = 6)),
      plot.subtitle         = element_text(size = base - 1, color = fia_gray,
                                           lineheight = 1.3,
                                           margin = margin(b = 18)),
      plot.caption          = element_text(size = base - 4, color = fia_gray,
                                           hjust = 0, lineheight = 1.25,
                                           margin = margin(t = 16)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title            = element_text(size = base - 3, color = fia_gray),
      axis.text             = element_text(size = base - 3, color = fia_navy_soft),
      axis.ticks            = element_blank(),
      panel.grid.minor      = element_blank(),
      panel.grid.major      = element_line(color = fia_gray_light, linewidth = 0.35),
      panel.background      = element_rect(fill = fia_white, color = NA),
      plot.background       = element_rect(fill = fia_white, color = NA),
      strip.text            = element_text(size = base - 2, face = "bold",
                                           color = fia_navy, hjust = 0,
                                           margin = margin(b = 8, t = 6)),
      strip.background      = element_blank(),
      legend.position       = "top",
      legend.justification  = "left",
      legend.text           = element_text(size = base - 3, color = fia_navy_soft),
      legend.title          = element_blank(),
      legend.key.size       = unit(11, "pt"),
      legend.margin         = margin(b = 6),
      plot.margin           = margin(22, 28, 18, 22)
    )
}

# Pie de exhibit: nota + fuente + firma
pie_exhibit <- function(nota = NULL) {
  base <- paste0(
    env(paste0(fuente_texto(TEMPORADA_APP), " Métricas ajustadas por ",
               "contexto (posesión, PPDA, nivel del equipo y del rival) ",
               "mediante splines naturales."), 104),
    "\n", FIRMA)
  if (!is.null(nota)) paste0(env(nota, 104), "\n", base) else base
}

# ── E1: Radar de indices ──────────────────────────────────────
plot_radar_viz <- function(j, perfiles) {
  vals  <- as.numeric(unlist(j[1, DIMS_INDICES]))
  etiqs <- unname(ETIQUETAS_INDICES[DIMS_INDICES])
  
  sub   <- j$subgrupo[1]
  grupo <- SUBS_ETIQ[[sub]]
  lg    <- j$liga[1]
  
  pop <- perfiles %>%
    dplyr::filter(subgrupo == sub, liga == lg, cumple_minimo)
  
  medianas <- if (nrow(pop) >= 10) {
    as.numeric(sapply(DIMS_INDICES, function(d) median(pop[[d]], na.rm = TRUE)))
  } else {
    rep(0, length(DIMS_INDICES))
  }
  
  etiqs_c <- c(etiqs, etiqs[1])
  vals_c  <- c(vals,  vals[1])
  meds_c  <- c(medianas, medianas[1])
  
  rango <- max(2.5, max(abs(vals), na.rm = TRUE) + 0.5)
  
  plot_ly() %>%
    add_trace(
      type = "scatterpolar", mode = "lines",
      r = meds_c, theta = etiqs_c,
      name = paste0("Mediana de ", tolower(grupo), " de ", lg),
      line = list(color = fia_gray, width = 1.5, dash = "dot"),
      fill = "toself", fillcolor = "rgba(107,114,128,0.10)",
      hovertemplate = "<b>%{theta}</b><br>Mediana: %{r:.2f}<extra></extra>"
    ) %>%
    add_trace(
      type = "scatterpolar", mode = "lines+markers",
      r = vals_c, theta = etiqs_c,
      name = nombre_display(j$jugador_id),
      line = list(color = fia_gold, width = 3),
      fill = "toself", fillcolor = "rgba(201,168,76,0.25)",
      marker = list(color = fia_navy, size = 9,
                    line = list(color = fia_gold, width = 2)),
      hovertemplate = "<b>%{theta}</b><br>Z-score: %{r:.2f}<extra></extra>"
    ) %>%
    layout(
      polar = list(
        bgcolor = fia_white,
        radialaxis = list(
          visible = TRUE, range = c(-rango, rango),
          gridcolor = fia_gray_light, linecolor = fia_gray_light,
          tickfont = list(size = 11, color = fia_gray),
          tickvals = c(-2, -1, 0, 1, 2)
        ),
        angularaxis = list(
          gridcolor = fia_gray_light, linecolor = fia_gray_light,
          tickfont = list(size = 13, color = fia_navy)
        )
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.10, x = 0,
                    font = list(size = 12, color = fia_navy_soft)),
      margin = list(t = 50, b = 70, l = 70, r = 70),
      paper_bgcolor = fia_white, plot_bgcolor = fia_white
    )
}

# ── E2: Una campana independiente por indice ──────────────────
plot_campana_una <- function(j, perfiles, dim_idx) {
  sub   <- j$subgrupo[1]
  grupo <- SUBS_ETIQ[[sub]]
  lg    <- j$liga[1]
  
  pop <- perfiles %>%
    dplyr::filter(subgrupo == sub, liga == lg, cumple_minimo)
  if (nrow(pop) < 15) return(NULL)
  
  valores <- pop[[dim_idx]]
  valores <- valores[!is.na(valores)]
  if (length(valores) < 15) return(NULL)
  
  v_focal <- as.numeric(j[[dim_idx]][1])
  if (is.na(v_focal)) return(NULL)
  
  pct     <- round(pnorm(v_focal) * 100)
  etiqueta <- unname(ETIQUETAS_INDICES[dim_idx])
  
  # Densidad para poder sombrear el area bajo la curva hasta el jugador
  dens <- density(valores, n = 512)
  df_d <- data.frame(x = dens$x, y = dens$y)
  df_izq <- df_d[df_d$x <= v_focal, ]
  
  p50 <- median(valores, na.rm = TRUE)
  ymax <- max(df_d$y)
  
  # Lectura del percentil
  lectura <- if (pct >= 80) "Muy por encima del grupo"
  else if (pct >= 60) "Por encima del grupo"
  else if (pct >= 40) "En línea con el grupo"
  else if (pct >= 20) "Por debajo del grupo"
  else "Muy por debajo del grupo"
  
  ggplot() +
    geom_area(data = df_d, aes(x = x, y = y),
              fill = fia_cream, color = NA) +
    geom_area(data = df_izq, aes(x = x, y = y),
              fill = fia_gold_pale, color = NA) +
    geom_line(data = df_d, aes(x = x, y = y),
              color = fia_navy_soft, linewidth = 0.6) +
    geom_vline(xintercept = p50, color = fia_gray,
               linetype = "dashed", linewidth = 0.4) +
    geom_segment(aes(x = v_focal, xend = v_focal, y = 0, yend = ymax * 1.04),
                 color = fia_gold, linewidth = 1.4) +
    geom_point(aes(x = v_focal, y = ymax * 1.04),
               color = fia_navy, fill = fia_gold,
               shape = 21, size = 3.6, stroke = 1.2) +
    annotate("label", x = v_focal, y = ymax * 1.34,
             label = paste0(ifelse(v_focal >= 0, "+", ""),
                            round(v_focal, 2), "\u03c3   \u00b7   p", pct),
             color = fia_navy, fontface = "bold", size = 3.7, hjust = 0.5,
             fill = fia_white, label.size = 0, label.padding = unit(3, "pt")) +
    annotate("text", x = p50, y = -ymax * 0.12,
             label = "mediana", color = fia_gray, size = 2.9, hjust = 0.5) +
    scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3.4, 3.4)) +
    scale_y_continuous(expand = expansion(mult = c(0.16, 0.42))) +
    labs(
      title    = paste0(etiqueta, " — ", lectura),
      subtitle = paste0("Densidad de z-scores de ", tolower(grupo), " de ", lg,
                        " con ", MIN_MINUTOS, "+ minutos (n = ", length(valores),
                        "). Area dorada: jugadores por debajo del seleccionado"),
      x = "Z-score dentro del grupo posicional", y = NULL
    ) +
    tema_exhibit(base = 12) +
    theme(
      axis.text.y        = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = fia_gray_light, linewidth = 0.3),
      plot.margin        = margin(14, 28, 10, 22),
      plot.subtitle      = element_text(size = 10, color = fia_gray,
                                        margin = margin(b = 10)),
      plot.title         = element_text(size = 14, face = "bold",
                                        color = fia_navy, margin = margin(b = 4))
    )
}

# ── E3: Ajuste contextual, escala libre por metrica ───────────
plot_barras_metricas <- function(j) {
  specs <- list(
    list(n = "xG sin penaltis por 90", aj = "xg_p90_aj",         cr = "xg_p90_crudo"),
    list(n = "xA por 90",              aj = "xa_p90_aj",         cr = "xa_p90_crudo"),
    list(n = "Tiros por 90",           aj = "tiros_p90_aj",      cr = "tiros_p90_crudo"),
    list(n = "Progresiones al último tercio por 90", aj = "progresiones_p90_aj", cr = "progresiones_p90_crudo"),
    list(n = "Regates exitosos por 90", aj = "regates_p90_aj",   cr = "regates_p90_crudo"),
    list(n = "Presiones por 90",       aj = "presiones_p90_aj",  cr = "presiones_p90_crudo"),
    list(n = "Acciones defensivas sobre lo esperado por 90", aj = "acc_def_sobre_esp_p90_aj",
         cr = "acc_def_sobre_esp_p90_crudo"),
    list(n = "Pases por 90",           aj = "pases_p90_aj",      cr = "pases_p90_crudo")
  )
  
  df <- purrr::map_dfr(specs, function(s) {
    if (!(s$aj %in% names(j)) || !(s$cr %in% names(j))) return(NULL)
    aj <- as.numeric(j[[s$aj]][1])
    cr <- as.numeric(j[[s$cr]][1])
    if (is.na(aj) || is.na(cr)) return(NULL)
    data.frame(metrica = s$n, ajustado = aj, crudo = cr, delta = aj - cr)
  })
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df$metrica <- factor(df$metrica, levels = df$metrica)
  
  df_long <- tidyr::pivot_longer(df, c("crudo", "ajustado"),
                                 names_to = "tipo", values_to = "valor")
  df_long$tipo <- factor(df_long$tipo, levels = c("crudo", "ajustado"),
                         labels = c("Valor observado", "Ajustado por contexto"))
  
  df_lab <- df %>%
    dplyr::mutate(
      xmax  = pmax(ajustado, crudo),
      texto = paste0(ifelse(delta >= 0, "+", ""), round(delta, 3)),
      col   = ifelse(delta >= 0, fia_gold, fia_red)
    )
  
  n_sube <- sum(df$delta > 0)
  
  ggplot(df_long, aes(x = valor, y = tipo, fill = tipo)) +
    geom_col(width = 0.46) +
    geom_text(aes(label = formatC(valor, format = "f", digits = 2)),
              hjust = -0.22, size = 3.3, color = fia_navy_soft) +
    geom_text(data = df_lab,
              aes(x = xmax, y = 2.85, label = texto, color = col),
              inherit.aes = FALSE, hjust = 1, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Valor observado"       = fia_gray_light,
                                 "Ajustado por contexto" = fia_navy)) +
    scale_color_identity() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.40))) +
    scale_y_discrete(expand = expansion(add = c(0.65, 1.35))) +
    facet_wrap(~metrica, ncol = 2, scales = "free_x") +
    labs(
      title    = env(paste0("El contexto del equipo corrige al alza ", n_sube,
                            " de ", nrow(df), " métricas clave"), 62),
      subtitle = env(paste0("Valor observado frente a valor ajustado por contexto, ",
                            "por 90 minutos. Cada métrica en su propia escala. ",
                            "La cifra sobre cada par es la diferencia entre ambos"), 88),
      x = NULL, y = NULL,
      caption = pie_exhibit(
        paste0("Nota: una diferencia positiva indica que el contexto del equipo ",
               "estaba deprimiendo el registro observado del jugador."))
    ) +
    tema_exhibit() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_blank(),
      panel.spacing.y    = unit(20, "pt"),
      panel.spacing.x    = unit(34, "pt")
    )
}

# ── E4: Posicion en la distribucion ───────────────────────────
plot_distribucion <- function(j, perfiles) {
  sub   <- j$subgrupo[1]
  grupo <- SUBS_ETIQ[[sub]]
  lg    <- j$liga[1]
  
  pop <- perfiles %>%
    dplyr::filter(subgrupo == sub, liga == lg, cumple_minimo) %>%
    dplyr::mutate(es_focal = jugador_id == j$jugador_id[1])
  if (nrow(pop) < 5) return(NULL)
  
  niveles <- rev(unname(ETIQUETAS_INDICES[DIMS_INDICES]))
  
  df <- purrr::map_dfr(DIMS_INDICES, function(d) {
    data.frame(valor    = pop[[d]],
               indice   = unname(ETIQUETAS_INDICES[d]),
               es_focal = pop$es_focal,
               stringsAsFactors = FALSE)
  })
  df$indice <- factor(df$indice, levels = niveles)
  
  df_iqr <- df %>%
    dplyr::group_by(indice) %>%
    dplyr::summarise(p25 = quantile(valor, .25, na.rm = TRUE),
                     p50 = median(valor, na.rm = TRUE),
                     p75 = quantile(valor, .75, na.rm = TRUE),
                     .groups = "drop")
  
  df_f <- df %>%
    dplyr::filter(es_focal) %>%
    dplyr::mutate(pct = round(pnorm(valor) * 100))
  
  n_sobre <- sum(df_f$valor > 0, na.rm = TRUE)
  
  ggplot() +
    geom_segment(data = df_iqr,
                 aes(x = p25, xend = p75, y = indice, yend = indice),
                 color = fia_gray_light, linewidth = 7, lineend = "round") +
    geom_point(data = dplyr::filter(df, !es_focal),
               aes(x = valor, y = indice),
               color = fia_navy_soft, size = 1.7, alpha = 0.35,
               position = position_jitter(height = 0.15, seed = 1)) +
    geom_point(data = df_iqr, aes(x = p50, y = indice),
               shape = 124, size = 7, color = fia_navy) +
    geom_point(data = df_f, aes(x = valor, y = indice),
               color = fia_navy, fill = fia_gold,
               shape = 23, size = 5, stroke = 1.3) +
    geom_text(data = df_f,
              aes(x = valor, y = indice, label = paste0("p", pct)),
              vjust = -1.7, size = 3.4, fontface = "bold", color = fia_navy) +
    geom_vline(xintercept = 0, color = fia_gray,
               linetype = "dashed", linewidth = 0.4) +
    scale_x_continuous(expand = expansion(mult = c(0.06, 0.06))) +
    labs(
      title    = env(paste0(nombre_display(j$jugador_id),
                            " supera la media del grupo en ",
                            n_sobre, " de ", nrow(df_f), " índices"), 52),
      subtitle = env(paste0("Z-score de cada jugador dentro de ", tolower(grupo),
                            " de ", lg, " con ", MIN_MINUTOS, "+ minutos (n = ", nrow(pop), "). ",
                            "Rombo dorado: jugador seleccionado. ",
                            "Barra: rango intercuartil. Marca vertical: mediana"), 78),
      x = "Z-score dentro del grupo posicional", y = NULL,
      caption = pie_exhibit()
    ) +
    tema_exhibit() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = 12, color = fia_navy, face = "bold")
    )
}

# ── E5: Rendimiento por nivel de rival ────────────────────────
plot_clutch <- function(j) {
  xg_top <- j$xg_vs_top20[1]
  xg_mid <- if ("xg_vs_medio" %in% names(j)) j$xg_vs_medio[1] else NA
  xg_bot <- j$xg_vs_bottom20[1]
  n_top  <- j$n_partidos_niv_top20[1]
  n_bot  <- j$n_partidos_niv_bottom20[1]
  n_mid  <- if ("n_partidos_niv_medio" %in% names(j)) j$n_partidos_niv_medio[1] else NA
  clutch <- j$clutch_index[1]
  
  if (all(is.na(c(xg_top, xg_bot)))) return(NULL)
  
  df <- data.frame(
    nivel = c("Ante el 20% más fuerte", "Ante rivales medios", "Ante el 20% más débil"),
    xg    = c(xg_top, xg_mid, xg_bot),
    n     = c(n_top, n_mid, n_bot),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$xg), ]
  
  df$es_top <- df$nivel == "Ante el 20% más fuerte"
  df$etiq   <- ifelse(is.na(df$n), df$nivel,
                      paste0(df$nivel, "\n(", df$n, " partidos)"))
  df$etiq   <- factor(df$etiq, levels = rev(df$etiq))
  df$fill   <- ifelse(df$es_top, fia_gold, fia_navy_soft)
  
  titulo <- if (!is.na(clutch)) {
    if (clutch > 0.05) "El rendimiento ofensivo sube ante los rivales más fuertes"
    else if (clutch < -0.05) "El rendimiento ofensivo cae ante los rivales más fuertes"
    else "El rendimiento ofensivo se mantiene estable ante cualquier rival"
  } else "Rendimiento ofensivo por nivel de rival"
  
  sub <- paste0("xG ajustado por 90 minutos, según el quintil de calidad del rival ",
                "dentro de la liga del jugador")
  if (!is.na(clutch)) {
    sub <- paste0(sub, "\nIndice clutch: ", ifelse(clutch >= 0, "+", ""),
                  round(clutch, 3), " (diferencia entre el primer y el último quintil)")
  }
  
  nota <- if (isTRUE(j$muestra_reducida_top20[1])) {
    paste0("Nota: menos de cuatro partidos ante equipos del quintil superior. ",
           "El índice clutch debe interpretarse con cautela.")
  } else NULL
  
  ggplot(df, aes(x = xg, y = etiq, fill = fill)) +
    geom_col(width = 0.46) +
    scale_fill_identity() +
    geom_text(aes(label = sprintf("%.3f", xg)),
              hjust = -0.26, size = 4.2, fontface = "bold", color = fia_navy) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
    scale_y_discrete(expand = expansion(add = c(0.55, 0.55))) +
    labs(title = env(titulo, 56), subtitle = env(sub, 92),
         x = "xG ajustado por 90 minutos", y = NULL,
         caption = pie_exhibit(nota)) +
    tema_exhibit() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 12, color = fia_navy, face = "bold")
    )
}

# ── UI ────────────────────────────────────────────────────────
visualizacionesPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        uiOutput(ns("cabecera")),
    
    # E1 + E4
    fluidRow(
      column(5, plotlyOutput(ns("g_radar"), height = "540px")),
      column(7, plotOutput(ns("g_dist"),   height = "540px"))
    ),
    tags$hr(style = "border-color:#DCE1E8; margin:36px 0;"),
    
    # E2 — campanas, cada una a ancho completo
    fluidRow(column(12,
                    tags$h4("Distribución del grupo posicional, índice por índice",
                            style = "font-weight:700; color:#0E2240; margin-bottom:6px;"),
                    tags$p("Cada curva muestra como se reparten los z-scores de los jugadores del mismo puesto y liga. La línea dorada marca al jugador seleccionado.",
                           style = "color:#46566C; font-size:0.9em; margin-bottom:20px;")
    )),
    fluidRow(column(12, plotOutput(ns("g_camp_1"), height = "250px"))),
    fluidRow(column(12, plotOutput(ns("g_camp_2"), height = "250px"))),
    fluidRow(column(12, plotOutput(ns("g_camp_3"), height = "250px"))),
    fluidRow(column(12, plotOutput(ns("g_camp_4"), height = "250px"))),
    fluidRow(column(12, plotOutput(ns("g_camp_5"), height = "250px"))),
    fluidRow(column(12,
                    tags$p(paste0(fuente_texto(TEMPORADA_APP), " Métricas ajustadas por contexto mediante splines naturales. ", FIRMA),
                           style = "color:#46566C; font-size:0.75em; margin-top:10px;")
    )),
    tags$hr(style = "border-color:#DCE1E8; margin:36px 0;"),
    
    # E3
    fluidRow(column(12, plotOutput(ns("g_barras"), height = "880px"))),
    tags$hr(style = "border-color:#DCE1E8; margin:36px 0;"),
    
    # E5 + E6
    fluidRow(
      column(7, plotOutput(ns("g_clutch"), height = "400px")),
      column(5,
             tags$h4("Contexto de temporada",
                     style = "font-weight:700; color:#0E2240; margin-bottom:4px;"),
             tags$p("Condiciones en las que se produjeron los registros del jugador.",
                    style = "color:#46566C; font-size:0.85em; margin-bottom:16px;"),
             uiOutput(ns("tabla_contexto"))
      )
    ),
    tags$hr(style = "border-color:#DCE1E8; margin:36px 0;"),
    
    fluidRow(column(12,
                    div(style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap;
                   padding-bottom:32px;",
                        span(style = "font-size:0.85em; color:#46566C; margin-right:6px;",
                             "Exportar PNG 300 dpi:"),
                        downloadButton(ns("dl_campanas"), "Campanas",     class = "btn btn-sm btn-default"),
                        downloadButton(ns("dl_barras"),   "Ajuste",       class = "btn btn-sm btn-default"),
                        downloadButton(ns("dl_dist"),     "Distribucion", class = "btn btn-sm btn-default"),
                        downloadButton(ns("dl_clutch"),   "Nivel de rival", class = "btn btn-sm btn-default")
                    )
    ))
  )
}

# ── Server ────────────────────────────────────────────────────
visualizacionesPageServer <- function(id, perfiles, jugador_id_sel) {
  moduleServer(id, function(input, output, session) {
    
    j <- reactive({
      jid <- jugador_id_sel()
      if (is.null(jid) || jid == "") return(NULL)
      perfiles %>%
        dplyr::filter(jugador_id == jid) %>%
        dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE)
    })
    
    output$cabecera <- renderUI({
      jj <- j()
      if (is.null(jj)) {
        return(div(class = "alert alert-info",
                   "Selecciona un jugador en la barra superior."))
      }
      div(class = "osos-encabezado",
          tags$h2(nombre_display(jj$jugador_id)),
          tags$p(sprintf("%s del %s, %s minutos en %s partidos. Percentil %sº en su posición.",
                         jj$subgrupo_lab, jj$club, format(round(jj$minutos_total), big.mark = ","),
                         jj$n_partidos, jj$percentil)))
    })
    
    p_barras <- reactive({ req(j()); plot_barras_metricas(j()) })
    p_dist   <- reactive({ req(j()); plot_distribucion(j(), perfiles) })
    p_clutch <- reactive({ req(j()); plot_clutch(j()) })
    
    output$g_radar  <- renderPlotly({ req(j()); plot_radar_viz(j(), perfiles) })
    output$g_dist   <- renderPlot({ p_dist()   }, res = 96)
    output$g_barras <- renderPlot({ p_barras() }, res = 96)
    output$g_clutch <- renderPlot({ p_clutch() }, res = 96)
    
    # Campanas: una salida por indice
    output$g_camp_1 <- renderPlot({ req(j()); plot_campana_una(j(), perfiles, DIMS_INDICES[1]) }, res = 96)
    output$g_camp_2 <- renderPlot({ req(j()); plot_campana_una(j(), perfiles, DIMS_INDICES[2]) }, res = 96)
    output$g_camp_3 <- renderPlot({ req(j()); plot_campana_una(j(), perfiles, DIMS_INDICES[3]) }, res = 96)
    output$g_camp_4 <- renderPlot({ req(j()); plot_campana_una(j(), perfiles, DIMS_INDICES[4]) }, res = 96)
    output$g_camp_5 <- renderPlot({ req(j()); plot_campana_una(j(), perfiles, DIMS_INDICES[5]) }, res = 96)
    
    output$tabla_contexto <- renderUI({
      jj <- j()
      if (is.null(jj)) return(NULL)
      
      items <- list(
        list(l = "Posesión media del equipo",
             v = paste0(round(jj$posesion_media, 1), "%"),
             n = if (jj$posesion_media > 55) "Dominante"
             else if (jj$posesion_media < 45) "Reactivo" else "Equilibrado"),
        list(l = "PPDA media", v = round(jj$ppda_media, 1),
             n = if (jj$ppda_media < 8) "Presión alta"
             else if (jj$ppda_media > 12) "Bloque bajo" else "Moderada"),
        list(l = "Calidad de companeros", v = round(jj$cal_comp_media, 0),
             n = if (jj$cal_comp_media >= 85) "Plantel de elite"
             else if (jj$cal_comp_media >= 75) "Plantel alto" else "Plantel medio"),
        list(l = "Calidad media del rival", v = round(jj$cal_rival_media, 0),
             n = if (jj$cal_rival_media >= 78) "Calendario exigente" else "Calendario mixto"),
        list(l = "Disponibilidad", v = paste0(jj$disponibilidad, "%"),
             n = if (isTRUE(jj$cumple_minimo)) "Muestra valida" else "Muestra reducida"),
        list(l = "Goles", v = jj$goles_tot, n = ""),
        list(l = "Asistencias", v = jj$asistencias_tot, n = ""),
        list(l = "xA acumulado", v = round(jj$xa_tot, 2), n = "")
      )
      
      div(style = "border:1px solid #DCE1E8; border-radius:2px; overflow:hidden;",
          lapply(seq_along(items), function(i) {
            it <- items[[i]]
            bg <- if (i %% 2 == 0) fia_cream else fia_white
            tags$div(style = paste0("display:flex; justify-content:space-between;
                                   align-items:baseline; padding:12px 16px;
                                   background:", bg, ";"),
                     tags$span(style = "color:#46566C; font-size:0.88em;", it$l),
                     tags$span(
                       tags$strong(style = "color:#0E2240; font-size:1.02em;", it$v),
                       if (nzchar(it$n))
                         tags$span(style = "color:#46566C; font-size:0.78em; margin-left:10px;", it$n)
                     )
            )
          })
      )
    })
    
    guardar <- function(p_reactive, sufijo, w, h) {
      downloadHandler(
        filename = function() {
          jid <- jugador_id_sel()
          nm <- if (!is.null(jid) && jid != "")
            gsub("[^A-Za-z0-9_]", "_", nombre_display(jid)) else "jugador"
          paste0("CIDE_OSOS_", nm, "_", sufijo, "_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
          p <- p_reactive()
          if (is.null(p)) return(NULL)
          ggplot2::ggsave(file, p, width = w, height = h, dpi = 300, bg = "white")
        }
      )
    }
    
    # Campanas apiladas en un solo PNG de exportacion
    p_campanas_export <- reactive({
      req(j())
      ps <- lapply(DIMS_INDICES, function(d) plot_campana_una(j(), perfiles, d))
      ps <- Filter(Negate(is.null), ps)
      if (length(ps) == 0) return(NULL)
      if (requireNamespace("patchwork", quietly = TRUE)) {
        Reduce(`/`, ps)
      } else ps[[1]]
    })
    
    output$dl_campanas <- guardar(p_campanas_export, "campanas",     11, 16)
    output$dl_barras   <- guardar(p_barras,          "ajuste",       12, 11)
    output$dl_dist     <- guardar(p_dist,            "distribucion", 12,  7)
    output$dl_clutch   <- guardar(p_clutch,          "nivel_rival",  10,  6)
  })
}