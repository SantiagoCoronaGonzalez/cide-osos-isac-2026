# ============================================================
# funciones_reporte.R — CIDE OSOS
# Graficas y textos del reporte "La historia de los entrenadores".
# Solo resultados agregados: el reporte no contiene datos crudos.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2); library(plotly); library(htmltools)
})

# Paleta (validada para daltonismo con etiquetas directas): un color por
# entrenador en orden cronologico, y la liga en gris.
COLORES_DT <- c("#B5487A", "#A8771F", "#2E6DA4", "#2A9D8F", "#6B5FA8", "#8A6D3B")
TINTA <- "#0E2240"; TEXTO2 <- "#46566C"; TEXTO3 <- "#6E7C8F"; LINEA <- "#DCE1E8"; LIGA <- "#B9C2CE"
FUENTE <- "Barlow, 'Helvetica Neue', Arial, sans-serif"

colores_de <- function(ents) setNames(COLORES_DT[seq_along(ents)], ents)

minus1 <- function(x) ifelse(grepl("^(xG|PPDA|OBV)", x), x, paste0(tolower(substr(x, 1, 1)), substring(x, 2)))
sin_unidad <- function(x) sub(" \\([^)]*\\)$", "", x)
fmt_v <- function(x) ifelse(is.na(x), "s/d", ifelse(abs(x) >= 100, num(x, 0), ifelse(abs(x) >= 10, num(x, 1), num(x, 2))))
apellido <- function(x) vapply(strsplit(as.character(x), " "), function(p) tail(p, 1), character(1))

num  <- function(x, d = 1) { x <- as.numeric(x); ifelse(is.na(x), "s/d", formatC(x, format = "f", digits = d, big.mark = ",")) }
pct  <- function(x, d = 0) { x <- as.numeric(x); ifelse(is.na(x), "s/d", paste0(formatC(x, format = "f", digits = d), "%")) }
signo <- function(x, d = 2) { x <- as.numeric(x); ifelse(is.na(x), "s/d", sprintf(paste0("%+.", d, "f"), x)) }

tema_rep <- function(base = 13) {
  theme_minimal(base_size = base, base_family = "Barlow") +
    theme(plot.title = element_text(face = "bold", color = TINTA, size = base + 2, hjust = 0),
          plot.subtitle = element_text(color = TEXTO2, hjust = 0, margin = margin(b = 8)),
          plot.caption = element_text(color = TEXTO3, hjust = 0, size = base - 3),
          axis.text = element_text(color = TEXTO2), axis.title = element_text(color = TEXTO2, size = base - 1),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#EDF0F4"),
          strip.text = element_text(face = "bold", color = TINTA, hjust = 0, size = base - 0.5),
          legend.position = "none", plot.title.position = "plot",
          plot.background = element_rect(fill = "white", color = NA))
}

# Plotly con la tipografia del reporte y sin barra de herramientas
pl_tema <- function(p, alto = 460) {
  p$width <- "100%"; p$height <- alto
  p %>% layout(font = list(family = FUENTE, color = TEXTO2, size = 13),
               hoverlabel = list(font = list(family = FUENTE, size = 13), bgcolor = "white", bordercolor = LINEA),
               paper_bgcolor = "white", plot_bgcolor = "white", margin = list(l = 10, r = 10, t = 40, b = 10)) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

# ── Linea del tiempo (cabecera) ───────────────────────────────
graf_linea_tiempo <- function(o) {
  col <- colores_de(o$entrenadores)
  d <- o$sim %>% left_join(o$serie %>% select(match_id, n_partido, match_date), by = "match_id") %>%
    arrange(match_date) %>% group_by(entrenador) %>%
    mutate(ppg_movil = zoo_rolling(puntos, 8), xppg_movil = zoo_rolling(xpts, 8)) %>% ungroup()
  et <- d %>% group_by(entrenador) %>% summarise(ini = min(match_date), fin = max(match_date), n = n(), .groups = "drop") %>%
    arrange(ini) %>%
    # Nombre arriba de su etapa: alineado al inicio; el ultimo, al final para que no se salga
    mutate(ultimo = row_number() == n(), xl = if_else(ultimo, fin, ini), hj = ifelse(ultimo, 1, 0),
           etiqueta = paste0(apellido(entrenador), "\n", n, " partidos"))
  ggplot(d, aes(match_date)) +
    geom_rect(data = et, aes(xmin = ini, xmax = fin, ymin = 0, ymax = 3, fill = entrenador),
              inherit.aes = FALSE, alpha = 0.08) +
    geom_hline(yintercept = c(1, 2), color = "#EDF0F4") +
    geom_line(aes(y = xppg_movil, group = entrenador), color = LIGA, linewidth = 0.8, linetype = "22", na.rm = TRUE) +
    geom_line(aes(y = ppg_movil, color = entrenador, group = entrenador), linewidth = 1.2, na.rm = TRUE) +
    geom_text(data = et, aes(x = xl, y = 3.12, label = etiqueta, color = entrenador, hjust = hj),
              family = "Barlow", fontface = "bold", size = 3.9, lineheight = 0.9, vjust = 0) +
    scale_color_manual(values = col) + scale_fill_manual(values = col) +
    scale_y_continuous(breaks = 0:3) +
    coord_cartesian(ylim = c(0, 3), clip = "off") +
    labs(x = NULL, y = "Puntos por partido",
         caption = "Línea sólida: puntos por partido (promedio de los últimos 8). Punteada gris: puntos esperados por las ocasiones de cada partido.") +
    tema_rep(12) + theme(plot.margin = margin(38, 12, 6, 6))
}
# Promedio movil; necesita al menos 4 partidos para no arrancar con saltos
zoo_rolling <- function(x, k, minimo = 4) {
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) if (i >= min(minimo, length(x))) out[i] <- mean(x[max(1, i - k + 1):i])
  out
}

# ── Contexto: puntos reales contra los que daban las ocasiones ─
graf_torneos_sim <- function(o) {
  col <- colores_de(o$entrenadores)
  d <- o$torneos_sim %>% group_by(torneo) %>% mutate(lab = if (n() > 1) paste0(torneo, " (", apellido(entrenador), ")") else torneo) %>%
    ungroup() %>% mutate(lab = factor(lab, levels = unique(lab)))
  p <- plot_ly(d, y = ~lab) %>%
    add_segments(x = ~p10, xend = ~p90, yend = ~lab, line = list(color = LIGA, width = 6), hoverinfo = "skip",
                 showlegend = FALSE) %>%
    add_markers(x = ~p50, marker = list(color = "white", size = 9, line = list(color = TEXTO3, width = 2)),
                hovertemplate = "Esperado: %{x:.0f} puntos<extra></extra>", showlegend = FALSE) %>%
    por_entrenador(d, function(p, dd, en) add_markers(p, data = dd, x = ~puntos, y = ~lab, name = en,
                marker = list(color = col[[en]], size = 12, line = list(color = "white", width = 1.5)),
                text = ~paste0(entrenador, "<br>", puntos, " puntos reales; en ", pct_mejor,
                               "% de 10,000 repeticiones del torneo sacó menos"),
                hovertemplate = "%{text}<extra></extra>"), ents = names(col)) %>%
    layout(xaxis = list(title = "Puntos en fase regular", zeroline = FALSE, gridcolor = "#EDF0F4"),
           yaxis = list(title = "", autorange = "reversed"),
           legend = list(orientation = "h", y = -0.15))
  pl_tema(p, 120 + 46 * nrow(d))
}

# Agrega una capa por entrenador con su color fijo
por_entrenador <- function(p, d, f, ents) {
  for (en in ents) { dd <- d[as.character(d$entrenador) == en, , drop = FALSE]; if (nrow(dd) > 0) p <- f(p, dd, en) }
  p
}

# ── Huella frente a la liga ───────────────────────────────────
graf_huella <- function(o, fases = NULL) {
  col <- colores_de(o$entrenadores)
  niveles <- rev(unique(o$kpis$etiqueta[o$kpis$kpi %in% KPIS_ESTILO_REP]))
  k <- length(o$entrenadores)
  # Cada entrenador en su propio renglon dentro del indicador (de arriba abajo, en orden cronologico)
  off <- setNames(if (k > 1) seq(0.27, -0.27, length.out = k) else 0, o$entrenadores)
  d <- o$huella %>% filter(kpi %in% KPIS_ESTILO_REP) %>%
    mutate(etiqueta = factor(etiqueta, levels = niveles), yy = as.integer(etiqueta) + off[as.character(entrenador)])
  if (!is.null(fases)) d <- d %>% filter(fase %in% fases)
  p <- plot_ly() %>%
    por_entrenador(d, function(p, dd, en) p %>%
      add_segments(data = dd, x = ~pmax(z_lo, -3), xend = ~pmin(z_hi, 3), y = ~yy, yend = ~yy, showlegend = FALSE,
                   line = list(width = 2.5, color = col[[en]]), opacity = 0.4, hoverinfo = "skip", legendgroup = en) %>%
      add_markers(data = dd, x = ~pmax(pmin(z, 3), -3), y = ~yy, name = en, legendgroup = en,
                  marker = list(size = 9, color = col[[en]], line = list(color = "white", width = 1)),
                  text = ~paste0("<b>", entrenador, "</b><br>", etiqueta, ": ", round(valor, 2),
                                 "<br>Liga: ", round(liga, 2), "<br>Más que el ", percentil, "% de los clubes-torneo"),
                  hovertemplate = "%{text}<extra></extra>"), ents = names(col)) %>%
    layout(xaxis = list(title = "Desviaciones frente al promedio de la liga (0 = promedio)", range = c(-3.2, 3.2),
                        zeroline = TRUE, zerolinecolor = TEXTO3, gridcolor = "#EDF0F4"),
           yaxis = list(title = "", tickvals = seq_along(niveles), ticktext = niveles, showgrid = FALSE, zeroline = FALSE,
                        range = c(0.5, length(niveles) + 0.5)),
           shapes = lapply(seq_len(length(niveles) - 1), function(i) list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                                                                          y0 = i + 0.5, y1 = i + 0.5, line = list(color = "#EDF0F4", width = 1))),
           legend = list(orientation = "h", y = 1.03, x = 0))
  pl_tema(p, 120 + 34 * length(niveles) * max(1, k / 2))
}
KPIS_ESTILO_REP <- c("posesion_pct", "pases_pos", "salida_limpia", "largo_portero", "velocidad", "pos_ult3",
                     "pos_tiro", "xg_100", "xg_transicion", "rapidas", "ppda", "presion_pos", "altura_recup",
                     "recup_altas", "recupera_5s", "altura_bloque", "rival_ult3", "xg_contra_100", "xg_contra_trans")

# Rasgos que mas distinguen a cada entrenador (texto)
rasgos <- function(o, ent, n = 3) {
  h <- o$huella %>% filter(entrenador == ent, kpi %in% KPIS_ESTILO_REP, !kpi %in% c("xg_100", "xg_contra_100")) %>%
    filter(sign(z_lo) == sign(z_hi)) %>% arrange(desc(abs(z))) %>% head(n)
  if (nrow(h) == 0) return("ningún rasgo que se distinga del promedio con seguridad")
  frase <- function(r) {
    mas <- r$z > 0
    paste0(minus1(sin_unidad(r$etiqueta)), " ", if (mas) "por encima" else "por debajo", " de la liga (",
           if (mas) paste0("más que el ", r$percentil, "%") else paste0("menos que el ", 100 - r$percentil, "%"),
           " de los clubes-torneo)")
  }
  x <- vapply(seq_len(nrow(h)), function(i) frase(h[i, ]), character(1))
  if (length(x) == 1) x else paste0(paste(x[-length(x)], collapse = "; "), " y ", x[length(x)])
}

# ── ¿Se reconoce al entrenador? ───────────────────────────────
graf_confusion <- function(o) {
  r <- o$reconocer; if (is.null(r)) return(NULL)
  d <- r$confusion %>% mutate(real = factor(real, levels = rev(o$entrenadores)),
                              predicho = factor(predicho, levels = o$entrenadores))
  ggplot(d, aes(predicho, real, fill = pct)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = paste0(round(pct), "%"), color = pct > 55), family = "Barlow", fontface = "bold", size = 4.6) +
    scale_fill_gradient(low = "#F3F5F8", high = "#1F4E7A", limits = c(0, 100)) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = TINTA)) +
    scale_x_discrete(position = "top", labels = apellido) +
    labs(x = "El modelo dice que lo dirigió…", y = "Lo dirigió en realidad") +
    tema_rep(12) + theme(panel.grid = element_blank(), axis.title.x.top = element_text(margin = margin(b = 6)))
}

graf_efecto <- function(o) {
  d <- o$efecto %>% filter(kpi %in% KPIS_ESTILO_REP) %>% head(10) %>%
    mutate(etiqueta = factor(etiqueta, levels = rev(etiqueta)), sig = p < 0.05)
  ggplot(d, aes(100 * eta2, etiqueta)) +
    geom_col(aes(fill = sig), width = 0.62) +
    geom_text(aes(label = paste0(round(100 * eta2), "%")), hjust = -0.2, family = "Barlow", size = 3.8, color = TINTA) +
    scale_fill_manual(values = c(`TRUE` = "#2E6DA4", `FALSE` = LIGA)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = "% de la variación explicada por el entrenador", y = NULL,
         caption = "Azul: p < 0.05. Gris: no se distingue del azar.") +
    tema_rep(12) + theme(panel.grid.major.y = element_blank())
}

# ── Mapas de estilo: cada club-torneo de la liga, y el club con cada DT
graf_mapa_estilo <- function(o, liga_t, x, y, tx, ty, nota_x = NULL, nota_y = NULL) {
  col <- colores_de(o$entrenadores)
  club <- o$evol_club %>% mutate(entrenador = factor(entrenador, levels = o$entrenadores))
  p <- plot_ly() %>%
    add_markers(data = liga_t, x = ~.data[[x]], y = ~.data[[y]], marker = list(color = LIGA, size = 7, opacity = 0.55),
                text = ~paste0(club, " · ", torneo), hovertemplate = "%{text}<extra></extra>", name = "Liga MX",
                showlegend = FALSE) %>%
    por_entrenador(club, function(p, dd, en) add_markers(p, data = dd, x = ~.data[[x]], y = ~.data[[y]], name = en,
                marker = list(size = 13, color = col[[en]], line = list(color = "white", width = 1.5)),
                text = ~paste0("<b>", entrenador, "</b><br>", torneo, " (", partidos, " partidos)"),
                hovertemplate = "%{text}<extra></extra>"), ents = names(col)) %>%
    layout(xaxis = list(title = tx, zeroline = FALSE, gridcolor = "#EDF0F4"),
           yaxis = list(title = ty, zeroline = FALSE, gridcolor = "#EDF0F4"),
           legend = list(orientation = "h", y = -0.18))
  pl_tema(p, 480)
}

# ── Carriles de entrada al ultimo tercio, sobre una cancha ─────
graf_carriles <- function(o) {
  col <- colores_de(o$entrenadores)
  d <- o$huella %>% filter(kpi %in% c("carril_izq", "carril_cen", "carril_der")) %>%
    mutate(carril = c(carril_izq = "Izquierdo", carril_cen = "Central", carril_der = "Derecho")[kpi],
           yc = c(Izquierdo = 13.3, Central = 40, Derecho = 66.7)[carril],
           entrenador = factor(entrenador, levels = o$entrenadores))
  liga <- d %>% distinct(carril, yc, liga)
  ggplot(d) +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = "#F7F9FB", color = "#C9D1DB") +
    annotate("rect", xmin = 80, xmax = 120, ymin = 0, ymax = 80, fill = "#EEF2F7", color = NA) +
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80, color = "#C9D1DB") +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, color = "#C9D1DB") +
    geom_hline(yintercept = c(80 / 3, 160 / 3), color = "#DDE3EA", linetype = "22") +
    geom_segment(aes(x = 52, xend = 52 + valor * 0.9, y = yc, yend = yc, color = entrenador, linewidth = valor),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"), lineend = "round") +
    geom_text(aes(x = 56 + valor * 0.9, y = yc, label = pct(valor)), hjust = 0, family = "Barlow",
              fontface = "bold", size = 4, color = TINTA) +
    geom_text(data = liga, aes(x = 2, y = yc + 9, label = paste0(carril, " · liga ", pct(liga))), hjust = 0,
              family = "Barlow", size = 3.3, color = TEXTO3) +
    facet_wrap(~ entrenador, nrow = 1) +
    scale_color_manual(values = col) + scale_linewidth(range = c(1.2, 4.5), guide = "none") +
    scale_y_reverse() + coord_fixed(ratio = 1, xlim = c(0, 120), ylim = c(80, 0), expand = FALSE) +
    labs(x = NULL, y = NULL) + tema_rep(12) +
    theme(axis.text = element_blank(), panel.grid = element_blank())
}

# ── Ajustes por contexto ──────────────────────────────────────
AJ_CONDUCTAS <- c("velocidad", "pos_tiro", "largo", "presion", "altura_recup")
AJ_NIVELES <- list(Marcador = c("Perdiendo", "Ganando"), Localía = "Local", Rival = "Rival del 20% más fuerte", Minuto = "75'+")
graf_ajustes <- function(o, ctx) {
  col <- colores_de(o$entrenadores)
  d <- o$ajustes %>% filter(contexto == ctx, n_club >= 3, conducta %in% AJ_CONDUCTAS, nivel %in% AJ_NIVELES[[ctx]]) %>%
    mutate(entrenador = factor(entrenador, levels = rev(o$entrenadores)),
           panel = factor(paste0(etiqueta, if (length(AJ_NIVELES[[ctx]]) > 1) paste0(" · ", tolower(nivel)) else ""),
                          levels = unique(paste0(rep(CONDUCTAS$etiqueta[match(AJ_CONDUCTAS, CONDUCTAS$conducta)], each = length(AJ_NIVELES[[ctx]])),
                                                 if (length(AJ_NIVELES[[ctx]]) > 1) paste0(" · ", tolower(AJ_NIVELES[[ctx]])) else ""))))
  if (nrow(d) == 0) return(NULL)
  ggplot(d, aes(propio, entrenador, color = entrenador)) +
    geom_vline(xintercept = 0, color = TEXTO3, linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 1.1, alpha = 0.5) +
    geom_point(size = 3.2) +
    facet_wrap(~ panel, scales = "free_x", ncol = if (length(AJ_NIVELES[[ctx]]) > 1) 2 else 3, drop = TRUE) +
    scale_color_manual(values = col) + scale_y_discrete(labels = apellido) +
    labs(x = "Cambio del América menos el cambio típico de la liga en la misma situación", y = NULL) +
    tema_rep(15) + theme(panel.grid.major.y = element_blank(), panel.spacing.y = unit(16, "pt"), panel.spacing.x = unit(26, "pt"))
}
alto_ajustes <- function(o, ctx) {
  n <- length(AJ_CONDUCTAS) * length(AJ_NIVELES[[ctx]]); nc <- if (length(AJ_NIVELES[[ctx]]) > 1) 2 else 3
  ceiling(n / nc) * (0.75 + 0.3 * length(o$entrenadores)) + 0.7
}

# Frase con los ajustes que si se distinguen de la liga
frases_ajustes <- function(o, ent) {
  d <- o$ajustes %>% filter(entrenador == ent, n_club >= 5, sign(lo) == sign(hi), conducta %in% AJ_CONDUCTAS,
                            purrr::map2_lgl(contexto, nivel, ~ .y %in% AJ_NIVELES[[.x]])) %>%
    arrange(desc(abs(propio / ee_propio))) %>% head(3)
  if (nrow(d) == 0) return(NULL)
  vapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    sprintf("%s (%s): %s cambia %s con el América y %s en la liga.",
            r$contexto, tolower(r$nivel), minus1(sin_unidad(r$etiqueta)), signo(r$dif_club), signo(r$dif_liga))
  }, character(1))
}

# ── Evolucion de un KPI con selector ──────────────────────────
graf_evolucion <- function(o, kpis_sel) {
  col <- colores_de(o$entrenadores)
  tor <- o$evol_club %>% distinct(torneo, orden_torneo) %>% arrange(orden_torneo)
  niveles <- tor$torneo
  p <- plot_ly()
  n_tr <- 0; vis <- list()
  for (i in seq_along(kpis_sel)) {
    k <- kpis_sel[i]
    lg <- o$evol_liga %>% filter(torneo %in% niveles) %>%
      transmute(torneo = factor(torneo, levels = niveles), q25 = .data[[paste0(k, "_q25")]], q75 = .data[[paste0(k, "_q75")]],
                q50 = .data[[paste0(k, "_q50")]]) %>% arrange(torneo)
    cl <- o$evol_club %>% transmute(torneo = factor(torneo, levels = niveles), entrenador, partidos, v = .data[[k]]) %>% arrange(torneo)
    v0 <- i == 1
    p <- p %>%
      add_ribbons(data = lg, x = ~torneo, ymin = ~q25, ymax = ~q75, fillcolor = "rgba(185,194,206,0.35)",
                  line = list(width = 0), name = "Mitad central de la liga", visible = v0, hoverinfo = "skip") %>%
      add_lines(data = lg, x = ~torneo, y = ~q50, line = list(color = LIGA, dash = "dot"), name = "Mediana de la liga",
                visible = v0, hovertemplate = "Mediana de la liga: %{y:.2f}<extra></extra>") %>%
      add_lines(data = cl, x = ~torneo, y = ~v, line = list(color = TINTA, width = 1), showlegend = FALSE, visible = v0,
                hoverinfo = "skip")
    n_tr <- n_tr + 3
    for (en in o$entrenadores) {
      ce <- cl %>% filter(entrenador == en)
      p <- p %>% add_markers(data = ce, x = ~torneo, y = ~v, marker = list(color = col[[en]], size = 12, line = list(color = "white", width = 1.5)),
                             name = en, visible = v0, legendgroup = en, showlegend = (i == 1),
                             text = ~paste0("<b>", en, "</b><br>", torneo, " (", partidos, " partidos)"),
                             hovertemplate = "%{text}<br>%{y:.2f}<extra></extra>")
      n_tr <- n_tr + 1
    }
  }
  por_kpi <- 3 + length(o$entrenadores)
  botones <- lapply(seq_along(kpis_sel), function(i) {
    v <- rep(FALSE, n_tr); v[((i - 1) * por_kpi + 1):(i * por_kpi)] <- TRUE
    list(method = "update", args = list(list(visible = v)), label = etiqueta_kpi(kpis_sel[i]))
  })
  p <- p %>% layout(updatemenus = list(list(buttons = botones, direction = "down", x = 0, y = 1.18, xanchor = "left",
                                            bgcolor = "white", bordercolor = LINEA, font = list(size = 13))),
                    xaxis = list(title = "", tickangle = -35, gridcolor = "#EDF0F4"),
                    yaxis = list(title = "", gridcolor = "#EDF0F4", zeroline = FALSE),
                    legend = list(orientation = "h", y = -0.3))
  pl_tema(p, 500) %>% layout(margin = list(t = 70, l = 10, r = 10, b = 10))
}

# ── Jugadores ─────────────────────────────────────────────────
graf_posiciones <- function(o) {
  orden <- c("CB", "FB", "DM", "CM", "AM", "W", "ST")
  et <- c(CB = "Centrales", FB = "Laterales", DM = "Pivotes", CM = "Interiores", AM = "Mediapuntas", W = "Extremos", ST = "Delanteros")
  d <- o$posiciones %>% mutate(subgrupo = factor(subgrupo, levels = orden), entrenador = factor(entrenador, levels = rev(o$entrenadores)))
  pal <- c(CB = "#0E2240", FB = "#3D5A80", DM = "#5C7FA6", CM = "#8EA9C9", AM = "#D9B467", W = "#B8862B", ST = "#7A5718")
  ggplot(d, aes(pct, entrenador, fill = subgrupo)) +
    geom_col(width = 0.6, color = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(pct >= 6, paste0(et[as.character(subgrupo)], "\n", round(pct), "%"), "")),
              position = position_stack(vjust = 0.5), family = "Barlow", size = 3, lineheight = 0.85,
              color = ifelse(as.character(d$subgrupo) %in% c("CM", "AM"), TINTA, "white")) +
    scale_fill_manual(values = pal) + scale_x_continuous(expand = c(0, 0)) +
    labs(x = "% de los minutos de campo", y = NULL) + tema_rep(12) + theme(panel.grid = element_blank())
}

graf_fichajes <- function(o) {
  col <- colores_de(o$entrenadores)
  d <- o$fichajes %>% filter(!is.na(z_antes), !is.na(z_despues), min_despues >= 300) %>%
    mutate(entrenador = factor(entrenador_torneo, levels = o$entrenadores))
  if (nrow(d) < 3) return(NULL)
  lim <- max(abs(c(d$z_antes, d$z_despues)), 1.5) * 1.05
  p <- plot_ly() %>%
    por_entrenador(d, function(p, dd, en) add_markers(p, data = dd, x = ~z_antes, y = ~z_despues, name = en,
               marker = list(size = ~pmin(7 + min_despues / 120, 18), color = col[[en]], line = list(color = "white", width = 1)),
               text = ~paste0("<b>", player_name, "</b> (", subgrupo_lab, ", ", round(edad), " años)<br>",
                              "Llegó en ", torneo, " desde ", club_antes, "<br>z antes ", round(z_antes, 2),
                              " · z con el club ", round(z_despues, 2), " (", round(min_despues), " min)"),
               hovertemplate = "%{text}<extra></extra>"), ents = names(col)) %>%
    add_segments(x = -lim, xend = lim, y = -lim, yend = lim, line = list(color = LIGA, dash = "dot"), inherit = FALSE,
                 showlegend = FALSE, hoverinfo = "skip") %>%
    layout(xaxis = list(title = "Nivel en su club anterior (z fiable)", range = c(-lim, lim), zeroline = TRUE, zerolinecolor = LINEA),
           yaxis = list(title = "Nivel en su primer torneo con el club", range = c(-lim, lim), zeroline = TRUE, zerolinecolor = LINEA),
           legend = list(orientation = "h", y = -0.2))
  pl_tema(p, 520)
}

# Tabla HTML simple con el estilo del reporte
tabla_html <- function(df, clase = "tabla", alinear_num = TRUE) {
  es_num <- vapply(df, function(v) is.numeric(v) || all(grepl("^[-+]?[0-9.,]+%?$|^s/d$|^—$", na.omit(as.character(v)))), logical(1))
  th <- lapply(seq_along(df), function(j) tags$th(class = if (es_num[j] && alinear_num) "num", names(df)[j]))
  filas <- lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j) {
    v <- df[[j]][i]
    tags$td(class = if (es_num[j] && alinear_num) "num", if (is.na(v)) "—" else as.character(v))
  })))
  tags$table(class = clase, tags$thead(tags$tr(th)), tags$tbody(filas))
}

# ── ¿Mejoro a sus jugadores? ──────────────────────────────────
graf_desarrollo <- function(o) {
  dv <- o$desarrollo; if (is.null(dv)) return(NULL)
  col <- colores_de(o$entrenadores)
  d <- dv$resumen %>% filter(entrenador %in% o$entrenadores) %>%
    mutate(entrenador = factor(entrenador, levels = rev(o$entrenadores)))
  ggplot(d, aes(mejora, entrenador, color = entrenador)) +
    geom_vline(xintercept = 0, color = TEXTO3, linewidth = 0.4) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 1.6, alpha = 0.45) +
    geom_point(size = 4) +
    geom_text(aes(x = hi, label = paste0(signo(mejora), "  (", casos, " casos)")), hjust = -0.15,
              family = "Barlow", size = 4, color = TINTA) +
    scale_color_manual(values = col) + scale_y_discrete(labels = apellido) +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.35))) +
    labs(x = "Cambio del índice de sus jugadores por encima de lo esperado (0 = lo normal en la liga)", y = NULL) +
    tema_rep(13) + theme(panel.grid.major.y = element_blank())
}
