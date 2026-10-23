# ============================================================
# helpers_reporte.R — CIDE OSOS (app)
# Piezas del reporte del jugador: mapa de calor, mapa de tiros,
# indices, fortalezas y debilidades, similares y clubes compatibles.
# Se usan en la pestaña Reporte y en el PDF descargable.
# ============================================================

ETIQ_METRICA <- c(
  xg_p90 = "xG sin penales", tiros_p90 = "Tiros", goles_p90 = "Goles sin penales",
  toques_area_p90 = "Toques en el área", xa_p90 = "xA", pases_clave_p90 = "Pases clave",
  pases_area_p90 = "Pases al área", pases_ult3_p90 = "Pases al último tercio", asist_p90 = "Asistencias",
  faltas_recibidas_p90 = "Faltas recibidas", pases_filtrados_p90 = "Pases filtrados",
  presiones_p90 = "Presiones", recup_presion_p90 = "Recuperaciones tras presión",
  intercep_p90 = "Intercepciones", entradas_p90 = "Entradas", recup_campo_rival_p90 = "Recuperaciones en campo rival",
  acc_def_sobre_esp_p90 = "Acciones defensivas sobre lo esperado", pct_aereos = "% duelos aéreos ganados",
  faltas_p90 = "Faltas cometidas", regateado_p90 = "Veces regateado", amarillas_p90 = "Amarillas",
  rojas_p90 = "Rojas", progresiones_p90 = "Progresiones al último tercio", pases_adelante_p90 = "Pases hacia adelante",
  pases_p90 = "Pases", balones_largos_p90 = "Balones largos completos", regates_p90 = "Regates exitosos",
  lbp_p90 = "Pases que rompen líneas", perdidas_p90 = "Pérdidas", desposesiones_p90 = "Desposesiones",
  pct_pases = "% de pase", var_obv_resid = "Variabilidad de OBV")

cargar_mapas <- function() {
  p <- ruta_proc("mapas_jugador.rds")
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

# Rejilla completa, suavizada con un promedio 3x3, en % de las acciones
rejilla_suave <- function(d) {
  nx <- 120 / TAM_CELDA; ny <- 80 / TAM_CELDA
  m <- matrix(0, nx, ny)
  if (nrow(d) > 0) for (k in seq_len(nrow(d))) m[d$bx[k] + 1, d$by[k] + 1] <- m[d$bx[k] + 1, d$by[k] + 1] + d$n[k]
  tot <- sum(m); if (tot == 0) return(NULL)
  s <- m
  for (i in seq_len(nx)) for (j in seq_len(ny)) {
    ii <- max(1, i - 1):min(nx, i + 1); jj <- max(1, j - 1):min(ny, j + 1)
    s[i, j] <- mean(m[ii, jj])
  }
  s <- 100 * s / sum(s)
  expand.grid(i = seq_len(nx), j = seq_len(ny)) %>%
    mutate(x = (i - 0.5) * TAM_CELDA, y = (j - 0.5) * TAM_CELDA, pct = s[cbind(i, j)])
}

graf_mapa_calor <- function(mapas, j, categoria = "Todas las acciones") {
  if (is.null(mapas)) return(NULL)
  d <- mapas$mapas %>% filter(player_id == j$player_id[1], temporada == j$temporada[1], categoria == !!categoria)
  if ("team_id" %in% names(j) && any(d$team_id == j$team_id[1])) d <- d[d$team_id == j$team_id[1], ]
  g <- rejilla_suave(d); if (is.null(g)) return(NULL)
  n_acc <- sum(d$n)
  zona <- c(sum(g$pct[g$x < 40]), sum(g$pct[g$x >= 40 & g$x < 80]), sum(g$pct[g$x >= 80]))
  ggplot(g) +
    geom_raster(aes(x, y, fill = pct), interpolate = TRUE) +
    scale_fill_gradientn(colours = c("#FFFFFF", "#E8EEF5", "#9DB8D6", "#2E6DA4", "#B8862B", "#8C2D04"),
                         name = "% de las acciones", guide = guide_colourbar(barwidth = 12, barheight = 0.6, title.position = "top", title.hjust = 0.5)) +
    cancha(media = FALSE, fondo = FALSE) +
    labs(title = paste("Mapa de calor:", tolower(categoria)),
         subtitle = sprintf("%s acciones · tercio defensivo %.0f%% · medio %.0f%% · ofensivo %.0f%% · ataque →",
                            format(n_acc, big.mark = ","), zona[1], zona[2], zona[3]))
}

graf_tiros_jugador <- function(mapas, j) {
  if (is.null(mapas)) return(NULL)
  d <- mapas$tiros %>% filter(player_id == j$player_id[1], temporada == j$temporada[1], !penal)
  if (nrow(d) == 0) return(NULL)
  ggplot(d) + cancha(media = TRUE) +
    geom_point(aes(x, y, size = xg, color = gol, shape = cabeza), alpha = 0.8) +
    scale_color_manual(values = c(`TRUE` = "#B8862B", `FALSE` = "#2E6DA4"), labels = c(`TRUE` = "Gol", `FALSE` = "Sin gol"), name = NULL) +
    scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), labels = c(`TRUE` = "Cabeza", `FALSE` = "Pie"), name = NULL) +
    scale_size_continuous(range = c(1.5, 8), name = "xG") +
    labs(title = "Tiros (sin penales)",
         subtitle = sprintf("%d tiros, %.1f xG, %d goles (%.2f xG por tiro)", nrow(d), sum(d$xg, na.rm = TRUE),
                            sum(d$gol), mean(d$xg, na.rm = TRUE)))
}

# Acciones a balon parado por 90 (informativas) y su percentil en la posicion
tabla_balon_parado <- function(j, perfiles_t) {
  cols <- c(bp_xg_p90 = "xG de sus remates", bp_xg_lanzado_p90 = "xG de los saques que cobra",
            bp_aereos_p90 = "Duelos aéreos ganados", bp_resp_xg_p90 = "xG concedido bajo su responsabilidad")
  if (!all(names(cols) %in% names(j)) || all(is.na(unlist(j[1, names(cols)])))) return(NULL)
  ref <- perfiles_t %>% filter(subgrupo == j$subgrupo[1], cumple_minimo)
  bind_rows(lapply(names(cols), function(k) {
    v <- j[[k]][1]; r <- ref[[k]][!is.na(ref[[k]])]
    pct <- if (is.na(v) || length(r) < 10) NA else if (k == "bp_resp_xg_p90") 100 * mean(r >= v) else 100 * mean(r <= v)
    tibble(`Acción` = cols[[k]], `Por 90` = round(v, 3),
           `Percentil en su posición` = ifelse(is.na(pct), "s/d", paste0(round(pct), "º")))
  }))
}

graf_indices_jugador <- function(j) {
  dims <- c(DIMS_INDICES, DIM_REV)
  etq <- c(unname(ETIQUETAS_INDICES[DIMS_INDICES]), "Revulsivo (informativo)")
  v <- vapply(dims, function(d) if (d %in% names(j)) as.numeric(j[[d]][1]) else NA_real_, numeric(1))
  if ("revulsivo_con_senal" %in% names(j) && isFALSE(as.logical(j$revulsivo_con_senal[1]))) v[length(v)] <- NA_real_
  d <- tibble(indice = factor(etq, levels = rev(etq)), z = v, informativo = dims %in% DIM_REV) %>%
    mutate(pct = round(100 * pnorm(z)), lab = ifelse(is.na(z), "s/d", sprintf("%+.2f  (%dº)", z, pct)))
  ggplot(d, aes(y = indice, x = z)) +
    geom_vline(xintercept = 0, color = "#6E7C8F") +
    geom_col(aes(fill = interaction(z >= 0, informativo)), width = 0.65, na.rm = TRUE) +
    geom_text(aes(x = ifelse(is.na(z), 0, z), label = lab, hjust = ifelse(!is.na(z) & z < 0, 1.1, -0.1)), size = 3.6) +
    scale_fill_manual(values = c(`TRUE.FALSE` = "#2E6DA4", `FALSE.FALSE` = "#B23A3A", `TRUE.TRUE` = "#C9CED6", `FALSE.TRUE` = "#C9CED6"),
                      guide = "none") +
    scale_x_continuous(limits = c(-3.8, 3.8)) +
    labs(title = "Índices (z dentro de su posición)", subtitle = "Entre paréntesis, percentil. Los informativos no entran al compuesto",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}

# Fortalezas y debilidades: percentil de cada componente (residuo
# ajustado) dentro de su subgrupo y temporada, con el signo del indice.
fortalezas_debilidades <- function(j, perfiles_t, n = 3) {
  comps <- COMPONENTES_DE[[if (!is.null(j$version_indices) && !is.na(j$version_indices[1])) j$version_indices[1] else "completo"]]
  ref <- perfiles_t %>% filter(subgrupo == j$subgrupo[1], cumple_minimo)
  filas <- list()
  for (idx in names(comps)) for (m in names(comps[[idx]])) {
    col <- if (m == "var_obv_resid") "var_obv_resid" else paste0(m, "_res")
    if (!col %in% names(j) || is.na(j[[col]][1]) || !col %in% names(ref)) next
    sgn <- comps[[idx]][[m]]; r <- ref[[col]]; r <- r[!is.na(r)]
    if (length(r) < 10) next
    pct <- 100 * mean(sgn * r <= sgn * j[[col]][1])
    filas[[length(filas) + 1]] <- tibble(metrica = unname(ifelse(is.na(ETIQ_METRICA[m]), m, ETIQ_METRICA[m])),
                                         indice = unname(ETIQUETAS_INDICES[paste0("z_", idx)]), percentil = round(pct))
  }
  d <- bind_rows(filas)
  if (nrow(d) == 0) return(list(fuertes = d, debiles = d))
  list(fuertes = d %>% arrange(desc(percentil)) %>% head(n), debiles = d %>% arrange(percentil) %>% head(n))
}

similares_jugador <- function(j, perfiles_t, n = 5) {
  pob <- perfiles_t %>% filter(subgrupo == j$subgrupo[1], cumple_minimo, jugador_id != j$jugador_id[1])
  if (nrow(pob) == 0) return(NULL)
  zj <- as.numeric(j[1, DIMS_INDICES]); zj[is.na(zj)] <- 0
  Z <- as.matrix(pob[, DIMS_INDICES]); Z[is.na(Z)] <- 0
  pob$distancia <- sqrt(rowSums((Z - matrix(zj, nrow(Z), length(zj), byrow = TRUE))^2))
  pob %>% arrange(distancia) %>% head(n) %>%
    transmute(Jugador = player_name, Club = club, Edad = round(edad), `z fiable` = round(z_compuesto_fiable, 2),
              Similitud = round(100 * exp(-distancia / 2)))
}

clubes_jugador <- function(j, perfiles_t, n = 5) {
  ec <- tryCatch(cargar_estilo_clubes(), error = function(e) NULL)
  if (is.null(ec)) return(NULL)
  f <- tryCatch(calcular_fit_clubes(j, ec, perfiles_t, top_n = n), error = function(e) NULL)
  if (is.null(f) || nrow(f) == 0) return(NULL)
  f %>% transmute(Club = club, Afinidad = round(afinidad_pct), Necesidad = round(encaje_pct),
                  Estilo = round(estilo_pct), Promedio = round(fit_medio))
}

# "Central del Toluca, 24 años, Mexico. Temporada 2025/2026."
descripcion_jugador <- function(x) {
  nac <- if (!is.null(x$nacionalidad) && !is.na(x$nacionalidad[1])) paste0(", ", x$nacionalidad[1]) else ""
  sprintf("%s del %s, %s años%s. Temporada %s.", x$subgrupo_lab[1], x$club[1], round(x$edad[1]), nac, x$temporada[1])
}

# Texto de la validacion que respalda el indice
texto_validacion <- function() {
  v <- cargar_validacion()
  if (is.null(v) || is.null(v$rendimiento)) return(NULL)
  r <- v$rendimiento %>% filter(predictor == "z compuesto ex ante", resultado == "z compuesto en el club nuevo")
  if (nrow(r) == 0) return(NULL)
  sprintf(paste0("Respaldo: en %d traspasos dentro de Liga MX, el índice ex ante se correlacionó %.2f [%.2f, %.2f] ",
                 "con el rendimiento en el club nuevo; los minutos previos, casi nada."), r$n[1], r$rho[1], r$lo[1], r$hi[1])
}

# ── PDF del reporte (sin LaTeX: grid + ggplot) ─────────────────
tabla_grob <- function(df, titulo, x, y, ancho, alto_fila = 0.022, tam = 8, w1 = 0.42) {
  if (is.null(df) || nrow(df) == 0) return(invisible())
  grid::grid.text(titulo, x = x, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#0E2240"))
  cols <- names(df); k <- length(cols)
  # Primera columna (nombres) mas ancha que las demas
  w <- if (k == 1) 1 else c(w1, rep((1 - w1) / (k - 1), k - 1))
  xs <- x + c(0, cumsum(w)[-k]) * ancho
  yy <- y - 0.028
  for (c in seq_len(k)) grid::grid.text(cols[c], x = xs[c], y = yy, just = c("left", "top"),
                                        gp = grid::gpar(fontsize = tam, fontface = "bold", col = "#46566C"))
  for (r in seq_len(nrow(df))) for (c in seq_len(k))
    grid::grid.text(as.character(df[[c]][r]), x = xs[c], y = yy - r * alto_fila, just = c("left", "top"),
                    gp = grid::gpar(fontsize = tam, col = "#0E2240"))
}

generar_pdf_reporte <- function(archivo, j, perfiles_t, mapas) {
  dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  dev(archivo, width = 8.27, height = 11.69, onefile = TRUE)
  on.exit(grDevices::dev.off())
  vp <- function(x, y, w, h) grid::viewport(x = x, y = y, width = w, height = h, just = c("left", "top"))
  pon <- function(g, x, y, w, h) if (!is.null(g)) print(g, vp = vp(x, y, w, h))
  # Pagina 1
  grid::grid.newpage()
  grid::grid.rect(x = 0, y = 1, width = 1, height = 0.085, just = c("left", "top"), gp = grid::gpar(fill = "#0E2240", col = NA))
  grid::grid.text(j$player_name[1], x = 0.05, y = 0.965, just = "left", gp = grid::gpar(fontsize = 20, fontface = "bold", col = "white"))
  grid::grid.text(descripcion_jugador(j),
                  x = 0.05, y = 0.935, just = "left", gp = grid::gpar(fontsize = 10, col = "#B8862B"))
  grid::grid.text("CIDE OSOS", x = 0.95, y = 0.965, just = "right", gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#B8862B"))
  kpis <- c(sprintf("Percentil %sº", j$percentil[1]), sprintf("z fiable %+.2f", j$z_compuesto_fiable[1]),
            sprintf("%s min", format(round(j$minutos_total[1]), big.mark = ",")), sprintf("Fiabilidad %d%%", round(100 * j$fiabilidad[1])),
            sprintf("%d goles, %.1f xA", j$goles_tot[1], j$xa_tot[1]))
  for (k in seq_along(kpis)) grid::grid.text(kpis[k], x = 0.05 + (k - 1) * 0.185, y = 0.895, just = "left",
                                             gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#0E2240"))
  pon(graf_indices_jugador(j), 0.03, 0.87, 0.94, 0.27)
  pon(graf_mapa_calor(mapas, j, "Todas las acciones"), 0.03, 0.59, 0.94, 0.31)
  pon(graf_tiros_jugador(mapas, j), 0.12, 0.275, 0.76, 0.215)
  grid::grid.text(paste(FUENTE, "Índices ajustados por contexto y contraídos por minutos.", FIRMA_OSOS),
                  x = 0.05, y = 0.012, just = "left", gp = grid::gpar(fontsize = 7, col = "#46566C"))
  # Pagina 2
  grid::grid.newpage()
  fd <- fortalezas_debilidades(j, perfiles_t)
  tabla_grob(fd$fuertes %>% transmute(`Métrica` = metrica, `Índice` = indice, Percentil = paste0(percentil, "º")),
             "Fortalezas (dentro de su posición)", 0.05, 0.96, 0.42)
  tabla_grob(fd$debiles %>% transmute(`Métrica` = metrica, `Índice` = indice, Percentil = paste0(percentil, "º")),
             "Áreas a mejorar", 0.53, 0.96, 0.42)
  tabla_grob(similares_jugador(j, perfiles_t), "Perfiles más parecidos (misma posición, misma temporada)", 0.05, 0.80, 0.90)
  tabla_grob(clubes_jugador(j, perfiles_t), "Clubes compatibles (afinidad, necesidad, estilo; 0-100)", 0.05, 0.62, 0.90)
  ctx <- tibble(Dato = c("Posesión media de su equipo", "PPDA media", "Nivel de su equipo", "Nivel medio del rival",
                         "Disponibilidad", "Salidas por lesión", "Entradas de cambio"),
                Valor = c(sprintf("%.1f%%", j$posesion_media[1]), sprintf("%.1f", j$ppda_media[1]), sprintf("%.0f", j$cal_comp_media[1]),
                          sprintf("%.0f", j$cal_rival_media[1]), paste0(j$disponibilidad[1], "%"), as.character(j$salidas_lesion[1]),
                          as.character(if ("entradas_cambio" %in% names(j)) j$entradas_cambio[1] else 0)))
  tabla_grob(ctx, "Contexto", 0.05, 0.44, 0.42)
  bp_t <- tabla_balon_parado(j, perfiles_t)
  if (!is.null(bp_t)) names(bp_t)[names(bp_t) == "Percentil en su posición"] <- "Percentil"
  tabla_grob(bp_t, "Balón parado (informativo, por 90)", 0.52, 0.44, 0.45, w1 = 0.56)
  val <- texto_validacion()
  nota <- paste0("Cómo leerlo. Cada índice compara al jugador con los de su misma posición en la liga (z = 0 es el promedio), ",
                 "después de descontar el estilo y el nivel de su equipo y del rival. El z fiable se contrae según sus minutos: ",
                 "con pocos minutos se acerca a 0. El balón parado y el revulsivo son informativos: no entran a la calificación. ",
                 if (!is.null(val)) val else "")
  grid::grid.text(paste(strwrap(nota, 120), collapse = "\n"), x = 0.05, y = 0.20, just = c("left", "top"),
                  gp = grid::gpar(fontsize = 8, col = "#0E2240", lineheight = 1.2))
  grid::grid.text(paste(FUENTE, FIRMA_OSOS), x = 0.05, y = 0.012, just = "left", gp = grid::gpar(fontsize = 7, col = "#46566C"))
}
