# ============================================================
# funciones_tactico.R — CIDE OSOS
# Analisis tactico sobre Events v11 y 360 (helpers_eventos.R):
#
#   1. Metricas de equipo: xG, OBV, pases progresivos, presion, PPDA,
#      field tilt, por partido y por torneo.
#   2. Cambios: que pasa con el partido cuando entra un jugador,
#      ajustado por el minuto y el marcador del momento del cambio.
#   3. Formaciones: rendimiento de cada club con cada formacion,
#      ajustado por el nivel del rival, y mejor XI segun la plantilla.
#   4. Balon parado ofensivo y defensivo: corners, tiros libres y
#      laterales en el ultimo tercio; zonas de entrega y de remate,
#      primer contacto, organizacion y marcaje (360), responsabilidad
#      defensiva por jugador.
#   5. Persistencia: cuanto de cada indicador se repite de un torneo
#      al siguiente (si no se repite, es ruido y no un rasgo del club).
#
# Coordenadas StatsBomb: x 0-120 hacia la porteria rival, y 0-80.
# Area grande: x >= 102, 18 <= y <= 62. Area chica: x >= 114, 30 <= y <= 50.
# ============================================================

VENTANA_CAMBIO <- 15     # minutos antes y despues de un cambio
MIN_VENTANA    <- 8      # una ventana mas corta no se usa
FIN_PARTIDO    <- 95

# ── 0. Preparacion ────────────────────────────────────────────
preparar_eventos <- function(evs, part) {
  e <- evs$eventos %>%
    inner_join(part %>% select(match_id, match_date, temporada, torneo, orden_torneo,
                               home_team_id, away_team_id), by = "match_id") %>%
    mutate(rival_id = ifelse(team_id == home_team_id, away_team_id, home_team_id),
           local = team_id == home_team_id)
  e
}

es_tiro_np <- function(e) e$type == "Shot" & !(e$shot_type %in% "Penalty")

goles_partido <- function(e) {
  bind_rows(
    e %>% filter(type == "Shot", shot_outcome == "Goal") %>% select(match_id, team_id, t),
    e %>% filter(type == "Own Goal For") %>% select(match_id, team_id, t)
  )
}

# ── 1. Metricas de equipo ─────────────────────────────────────
# Pase progresivo: pase en juego, completo, que avanza 10+ yardas y
# termina en los 60% de cancha mas cercanos a la porteria rival.
metricas_equipo_partido <- function(e) {
  np <- es_tiro_np(e)
  base <- e %>%
    mutate(np_xg = ifelse(np, shot_xg, 0),
           tiro = np,
           obv = coalesce(obv_total_net, 0),
           pase = type == "Pass",
           pase_ok = pase & is.na(pass_outcome),
           prog = pase_ok & is.na(pass_type) & !is.na(end_x) & (end_x - x) >= 10 & end_x >= 48,
           ult3 = pase & !is.na(x) & x >= 80,
           presion = type == "Pressure",
           contrap = presion & counterpress,
           presion_alta = presion & !is.na(x) & x >= 80,
           acc_def = type %in% c("Pressure", "Duel", "Interception", "Foul Committed") & !is.na(x) & x >= 48,
           pase_propio60 = pase & !is.na(x) & x < 72) %>%
    group_by(match_id, torneo, orden_torneo, temporada, team_id, rival_id, local) %>%
    summarise(np_xg = sum(np_xg, na.rm = TRUE), tiros = sum(tiro), obv = sum(obv),
              pases = sum(pase), pases_ok = sum(pase_ok), pases_prog = sum(prog),
              pases_ult3 = sum(ult3), presiones = sum(presion), contrapresiones = sum(contrap),
              presiones_altas = sum(presion_alta), acc_def = sum(acc_def),
              pases_propio60 = sum(pase_propio60), .groups = "drop")
  goles <- goles_partido(e) %>% count(match_id, team_id, name = "goles")
  riv <- base %>% select(match_id, rival_id = team_id, np_xg_contra = np_xg, tiros_contra = tiros,
                         obv_contra = obv, pases_ult3_rival = pases_ult3,
                         pases_propio60_rival = pases_propio60)
  base %>%
    left_join(riv, by = c("match_id", "rival_id")) %>%
    left_join(goles, by = c("match_id", "team_id")) %>%
    left_join(goles %>% rename(rival_id = team_id, goles_contra = goles), by = c("match_id", "rival_id")) %>%
    mutate(goles = coalesce(goles, 0L), goles_contra = coalesce(goles_contra, 0L),
           field_tilt = 100 * pases_ult3 / pmax(pases_ult3 + pases_ult3_rival, 1),
           ppda = pases_propio60_rival / pmax(acc_def, 1),
           pct_pase = 100 * pases_ok / pmax(pases, 1),
           xgd = np_xg - np_xg_contra, obv_dif = obv - obv_contra)
}

metricas_equipo_torneo <- function(mp) {
  mp %>%
    group_by(team_id, torneo, orden_torneo, temporada) %>%
    summarise(partidos = n(),
              across(c(np_xg, np_xg_contra, xgd, obv, obv_contra, obv_dif, tiros, tiros_contra,
                       goles, goles_contra, pases_prog, presiones, contrapresiones, presiones_altas),
                     ~ mean(.x, na.rm = TRUE)),
              field_tilt = mean(field_tilt, na.rm = TRUE), ppda = mean(ppda, na.rm = TRUE),
              pct_pase = mean(pct_pase, na.rm = TRUE), .groups = "drop") %>%
    group_by(torneo) %>%
    mutate(across(c(np_xg, np_xg_contra, xgd, obv_dif, field_tilt, ppda, pases_prog, presiones_altas),
                  ~ rank(-.x, ties.method = "min"), .names = "rk_{.col}")) %>%
    ungroup() %>%
    mutate(rk_np_xg_contra = ave(np_xg_contra, torneo, FUN = function(v) rank(v, ties.method = "min")),
           rk_ppda = ave(ppda, torneo, FUN = function(v) rank(v, ties.method = "min")))
}

# ── Utilidad: acumulados en el tiempo por partido y equipo ────
# Para sumar xG u OBV en cualquier intervalo [a, b) sin recorrer la
# tabla de eventos cada vez.
acumulados <- function(e) {
  np <- es_tiro_np(e)
  d <- e %>%
    transmute(match_id, team_id, t, obv = coalesce(obv_total_net, 0),
              xg = ifelse(np, coalesce(shot_xg, 0), 0)) %>%
    arrange(match_id, team_id, t)
  lst <- split(d, paste(d$match_id, d$team_id))
  lapply(lst, function(g) list(t = g$t, obv = cumsum(g$obv), xg = cumsum(g$xg)))
}
suma_intervalo <- function(ac, clave, a, b, var) {
  g <- ac[[clave]]
  if (is.null(g) || length(g$t) == 0) return(0)
  f <- function(z) { k <- findInterval(z, g$t, left.open = TRUE); if (k == 0) 0 else g[[var]][k] }
  f(b) - f(a)
}

# Contraccion empirica de Bayes de medias por grupo
contraer <- function(media, n, var_dentro) {
  ok <- !is.na(media) & n > 0
  if (sum(ok) < 5) return(media)
  ruido <- var_dentro / n
  tau2 <- max(stats::var(media[ok]) - mean(ruido[ok]), 0)
  media * tau2 / (tau2 + ruido)
}

# ── 2. Cambios ────────────────────────────────────────────────
# Cada "ventana de cambio" junta los cambios simultaneos de un equipo
# (menos de un minuto entre ellos). Se compara la diferencia de OBV y
# de xG del equipo contra el rival por 90 minutos en los 15 minutos
# antes y despues. Como los cambios se hacen cuando el partido ya va
# de cierta forma, se resta lo que en promedio cambia el partido
# despues de un cambio en ese mismo tramo de minutos y con ese mismo
# marcador (perdiendo, empatando, ganando).
analizar_cambios <- function(e, ac = acumulados(e)) {
  subs <- e %>% filter(type == "Substitution", !is.na(sub_replacement_id)) %>%
    select(match_id, torneo, orden_torneo, temporada, team_id, rival_id, t,
           sale = player_id, entra = sub_replacement_id, motivo = sub_outcome) %>%
    arrange(match_id, team_id, t) %>%
    group_by(match_id, team_id) %>%
    mutate(ventana = cumsum(c(1, diff(t) > 1))) %>% ungroup()
  if (nrow(subs) == 0) return(NULL)

  gol <- goles_partido(e)
  vent <- subs %>%
    group_by(match_id, torneo, orden_torneo, temporada, team_id, rival_id, ventana) %>%
    summarise(t0 = min(t), n_cambios = n(), lesion = any(motivo %in% "Injury"), .groups = "drop")

  calc <- function(i) {
    v <- vent[i, ]
    a <- max(0, v$t0 - VENTANA_CAMBIO); b <- min(FIN_PARTIDO, v$t0 + VENTANA_CAMBIO)
    if (v$t0 - a < MIN_VENTANA || b - v$t0 < MIN_VENTANA) return(NULL)
    kt <- paste(v$match_id, v$team_id); kr <- paste(v$match_id, v$rival_id)
    tasa <- function(x0, x1, var) 90 * (suma_intervalo(ac, kt, x0, x1, var) - suma_intervalo(ac, kr, x0, x1, var)) / (x1 - x0)
    g <- gol[gol$match_id == v$match_id & gol$t < v$t0, ]
    dif <- sum(g$team_id == v$team_id) - sum(g$team_id == v$rival_id)
    tibble(i = i, obv_antes = tasa(a, v$t0, "obv"), obv_despues = tasa(v$t0, b, "obv"),
           xg_antes = tasa(a, v$t0, "xg"), xg_despues = tasa(v$t0, b, "xg"),
           estado = ifelse(dif < 0, "Perdiendo", ifelse(dif == 0, "Empatando", "Ganando")))
  }
  r <- bind_rows(lapply(seq_len(nrow(vent)), calc))
  if (nrow(r) == 0) return(NULL)
  vent <- vent[r$i, ] %>% bind_cols(r %>% select(-i)) %>%
    mutate(tramo = cut(t0, c(-Inf, 55, 65, 75, Inf), labels = c("Hasta 55'", "56'-65'", "66'-75'", "76' o más")),
           d_obv = obv_despues - obv_antes, d_xg = xg_despues - xg_antes) %>%
    group_by(tramo, estado) %>%
    mutate(esperado_obv = mean(d_obv), esperado_xg = mean(d_xg), n_celda = n()) %>%
    ungroup() %>%
    mutate(impacto_obv = d_obv - esperado_obv, impacto_xg = d_xg - esperado_xg)
  list(ventanas = vent,
       detalle = subs %>% inner_join(vent %>% select(match_id, team_id, ventana, t0, tramo, estado,
                                                     impacto_obv, impacto_xg, n_cambios),
                                     by = c("match_id", "team_id", "ventana")))
}

# Cambios de formacion durante el partido (Tactical Shift). Mismo
# metodo que los cambios de jugador. Si el cambio tactico coincide con
# una sustitucion (StatsBomb suele registrarlos juntos), el efecto es
# de los dos a la vez: se marca.
analizar_cambios_tacticos <- function(evs, e, ac) {
  tac <- evs$tacticas
  if (is.null(tac) || nrow(tac) == 0) return(NULL)
  f <- tac %>% distinct(match_id, team_id, type, t, formation) %>% arrange(match_id, team_id, t) %>%
    group_by(match_id, team_id) %>% mutate(desde = lag(formation)) %>% ungroup() %>%
    filter(type == "Tactical Shift", !is.na(desde), desde != formation) %>%
    mutate(de = formacion_a_texto(desde), a = formacion_a_texto(formation))
  if (nrow(f) == 0) return(NULL)
  info <- e %>% distinct(match_id, team_id, rival_id, torneo, orden_torneo)
  f <- f %>% inner_join(info, by = c("match_id", "team_id"))
  subs_t <- e %>% filter(type == "Substitution") %>% select(match_id, team_id, ts = t)
  gol <- goles_partido(e)
  r <- bind_rows(lapply(seq_len(nrow(f)), function(i) {
    v <- f[i, ]; t0 <- v$t
    a <- max(0, t0 - VENTANA_CAMBIO); b <- min(FIN_PARTIDO, t0 + VENTANA_CAMBIO)
    if (t0 - a < MIN_VENTANA || b - t0 < MIN_VENTANA) return(NULL)
    kt <- paste(v$match_id, v$team_id); kr <- paste(v$match_id, v$rival_id)
    tasa <- function(x0, x1, var) 90 * (suma_intervalo(ac, kt, x0, x1, var) - suma_intervalo(ac, kr, x0, x1, var)) / (x1 - x0)
    g <- gol[gol$match_id == v$match_id & gol$t < t0, ]
    dif <- sum(g$team_id == v$team_id) - sum(g$team_id == v$rival_id)
    s <- subs_t[subs_t$match_id == v$match_id & subs_t$team_id == v$team_id, ]
    tibble(i = i, d_obv = tasa(t0, b, "obv") - tasa(a, t0, "obv"), d_xg = tasa(t0, b, "xg") - tasa(a, t0, "xg"),
           estado = ifelse(dif < 0, "Perdiendo", ifelse(dif == 0, "Empatando", "Ganando")),
           con_sustitucion = any(abs(s$ts - t0) < 1))
  }))
  if (nrow(r) == 0) return(NULL)
  f[r$i, ] %>% bind_cols(r %>% select(-i)) %>%
    mutate(tramo = cut(t, c(-Inf, 55, 65, 75, Inf), labels = c("Hasta 55'", "56'-65'", "66'-75'", "76' o más"))) %>%
    group_by(tramo, estado) %>% mutate(impacto_obv = d_obv - mean(d_obv), impacto_xg = d_xg - mean(d_xg)) %>% ungroup()
}

resumir_cambios_tacticos <- function(ct, filtro_torneos = NULL) {
  if (is.null(ct)) return(NULL)
  d <- if (is.null(filtro_torneos)) ct else ct %>% filter(torneo %in% filtro_torneos)
  if (nrow(d) == 0) return(NULL)
  v <- stats::var(d$impacto_obv)
  d %>% group_by(team_id, de, a) %>%
    summarise(veces = n(), minuto_medio = mean(t), pct_con_sustitucion = 100 * mean(con_sustitucion),
              pct_perdiendo = 100 * mean(estado == "Perdiendo"),
              impacto_obv = mean(impacto_obv), impacto_xg = mean(impacto_xg), .groups = "drop") %>%
    mutate(impacto_obv_contraido = contraer(impacto_obv, veces, v))
}

# Resumen por jugador que entra, por club (decisiones del cuerpo
# tecnico) y por club y tramo de minutos.
resumir_cambios <- function(cb, filtro_torneos = NULL) {
  if (is.null(cb)) return(NULL)
  v <- cb$ventanas; d <- cb$detalle
  if (!is.null(filtro_torneos)) { v <- v %>% filter(torneo %in% filtro_torneos); d <- d %>% filter(torneo %in% filtro_torneos) }
  var_v <- stats::var(v$impacto_obv, na.rm = TRUE)
  jug <- d %>% group_by(player_id = entra, team_id) %>%
    summarise(entradas = n(), ee_obv = sd(impacto_obv) / sqrt(n()),
              impacto_obv = mean(impacto_obv), impacto_xg = mean(impacto_xg),
              minuto_medio = mean(t0), .groups = "drop") %>%
    mutate(impacto_obv_contraido = contraer(impacto_obv, entradas, var_v))
  club <- v %>% group_by(team_id) %>%
    summarise(ventanas = n(), ee_obv = sd(impacto_obv) / sqrt(n()),
              impacto_obv = mean(impacto_obv), impacto_xg = mean(impacto_xg), minuto_medio = mean(t0),
              pct_perdiendo = 100 * mean(estado == "Perdiendo"), .groups = "drop") %>%
    mutate(impacto_obv_contraido = contraer(impacto_obv, ventanas, var_v))
  club_tramo <- v %>% group_by(team_id, tramo) %>%
    summarise(ventanas = n(), impacto_obv = mean(impacto_obv), impacto_xg = mean(impacto_xg),
              .groups = "drop") %>%
    mutate(impacto_obv_contraido = contraer(impacto_obv, ventanas, var_v))
  liga_tramo <- v %>% group_by(tramo, estado) %>%
    summarise(ventanas = n(), cambio_obv_medio = mean(d_obv), cambio_xg_medio = mean(d_xg), .groups = "drop")
  list(jugador = jug, club = club, club_tramo = club_tramo, liga_tramo = liga_tramo)
}

# ── 3. Formaciones ────────────────────────────────────────────
# Tramos de partido con una misma formacion (del Starting XI hasta el
# siguiente cambio tactico). xG y OBV por 90 de cada tramo; despues
# se ajusta por nivel del rival y localia con una regresion de toda
# la liga, ponderada por minutos.
tramos_formacion <- function(evs, e, ac, niv) {
  tac <- evs$tacticas
  if (is.null(tac) || nrow(tac) == 0) return(NULL)
  f <- tac %>% distinct(match_id, id, team_id, t, formation) %>%
    arrange(match_id, team_id, t) %>%
    group_by(match_id, team_id) %>%
    mutate(fin = lead(t, default = FIN_PARTIDO)) %>% ungroup() %>%
    filter(fin - t >= 5) %>%
    mutate(formacion = formacion_a_texto(formation))
  info <- e %>% distinct(match_id, team_id, rival_id, local, torneo, orden_torneo, temporada)
  f <- f %>% inner_join(info, by = c("match_id", "team_id"))
  r <- bind_rows(lapply(seq_len(nrow(f)), function(i) {
    kt <- paste(f$match_id[i], f$team_id[i]); kr <- paste(f$match_id[i], f$rival_id[i])
    a <- f$t[i]; b <- f$fin[i]
    tibble(xg_f = suma_intervalo(ac, kt, a, b, "xg"), xg_c = suma_intervalo(ac, kr, a, b, "xg"),
           obv_f = suma_intervalo(ac, kt, a, b, "obv"), obv_c = suma_intervalo(ac, kr, a, b, "obv"))
  }))
  f <- bind_cols(f, r) %>%
    mutate(minutos = fin - t, xgd90 = 90 * (xg_f - xg_c) / minutos, obvd90 = 90 * (obv_f - obv_c) / minutos) %>%
    left_join(niv %>% select(rival_id = team_id, torneo, nivel_rival = nivel), by = c("rival_id", "torneo")) %>%
    mutate(nivel_rival = coalesce(nivel_rival, 70))
  m1 <- lm(xgd90 ~ nivel_rival + local, data = f, weights = minutos)
  m2 <- lm(obvd90 ~ nivel_rival + local, data = f, weights = minutos)
  f$xgd90_aj <- f$xgd90 - (predict(m1, f) - weighted.mean(predict(m1, f), f$minutos))
  f$obvd90_aj <- f$obvd90 - (predict(m2, f) - weighted.mean(predict(m2, f), f$minutos))
  f
}

resumir_formaciones <- function(tf, filtro_torneos = NULL) {
  if (is.null(tf)) return(NULL)
  d <- if (is.null(filtro_torneos)) tf else tf %>% filter(torneo %in% filtro_torneos)
  wv <- function(x, w) sum(w * (x - weighted.mean(x, w))^2) / sum(w)
  v_dentro <- wv(d$xgd90_aj, d$minutos) * 90   # varianza por partido completo, aprox.
  d %>% group_by(team_id, formacion) %>%
    # OJO: los promedios ponderados van antes de minutos = sum(minutos),
    # que sobrescribe la columna dentro de summarise.
    summarise(tramos = n(),
              xgd90 = weighted.mean(xgd90, minutos), xgd90_aj = weighted.mean(xgd90_aj, minutos),
              obvd90_aj = weighted.mean(obvd90_aj, minutos),
              xg90 = 90 * sum(xg_f) / sum(minutos), xga90 = 90 * sum(xg_c) / sum(minutos),
              minutos = sum(minutos), .groups = "drop") %>%
    group_by(team_id) %>%
    mutate(pct_minutos = 100 * minutos / sum(minutos),
           media_club = weighted.mean(xgd90_aj, minutos), vs_club = xgd90_aj - media_club) %>%
    ungroup() %>%
    # Se contrae la diferencia contra el promedio del propio club: con
    # pocos minutos, la formacion se parece al club (no a cero).
    mutate(vs_club_contraido = contraer(vs_club, minutos, v_dentro),
           xgd90_contraido = media_club + vs_club_contraido)
}

# ¿La calidad del XI predice el partido? Indice medio (con contraccion)
# de los titulares en el torneo ANTERIOR, equipo menos rival, contra la
# diferencia de xG del partido. Usa solo alineaciones, asi que corre
# aunque no haya eventos.
validar_calidad_xi <- function(cr, part, perf_t, mp = NULL) {
  ord <- sort(unique(perf_t$orden_torneo))
  q <- perf_t %>% group_by(player_id, orden_torneo) %>%
    slice_max(minutos_total, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(player_id, orden_sig = ord[match(orden_torneo, ord) + 1], q = z_compuesto_fiable) %>%
    filter(!is.na(orden_sig))
  tit <- cr$jugadores %>% filter(titular) %>% select(match_id, team_id, player_id) %>%
    inner_join(part %>% select(match_id, orden_torneo, torneo, home_team_id, away_team_id), by = "match_id") %>%
    left_join(q, by = c("player_id", "orden_torneo" = "orden_sig"))
  xi <- tit %>% group_by(match_id, team_id, torneo, home_team_id, away_team_id) %>%
    summarise(calidad_xi = mean(q, na.rm = TRUE), cubiertos = sum(!is.na(q)), .groups = "drop") %>%
    filter(cubiertos >= 6) %>%
    mutate(rival_id = ifelse(team_id == home_team_id, away_team_id, home_team_id),
           local = team_id == home_team_id)
  xi <- xi %>% inner_join(xi %>% select(match_id, rival_id = team_id, calidad_rival = calidad_xi),
                          by = c("match_id", "rival_id")) %>%
    mutate(dif_calidad = calidad_xi - calidad_rival)
  # Resultado: xG del partido (de team_match, que existe aunque no haya eventos)
  tm <- cr$tm %>% transmute(match_id = as.integer(match_id), team_id = as.integer(team_id),
                            np_xg = suppressWarnings(as.numeric(team_match_np_xg)))
  xi <- xi %>% inner_join(tm, by = c("match_id", "team_id")) %>%
    inner_join(tm %>% rename(rival_id = team_id, np_xg_c = np_xg), by = c("match_id", "rival_id")) %>%
    mutate(xgd = np_xg - np_xg_c) %>% filter(local)   # una fila por partido
  if (nrow(xi) < 30) return(NULL)
  m <- lm(xgd ~ dif_calidad, data = xi)
  rho <- suppressWarnings(cor(xi$dif_calidad, xi$xgd, method = "spearman"))
  b <- replicate(500, { s <- xi[sample(nrow(xi), replace = TRUE), ]
    suppressWarnings(cor(s$dif_calidad, s$xgd, method = "spearman")) })
  list(partidos = nrow(xi), pendiente = unname(coef(m)[2]), ee = summary(m)$coef[2, 2],
       rho = rho, rho_lo = unname(quantile(b, .025)), rho_hi = unname(quantile(b, .975)),
       datos = xi %>% select(match_id, torneo, team_id, rival_id, dif_calidad, xgd))
}

# Mejor XI para una formacion con la plantilla de un club. Llena
# primero los puestos mas escasos; si no hay jugador del subgrupo,
# usa uno de un subgrupo vecino con castigo. 10 jugadores de campo
# (el modelo no evalua porteros).
VECINOS <- list(CB = c("DM", "FB"), FB = c("W", "CB"), DM = c("CM", "CB"), CM = c("DM", "AM"),
                AM = c("CM", "W", "ST"), W = c("FB", "AM", "ST"), ST = c("AM", "W"))
CASTIGO_VECINO <- 0.5

mejor_xi <- function(plantilla, formacion) {
  cupos <- titulares_de_formacion(formacion)
  cupos <- cupos[cupos > 0]
  pl <- plantilla %>% filter(!is.na(z_compuesto_fiable)) %>% arrange(desc(z_compuesto_fiable))
  usados <- character(0); filas <- list()
  orden <- names(cupos)[order(vapply(names(cupos), function(s) sum(pl$subgrupo == s) / cupos[[s]], numeric(1)))]
  for (s in orden) for (k in seq_len(cupos[[s]])) {
    disp <- pl[!pl$jugador_id %in% usados, ]
    nat <- disp[disp$subgrupo == s, ]
    if (nrow(nat) > 0) { j <- nat[1, ]; val <- j$z_compuesto_fiable; tipo <- "Natural" }
    else {
      vec <- disp[disp$subgrupo %in% VECINOS[[s]], ]
      if (nrow(vec) == 0) { filas[[length(filas) + 1]] <- tibble(puesto = s, jugador_id = NA_character_,
                                                                 player_name = "(vacante)", subgrupo = NA_character_,
                                                                 z = NA_real_, valor = -1, tipo = "Vacante"); next }
      j <- vec[1, ]; val <- j$z_compuesto_fiable - CASTIGO_VECINO; tipo <- paste("Fuera de puesto:", j$subgrupo)
    }
    usados <- c(usados, j$jugador_id)
    filas[[length(filas) + 1]] <- tibble(puesto = s, jugador_id = j$jugador_id, player_name = j$player_name,
                                         subgrupo = j$subgrupo, z = j$z_compuesto_fiable, valor = val, tipo = tipo)
  }
  xi <- bind_rows(filas)
  # Suplente de cada puesto: el mejor natural que no quedo en el XI
  resto <- pl[!pl$jugador_id %in% usados, ]
  prof <- vapply(names(cupos), function(s) { r <- resto$z_compuesto_fiable[resto$subgrupo == s]
    if (length(r) == 0) NA_real_ else mean(head(r, cupos[[s]])) }, numeric(1))
  list(xi = xi, formacion = formacion, calidad = mean(xi$valor), fuera_de_puesto = sum(xi$tipo != "Natural"),
       profundidad = mean(prof, na.rm = TRUE))
}

comparar_formaciones <- function(plantilla, formaciones = names(TABLA_FORMACIONES)) {
  formaciones <- unique(formaciones)
  bind_rows(lapply(formaciones, function(f) {
    r <- mejor_xi(plantilla, f)
    tibble(formacion = f, calidad_xi = r$calidad, fuera_de_puesto = r$fuera_de_puesto, profundidad = r$profundidad)
  })) %>% arrange(desc(calidad_xi))
}

# ── 4. Balon parado ───────────────────────────────────────────
# y relativa al lado del corner: 0 = banda desde la que se saca.
y_rel <- function(y_saque, y) ifelse(y_saque < 40, y, 80 - y)

zona_entrega <- function(x, yr) {
  dplyr::case_when(
    is.na(x) ~ "Sin dato",
    x < 102 | yr < 18 | yr > 62 ~ ifelse(x >= 96 & yr >= 18 & yr <= 62, "Borde del área", "Corto / fuera del área"),
    x >= 114 & yr >= 30 & yr <= 50 ~ "Área chica",
    yr < 36 ~ "Primer palo",
    yr > 44 ~ "Segundo palo",
    TRUE ~ "Punto penal")
}
ZONAS_ENTREGA <- c("Primer palo", "Área chica", "Punto penal", "Segundo palo", "Borde del área", "Corto / fuera del área")

zona_tiro <- function(x, y) {
  dplyr::case_when(
    is.na(x) ~ "Sin dato",
    x >= 114 & y >= 30 & y <= 50 ~ "Área chica",
    x >= 102 & y >= 30 & y <= 50 ~ "Área: centro",
    x >= 102 & y >= 18 & y < 30 ~ "Área: costado izquierdo",
    x >= 102 & y > 50 & y <= 62 ~ "Área: costado derecho",
    TRUE ~ "Fuera del área")
}

# Un saque por fila: corners, tiros libres (desde x >= 60) y laterales
# en el ultimo tercio, con lo que pasa despues en esa posesion.
saques_balon_parado <- function(e) {
  s <- e %>% filter(type == "Pass", pass_type %in% c("Corner", "Free Kick", "Throw-in")) %>%
    filter(pass_type == "Corner" | (pass_type == "Free Kick" & x >= 60) | (pass_type == "Throw-in" & x >= 80))
  # Tiros libres directos (sin pase)
  td <- e %>% filter(type == "Shot", shot_type %in% "Free Kick") %>%
    transmute(match_id, id, index, t, team_id, rival_id, torneo, orden_torneo, temporada, possession,
              player_id, x, y, end_x = NA_real_, end_y = NA_real_, pass_type = "Free Kick (directo)",
              pass_technique = NA_character_, pass_height = NA_character_)
  s <- bind_rows(
    s %>% select(match_id, id, index, t, team_id, rival_id, torneo, orden_torneo, temporada, possession,
                 player_id, x, y, end_x, end_y, pass_type, pass_technique, pass_height),
    td)
  if (nrow(s) == 0) return(NULL)
  tiros <- e %>% filter(es_tiro_np(e)) %>%
    select(match_id, possession, team_id, index_tiro = index, xg = shot_xg, gol = shot_outcome,
           tiro_x = x, tiro_y = y, cabeza = shot_body_part, rematador = player_id, tiro_id = id)
  # Tiros del mismo equipo en la misma posesion, despues del saque
  st <- s %>% select(match_id, possession, team_id, id, index) %>%
    inner_join(tiros, by = c("match_id", "possession", "team_id"), relationship = "many-to-many") %>%
    filter(index_tiro >= index)
  res <- st %>% group_by(id) %>%
    summarise(tiros = n(), xg = sum(xg, na.rm = TRUE), goles = sum(gol == "Goal", na.rm = TRUE),
              primer_tiro_x = tiro_x[1], primer_tiro_y = tiro_y[1], .groups = "drop")
  # Primer contacto: primer evento con balon despues del saque
  contacto <- c("Shot", "Clearance", "Duel", "Interception", "Goal Keeper", "Pass", "Miscontrol",
                "Block", "Ball Recovery", "Carry")
  # Solo los 4 eventos siguientes a cada saque (cruzar con todo el
  # partido serian ~100 millones de filas con datos reales)
  ev_c <- e %>% filter(type %in% contacto) %>% select(match_id, index_c = index, team_c = team_id)
  cand <- s %>% select(id, match_id, index, team_id) %>%
    tidyr::crossing(paso = 1:4) %>% mutate(index_c = index + paso)
  pc <- cand %>% inner_join(ev_c, by = c("match_id", "index_c")) %>%
    group_by(id) %>% slice_min(paso, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(id, primer_contacto_ataque = team_c == team_id)
  s %>% left_join(res, by = "id") %>% left_join(pc, by = "id") %>%
    mutate(tiros = coalesce(tiros, 0L), xg = coalesce(xg, 0), goles = coalesce(goles, 0L),
           corto = pass_type == "Corner" & (is.na(end_x) | end_x < 102 | pass_height %in% "Ground Pass"),
           yr_entrega = ifelse(pass_type == "Corner", y_rel(y, end_y), end_y),
           zona = ifelse(pass_type == "Corner", zona_entrega(end_x, yr_entrega), zona_entrega(end_x, end_y)),
           tipo = recode(pass_type, Corner = "Córner", `Free Kick` = "Tiro libre indirecto",
                         `Free Kick (directo)` = "Tiro libre directo", `Throw-in` = "Lateral en último tercio"))
}

resumir_bp <- function(sq, lado = c("favor", "contra"), filtro_torneos = NULL) {
  lado <- match.arg(lado)
  d <- if (is.null(filtro_torneos)) sq else sq %>% filter(torneo %in% filtro_torneos)
  d$club <- if (lado == "favor") d$team_id else d$rival_id
  d %>% group_by(team_id = club, tipo) %>%
    # Promedios por saque ANTES de las sumas (summarise sobrescribe en orden)
    summarise(saques = n(), partidos = n_distinct(match_id),
              xg_por_saque = mean(xg), pct_con_tiro = 100 * mean(tiros > 0),
              tiros = sum(tiros), xg = sum(xg), goles = sum(goles),
              pct_primer_contacto = 100 * mean(primer_contacto_ataque, na.rm = TRUE),
              pct_cortos = 100 * mean(corto, na.rm = TRUE),
              pct_cerrado = 100 * mean(pass_technique %in% "Inswinging"),
              pct_abierto = 100 * mean(pass_technique %in% "Outswinging"), .groups = "drop") %>%
    group_by(tipo) %>%
    mutate(xg_por_saque_liga = sum(xg) / sum(saques),
           indice = 100 * xg_por_saque / xg_por_saque_liga,
           rk = if (lado == "favor") rank(-xg_por_saque, ties.method = "min") else rank(xg_por_saque, ties.method = "min")) %>%
    ungroup()
}

# Zonas de entrega del corner: cuanto se manda a cada zona y cuanto xG
# sale de ahi, contra el promedio de la liga en esa zona.
zonas_corner <- function(sq, lado = c("favor", "contra"), filtro_torneos = NULL) {
  lado <- match.arg(lado)
  d <- sq %>% filter(tipo == "Córner")
  if (!is.null(filtro_torneos)) d <- d %>% filter(torneo %in% filtro_torneos)
  d$club <- if (lado == "favor") d$team_id else d$rival_id
  liga <- d %>% group_by(zona) %>% summarise(xg_liga = mean(xg), pct_liga = n() / nrow(d) * 100, .groups = "drop")
  d %>% group_by(team_id = club) %>% mutate(n_club = n()) %>%
    group_by(team_id, zona) %>%
    summarise(saques = n(), pct = 100 * n() / first(n_club), xg_por_saque = mean(xg), xg = sum(xg),
              tiros = sum(tiros), .groups = "drop") %>%
    left_join(liga, by = "zona") %>%
    mutate(indice = 100 * xg_por_saque / pmax(xg_liga, 1e-6),
           zona = factor(zona, levels = c(ZONAS_ENTREGA, "Sin dato")))
}

# Zonas de remate de los tiros de balon parado
zonas_remate <- function(e, lado = c("favor", "contra"), filtro_torneos = NULL) {
  lado <- match.arg(lado)
  d <- e %>% filter(es_tiro_np(e), play_pattern %in% c("From Corner", "From Free Kick", "From Throw In"))
  if (!is.null(filtro_torneos)) d <- d %>% filter(torneo %in% filtro_torneos)
  d$club <- if (lado == "favor") d$team_id else d$rival_id
  d %>% mutate(zona = zona_tiro(x, y)) %>%
    group_by(team_id = club, zona) %>%
    summarise(tiros = n(), xg = sum(shot_xg, na.rm = TRUE), goles = sum(shot_outcome == "Goal"),
              cabezazos = sum(shot_body_part %in% "Head"), .groups = "drop")
}

# Puntos de remate individuales (para dibujar en la cancha)
tiros_balon_parado <- function(e) {
  e %>% filter(es_tiro_np(e), play_pattern %in% c("From Corner", "From Free Kick", "From Throw In")) %>%
    transmute(match_id, torneo, team_id, rival_id, player_id, x, y, xg = shot_xg,
              gol = shot_outcome == "Goal", cabeza = shot_body_part %in% "Head",
              patron = recode(play_pattern, `From Corner` = "Córner", `From Free Kick` = "Tiro libre",
                              `From Throw In` = "Lateral"))
}

rematadores_bp <- function(e, filtro_torneos = NULL) {
  tiros_balon_parado(e) %>% { if (is.null(filtro_torneos)) . else filter(., torneo %in% filtro_torneos) } %>%
    group_by(player_id, team_id) %>%
    summarise(tiros = n(), xg = sum(xg, na.rm = TRUE), goles = sum(gol), cabezazos = sum(cabeza), .groups = "drop")
}

lanzadores_bp <- function(sq, filtro_torneos = NULL) {
  sq %>% filter(tipo == "Córner") %>% { if (is.null(filtro_torneos)) . else filter(., torneo %in% filtro_torneos) } %>%
    group_by(player_id, team_id) %>%
    summarise(corners = n(), xg_por_corner = mean(xg), xg_generado = sum(xg),
              pct_primer_contacto = 100 * mean(primer_contacto_ataque, na.rm = TRUE),
              pct_cerrado = 100 * mean(pass_technique %in% "Inswinging"), .groups = "drop")
}

# Responsabilidad defensiva (v11): en cada saque, StatsBomb da a cada
# defensor la probabilidad de que le toque responder. Se reparte el xG
# que sale de ese saque segun esa probabilidad: "xG concedido bajo su
# responsabilidad".
responsabilidad_bp <- function(evs, sq, filtro_torneos = NULL) {
  rd <- evs$resp_def
  if (is.null(rd) || nrow(rd) == 0) return(NULL)
  d <- sq %>% select(id, match_id, torneo, rival_id, xg, tiros)
  if (!is.null(filtro_torneos)) d <- d %>% filter(torneo %in% filtro_torneos)
  rd %>% inner_join(d, by = c("id", "match_id")) %>%
    group_by(id) %>% mutate(p = prob / sum(prob, na.rm = TRUE)) %>% ungroup() %>%
    group_by(player_id, team_id = rival_id) %>%
    summarise(saques = n(), prob_media = mean(prob, na.rm = TRUE),
              xg_responsable = sum(p * xg, na.rm = TRUE),
              xg_resp_por_saque = sum(p * xg, na.rm = TRUE) / n(), .groups = "drop")
}

# Organizacion en el momento del saque (360). Del lado del que
# defiende: cuantos defensores hay en el area y en el area chica, y
# que tan pegados estan a un atacante. "Al hombre" = la mayoria de los
# defensores del area esta a menos de 2 yardas de un atacante; "zonal"
# = la mayoria no. Del lado del que ataca: cuantos atacantes hay y en
# que zonas se colocan.
organizacion_360 <- function(fr, sq) {
  if (is.null(fr) || nrow(fr) == 0) return(NULL)
  info <- sq %>% filter(tipo %in% c("Córner", "Tiro libre indirecto", "Lateral en último tercio")) %>%
    select(id, match_id, torneo, tipo, team_id, rival_id, y_saque = y, xg, tiros)
  f <- fr %>% inner_join(info, by = c("id", "match_id")) %>%
    mutate(en_area = x >= 102 & y >= 18 & y <= 62, en_chica = x >= 114 & y >= 30 & y <= 50,
           yr = ifelse(tipo == "Córner", y_rel(y_saque, y), y))
  # Vectorizado: cada defensor del area contra cada atacante del area
  # del mismo cuadro, y su distancia al mas cercano.
  at <- f %>% filter(teammate, !actor, en_area) %>% select(id, ax = x, ay = y)
  de <- f %>% filter(!teammate, !keeper, en_area) %>% mutate(k = row_number()) %>% select(id, k, x, y)
  pegado <- de %>% inner_join(at, by = "id", relationship = "many-to-many") %>%
    mutate(d = sqrt((x - ax)^2 + (y - ay)^2)) %>%
    group_by(id, k) %>% summarise(dmin = min(d), .groups = "drop") %>%
    group_by(id) %>% summarise(pct_pegados = 100 * mean(dmin < 2), .groups = "drop")
  por_saque <- f %>% group_by(id, match_id, torneo, tipo, team_id, rival_id, xg, tiros) %>%
    summarise(atacantes_area = sum(teammate & !actor & en_area),
              defensores_area = sum(!teammate & !keeper & en_area),
              defensores_chica = sum(!teammate & !keeper & en_chica),
              at_primer_palo = sum(teammate & !actor & en_area & yr < 36),
              at_centro = sum(teammate & !actor & en_area & yr >= 36 & yr <= 44),
              at_segundo_palo = sum(teammate & !actor & en_area & yr > 44), .groups = "drop") %>%
    left_join(pegado, by = "id")
  defensa <- por_saque %>% group_by(team_id = rival_id, tipo) %>%
    summarise(saques_360 = n(), defensores_area = mean(defensores_area), defensores_chica = mean(defensores_chica),
              pct_pegados = mean(pct_pegados, na.rm = TRUE), xg_concedido_por_saque = mean(xg), .groups = "drop") %>%
    mutate(estilo_marcaje = case_when(pct_pegados >= 60 ~ "Al hombre", pct_pegados <= 35 ~ "Zonal", TRUE ~ "Mixto"))
  ataque <- por_saque %>% group_by(team_id, tipo) %>%
    summarise(saques_360 = n(), atacantes_area = mean(atacantes_area), at_primer_palo = mean(at_primer_palo),
              at_centro = mean(at_centro), at_segundo_palo = mean(at_segundo_palo), .groups = "drop")
  list(por_saque = por_saque, defensa = defensa, ataque = ataque)
}

# ── 4b. Balon parado por jugador y partido (indice 6) ─────────
# Totales por jugador y partido, para sumarse a la base del modelo de
# jugadores (02_pipeline.R los pasa a tasas por 90):
#   bp_xg          xG sin penales de sus tiros en jugadas de balon parado
#   bp_xg_lanzado  xG que sale de los corners y tiros libres que cobra
#   bp_aereos      duelos aereos ganados en esas jugadas (despejes y
#                  pases de cabeza ganados, y remates de cabeza)
#   bp_resp_xg     xG concedido bajo su responsabilidad defensiva (v11)
# Los partidos con eventos traen 0 para quien jugo y no registro nada;
# los partidos sin eventos quedan NA (no cuentan).
metricas_bp_partido <- function(e, evs, sq = saques_balon_parado(e)) {
  patrones <- c("From Corner", "From Free Kick", "From Throw In")
  enbp <- e$play_pattern %in% patrones
  tiro <- e %>% filter(es_tiro_np(e), enbp) %>%
    group_by(match_id, player_id) %>% summarise(bp_xg = sum(shot_xg, na.rm = TRUE),
                                                cabezazos = sum(shot_body_part %in% "Head"), .groups = "drop")
  aer_pass <- if ("pass_aerial_won" %in% names(e)) e$pass_aerial_won %in% TRUE else FALSE
  aer_clr  <- if ("clearance_aerial_won" %in% names(e)) e$clearance_aerial_won %in% TRUE else FALSE
  aer <- e[enbp & (aer_pass | aer_clr), ] %>% count(match_id, player_id, name = "aereos")
  lanz <- sq %>% filter(tipo %in% c("Córner", "Tiro libre indirecto")) %>%
    group_by(match_id, player_id) %>% summarise(bp_xg_lanzado = sum(xg), .groups = "drop")
  resp <- if (!is.null(evs$resp_def) && nrow(evs$resp_def) > 0)
    evs$resp_def %>% inner_join(sq %>% select(id, match_id, xg), by = c("id", "match_id")) %>%
      group_by(id) %>% mutate(p = prob / sum(prob, na.rm = TRUE)) %>% ungroup() %>%
      group_by(match_id, player_id) %>% summarise(bp_resp_xg = sum(p * xg, na.rm = TRUE), .groups = "drop")
    else tibble(match_id = integer(), player_id = integer(), bp_resp_xg = numeric())
  jug <- e %>% filter(!is.na(player_id)) %>% distinct(match_id, player_id)
  jug %>% left_join(tiro, by = c("match_id", "player_id")) %>%
    left_join(aer, by = c("match_id", "player_id")) %>%
    left_join(lanz, by = c("match_id", "player_id")) %>%
    left_join(resp, by = c("match_id", "player_id")) %>%
    transmute(match_id, player_id,
              bp_xg = coalesce(bp_xg, 0), bp_xg_lanzado = coalesce(bp_xg_lanzado, 0),
              bp_aereos = coalesce(aereos, 0L) + coalesce(cabezazos, 0L),
              bp_resp_xg = coalesce(bp_resp_xg, 0))
}

# ── 4c. Mapas de calor por jugador ────────────────────────────
# Acciones con ubicacion por jugador, temporada y club, contadas en una
# rejilla de 5 x 5 yardas (24 x 16 celdas). Siempre en la direccion de
# ataque del equipo del jugador.
CATEGORIAS_MAPA <- list(
  "Todas las acciones"  = NULL,
  "Pases"               = "Pass",
  "Conducciones"        = "Carry",
  "Presiones"           = "Pressure",
  "Acciones defensivas" = c("Duel", "Interception", "Clearance", "Ball Recovery", "Block", "Foul Committed"),
  "Tiros"               = "Shot")
TAM_CELDA <- 5

mapas_jugador <- function(e) {
  d <- e %>% filter(!is.na(player_id), !is.na(x), !is.na(y),
                    !type %in% c("Substitution", "Starting XI", "Tactical Shift")) %>%
    transmute(player_id, temporada, team_id, type,
              bx = pmin(floor(x / TAM_CELDA), 120 / TAM_CELDA - 1),
              by = pmin(floor(y / TAM_CELDA), 80 / TAM_CELDA - 1))
  bind_rows(lapply(names(CATEGORIAS_MAPA), function(cat) {
    tipos <- CATEGORIAS_MAPA[[cat]]
    dd <- if (is.null(tipos)) d else d[d$type %in% tipos, ]
    dd %>% count(player_id, temporada, team_id, bx, by, name = "n") %>% mutate(categoria = cat)
  })) %>% mutate(bx = as.integer(bx), by = as.integer(by))
}

tiros_jugador <- function(e) {
  e %>% filter(type == "Shot", !is.na(player_id)) %>%
    transmute(player_id, temporada, team_id, x, y, xg = shot_xg, gol = shot_outcome %in% "Goal",
              penal = shot_type %in% "Penalty", cabeza = shot_body_part %in% "Head",
              balon_parado = play_pattern %in% c("From Corner", "From Free Kick", "From Throw In"))
}

# Entradas de cambio por jugador (insumo del indice 7, revulsivo)
entradas_revulsivo <- function(cb, part) {
  if (is.null(cb)) return(NULL)
  cb$detalle %>% transmute(match_id, team_id, player_id = entra, t0, impacto_obv, impacto_xg) %>%
    inner_join(part %>% select(match_id, match_date, torneo, orden_torneo), by = "match_id")
}

# ── 5. Persistencia de un torneo al siguiente ─────────────────
# Correlacion entre clubes del indicador en el torneo k y en el k+1.
# Cerca de 0: el indicador es mas ruido que rasgo del club.
persistencia <- function(tabla, var, etiqueta) {
  d <- tabla %>% select(team_id, orden_torneo, v = all_of(var)) %>% filter(!is.na(v))
  p <- d %>% inner_join(d %>% mutate(orden_torneo = orden_torneo - 1) %>% rename(v_sig = v),
                        by = c("team_id", "orden_torneo"))
  if (nrow(p) < 10) return(tibble(indicador = etiqueta, pares = nrow(p), r = NA_real_, lo = NA_real_, hi = NA_real_))
  r <- suppressWarnings(cor(p$v, p$v_sig, method = "spearman"))
  b <- replicate(500, { s <- p[sample(nrow(p), replace = TRUE), ]; suppressWarnings(cor(s$v, s$v_sig, method = "spearman")) })
  tibble(indicador = etiqueta, pares = nrow(p), r = r,
         lo = unname(quantile(b, .025, na.rm = TRUE)), hi = unname(quantile(b, .975, na.rm = TRUE)))
}
