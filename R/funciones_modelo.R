# ============================================================
# funciones_modelo.R — CIDE OSOS
# Maquinaria del modelo, compartida por 02_pipeline.R,
# 03_validacion.R y la app.
#
# Hereda la logica de la version anterior del modelo (ajuste contextual con splines,
# siete subgrupos, cinco indices, z compuesto con pesos por
# subgrupo, clutch) y cambia cinco cosas:
#
#   1. Sin tribble de calidad. El nivel de cada equipo sale de su
#      diferencial de xG sin penaltis por partido, por torneo.
#   2. Sin fuga temporal. El ajuste contextual se puede estimar solo
#      con partidos anteriores a una fecha de corte (backtest).
#   3. El perfil se construye con residuos: rendimiento por encima
#      de lo esperado para la posicion jugada EN ESE partido y su
#      contexto. Un jugador que alterna central y lateral se mide
#      contra lo esperado para cada puesto.
#   4. Consistencia redefinida. Antes era la desviacion estandar de
#      xG/xA/presiones entre partidos, que premiaba al que produce
#      poco (un lateral con xG ~0 siempre tiene varianza ~0). Ahora:
#      perdidas, desposesiones, precision de pase y la variabilidad
#      de OBV que NO explica el volumen de participacion.
#   5. OBV queda fuera de los indices a proposito: es un modelo de
#      valor independiente de StatsBomb y sirve como validacion
#      externa del z compuesto.
#
# Ademas: edad, disponibilidad, indicador de lesiones a partir de
# las alineaciones, y contraccion del z por minutos jugados.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(splines)
})

LIGA           <- "Liga MX"
MIN_MINUTOS    <- 300   # poblacion de referencia para estandarizar
MIN_MIN_RANKING <- 450  # minimo para aparecer en los rankings de la app (se puede mover ahi)
TOPE_Z         <- 3     # tope (en desv. estandar) de cada componente y de cada indice
MIN_MIN_AJUSTE <- 10    # partidos con menos minutos no entran a estimar el ajuste
MIN_MIN_VAR    <- 30    # partidos con menos minutos no entran a la variabilidad
FUENTE         <- "Fuente: StatsBomb, Liga MX."
FIRMA_OSOS     <- "CIDE OSOS"

# ── Posiciones StatsBomb (1-25) -> subgrupo ───────────────────
# 12/16 (volante por banda) y 18/20 (mediapunta abierto) se tratan
# como extremos, igual que en la version anterior. 25 (segundo
# delantero, SS) va con los mediapuntas: juega por detras del 9.
MAPA_POSICION <- c(
  `2` = "FB", `6` = "FB", `7` = "FB", `8` = "FB",
  `3` = "CB", `4` = "CB", `5` = "CB",
  `9` = "DM", `10` = "DM", `11` = "DM",
  `13` = "CM", `14` = "CM", `15` = "CM",
  `19` = "AM", `25` = "AM",
  `12` = "W", `16` = "W", `17` = "W", `18` = "W", `20` = "W", `21` = "W",
  `22` = "ST", `23` = "ST", `24` = "ST"
)

ABREV_POSICION <- c(
  `1` = "GK", `2` = "RB", `3` = "RCB", `4` = "CB", `5` = "LCB", `6` = "LB",
  `7` = "RWB", `8` = "LWB", `9` = "RDM", `10` = "CDM", `11` = "LDM",
  `12` = "RM", `13` = "RCM", `14` = "CM", `15` = "LCM", `16` = "LM",
  `17` = "RW", `18` = "RAM", `19` = "CAM", `20` = "LAM", `21` = "LW",
  `22` = "RCF", `23` = "ST", `24` = "LCF", `25` = "SS"
)

SUBS_ORDEN   <- c("CB", "FB", "DM", "CM", "AM", "W", "ST")
GRUPO_DE_SUB <- c(CB = "DEF", FB = "DEF", DM = "MID", CM = "MID", AM = "MID",
                  W = "ATT", ST = "ATT")
ETIQUETA_SUB <- c(CB = "Central", FB = "Lateral", DM = "Pivote", CM = "Interior",
                  AM = "Mediapunta", W = "Extremo", ST = "Delantero centro")

# Pesos del z compuesto por subgrupo (mismos que la version anterior). Cada fila suma 1.
PESOS_SUB <- tribble(
  ~subgrupo, ~w_of, ~w_cr, ~w_def, ~w_prog, ~w_cons,
  "CB",       0.05,  0.05,  0.45,   0.25,    0.20,
  "FB",       0.10,  0.20,  0.30,   0.30,    0.10,
  "DM",       0.05,  0.15,  0.40,   0.30,    0.10,
  "CM",       0.15,  0.25,  0.25,   0.25,    0.10,
  "AM",       0.25,  0.35,  0.10,   0.20,    0.10,
  "W",        0.30,  0.35,  0.10,   0.15,    0.10,
  "ST",       0.45,  0.20,  0.10,   0.10,    0.15
)
stopifnot(all(abs(rowSums(PESOS_SUB[, -1]) - 1) < 1e-9))

# Peso del indice 6 (balon parado) en el compuesto, si la validacion
# decide que entra. Los otros cinco se reescalan para que sigan sumando 1.
PESOS_BP <- c(CB = 0.10, FB = 0.05, DM = 0.05, CM = 0.05, AM = 0.05, W = 0.05, ST = 0.10)

pesos_de <- function(sub, con_bp = FALSE) {
  f <- PESOS_SUB[PESOS_SUB$subgrupo == sub, -1]
  w <- if (nrow(f) == 0) rep(0.2, 5) else as.numeric(f[1, ])
  if (!con_bp) return(w)
  wb <- if (sub %in% names(PESOS_BP)) PESOS_BP[[sub]] else 0.05
  c(w * (1 - wb), wb)
}

# El balon parado ya no es indice: sus acciones se muestran como dato.
bp_vigente <- function(dir_proc = "data/processed") FALSE

# ── Metricas por 90 (nombre CIDE OSOS = columna de la API) ────
METRICAS_P90 <- c(
  # ofensivo
  xg_p90               = "player_match_np_xg",
  tiros_p90            = "player_match_np_shots",
  goles_p90            = "player_match_np_goals",
  toques_area_p90      = "player_match_touches_inside_box",
  # creacion
  xa_p90               = "player_match_xa",
  pases_clave_p90      = "player_match_key_passes",
  pases_area_p90       = "player_match_passes_into_box",
  pases_ult3_p90       = "player_match_op_f3_passes",
  asist_p90            = "player_match_assists",
  faltas_recibidas_p90 = "player_match_fouls_won",
  pases_filtrados_p90  = "player_match_through_balls",
  # defensivo
  presiones_p90        = "player_match_pressures",
  recup_presion_p90    = "player_match_pressure_regains",
  intercep_p90         = "player_match_interceptions",
  entradas_p90         = "player_match_tackles",
  recup_campo_rival_p90 = "player_match_fhalf_ball_recoveries",
  acc_def_sobre_esp_p90 = "player_match_defensive_actions_above_expectation",
  faltas_p90           = "player_match_fouls",
  regateado_p90        = "player_match_dribbled_past",
  # progresion
  progresiones_p90     = "player_match_deep_progressions",
  pases_adelante_p90   = "player_match_forward_passes",
  pases_p90            = "player_match_passes",
  balones_largos_p90   = "player_match_successful_long_balls",
  regates_p90          = "player_match_dribbles",
  # consistencia / fiabilidad
  perdidas_p90         = "player_match_turnovers",
  desposesiones_p90    = "player_match_dispossessions",
  # volumen (no entra a indices; se usa para corregir la variabilidad)
  toques_p90           = "player_match_touches",
  # OBV: fuera de los indices, validacion externa
  obv_p90              = "player_match_obv",
  obv_pase_p90         = "player_match_obv_pass",
  obv_conduccion_p90   = "player_match_obv_dribble_carry",
  obv_defensa_p90      = "player_match_obv_defensive_action",
  obv_tiro_p90         = "player_match_obv_shot"
)

# Ratios: no se dividen entre minutos
METRICAS_RATIO <- c(
  pct_pases  = "player_match_passing_ratio",
  pct_aereos = "player_match_aerial_ratio"
)

# Line-breaking passes: metrica 360, su denominador son los minutos
# CON datos 360, no los minutos totales (si no, se subestima).
METRICA_360 <- c(lbp_p90 = "player_match_lbp_completed")

METRICAS_TARJETAS <- c("amarillas_p90", "rojas_p90")

# Balon parado (indice 6): salen de los eventos (funciones_tactico.R,
# metricas_bp_partido). Sin eventos quedan NA y el indice no se calcula.
METRICAS_BP <- c(bp_xg_p90 = "bp_xg", bp_xg_lanzado_p90 = "bp_xg_lanzado",
                 bp_aereos_p90 = "bp_aereos", bp_resp_xg_p90 = "bp_resp_xg")

# Las de balon parado NO se ajustan ni entran a ningun indice: son solo
# informativas (por 90 minutos, sin ajuste).
METRICAS_AJUSTAR <- c(names(METRICAS_P90), names(METRICAS_RATIO),
                      names(METRICA_360), METRICAS_TARJETAS)

# Suma las metricas de balon parado a la base jugador-partido. `bp` es
# la salida de metricas_bp_partido(); NULL = no hay eventos.
agregar_bp <- function(base, bp = NULL) {
  for (m in names(METRICAS_BP)) base[[m]] <- NA_real_
  if (is.null(bp) || nrow(bp) == 0) return(base)
  b <- base %>% select(-all_of(names(METRICAS_BP))) %>%
    left_join(bp, by = c("match_id", "player_id"))
  con_ev <- b$match_id %in% unique(bp$match_id)
  for (k in names(METRICAS_BP)) {
    tot <- b[[METRICAS_BP[[k]]]]
    tot[con_ev & is.na(tot)] <- 0          # jugo un partido con eventos y no registro nada
    b[[k]] <- ifelse(con_ev, tot / b$minutos * 90, NA_real_)
  }
  b %>% select(-any_of(unname(METRICAS_BP)))
}

# Componentes de cada indice, con su signo. var_obv_resid se calcula
# a nivel jugador (no por partido). Dos versiones compiten en
# 03_validacion.R (carrera de indices):
#   completo      — todas las metricas (32), como la version anterior.
#   parsimonioso  — 13 metricas. Regla: maximo 3 por indice; de cada par
#                   con |r| > 0.75 se queda una; fuera los conteos de
#                   eventos raros (asistencias, pases filtrados, tarjetas)
#                   y la variabilidad de OBV, que son los mas ruidosos.
COMPONENTES_COMPLETO <- list(
  idx_ofensivo     = c(xg_p90 = 1, tiros_p90 = 1, goles_p90 = 1, toques_area_p90 = 1),
  idx_creacion     = c(xa_p90 = 1, pases_clave_p90 = 1, pases_area_p90 = 1,
                       pases_ult3_p90 = 1, asist_p90 = 1, faltas_recibidas_p90 = 1,
                       pases_filtrados_p90 = 1),
  idx_defensivo    = c(presiones_p90 = 1, recup_presion_p90 = 1, intercep_p90 = 1,
                       entradas_p90 = 1, recup_campo_rival_p90 = 1,
                       acc_def_sobre_esp_p90 = 1, pct_aereos = 1,
                       faltas_p90 = -1, regateado_p90 = -1,
                       amarillas_p90 = -1, rojas_p90 = -1),
  idx_progresion   = c(progresiones_p90 = 1, pases_adelante_p90 = 1, pases_p90 = 1,
                       balones_largos_p90 = 1, regates_p90 = 1, lbp_p90 = 1),
  idx_consistencia = c(perdidas_p90 = -1, desposesiones_p90 = -1, pct_pases = 1,
                       var_obv_resid = -1)
)
COMPONENTES_PARSIMONIOSO <- list(
  idx_ofensivo     = c(xg_p90 = 1, goles_p90 = 1, toques_area_p90 = 1),        # tiros ~ xG (r 0.96)
  idx_creacion     = c(xa_p90 = 1, pases_area_p90 = 1),                        # pases clave ~ xA
  idx_defensivo    = c(presiones_p90 = 1, intercep_p90 = 1, pct_aereos = 1),   # recup. y entradas ~ presiones
  idx_progresion   = c(progresiones_p90 = 1, lbp_p90 = 1, regates_p90 = 1),    # pases ~ pases adelante ~ lbp
  idx_consistencia = c(perdidas_p90 = -1, pct_pases = 1)
)
VERSIONES_INDICES <- c(completo     = "Índices completos (32 métricas)",
                       parsimonioso = "Índices parsimoniosos (13 métricas)")
COMPONENTES_DE <- list(completo = COMPONENTES_COMPLETO, parsimonioso = COMPONENTES_PARSIMONIOSO)
COMPONENTES <- COMPONENTES_COMPLETO   # compatibilidad
INDICES <- names(COMPONENTES)

# Version vigente: OSOS_INDICES > la que eligio la validacion > completo
indices_vigentes <- function(dir_proc = "data/processed") {
  e <- Sys.getenv("OSOS_INDICES", "")
  if (nzchar(e) && e %in% names(VERSIONES_INDICES)) return(e)
  f <- file.path(dir_proc, "indices_elegidos.rds")
  if (file.exists(f)) { v <- readRDS(f); if (v %in% names(VERSIONES_INDICES)) return(v) }
  "completo"
}
# Los cinco indices de juego (los que usan encaje, necesidad y estilo).
# El 6 (balon parado) se calcula aparte y solo entra al compuesto si la
# validacion lo elige. El 7 (revulsivo) nunca entra: mide un rol.
DIMS_FIT <- c("z_idx_ofensivo", "z_idx_creacion", "z_idx_defensivo", "z_idx_progresion", "z_idx_consistencia")
DIM_BP   <- "z_idx_balon_parado"
DIM_REV  <- "z_idx_revulsivo"
MIN_ENTRADAS_REV <- 3   # entradas de cambio para tener indice de revulsivo

# Indice 7: impacto contraido cuando el jugador entra de cambio, en una
# ventana. `entradas` sale de entradas_revulsivo() (funciones_tactico.R).
# Contraccion empirica de Bayes: con pocas entradas se acerca a 0.
revulsivo_ventana <- function(entradas, desde = NULL, hasta = NULL) {
  if (is.null(entradas) || nrow(entradas) == 0) return(NULL)
  d <- entradas
  if (!is.null(desde)) d <- d[d$match_date >= as.Date(desde), ]
  if (!is.null(hasta)) d <- d[d$match_date <  as.Date(hasta), ]
  if (nrow(d) < 30) return(NULL)
  s2 <- stats::var(d$impacto_obv, na.rm = TRUE)
  j <- d %>% group_by(player_id) %>%
    summarise(entradas = n(), revulsivo_bruto = mean(impacto_obv, na.rm = TRUE),
              revulsivo_xg = mean(impacto_xg, na.rm = TRUE), .groups = "drop")
  ok <- j$entradas >= MIN_ENTRADAS_REV
  tau2 <- if (sum(ok) >= 10) max(stats::var(j$revulsivo_bruto[ok]) - mean(s2 / j$entradas[ok]), 0) else 0
  j$revulsivo <- j$revulsivo_bruto * tau2 / (tau2 + s2 / j$entradas)
  sdv <- stats::sd(j$revulsivo[ok])
  j[[DIM_REV]] <- if (is.na(sdv) || sdv == 0) ifelse(ok, 0, NA_real_)
                  else ifelse(ok, pmin(pmax((j$revulsivo - mean(j$revulsivo[ok])) / sdv, -TOPE_Z), TOPE_Z), NA_real_)
  j$revulsivo_con_senal <- tau2 > 0
  j
}

# ── Utilidades ────────────────────────────────────────────────
wm <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x)
  if (!any(ok) || sum(w[ok]) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

torneo_de_fecha <- function(f) {
  f <- as.Date(f)
  y <- as.integer(format(f, "%Y")); m <- as.integer(format(f, "%m"))
  ifelse(m >= 7, paste("Apertura", y), paste("Clausura", y))
}
# Orden cronologico: Clausura 2022 < Apertura 2022 < Clausura 2023
orden_torneo <- function(f) {
  f <- as.Date(f)
  as.integer(format(f, "%Y")) * 2L + as.integer(as.integer(format(f, "%m")) >= 7)
}

formacion_a_texto <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  ifelse(nchar(x) >= 3, vapply(strsplit(x, ""), paste, character(1), collapse = "-"), NA)
}

moda <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# ============================================================
# 1. CARGA DE CRUDOS
# ============================================================
cargar_crudos <- function(dir_raw = "data/raw", cache = "data/processed/crudos.rds",
                          refrescar = FALSE) {
  if (!refrescar && file.exists(cache)) {
    cr <- readRDS(cache)
    n_disco <- length(list.files(file.path(dir_raw, "player_match"), pattern = "\\.rds$"))
    if (isTRUE(cr$n_archivos == n_disco)) return(cr)
  }
  leer <- function(sub) {
    fs <- list.files(file.path(dir_raw, sub), pattern = "\\.rds$", full.names = TRUE)
    lapply(fs, readRDS)
  }
  lu <- leer("lineups")
  pm <- leer("player_match")
  cr <- list(
    partidos    = readRDS(file.path(dir_raw, "partidos.rds")),
    jugadores   = bind_rows(map(lu, "jugadores")),
    posiciones  = bind_rows(map(lu, "posiciones")),
    formaciones = bind_rows(map(lu, "formaciones")),
    tarjetas    = bind_rows(map(lu, "tarjetas")),
    pm          = bind_rows(pm),
    tm          = bind_rows(leer("team_match")),
    n_archivos  = length(pm)
  )
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cr, cache)
  cr
}

# ============================================================
# 2. CONTEXTO DE EQUIPO Y NIVEL
# ============================================================

# Partidos con torneo y orden
preparar_partidos <- function(partidos) {
  partidos %>%
    filter(match_status == "available" | is.na(match_status)) %>%
    mutate(torneo = torneo_de_fecha(match_date),
           orden_torneo = orden_torneo(match_date),
           temporada = season_name)
}

# Contexto de cada equipo en cada partido
contexto_equipo_partido <- function(tm) {
  pos <- tm$team_match_possession
  if (isTRUE(max(pos, na.rm = TRUE) <= 1.5)) pos <- pos * 100   # viene como 0-1
  npxgd <- if ("team_match_np_xgd" %in% names(tm)) tm$team_match_np_xgd
           else tm$team_match_np_xg - tm$team_match_np_xg_conceded
  tibble(
    match_id      = as.integer(tm$match_id),
    team_id       = as.integer(tm$team_id),
    team_name     = as.character(tm$team_name),
    opposition_id = as.integer(tm$opposition_id),
    posesion      = pos,
    ppda          = tm$team_match_ppda,
    np_xgd        = npxgd,
    directness    = if ("team_match_directness" %in% names(tm)) tm$team_match_directness else NA_real_
  )
}

# Nivel de cada equipo por torneo: diferencial de xG sin penaltis por
# partido, estandarizado dentro del torneo y llevado a una escala
# 50-90 aprox. (media 70, 10 puntos por desviacion). Sustituye al
# tribble manual de la version anterior.
nivel_equipos <- function(ctx, part) {
  ctx %>%
    inner_join(part %>% select(match_id, torneo, orden_torneo), by = "match_id") %>%
    group_by(team_id, torneo, orden_torneo) %>%
    summarise(np_xgd_pp = mean(np_xgd, na.rm = TRUE), n = n(), .groups = "drop") %>%
    group_by(torneo) %>%
    mutate(
      z = (np_xgd_pp - mean(np_xgd_pp, na.rm = TRUE)) / sd(np_xgd_pp, na.rm = TRUE),
      nivel   = round(70 + 10 * ifelse(is.na(z), 0, z), 1),
      quintil = ntile(np_xgd_pp, 5)
    ) %>%
    ungroup() %>%
    select(team_id, torneo, orden_torneo, np_xgd_pp, nivel, quintil)
}

# ============================================================
# 3. BASE JUGADOR-PARTIDO
# ============================================================
construir_base <- function(cr) {
  part <- preparar_partidos(cr$partidos)
  ctx  <- contexto_equipo_partido(cr$tm)
  niv  <- nivel_equipos(ctx, part)

  # Posicion del partido: la que ocupo mas minutos
  pos_partido <- cr$posiciones %>%
    group_by(match_id, player_id, position_id) %>%
    summarise(dur = sum(dur_min, na.rm = TRUE), .groups = "drop") %>%
    group_by(match_id, player_id) %>%
    slice_max(dur, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(match_id, player_id, position_id)

  tarj <- if (nrow(cr$tarjetas) > 0) {
    cr$tarjetas %>%
      group_by(match_id, player_id) %>%
      summarise(amarillas = sum(tarjeta == "Yellow Card"),
                rojas     = sum(tarjeta %in% c("Red Card", "Second Yellow")),
                .groups = "drop")
  } else tibble(match_id = integer(), player_id = integer(),
                amarillas = integer(), rojas = integer())

  pm <- cr$pm
  faltan <- setdiff(c(unname(METRICAS_P90), unname(METRICAS_RATIO),
                      unname(METRICA_360), "player_match_360_minutes",
                      "player_match_goals", "player_match_assists"), names(pm))
  for (f in faltan) pm[[f]] <- NA_real_
  if (length(faltan) > 0)
    message("Columnas ausentes en player-match (quedan NA): ", paste(faltan, collapse = ", "))

  base <- pm %>%
    transmute(
      match_id   = as.integer(match_id),
      team_id    = as.integer(team_id),
      player_id  = as.integer(player_id),
      player_name = as.character(player_name),
      minutos    = as.numeric(player_match_minutes),
      minutos_360 = as.numeric(player_match_360_minutes),
      goles_raw  = as.numeric(player_match_goals),
      asist_raw  = as.numeric(player_match_assists),
      xa_raw     = as.numeric(player_match_xa),
      !!!setNames(lapply(unname(METRICAS_P90), as.name), names(METRICAS_P90)),
      !!!setNames(lapply(unname(METRICAS_RATIO), as.name), names(METRICAS_RATIO)),
      lbp_tot    = as.numeric(.data[[unname(METRICA_360)]])
    ) %>%
    filter(!is.na(minutos), minutos > 0) %>%
    inner_join(part %>% select(match_id, match_date, temporada, torneo, orden_torneo),
               by = "match_id") %>%
    left_join(pos_partido, by = c("match_id", "player_id")) %>%
    filter(!is.na(position_id), position_id != 1) %>%       # sin porteros
    left_join(ctx %>% select(match_id, team_id, opposition_id, posesion, ppda),
              by = c("match_id", "team_id")) %>%
    left_join(niv %>% select(team_id, torneo, calidad_companeros = nivel),
              by = c("team_id", "torneo")) %>%
    left_join(niv %>% select(opposition_id = team_id, torneo,
                             calidad_rival = nivel, quintil_rival = quintil),
              by = c("opposition_id", "torneo")) %>%
    left_join(tarj, by = c("match_id", "player_id")) %>%
    mutate(
      sub_partido = unname(MAPA_POSICION[as.character(position_id)]),
      amarillas   = replace_na(amarillas, 0L),
      rojas       = replace_na(rojas, 0L)
    ) %>%
    filter(!is.na(sub_partido))

  # Por 90
  for (m in names(METRICAS_P90)) base[[m]] <- base[[m]] / base$minutos * 90
  base$amarillas_p90 <- base$amarillas / base$minutos * 90
  base$rojas_p90     <- base$rojas     / base$minutos * 90
  base$lbp_p90 <- ifelse(!is.na(base$minutos_360) & base$minutos_360 > 0,
                         base$lbp_tot / base$minutos_360 * 90, NA_real_)

  # Contexto faltante -> valores centrales; PPDA recortado en colas
  base <- base %>%
    mutate(
      posesion = ifelse(is.na(posesion), median(posesion, na.rm = TRUE), posesion),
      ppda     = ifelse(is.na(ppda),     median(ppda,     na.rm = TRUE), ppda),
      ppda     = pmin(ppda, quantile(ppda, 0.99, na.rm = TRUE)),
      calidad_companeros = replace_na(calidad_companeros, 70),
      calidad_rival      = replace_na(calidad_rival, 70),
      quintil_rival      = replace_na(quintil_rival, 3L)
    )

  base
}

# ============================================================
# 4. MODELO 1 — AJUSTE CONTEXTUAL (tres especificaciones)
# ============================================================
# Por subgrupo de la posicion jugada en el partido, cada metrica por 90
# se limpia del contexto. El residuo es lo que el jugador produjo por
# encima de lo esperado en ese contexto y en esa posicion. Tres formas
# de definir "contexto", que compiten en 03_validacion.R:
#
#   "nivel"         A. Splines de posesion, PPDA, nivel del propio
#                      equipo y del rival (+ efecto fijo de temporada).
#   "alineacion"    B. Igual, pero en vez del nivel del equipo, la
#                      calidad de los companeros y rivales que estuvieron
#                      EN LA CANCHA en ese partido (indice de cada uno en
#                      el torneo anterior, ponderado por sus minutos).
#                      Varia con rotaciones y lesiones.
#   "efectos_fijos" C. Dos vias: efecto fijo de jugador + efecto fijo de
#                      equipo x torneo + efecto fijo de rival x torneo,
#                      con splines de posesion y PPDA. Se identifica con
#                      los jugadores que cambian de club (tipo AKM). El
#                      residuo que se usa es y - controles - efecto de
#                      equipo - efecto de rival = habilidad + ruido.
#
# `hasta`: si se da, el modelo se estima SOLO con partidos anteriores a
# esa fecha y solo se devuelven esos partidos (backtest sin fuga).
METODOS_AJUSTE <- c(nivel         = "A. Nivel del equipo",
                    alineacion    = "B. Calidad de la alineación en la cancha",
                    efectos_fijos = "C. Efectos fijos en dos vías")

# Metodo vigente: el que eligio la validacion (si ya corrio), o el A.
metodo_ajuste_vigente <- function(dir_proc = "data/processed") {
  f <- file.path(dir_proc, "metodo_ajuste.rds")
  e <- Sys.getenv("OSOS_METODO", "")
  if (nzchar(e) && e %in% names(METODOS_AJUSTE)) return(e)
  if (file.exists(f)) { m <- readRDS(f); if (m %in% names(METODOS_AJUSTE)) return(m) }
  "nivel"
}

ajustar_contexto <- function(base, hasta = NULL, metricas = METRICAS_AJUSTAR,
                             metodo = "nivel") {
  metodo <- match.arg(metodo, names(METODOS_AJUSTE))
  d <- if (!is.null(hasta)) filter(base, match_date < as.Date(hasta)) else base
  for (m in setdiff(metricas, names(d))) d[[m]] <- NA_real_   # p. ej. balon parado sin eventos
  if (metodo == "alineacion" && !all(c("cal_comp_alin", "cal_rival_alin") %in% names(d)))
    stop("El metodo 'alineacion' necesita agregar_calidad_alineacion() sobre la base.")
  salida <- list()

  for (sg in SUBS_ORDEN) {
    g <- d[d$sub_partido == sg, , drop = FALSE]
    if (nrow(g) == 0) next
    w <- pmin(g$minutos, 90)
    entra <- g$minutos >= MIN_MIN_AJUSTE

    if (metodo == "efectos_fijos") {
      R <- ajuste_dos_vias_todas(g, metricas, w, entra)
      for (m in metricas) g[[paste0(m, "_res")]] <- R[, m]
      salida[[sg]] <- g
      next
    }

    fe <- n_distinct(g$temporada) > 1
    rhs <- if (metodo == "nivel")
      "ns(posesion, df = 3) + ns(ppda, df = 3) + ns(calidad_companeros, df = 3) + ns(calidad_rival, df = 3)"
    else
      "ns(posesion, df = 3) + ns(ppda, df = 3) + ns(cal_comp_alin, df = 3) + ns(cal_rival_alin, df = 3)"
    if (fe) rhs <- paste(rhs, "+ factor(temporada)")
    X <- tryCatch(model.matrix(as.formula(paste("~", rhs)), g), error = function(e) NULL)

    for (m in metricas) {
      y  <- g[[m]]
      ok <- !is.na(y) & is.finite(y) & entra
      res <- rep(NA_real_, nrow(g))
      if (!is.null(X) && sum(ok) >= 60) {
        fit  <- lm.wfit(X[ok, , drop = FALSE], y[ok], w[ok])
        beta <- fit$coefficients; beta[is.na(beta)] <- 0
        res  <- y - drop(X %*% beta)
      } else if (any(ok)) {
        res <- y - wm(y[ok], w[ok])
      }
      g[[paste0(m, "_res")]] <- res
    }
    salida[[sg]] <- g
  }
  bind_rows(salida)
}

# ── Efectos fijos en dos vias ─────────────────────────────────
# y = X b + alfa_jugador + psi_equipo_torneo + phi_rival_torneo + e,
# minimos cuadrados ponderados por minutos (X = splines de posesion y
# PPDA). Se resuelve de forma directa: ecuaciones normales con matrices
# dispersas y una sola factorizacion de Cholesky por subgrupo, que sirve
# para todas las metricas que comparten filas. Da lo mismo que lm() con
# dummies (verificado), en segundos.
# Se devuelve y - X b - psi - phi (= habilidad del jugador + ruido),
# centrado en cero dentro del subgrupo, tambien para los partidos con
# pocos minutos que no entraron a estimar.
ajuste_dos_vias_todas <- function(g, metricas, w, entra) {
  out <- matrix(NA_real_, nrow(g), length(metricas), dimnames = list(NULL, metricas))
  X <- tryCatch(cbind(ns(g$posesion, df = 3), ns(g$ppda, df = 3)), error = function(e) NULL)
  Y <- as.matrix(as.data.frame(g)[, metricas, drop = FALSE])
  OK <- !is.na(Y) & is.finite(Y) & entra
  f1 <- as.character(g$player_id)
  f2 <- paste(g$team_id, g$torneo)
  f3 <- paste(g$opposition_id, g$torneo)

  claves <- apply(OK, 2, function(o) paste(which(o), collapse = ","))
  for (cl in unique(claves)) {
    ms <- metricas[claves == cl]
    ok <- OK[, ms[1]]
    if (sum(ok) < 100 || is.null(X)) {
      for (m in ms) if (any(ok)) out[, m] <- Y[, m] - wm(Y[ok, m], w[ok])
      next
    }
    ww <- w[ok]
    l1 <- unique(f1[ok]); l2 <- unique(f2[ok]); l3 <- unique(f3[ok])
    ind <- function(f, l, quitar) {
      j <- match(f, l)
      M <- Matrix::sparseMatrix(i = seq_along(j), j = j, x = 1, dims = c(length(j), length(l)))
      if (quitar) M[, -1, drop = FALSE] else M   # nivel de referencia = 0
    }
    Z <- cbind(Matrix::Matrix(X[ok, , drop = FALSE], sparse = TRUE),
               ind(f1[ok], l1, FALSE), ind(f2[ok], l2, TRUE), ind(f3[ok], l3, TRUE))
    A <- Matrix::crossprod(Z, Z * ww)
    A <- A + Matrix::Diagonal(ncol(A), 1e-8 * mean(Matrix::diag(A)))  # por si hay componentes desconectadas
    ch <- Matrix::Cholesky(Matrix::forceSymmetric(A))
    B <- as.matrix(Matrix::solve(ch, Matrix::crossprod(Z, Y[ok, ms, drop = FALSE] * ww)))

    p <- ncol(X); k1 <- length(l1); k2 <- length(l2) - 1; k3 <- length(l3) - 1
    b   <- B[seq_len(p), , drop = FALSE]
    psi <- rbind(0, B[p + k1 + seq_len(k2), , drop = FALSE])
    phi <- rbind(0, B[p + k1 + k2 + seq_len(k3), , drop = FALSE])
    j2 <- match(f2, l2); j3 <- match(f3, l3)
    P2 <- psi[j2, , drop = FALSE]; P2[is.na(P2)] <- 0   # equipo sin partidos largos: 0
    P3 <- phi[j3, , drop = FALSE]; P3[is.na(P3)] <- 0
    R  <- Y[, ms, drop = FALSE] - X %*% b - P2 - P3
    R  <- sweep(R, 2, colSums(R[ok, , drop = FALSE] * ww) / sum(ww))
    R[is.na(Y[, ms, drop = FALSE])] <- NA_real_
    out[, ms] <- R
  }
  out
}

# Version de una metrica (para pruebas)
ajuste_dos_vias <- function(g, m, w, entra, ...) ajuste_dos_vias_todas(g, m, w, entra)[, 1]

# ── Calidad de la alineacion en la cancha (metodo B) ──────────
# Para cada jugador y partido: indice promedio de sus companeros que
# estuvieron en la cancha (sin contarlo a el) y de los rivales en la
# cancha, ponderado por minutos jugados en ese partido. El indice de
# cada jugador es su z compuesto con contraccion del TORNEO ANTERIOR,
# asi que es informacion previa al partido. Los que no tienen torneo
# anterior en la liga no cuentan en el promedio.
agregar_calidad_alineacion <- function(base, cr, perf_t) {
  part <- preparar_partidos(cr$partidos) %>% select(match_id, orden_torneo)
  ord_prev <- sort(unique(perf_t$orden_torneo))
  q <- perf_t %>%
    group_by(player_id, orden_torneo) %>%
    slice_max(minutos_total, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(player_id, orden_prev = orden_torneo, q = z_compuesto_fiable)
  # Siguiente torneo en el que aplica cada perfil
  sig <- setNames(c(ord_prev[-1], NA), ord_prev)
  q$orden_torneo <- unname(sig[as.character(q$orden_prev)])
  q <- q %>% filter(!is.na(orden_torneo)) %>% select(player_id, orden_torneo, q)

  en_cancha <- cr$posiciones %>%
    group_by(match_id, team_id, player_id) %>%
    summarise(m = sum(dur_min, na.rm = TRUE), .groups = "drop") %>%
    inner_join(part, by = "match_id") %>%
    left_join(q, by = c("player_id", "orden_torneo"))

  eq <- en_cancha %>%
    filter(!is.na(q), m > 0) %>%
    group_by(match_id, team_id) %>%
    summarise(S = sum(q * m), M = sum(m), .groups = "drop")

  propio <- en_cancha %>% select(match_id, team_id, player_id, q_i = q, m_i = m)

  out <- base %>%
    select(-any_of(c("cal_comp_alin", "cal_rival_alin"))) %>%
    left_join(propio, by = c("match_id", "team_id", "player_id")) %>%
    left_join(eq, by = c("match_id", "team_id")) %>%
    left_join(eq %>% rename(opposition_id = team_id, S_r = S, M_r = M),
              by = c("match_id", "opposition_id")) %>%
    mutate(
      tiene = !is.na(q_i) & !is.na(m_i) & m_i > 0,
      cal_comp_alin  = ifelse(tiene, (S - q_i * m_i) / pmax(M - m_i, 1e-6), S / M),
      cal_rival_alin = S_r / M_r
    ) %>%
    select(-q_i, -m_i, -S, -M, -S_r, -M_r, -tiene)

  # Sin informacion previa (primer torneo, alineaciones de puros debutantes)
  out$cal_comp_alin[!is.finite(out$cal_comp_alin)]   <- NA_real_
  out$cal_rival_alin[!is.finite(out$cal_rival_alin)] <- NA_real_
  out$cal_comp_alin  <- ifelse(is.na(out$cal_comp_alin),
                               median(out$cal_comp_alin, na.rm = TRUE), out$cal_comp_alin)
  out$cal_rival_alin <- ifelse(is.na(out$cal_rival_alin),
                               median(out$cal_rival_alin, na.rm = TRUE), out$cal_rival_alin)
  out
}

# ============================================================
# 5. DISPONIBILIDAD Y LESIONES (desde alineaciones)
# ============================================================
# No es un historial medico: es lo que se puede inferir de quien
# estuvo en la hoja de cada partido y por que salio.
#   convocatorias      — partidos en la hoja del partido
#   salidas_lesion     — cambios registrados como "Substitution - Off (Injury)"
#   partidos_baja_lesion — partidos consecutivos fuera de la hoja
#                          despues de cada salida por lesion
#   lesiones_relevantes  — salidas por lesion seguidas de 2+ partidos de baja
indicadores_disponibilidad <- function(cr, desde = NULL, hasta = NULL, por_club = TRUE) {
  part <- preparar_partidos(cr$partidos)
  if (!is.null(desde)) part <- filter(part, match_date >= as.Date(desde))
  if (!is.null(hasta)) part <- filter(part, match_date <  as.Date(hasta))
  if (nrow(part) == 0) return(NULL)
  fin_ventana <- max(part$match_date)

  part_eq <- bind_rows(
    part %>% transmute(match_id, match_date, team_id = home_team_id),
    part %>% transmute(match_id, match_date, team_id = away_team_id)
  )

  hoja <- cr$jugadores %>%
    semi_join(part, by = "match_id") %>%
    left_join(part %>% select(match_id, match_date), by = "match_id") %>%
    distinct(match_id, player_id, .keep_all = TRUE)

  if (nrow(hoja) == 0) return(NULL)

  # Tenencia por jugador-club. El ultimo club se extiende hasta el
  # final de la ventana: si no, una lesion larga al cierre no contaria.
  ten <- hoja %>%
    group_by(player_id, team_id) %>%
    summarise(ini = min(match_date), fin = max(match_date), .groups = "drop") %>%
    group_by(player_id) %>%
    mutate(fin = if_else(fin == max(fin), fin_ventana, fin)) %>%
    ungroup()

  pe <- ten %>%
    inner_join(part_eq, by = "team_id", relationship = "many-to-many") %>%
    filter(match_date >= ini, match_date <= fin) %>%
    left_join(hoja %>% select(match_id, player_id, team_id, jugo, titular, salio_lesion) %>%
                mutate(en_hoja = TRUE),
              by = c("match_id", "player_id", "team_id")) %>%
    mutate(en_hoja = replace_na(en_hoja, FALSE),
           jugo = replace_na(jugo, FALSE),
           titular = replace_na(titular, FALSE),
           salio_lesion = replace_na(salio_lesion, FALSE)) %>%
    arrange(player_id, team_id, match_date)

  # Rachas de baja despues de cada salida por lesion
  bajas <- function(en_hoja, lesion) {
    idx <- which(lesion)
    if (length(idx) == 0) return(c(baja = 0, relevantes = 0))
    r <- vapply(idx, function(i) {
      if (i == length(en_hoja)) return(0)
      sig <- en_hoja[(i + 1):length(en_hoja)]
      k <- which(sig)[1]
      if (is.na(k)) length(sig) else k - 1
    }, numeric(1))
    c(baja = sum(r), relevantes = sum(r >= 2))
  }

  por <- pe %>%
    group_by(player_id, team_id) %>%
    summarise(
      partidos_tenencia = n(),
      convocatorias     = sum(en_hoja),
      apariciones       = sum(jugo),
      titularidades     = sum(titular),
      salidas_lesion    = sum(salio_lesion),
      partidos_baja_lesion = bajas(en_hoja, salio_lesion)[["baja"]],
      lesiones_relevantes  = bajas(en_hoja, salio_lesion)[["relevantes"]],
      .groups = "drop"
    )

  if (!por_club) {
    por <- por %>%
      group_by(player_id) %>%
      summarise(across(c(partidos_tenencia, convocatorias, apariciones, titularidades,
                         salidas_lesion, partidos_baja_lesion, lesiones_relevantes), sum),
                .groups = "drop")
  }

  por %>% mutate(pct_convocatorias = round(100 * convocatorias / pmax(partidos_tenencia, 1), 1))
}

# Datos personales: fecha de nacimiento, pais, estatura (moda)
datos_personales <- function(cr) {
  cr$jugadores %>%
    group_by(player_id) %>%
    summarise(
      birth_date   = moda(birth_date),
      nacionalidad = moda(country_name),
      estatura     = suppressWarnings(median(player_height, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(birth_date = as.Date(birth_date),
           extranjero = !is.na(nacionalidad) & nacionalidad != "Mexico")
}

edad_en <- function(birth_date, fecha) {
  round(as.numeric(difftime(as.Date(fecha), as.Date(birth_date), units = "days")) / 365.25, 1)
}

# ============================================================
# 6. MODELO 2 — PERFILES, INDICES Y Z-SCORES
# ============================================================
# Agrega una ventana de partidos (temporada, torneo o el ano previo a
# una fecha de corte) y estandariza dentro de cada subgrupo contra la
# poblacion de referencia (jugadores con MIN_MINUTOS+ en la ventana).
construir_perfiles <- function(base_aj, cr, desde = NULL, hasta = NULL,
                               por_club = TRUE, min_ref = MIN_MINUTOS,
                               fecha_edad = NULL, indices = "completo",
                               con_bp = FALSE, entradas = NULL) {
  componentes <- COMPONENTES_DE[[match.arg(indices, names(VERSIONES_INDICES))]]
  d <- base_aj
  if (!is.null(desde)) d <- filter(d, match_date >= as.Date(desde))
  if (!is.null(hasta)) d <- filter(d, match_date <  as.Date(hasta))
  if (nrow(d) == 0) return(NULL)

  # Subgrupo del jugador en la ventana: primero el BLOQUE (defensa,
  # medio, ataque) donde jugo mas minutos y, dentro de el, el subgrupo
  # con mas minutos. Antes se tomaba el subgrupo con mas minutos y se
  # tiraban los partidos de otros bloques: un mediapunta que a veces
  # jugaba de 9 podia quedar como delantero y perder la mayoria de
  # sus minutos de medio campo.
  # Se conservan TODOS sus partidos: el residuo de cada uno ya esta
  # medido contra la norma de la posicion que ocupo en ese partido. El
  # subgrupo solo define contra quien se le compara y con que pesos.
  dom <- d %>%
    mutate(bloque = unname(GRUPO_DE_SUB[sub_partido])) %>%
    group_by(player_id, bloque, sub_partido) %>%
    summarise(m = sum(minutos), .groups = "drop") %>%
    group_by(player_id, bloque) %>%
    mutate(m_bloque = sum(m)) %>%
    group_by(player_id) %>%
    arrange(desc(m_bloque), desc(m), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(player_id, subgrupo = sub_partido)

  # Posicion mas jugada DENTRO del subgrupo asignado
  pos_dom <- d %>%
    inner_join(dom, by = "player_id") %>%
    filter(sub_partido == subgrupo) %>%
    group_by(player_id, position_id) %>%
    summarise(m = sum(minutos), .groups = "drop") %>%
    group_by(player_id) %>%
    slice_max(m, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(player_id, posicion = unname(ABREV_POSICION[as.character(position_id)]))

  d <- d %>% inner_join(dom, by = "player_id")

  claves <- if (por_club) c("player_id", "team_id") else "player_id"
  cols_res <- paste0(METRICAS_AJUSTAR, "_res")

  nombres_eq <- cr$tm %>%
    distinct(team_id = as.integer(team_id), team_name = as.character(team_name)) %>%
    group_by(team_id) %>% slice_tail(n = 1) %>% ungroup()

  agregar <- function(dd, ref_min) {
    agg <- dd %>%
      arrange(match_date) %>%
      group_by(across(all_of(claves))) %>%
      summarise(
        # OJO: va primero. summarise es secuencial y el across de abajo
        # sobrescribe obv_p90_res con su promedio.
        var_obv = {
          v <- obv_p90_res[minutos >= MIN_MIN_VAR]
          if (sum(!is.na(v)) >= 5) sd(v, na.rm = TRUE) else NA_real_
        },
        fecha_ult       = max(match_date),
        player_name     = last(player_name),
        team_id_ult     = last(team_id),
        subgrupo        = first(subgrupo),
        n_partidos      = n(),
        minutos_total   = sum(minutos),
        pct_min_subgrupo = round(100 * sum(minutos[sub_partido == subgrupo]) / max(sum(minutos), 1)),
        goles_tot       = sum(goles_raw, na.rm = TRUE),
        asistencias_tot = sum(asist_raw, na.rm = TRUE),
        xa_tot          = round(sum(xa_raw, na.rm = TRUE), 2),
        across(all_of(cols_res), ~ wm(.x, minutos)),
        across(all_of(METRICAS_AJUSTAR), ~ wm(.x, minutos), .names = "{.col}_crudo"),
        across(any_of(names(METRICAS_BP)), ~ wm(.x, minutos)),   # balon parado: informativo
        posesion_media  = wm(posesion, minutos),
        ppda_media      = wm(ppda, minutos),
        cal_comp_media  = wm(calidad_companeros, minutos),
        cal_rival_media = wm(calidad_rival, minutos),
        .groups = "drop"
      )

    if (!por_club) agg$team_id <- agg$team_id_ult
    agg <- agg %>%
      select(-team_id_ult) %>%
      left_join(nombres_eq, by = "team_id") %>%
      left_join(pos_dom, by = "player_id") %>%
      rename(club = team_name)

    # Clutch: xG ajustado ante el quintil mas fuerte vs el mas debil
    cl <- dd %>%
      mutate(nivel_rival = case_when(quintil_rival == 5 ~ "top20",
                                     quintil_rival == 1 ~ "bottom20",
                                     TRUE ~ "medio")) %>%
      group_by(across(all_of(claves)), nivel_rival) %>%
      summarise(xg_vs = wm(xg_p90_res, minutos), n_partidos_niv = n(), .groups = "drop") %>%
      pivot_wider(id_cols = all_of(claves), names_from = nivel_rival,
                  values_from = c(xg_vs, n_partidos_niv), names_sep = "_")
    for (cc in c("xg_vs_top20", "xg_vs_bottom20", "xg_vs_medio",
                 "n_partidos_niv_top20", "n_partidos_niv_bottom20", "n_partidos_niv_medio"))
      if (!cc %in% names(cl)) cl[[cc]] <- NA_real_
    agg <- agg %>% left_join(cl, by = claves)

    estandarizar_perfiles(agg, ref_min, componentes, con_bp)
  }

  agg <- agregar(d, min_ref)

  # Fiabilidad por mitades: partidos pares vs impares de cada jugador.
  # Cuanto coincide el z compuesto entre mitades, segun los minutos,
  # dice cuanto del indice es senal. De ahi sale m0 para la contraccion.
  d_h <- d %>%
    group_by(across(all_of(claves))) %>%
    arrange(match_date, .by_group = TRUE) %>%
    mutate(mitad = row_number() %% 2L) %>%
    ungroup()
  m0 <- tryCatch(
    estimar_m0_mitades(agregar(filter(d_h, mitad == 1L), min_ref / 2),
                       agregar(filter(d_h, mitad == 0L), min_ref / 2), claves),
    error = function(e) 600)
  agg$m0_contraccion     <- m0
  agg$fiabilidad         <- round(agg$minutos_total / (agg$minutos_total + m0), 3)
  agg$z_compuesto_fiable <- agg$z_compuesto * agg$fiabilidad
  agg$version_indices    <- indices
  agg$con_balon_parado   <- con_bp

  # Indice 7: revulsivo (por jugador en la ventana, sin importar el club)
  rv <- revulsivo_ventana(entradas, desde, hasta)
  if (!is.null(rv)) agg <- agg %>% left_join(rv %>% select(player_id, entradas_cambio = entradas,
                                                         revulsivo_bruto, revulsivo, revulsivo_xg,
                                                         all_of(DIM_REV), revulsivo_con_senal),
                                             by = "player_id")
  for (cc in c("entradas_cambio", "revulsivo_bruto", "revulsivo", "revulsivo_xg", DIM_REV))
    if (!cc %in% names(agg)) agg[[cc]] <- NA_real_
  agg$entradas_cambio <- coalesce(as.numeric(agg$entradas_cambio), 0)

  # Percentil con contraccion, dentro del subgrupo y contra la poblacion
  # de referencia: es el que se valido en el backtest. El de antes (sin
  # contraccion) se guarda aparte.
  agg <- agg %>%
    rename(percentil_sin_contraccion = percentil) %>%
    group_by(subgrupo) %>%
    mutate(percentil = {
      ref <- cumple_minimo & !is.na(z_compuesto_fiable)
      if (sum(ref) >= 3) round(100 * ecdf(z_compuesto_fiable[ref])(z_compuesto_fiable), 1)
      else percentil_sin_contraccion
    }) %>%
    ungroup()

  # Disponibilidad, lesiones, edad
  disp <- indicadores_disponibilidad(cr, desde, hasta, por_club)
  if (!is.null(disp)) agg <- agg %>% left_join(disp, by = claves)
  for (cc in c("partidos_tenencia", "convocatorias", "apariciones", "titularidades",
               "salidas_lesion", "partidos_baja_lesion", "lesiones_relevantes",
               "pct_convocatorias"))
    if (!cc %in% names(agg)) agg[[cc]] <- NA_real_
  per <- datos_personales(cr)
  f_edad <- if (!is.null(fecha_edad)) as.Date(fecha_edad)
            else if (!is.null(hasta)) as.Date(hasta) else max(d$match_date)
  agg <- agg %>%
    left_join(per, by = "player_id") %>%
    mutate(edad = edad_en(birth_date, f_edad))

  n_part_eq <- d %>% distinct(team_id, match_id) %>% count(team_id, name = "partidos_equipo")
  agg %>%
    left_join(n_part_eq, by = "team_id") %>%
    mutate(
      partidos_tenencia = coalesce(partidos_tenencia, partidos_equipo),
      pct_minutos    = round(100 * minutos_total / pmax(partidos_tenencia * 90, 1), 1),
      disponibilidad = pmin(100, pct_minutos),
      liga           = LIGA,
      grupo_posicion = unname(GRUPO_DE_SUB[subgrupo]),
      subgrupo_lab   = unname(ETIQUETA_SUB[subgrupo]),
      jugador_id     = paste0(player_id, "_", player_name)
    )
}

# Estandarizacion dentro de subgrupo contra la poblacion de referencia
estandarizar_perfiles <- function(agg, min_ref = MIN_MINUTOS, componentes = COMPONENTES, con_bp = FALSE) {
  agg$cumple_minimo <- agg$minutos_total >= min_ref
  resultado <- list()

  for (sg in SUBS_ORDEN) {
    g <- agg[agg$subgrupo == sg, , drop = FALSE]
    if (nrow(g) == 0) next
    ref <- g$cumple_minimo
    if (sum(ref) < 8) ref <- rep(TRUE, nrow(g))   # subgrupo muy chico: todos son referencia

    # Variabilidad de OBV neta del volumen de participacion
    ok <- ref & !is.na(g$var_obv) & !is.na(g$toques_p90_crudo)
    g$var_obv_resid <- NA_real_
    if (sum(ok) >= 10) {
      mod <- lm(var_obv ~ toques_p90_crudo, data = g[ok, ])
      tiene <- !is.na(g$var_obv) & !is.na(g$toques_p90_crudo)
      g$var_obv_resid[tiene] <- g$var_obv[tiene] - predict(mod, newdata = g[tiene, ])
    }

    z_de <- function(x) {
      if (all(is.na(x[ref]))) return(rep(NA_real_, length(x)))   # sin datos (p. ej. sin eventos)
      mu <- mean(x[ref], na.rm = TRUE); s <- sd(x[ref], na.rm = TRUE)
      if (is.na(s) || s == 0) return(ifelse(is.na(x), NA_real_, 0))
      (x - mu) / s
    }

    for (idx in INDICES) {
      comp <- componentes[[idx]]
      # Cada componente se topa en +-TOPE_Z antes de promediar: un valor
      # extremo en una sola metrica (tipico con pocos minutos) ya no
      # arrastra el indice entero.
      zs <- sapply(names(comp), function(m) {
        col <- if (m == "var_obv_resid") "var_obv_resid" else paste0(m, "_res")
        if (!col %in% names(g)) return(rep(NA_real_, nrow(g)))
        comp[[m]] * pmin(pmax(z_de(g[[col]]), -TOPE_Z), TOPE_Z)
      })
      if (is.null(dim(zs))) zs <- matrix(zs, nrow = nrow(g))
      g[[idx]] <- rowMeans(zs, na.rm = TRUE)
      g[[idx]][is.nan(g[[idx]])] <- NA_real_
      # El indice reestandarizado tambien se topa: con pocos componentes,
      # el promedio puede volver a inflarse al dividir entre su desviacion.
      g[[paste0("z_", idx)]] <- pmin(pmax(z_de(g[[idx]]), -TOPE_Z), TOPE_Z)
    }

    w <- pesos_de(sg)
    zc <- as.matrix(g[, DIMS_FIT])
    zc[is.na(zc)] <- 0
    bruto <- drop(zc %*% w)
    g$z_compuesto <- z_de(bruto)
    ecdf_ref <- ecdf(g$z_compuesto[ref])
    g$percentil <- round(100 * ecdf_ref(g$z_compuesto), 1)

    # Niveles ajustados para mostrar (residuo + media cruda de referencia)
    for (m in METRICAS_AJUSTAR) {
      cr_col <- paste0(m, "_crudo"); rs_col <- paste0(m, "_res")
      if (cr_col %in% names(g) && rs_col %in% names(g))
        g[[paste0(m, "_aj")]] <- g[[rs_col]] + mean(g[[cr_col]][ref], na.rm = TRUE)
    }
    mu_xg <- mean(g$xg_p90_crudo[ref], na.rm = TRUE)
    g$xg_vs_top20    <- g$xg_vs_top20    + mu_xg
    g$xg_vs_bottom20 <- g$xg_vs_bottom20 + mu_xg
    g$xg_vs_medio    <- g$xg_vs_medio    + mu_xg

    resultado[[sg]] <- g
  }

  out <- bind_rows(resultado) %>%
    mutate(
      clutch_index = xg_vs_top20 - xg_vs_bottom20,
      muestra_reducida_top20 = is.na(n_partidos_niv_top20) | n_partidos_niv_top20 < 4
    )

  out
}

# Minutos a los que el z compuesto es mitad senal y mitad ruido.
#
# Con dos mitades de m/2 minutos cada una, la correlacion entre
# mitades es la fiabilidad de un indice de m/2 minutos:
#     r = (m/2) / (m/2 + m0)   =>   m0 = (m/2) (1 - r) / r
# Se calcula por tramos de minutos y se toma la mediana ponderada.
# A diferencia de un ajuste de varianza por tramos, no supone que la
# varianza real del indice sea igual entre los que juegan mucho y
# los que juegan poco (no lo es: los minutos dependen de la calidad).
estimar_m0_mitades <- function(a1, a0, claves, defecto = 600) {
  d <- inner_join(a1 %>% select(all_of(claves), z1 = z_compuesto, m1 = minutos_total),
                  a0 %>% select(all_of(claves), z0 = z_compuesto, m0 = minutos_total),
                  by = claves) %>%
    mutate(m_mitad = (m1 + m0) / 2) %>%
    filter(m1 >= 45, m0 >= 45, !is.na(z1), !is.na(z0))
  if (nrow(d) < 60) return(defecto)
  cortes <- unique(quantile(d$m_mitad, probs = seq(0, 1, 0.2)))
  if (length(cortes) < 3) return(defecto)
  d$tramo <- cut(d$m_mitad, cortes, include.lowest = TRUE)
  t <- d %>%
    group_by(tramo) %>%
    summarise(m = mean(m_mitad), r = suppressWarnings(cor(z1, z0)), n = n(), .groups = "drop") %>%
    filter(n >= 15, !is.na(r), r > 0.05, r < 0.98) %>%
    mutate(m0_tramo = m * (1 - r) / r)
  if (nrow(t) == 0) return(defecto)
  o <- order(t$m0_tramo); w <- cumsum(t$n[o]) / sum(t$n)
  round(min(max(t$m0_tramo[o][which(w >= 0.5)[1]], 30), 3000))
}

# ============================================================
# 7. ESTILO Y FORMACION DE CLUBES (insumo de la app)
# ============================================================
contexto_equipos_app <- function(cr) {
  part <- preparar_partidos(cr$partidos)
  contexto_equipo_partido(cr$tm) %>%
    inner_join(part %>% select(match_id, fecha = match_date, temporada, torneo),
               by = "match_id") %>%
    transmute(club = team_name, team_id, liga_eq = LIGA, temporada, torneo, fecha,
              posesion, ppda, directness, np_xgd)
}

formaciones_club <- function(cr) {
  part <- preparar_partidos(cr$partidos)
  if (nrow(cr$formaciones) == 0) return(NULL)
  cr$formaciones %>%
    filter(reason == "Starting XI") %>%
    distinct(match_id, team_id, .keep_all = TRUE) %>%
    inner_join(part %>% select(match_id, temporada), by = "match_id") %>%
    mutate(formacion = formacion_a_texto(formation)) %>%
    filter(!is.na(formacion)) %>%
    count(team_id, club = team_name, temporada, formacion, name = "n_partidos_esq") %>%
    group_by(team_id, temporada) %>%
    mutate(pct_uso = round(100 * n_partidos_esq / sum(n_partidos_esq), 1)) %>%
    slice_max(n_partidos_esq, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(liga = LIGA)
}

# ============================================================
# 8. ENCAJE JUGADOR-CLUB (lo usan la validacion y la app)
# ============================================================
# Afinidad: distancia en posesion y PPDA entre el contexto del jugador
# y el del club destino, normalizada por la dispersion entre clubes.
afinidad_contexto <- function(pos_j, ppda_j, pos_c, ppda_c, sd_pos, sd_ppda) {
  d <- sqrt(((pos_c - pos_j) / sd_pos)^2 + ((ppda_c - ppda_j) / sd_ppda)^2)
  round(pmax(0, pmin(100, 100 * exp(-d / sqrt(2)))), 1)
}

# Necesidad: fortaleza del jugador en los indices donde el club esta
# por debajo de la media (huecos), con saturacion suave a 0-100.
necesidad_club <- function(z_jug, z_club) {
  hueco  <- pmax(0, -z_club); hueco[is.na(hueco)] <- 1
  aporte <- pmax(0, z_jug);   aporte[is.na(aporte)] <- 0
  round(100 * (1 - exp(-sum(hueco * aporte) / 1.5)), 1)
}

# Tabla formacion -> titulares por subgrupo (de la version anterior)
TABLA_FORMACIONES <- list(
  "4-2-3-1" = c(CB=2, FB=2, DM=2, CM=0, AM=1, W=2, ST=1),
  "4-4-2"   = c(CB=2, FB=2, DM=0, CM=2, AM=0, W=2, ST=2),
  "4-3-3"   = c(CB=2, FB=2, DM=1, CM=2, AM=0, W=2, ST=1),
  "3-4-2-1" = c(CB=3, FB=2, DM=0, CM=2, AM=2, W=0, ST=1),
  "3-4-3"   = c(CB=3, FB=2, DM=0, CM=2, AM=0, W=2, ST=1),
  "3-5-2"   = c(CB=3, FB=2, DM=1, CM=2, AM=0, W=0, ST=2),
  "5-4-1"   = c(CB=3, FB=2, DM=0, CM=2, AM=0, W=2, ST=1),
  "5-3-2"   = c(CB=3, FB=2, DM=1, CM=2, AM=0, W=0, ST=2),
  "4-1-4-1" = c(CB=2, FB=2, DM=1, CM=2, AM=0, W=2, ST=1),
  "3-4-1-2" = c(CB=3, FB=2, DM=0, CM=2, AM=1, W=0, ST=2),
  "4-4-1-1" = c(CB=2, FB=2, DM=0, CM=2, AM=1, W=2, ST=1),
  "4-1-3-2" = c(CB=2, FB=2, DM=1, CM=1, AM=1, W=1, ST=2),
  "4-5-1"   = c(CB=2, FB=2, DM=1, CM=2, AM=0, W=2, ST=1),
  "4-3-1-2" = c(CB=2, FB=2, DM=1, CM=2, AM=1, W=0, ST=2),
  "4-3-2-1" = c(CB=2, FB=2, DM=1, CM=2, AM=2, W=0, ST=1),
  "3-1-4-2" = c(CB=3, FB=2, DM=1, CM=2, AM=0, W=0, ST=2),
  "3-5-1-1" = c(CB=3, FB=2, DM=1, CM=2, AM=1, W=0, ST=1),
  "4-2-2-2" = c(CB=2, FB=2, DM=2, CM=0, AM=0, W=2, ST=2),
  "4-1-2-1-2" = c(CB=2, FB=2, DM=1, CM=2, AM=1, W=0, ST=2),
  "3-4-2-1" = c(CB=3, FB=2, DM=0, CM=2, AM=2, W=0, ST=1)
)
FORMACION_DEFECTO <- c(CB=2, FB=2, DM=1, CM=2, AM=0, W=2, ST=1)

titulares_de_formacion <- function(formacion) {
  if (is.null(formacion) || is.na(formacion) || !nzchar(formacion)) return(FORMACION_DEFECTO)
  if (!is.null(TABLA_FORMACIONES[[formacion]])) return(TABLA_FORMACIONES[[formacion]])
  partes <- suppressWarnings(as.numeric(strsplit(formacion, "-")[[1]]))
  partes <- partes[!is.na(partes)]
  if (length(partes) < 3) return(FORMACION_DEFECTO)
  n_def <- partes[1]; n_att <- partes[length(partes)]
  n_med <- sum(partes[-c(1, length(partes))])
  cb <- if (n_def >= 5 || n_def == 3) 3 else 2
  fb <- max(0, n_def - cb)
  dm <- if (n_med >= 4) 1 else 0
  c(CB = cb, FB = fb, DM = dm, CM = max(0, n_med - dm), AM = 0, W = 0, ST = n_att)
}
