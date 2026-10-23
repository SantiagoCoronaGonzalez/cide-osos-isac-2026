# ============================================================
# funciones_entrenador.R — CIDE OSOS
# La historia de un entrenador contada con eventos de StatsBomb.
#
# MARCO ANALITICO (se define antes de medir nada)
#
#   Posesion        Tramo continuo en que un equipo controla el balon.
#                   Se usa el numero de posesion de StatsBomb; el dueno
#                   es el equipo con mas acciones con balon (pase,
#                   conduccion, tiro, regate, mal control, perdida).
#   Inicio          Como empieza la posesion:
#                     Balon parado      corner, o tiro libre o lateral en
#                                       el ultimo tercio
#                     Reanudacion       tiro libre o lateral fuera del
#                                       ultimo tercio
#                     Desde el portero  saque de meta o del portero
#                     Saque inicial     despues de un gol o al inicio
#                     Recuperacion      el rival tenia el balon en juego
#                                       y se lo quitamos (o "From Counter");
#                                       si lo atrapa el portero cuenta como
#                                       "desde el portero"
#                     Juego abierto     el resto
#   Secuencia ofensiva  Una posesion propia, de su inicio a su final.
#                   Fases: construccion (empieza en el tercio propio,
#                   x < 40), progresion (llega al medio, x >= 40, y al
#                   ultimo tercio, x >= 80) y finalizacion (tiro).
#   Transicion ofensiva  Posesion que empieza con una recuperacion.
#                   Es rapida si llega al ultimo tercio en 10 s o menos
#                   o termina en tiro en 15 s o menos.
#   Transicion defensiva Posesion del rival que empieza porque perdimos
#                   el balon en juego.
#   Fase de presion  Presiones propias durante una posesion del rival.
#                   Se resume con presiones por posesion rival, PPDA y
#                   la altura (x) donde se recupera el balon.
#   Contrapresion   Tras perder el balon en juego, recuperarlo en 5 s
#                   o menos.
#
# SUPUESTOS
#   - Coordenadas StatsBomb: cada equipo ataca hacia x = 120; y = 0 es
#     su banda izquierda. Carriles: izquierdo y < 26.7, central hasta
#     53.3, derecho el resto.
#   - El reloj (t) es minuto + segundo / 60 del partido; las
#     comparaciones de tiempo se hacen dentro del mismo periodo.
#   - xG sin penales para medir lo que genera el juego.
#   - Todo se compara contra lo que hace la liga EN EL MISMO CONTEXTO
#     (marcador, localia, nivel del rival, minuto). Asi un cambio de
#     conducta no se confunde con lo que hace cualquier equipo cuando,
#     por ejemplo, va perdiendo.
#
# ALCANCE Y LIMITES
#   - Eventos, no tracking: se ve donde ocurre cada accion con balon,
#     no los movimientos sin balon (salvo el 360 en balon parado).
#   - Un entrenador hereda una plantilla y un calendario: por eso se
#     muestra al lado el nivel del plantel y el de los rivales.
#   - Las diferencias se reportan con su intervalo; con pocos partidos
#     (un interinato) la incertidumbre es grande.
# ============================================================

ON_BALL <- c("Pass", "Carry", "Shot", "Dribble", "Miscontrol", "Dispossessed")
ACC_DEF <- c("Pressure", "Duel", "Interception", "Clearance", "Block", "Ball Recovery", "Foul Committed")
LARGO_YD <- 35        # pase largo: 35 yardas o mas
SEG_RAPIDA_ULT3 <- 10
SEG_RAPIDA_TIRO <- 15
SEG_CONTRAPRESION <- 5

maxna   <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
primero <- function(v) { v <- v[!is.na(v)]; if (length(v) == 0) v[NA_integer_] else v[1] }

# ── 1. Posesiones ─────────────────────────────────────────────
# e: salida de preparar_eventos() (funciones_tactico.R)
# niv: nivel_equipos.rds (team_id, torneo, nivel, quintil)
construir_posesiones <- function(e, niv) {
  e <- e %>%
    filter(!is.na(possession), !type %in% c("Starting XI", "Substitution", "Tactical Shift")) %>%
    arrange(match_id, index)

  dueno <- e %>% filter(type %in% ON_BALL) %>%
    count(match_id, possession, team_id) %>%
    arrange(match_id, possession, desc(n)) %>%
    distinct(match_id, possession, .keep_all = TRUE) %>%
    select(match_id, possession, pos_team = team_id)

  ej <- e %>%
    inner_join(dueno, by = c("match_id", "possession")) %>%
    mutate(
      propio  = team_id == pos_team,
      ob      = propio & type %in% ON_BALL,
      mov_ok  = propio & ((type == "Pass" & is.na(pass_outcome)) | type == "Carry") & !is.na(end_x),
      x_alc   = ifelse(mov_ok, pmax(x, end_x), ifelse(ob, x, NA_real_)),
      y_ent   = case_when(mov_ok & x < 80 & end_x >= 80 ~ end_y,
                          ob & !is.na(x) & x >= 80 ~ y, TRUE ~ NA_real_),
      t_ent   = ifelse(is.na(y_ent), NA_real_, t),
      en_area = ob & ((!is.na(x) & x >= 102 & y >= 18 & y <= 62) |
                      (mov_ok & end_x >= 102 & end_y >= 18 & end_y <= 62)),
      tiro    = propio & type == "Shot",
      xg_np   = ifelse(tiro & !(shot_type %in% "Penalty"), coalesce(shot_xg, 0), 0),
      gol     = tiro & shot_outcome %in% "Goal",
      t_tiro  = ifelse(tiro, t, NA_real_),
      pase    = propio & type == "Pass" & is.na(pass_type),
      largo   = ifelse(propio & type == "Pass" & !is.na(end_x),
                       sqrt((end_x - x)^2 + (end_y - y)^2) >= LARGO_YD, NA),
      presion_def = !propio & type == "Pressure",
      obv_p   = ifelse(propio, coalesce(obv_total_net, 0), 0),
      # Donde empieza: primera accion propia con ubicacion (en una
      # recuperacion es el punto donde se gano el balon)
      x_ob = ifelse(propio & !is.na(x), x, NA_real_), y_ob = ifelse(propio & !is.na(x), y, NA_real_),
      largo_pase = ifelse(propio & type == "Pass", largo, NA),
      tipo_pr = ifelse(propio, type, NA_character_)
    )

  pos <- ej %>%
    group_by(match_id, possession, pos_team) %>%
    summarise(
      period = first(period), t0 = first(t), t1 = last(t),
      patron = first(play_pattern),
      x0 = primero(x_ob), y0 = primero(y_ob), tipo0 = primero(tipo_pr),
      x_max = maxna(x_alc),
      y_entrada = primero(y_ent), t_entrada = primero(t_ent),
      area = any(en_area), tiros = sum(tiro), xg = sum(xg_np), goles = sum(gol),
      t_tiro = primero(t_tiro), pases = sum(pase),
      primer_pase_largo = primero(largo_pase),
      presiones_def = sum(presion_def), obv = sum(obv_p),
      .groups = "drop") %>%
    arrange(match_id, possession)

  # Partido, localia, rival
  info <- e %>% distinct(match_id, torneo, orden_torneo, temporada, match_date, home_team_id, away_team_id)
  pos <- pos %>%
    inner_join(info, by = "match_id") %>%
    mutate(rival_id = ifelse(pos_team == home_team_id, away_team_id, home_team_id),
           local = pos_team == home_team_id)

  # Inicio de la posesion (necesita la anterior)
  pos <- pos %>%
    group_by(match_id) %>%
    mutate(prev_team = lag(pos_team), prev_period = lag(period),
           inicio = case_when(
             # Balon parado: corner, o tiro libre / lateral en el ultimo tercio.
             # Fuera del ultimo tercio es solo una reanudacion del juego.
             patron == "From Corner" ~ "Balón parado",
             patron %in% c("From Free Kick", "From Throw In") & coalesce(x0, 0) >= 80 ~ "Balón parado",
             patron %in% c("From Free Kick", "From Throw In") ~ "Reanudación",
             patron %in% c("From Goal Kick", "From Keeper") ~ "Desde el portero",
             patron == "From Kick Off" ~ "Saque inicial",
             patron == "From Counter" ~ "Recuperación",
             # El portero atrapa o controla un tiro o centro: empieza desde el portero
             coalesce(tipo0, "") == "Goal Keeper" ~ "Desde el portero",
             !is.na(prev_team) & prev_team != pos_team & prev_period == period ~ "Recuperación",
             TRUE ~ "Juego abierto")) %>%
    mutate(
      # Perdida en juego: la siguiente es del rival y empieza como recuperacion
      sig_team = lead(pos_team), sig_inicio = lead(inicio), sig_t0 = lead(t0), sig_period = lead(period),
      sig2_team = lead(pos_team, 2), sig2_inicio = lead(inicio, 2), sig2_t0 = lead(t0, 2), sig2_period = lead(period, 2),
      perdida_juego = !is.na(sig_team) & sig_team != pos_team & sig_inicio == "Recuperación",
      recupera_5s = perdida_juego & !is.na(sig2_team) & sig2_team == pos_team & sig2_inicio == "Recuperación" &
                    sig2_period == period & (sig2_t0 - sig_t0) * 60 <= SEG_CONTRAPRESION) %>%
    ungroup() %>%
    select(-starts_with("sig_"), -starts_with("sig2_"), -prev_team, -prev_period)

  pos <- pos %>%
    mutate(
      dur_s     = pmax((t1 - t0) * 60, 0),
      avance    = x_max - x0,
      velocidad = ifelse(!is.na(avance) & avance >= 10, avance / pmax(dur_s, 1), NA_real_),
      ult3      = !is.na(x_max) & x_max >= 80,
      medio     = !is.na(x_max) & x_max >= 40,
      con_tiro  = tiros > 0,
      carril    = cut(y_entrada, c(-Inf, 80 / 3, 160 / 3, Inf), labels = c("Izquierdo", "Central", "Derecho")),
      transicion = inicio == "Recuperación",
      rapida    = transicion & ((!is.na(t_entrada) & (t_entrada - t0) * 60 <= SEG_RAPIDA_ULT3 & coalesce(x0, 0) < 80) |
                                (!is.na(t_tiro) & (t_tiro - t0) * 60 <= SEG_RAPIDA_TIRO)),
      construccion = !is.na(x0) & x0 < 40 & inicio %in% c("Desde el portero", "Juego abierto", "Recuperación", "Reanudación"),
      tramo     = cut(t0, c(-Inf, 30, 60, 75, Inf), labels = c("0–30'", "30–60'", "60–75'", "75'+"), right = FALSE)
    )

  # Marcador al empezar la posesion (goles anteriores, mismo orden period/t)
  goles <- bind_rows(
    e %>% filter(type == "Shot", shot_outcome %in% "Goal") %>% select(match_id, team_id, period, t),
    e %>% filter(type == "Own Goal For") %>% select(match_id, team_id, period, t)) %>%
    mutate(ord = period * 1000 + t)
  pos$ord0 <- pos$period * 1000 + pos$t0
  if (nrow(goles) > 0) {
    gp <- pos %>% select(match_id, possession, pos_team, ord0) %>%
      inner_join(goles, by = "match_id", relationship = "many-to-many") %>%
      filter(ord < ord0) %>%
      group_by(match_id, possession) %>%
      summarise(gf = sum(team_id == first(pos_team)), gc = sum(team_id != first(pos_team)), .groups = "drop")
    pos <- pos %>% left_join(gp, by = c("match_id", "possession"))
  } else { pos$gf <- 0L; pos$gc <- 0L }
  pos <- pos %>%
    mutate(gf = coalesce(gf, 0L), gc = coalesce(gc, 0L),
           estado = factor(sign(gf - gc), levels = c(-1, 0, 1), labels = c("Perdiendo", "Empatando", "Ganando"))) %>%
    left_join(niv %>% select(rival_id = team_id, torneo, quintil_rival = quintil), by = c("rival_id", "torneo")) %>%
    mutate(rival_fuerte = coalesce(quintil_rival, 0L) == 5L)
  pos
}

# ── 2. KPIs por equipo y partido ──────────────────────────────
# Catalogo: que mide cada indicador y como leerlo.
KPIS <- tribble(
  ~kpi,              ~etiqueta,                                  ~fase,         ~definicion,
  "posesion_pct",    "Posesión (%)",                             "Ataque",      "Porcentaje del tiempo con balón.",
  "pases_pos",       "Pases por posesión",                       "Ataque",      "Pases en juego por posesión propia: paciencia en la circulación.",
  "salida_limpia",   "Salida limpia (%)",                        "Construcción","De las posesiones que empiezan en el tercio propio, % que llega al medio campo.",
  "largo_portero",   "Saque largo del portero (%)",              "Construcción","Posesiones desde el portero cuyo primer pase es de 35+ yardas.",
  "velocidad",       "Velocidad vertical (yd/s)",                "Progresión",  "Yardas ganadas hacia el arco rival por segundo, en posesiones que avanzan 10+ yardas.",
  "pos_ult3",        "Posesiones al último tercio (%)",          "Progresión",  "% de posesiones propias que llegan a x ≥ 80.",
  "carril_izq",      "Entradas por la izquierda (%)",            "Progresión",  "De las entradas al último tercio, % por el carril izquierdo.",
  "carril_cen",      "Entradas por el centro (%)",               "Progresión",  "De las entradas al último tercio, % por el carril central.",
  "carril_der",      "Entradas por la derecha (%)",              "Progresión",  "De las entradas al último tercio, % por el carril derecho.",
  "pos_tiro",        "Posesiones con tiro (%)",                  "Finalización","% de posesiones propias que terminan en tiro.",
  "xg_100",          "xG cada 100 posesiones",                   "Finalización","xG sin penales generado por cada 100 posesiones propias.",
  "xg_transicion",   "xG en transición (%)",                     "Transición",  "Parte del xG propio que nace de una recuperación.",
  "rapidas",         "Transiciones rápidas por partido",         "Transición",  "Recuperaciones que llegan al último tercio en ≤10 s o terminan en tiro en ≤15 s.",
  "ppda",            "PPDA",                                     "Presión",     "Pases del rival en su 60% de cancha por acción defensiva propia ahí. Menos = presión más intensa.",
  "presion_pos",     "Presiones por posesión rival",             "Presión",     "Presiones propias durante cada posesión del rival.",
  "altura_recup",    "Altura de recuperación (x)",               "Presión",     "Dónde se recupera el balón (0 = arco propio, 120 = arco rival).",
  "recup_altas",     "Recuperaciones en campo rival por partido","Presión",     "Recuperaciones con x ≥ 60.",
  "recupera_5s",     "Contrapresión eficaz (%)",                 "Transición",  "Pérdidas en juego recuperadas en 5 s o menos.",
  "altura_bloque",   "Altura del bloque (x)",                    "Defensa",     "Altura media de las acciones defensivas propias.",
  "rival_ult3",      "Rival al último tercio (%)",               "Defensa",     "% de posesiones del rival que llegan a nuestro último tercio.",
  "xg_contra_100",   "xG concedido cada 100 posesiones rivales", "Defensa",     "xG sin penales del rival por cada 100 de sus posesiones.",
  "xg_contra_trans", "xG concedido en transición por partido",   "Defensa",     "xG del rival en posesiones que empiezan tras una pérdida nuestra.",
  "field_tilt",      "Field tilt (%)",                           "Dominio",     "% de los pases en el último tercio que son propios.",
  "np_xg",           "xG a favor por partido",                   "Resultado",   "xG sin penales.",
  "np_xg_contra",    "xG en contra por partido",                 "Resultado",   "xG sin penales del rival."
)
# KPIs de estilo (no de resultado) para la huella y el clasificador
KPIS_ESTILO <- c("posesion_pct", "pases_pos", "salida_limpia", "largo_portero", "velocidad", "pos_ult3",
                 "carril_izq", "carril_cen", "pos_tiro", "xg_transicion", "rapidas", "ppda", "presion_pos",
                 "altura_recup", "recup_altas", "recupera_5s", "altura_bloque", "rival_ult3")
# Sentido "mejor" para el tablero (1 = mas es mejor, -1 = menos es mejor, 0 = estilo)
SENTIDO <- c(posesion_pct = 0, pases_pos = 0, salida_limpia = 1, largo_portero = 0, velocidad = 0, pos_ult3 = 1,
             carril_izq = 0, carril_cen = 0, carril_der = 0, pos_tiro = 1, xg_100 = 1, xg_transicion = 0, rapidas = 1,
             ppda = -1, presion_pos = 0, altura_recup = 0, recup_altas = 1, recupera_5s = 1, altura_bloque = 0,
             rival_ult3 = -1, xg_contra_100 = -1, xg_contra_trans = -1, field_tilt = 1, np_xg = 1, np_xg_contra = -1)
etiqueta_kpi <- function(k) KPIS$etiqueta[match(k, KPIS$kpi)]

kpis_equipo_partido <- function(pos, e, mp) {
  prop <- pos %>%
    group_by(match_id, team_id = pos_team) %>%
    summarise(
      n_pos = n(), dur = sum(dur_s),
      pases_pos = mean(pases),
      salida_limpia = 100 * mean(medio[construccion]),
      largo_portero = 100 * mean(primer_pase_largo[inicio == "Desde el portero"], na.rm = TRUE),
      velocidad = mean(velocidad, na.rm = TRUE),
      pos_ult3 = 100 * mean(ult3),
      carril_izq = 100 * mean(carril == "Izquierdo", na.rm = TRUE),
      carril_cen = 100 * mean(carril == "Central", na.rm = TRUE),
      carril_der = 100 * mean(carril == "Derecho", na.rm = TRUE),
      pos_tiro = 100 * mean(con_tiro),
      xg_100 = 100 * sum(xg) / n(),
      xg_transicion = ifelse(sum(xg) > 0, 100 * sum(xg[transicion]) / sum(xg), NA_real_),
      rapidas = sum(rapida),
      altura_recup = mean(x0[transicion], na.rm = TRUE),
      recup_altas = sum(transicion & coalesce(x0, 0) >= 60),
      recupera_5s = ifelse(sum(perdida_juego) > 0, 100 * sum(recupera_5s) / sum(perdida_juego), NA_real_),
      .groups = "drop")
  # Lo que hace el rival en sus posesiones, visto desde el que defiende
  ajen <- pos %>%
    group_by(match_id, team_id = rival_id) %>%
    summarise(
      presion_pos = mean(presiones_def),
      rival_ult3 = 100 * mean(ult3),
      xg_contra_100 = 100 * sum(xg) / n(),
      xg_contra_trans = sum(xg[transicion]),
      dur_rival = sum(dur_s), .groups = "drop")
  bloque <- e %>% filter(type %in% ACC_DEF, !is.na(x)) %>%
    group_by(match_id, team_id) %>% summarise(altura_bloque = mean(x), .groups = "drop")
  prop %>%
    inner_join(ajen, by = c("match_id", "team_id")) %>%
    left_join(bloque, by = c("match_id", "team_id")) %>%
    mutate(posesion_pct = 100 * dur / pmax(dur + dur_rival, 1)) %>%
    select(-dur, -dur_rival) %>%
    left_join(mp %>% select(match_id, team_id, rival_id, local, torneo, orden_torneo, temporada,
                            ppda, field_tilt, np_xg, np_xg_contra, goles, goles_contra),
              by = c("match_id", "team_id")) %>%
    mutate(puntos = case_when(goles > goles_contra ~ 3L, goles == goles_contra ~ 1L, TRUE ~ 0L))
}

# ── 3. Referencia de la liga ──────────────────────────────────
# Medias de cada club en cada torneo: la "poblacion" contra la que se
# compara a un entrenador.
liga_equipo_torneo <- function(kp) {
  kp %>% group_by(team_id, torneo, orden_torneo) %>%
    summarise(partidos = n(), across(all_of(KPIS$kpi), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

# Huella: cada KPI del entrenador como z y percentil frente a los
# clubes-torneo de la liga, con intervalo por remuestreo de partidos.
huella_entrenadores <- function(kp_club, liga_t, B = 1000) {
  map_dfr(KPIS$kpi, function(k) {
    ref <- liga_t[[k]]; ref <- ref[is.finite(ref)]
    mu <- mean(ref); s <- sd(ref)
    kp_club %>% group_by(entrenador) %>%
      group_modify(function(d, g) {
        v <- d[[k]]; v <- v[is.finite(v)]
        if (length(v) < 3) return(tibble())
        bs <- replicate(B, mean(sample(v, replace = TRUE)))
        tibble(kpi = k, valor = mean(v), lo = unname(quantile(bs, .025)), hi = unname(quantile(bs, .975)),
               z = (mean(v) - mu) / s, z_lo = (unname(quantile(bs, .025)) - mu) / s,
               z_hi = (unname(quantile(bs, .975)) - mu) / s,
               percentil = round(100 * mean(ref <= mean(v))), liga = mu, partidos = length(v))
      }) %>% ungroup()
  }) %>% mutate(etiqueta = etiqueta_kpi(kpi), fase = KPIS$fase[match(kpi, KPIS$kpi)])
}

# ¿Se reconoce al entrenador por sus partidos? Analisis discriminante
# lineal con validacion "deja uno fuera" y prueba de permutacion.
reconocer_entrenador <- function(kp_club, vars = KPIS_ESTILO, n_perm = 200) {
  d <- kp_club %>% select(entrenador, all_of(vars))
  X <- as.matrix(d[, vars])
  for (j in seq_len(ncol(X))) { m <- mean(X[, j], na.rm = TRUE); X[is.na(X[, j]), j] <- m }
  ok <- apply(X, 2, function(c) sd(c) > 0)
  X <- scale(X[, ok, drop = FALSE])
  y <- factor(d$entrenador)
  if (nlevels(y) < 2 || nrow(X) < 20) return(NULL)
  # Acierto balanceado (promedio del acierto de cada entrenador) con
  # probabilidades iguales a priori: un interinato corto pesa lo mismo
  # que un ciclo largo. El azar es 1 / numero de entrenadores.
  K <- nlevels(y)
  bal <- function(pred, yy) mean(tapply(as.character(pred) == as.character(yy), yy, mean))
  ajusta <- function(yy) {
    f <- tryCatch(MASS::lda(X, grouping = yy, CV = TRUE, prior = rep(1 / K, K)), error = function(e) NULL)
    if (is.null(f)) return(NA_real_)
    bal(f$class, yy)
  }
  f <- MASS::lda(X, grouping = y, CV = TRUE, prior = rep(1 / K, K))
  acierto <- bal(f$class, y)
  nulo <- replicate(n_perm, ajusta(sample(y)))
  conf <- as.data.frame(table(real = y, predicho = factor(f$class, levels = levels(y)))) %>%
    group_by(real) %>% mutate(pct = 100 * Freq / sum(Freq)) %>% ungroup()
  # Que variables separan mas (coeficientes del primer discriminante, ajuste completo)
  full <- MASS::lda(X, grouping = y, prior = rep(1 / K, K))
  sep <- tibble(kpi = colnames(X), peso = abs(full$scaling[, 1])) %>% arrange(desc(peso))
  scores <- as.data.frame(predict(full)$x)
  list(acierto = acierto, azar = 1 / K, nulo = nulo,
       p_valor = mean(c(nulo, acierto) >= acierto, na.rm = TRUE), confusion = conf, separa = sep,
       puntos = bind_cols(kp_club %>% select(match_id, entrenador, torneo), scores[, 1:min(2, ncol(scores)), drop = FALSE]),
       n = nrow(X))
}

# Cuanto de la variacion entre partidos explica el entrenador (eta^2)
efecto_entrenador <- function(kp_club, vars = KPIS_ESTILO) {
  map_dfr(vars, function(k) {
    d <- kp_club %>% filter(is.finite(.data[[k]]))
    if (n_distinct(d$entrenador) < 2) return(NULL)
    a <- summary(aov(d[[k]] ~ d$entrenador))[[1]]
    tibble(kpi = k, eta2 = a$`Sum Sq`[1] / sum(a$`Sum Sq`), p = a$`Pr(>F)`[1])
  }) %>% mutate(etiqueta = etiqueta_kpi(kpi)) %>% arrange(desc(eta2))
}

# ── 4. Ajustes por contexto ───────────────────────────────────
# Para cada contexto (marcador, localia, rival, minuto) y cada
# conducta, la diferencia contra el nivel de referencia, para el club
# con cada entrenador y para la liga. Se calcula por partido (cada
# partido pesa igual) para que los errores esten agrupados.
CONDUCTAS <- tribble(
  ~conducta,      ~etiqueta,                         ~quien,
  "velocidad",    "Velocidad vertical (yd/s)",       "ataca",
  "pos_tiro",     "Posesiones con tiro (%)",         "ataca",
  "ult3",         "Posesiones al último tercio (%)", "ataca",
  "largo",        "Primer pase largo (%)",           "ataca",
  "presion",      "Presiones por posesión rival",    "defiende",
  "altura_recup", "Altura de recuperación (x)",      "defiende"
)
CONTEXTOS <- list(
  estado = list(var = "estado", ref = "Empatando", etiqueta = "Marcador"),
  local  = list(var = "local_lab", ref = "Visitante", etiqueta = "Localía"),
  rival  = list(var = "rival_lab", ref = "Resto de la liga", etiqueta = "Rival"),
  tramo  = list(var = "tramo", ref = "0–30'", etiqueta = "Minuto")
)

conductas_por_contexto <- function(pos, ctx_var) {
  p <- pos %>% mutate(local_lab = ifelse(local, "Local", "Visitante"),
                      rival_lab = ifelse(rival_fuerte, "Rival del 20% más fuerte", "Resto de la liga"))
  at <- p %>% group_by(match_id, team_id = pos_team, nivel = .data[[ctx_var]]) %>%
    summarise(n = n(),
              velocidad = mean(velocidad, na.rm = TRUE), pos_tiro = 100 * mean(con_tiro),
              ult3 = 100 * mean(ult3), largo = 100 * mean(primer_pase_largo, na.rm = TRUE), .groups = "drop")
  # El estado del marcador es del que ataca; para el que defiende se invierte
  p_def <- p
  if (ctx_var == "estado") p_def$estado <- factor(c(Perdiendo = "Ganando", Empatando = "Empatando", Ganando = "Perdiendo")[as.character(p$estado)],
                                                  levels = levels(p$estado))
  if (ctx_var == "local_lab") p_def$local_lab <- ifelse(p$local, "Visitante", "Local")
  df <- p_def %>% filter(rival_id != pos_team) %>%
    group_by(match_id, team_id = rival_id, nivel = .data[[ctx_var]]) %>%
    summarise(presion = mean(presiones_def), .groups = "drop")
  rec <- p %>% filter(transicion) %>% group_by(match_id, team_id = pos_team, nivel = .data[[ctx_var]]) %>%
    summarise(altura_recup = mean(x0, na.rm = TRUE), .groups = "drop")
  if (ctx_var == "rival_lab") {
    df <- p %>% group_by(match_id, team_id = rival_id) %>% summarise(presion = mean(presiones_def), .groups = "drop") %>%
      left_join(p %>% distinct(match_id, team_id = pos_team, nivel = rival_lab) %>%
                  group_by(match_id, team_id) %>% slice(1) %>% ungroup(), by = c("match_id", "team_id"))
  }
  at %>% full_join(df, by = c("match_id", "team_id", "nivel")) %>%
    full_join(rec, by = c("match_id", "team_id", "nivel")) %>% filter(!is.na(nivel))
}

ajustes_contexto <- function(pos, partidos_club) {
  # partidos_club: match_id, team_id (el club), entrenador
  tor <- pos %>% distinct(match_id, torneo)
  map_dfr(names(CONTEXTOS), function(cn) {
    cx <- CONTEXTOS[[cn]]
    pm <- conductas_por_contexto(pos, cx$var)
    entre_partidos <- cx$var %in% c("local_lab", "rival_lab")
    map_dfr(CONDUCTAS$conducta, function(cd) {
      d <- pm %>% select(match_id, team_id, nivel, v = all_of(cd)) %>% filter(is.finite(v))
      if (entre_partidos) {
        # La localia y el rival no cambian dentro de un partido: se comparan
        # partidos distintos del mismo club en el mismo torneo.
        d <- d %>% left_join(tor, by = "match_id")
        liga <- d %>% group_by(team_id, torneo, es_ref = nivel == cx$ref) %>% summarise(v = mean(v), .groups = "drop") %>%
          pivot_wider(names_from = es_ref, values_from = v, names_prefix = "r") %>%
          filter(!is.na(rTRUE), !is.na(rFALSE)) %>% mutate(dif = rFALSE - rTRUE)
        nivel_otro <- setdiff(unique(as.character(d$nivel)), cx$ref)[1]
        liga <- tibble(nivel = nivel_otro, dif_liga = mean(liga$dif), ee_liga = sd(liga$dif) / sqrt(nrow(liga)), n_liga = nrow(liga))
        club <- d %>% inner_join(partidos_club, by = c("match_id", "team_id")) %>%
          group_by(entrenador) %>%
          summarise(dif_club = mean(v[nivel != cx$ref]) - mean(v[nivel == cx$ref]),
                    ee_club = sqrt(var(v[nivel != cx$ref]) / sum(nivel != cx$ref) + var(v[nivel == cx$ref]) / sum(nivel == cx$ref)),
                    n_club = min(sum(nivel != cx$ref), sum(nivel == cx$ref)), .groups = "drop") %>%
          mutate(nivel = nivel_otro)
        return(club %>% left_join(liga, by = "nivel") %>%
                 mutate(contexto = cx$etiqueta, referencia = cx$ref, conducta = cd,
                        propio = dif_club - dif_liga, ee_propio = sqrt(ee_club^2 + ee_liga^2),
                        lo = propio - 1.96 * ee_propio, hi = propio + 1.96 * ee_propio))
      }
      ref <- d %>% filter(nivel == cx$ref) %>% select(match_id, team_id, v_ref = v)
      dd <- d %>% filter(nivel != cx$ref) %>% inner_join(ref, by = c("match_id", "team_id")) %>%
        mutate(dif = v - v_ref)
      liga <- dd %>% group_by(nivel) %>%
        summarise(dif_liga = mean(dif), ee_liga = sd(dif) / sqrt(n()), n_liga = n(), .groups = "drop")
      club <- dd %>% inner_join(partidos_club, by = c("match_id", "team_id")) %>%
        group_by(entrenador, nivel) %>%
        summarise(dif_club = mean(dif), ee_club = sd(dif) / sqrt(n()), n_club = n(), .groups = "drop")
      club %>% left_join(liga, by = "nivel") %>%
        mutate(contexto = cx$etiqueta, referencia = cx$ref, conducta = cd,
               propio = dif_club - dif_liga, ee_propio = sqrt(ee_club^2 + ee_liga^2),
               lo = propio - 1.96 * ee_propio, hi = propio + 1.96 * ee_propio)
    })
  }) %>% mutate(nivel = as.character(nivel),
                etiqueta = CONDUCTAS$etiqueta[match(conducta, CONDUCTAS$conducta)])
}

# ── 5. Simulacion de partidos ─────────────────────────────────
# Si el partido se jugara infinitas veces con las mismas ocasiones,
# cada tiro entra con probabilidad igual a su xG. Los goles de cada
# equipo siguen una Poisson-binomial; se calcula exacta.
pmf_goles <- function(xg) {
  p <- 1
  for (q in xg) p <- c(p * (1 - q), 0) + c(0, p * q)
  p
}
simular_partidos <- function(e, club_id) {
  tiros <- e %>% filter(type == "Shot", !is.na(shot_xg)) %>% select(match_id, team_id, shot_xg)
  part_club <- e %>% filter(team_id == club_id) %>% distinct(match_id)
  map_dfr(part_club$match_id, function(m) {
    tm <- tiros %>% filter(match_id == m)
    a <- pmf_goles(tm$shot_xg[tm$team_id == club_id]); b <- pmf_goles(tm$shot_xg[tm$team_id != club_id])
    M <- outer(a, b)                      # P(goles club = i-1, goles rival = j-1)
    dif <- outer(seq_along(a), seq_along(b), "-")
    tibble(match_id = m, p_gana = sum(M[dif > 0]), p_empata = sum(M[dif == 0]), p_pierde = sum(M[dif < 0]),
           xpts = 3 * sum(M[dif > 0]) + sum(M[dif == 0]))
  })
}
# Torneo repetido n veces: distribucion de puntos con las ocasiones reales
simular_torneos <- function(sim, n = 10000) {
  sim %>% group_by(entrenador, torneo) %>%
    group_modify(function(d, g) {
      pts <- vapply(seq_len(nrow(d)), function(i)
        sample(c(3, 1, 0), n, replace = TRUE, prob = c(d$p_gana[i], d$p_empata[i], d$p_pierde[i])), numeric(n))
      if (is.null(dim(pts))) pts <- matrix(pts, ncol = 1)
      tot <- rowSums(pts)
      tibble(partidos = nrow(d), puntos = sum(d$puntos), xpts = sum(d$xpts),
             p10 = quantile(tot, .1), p50 = quantile(tot, .5), p90 = quantile(tot, .9),
             pct_mejor = round(100 * mean(tot < sum(d$puntos))))
    }) %>% ungroup()
}

# ── 6. Jugadores con cada entrenador ──────────────────────────
uso_jugadores <- function(cr, part_club, club_id, perf_t) {
  jug <- cr$jugadores %>% filter(team_id == club_id) %>% inner_join(part_club, by = "match_id")
  pos <- cr$posiciones %>% filter(team_id == club_id) %>% inner_join(part_club, by = "match_id") %>%
    group_by(match_id, player_id) %>% summarise(min = sum(dur_min, na.rm = TRUE), .groups = "drop")
  fechas <- part_club %>% select(match_id, match_date, entrenador)
  nac <- jug %>% filter(!is.na(birth_date)) %>% distinct(player_id, birth_date) %>% group_by(player_id) %>% slice(1) %>% ungroup()
  # Rotacion: titulares que cambian de un partido al siguiente
  tit <- jug %>% filter(titular) %>% select(match_id, player_id) %>% inner_join(fechas, by = "match_id")
  rot <- tit %>% group_by(entrenador, match_id, match_date) %>% summarise(xi = list(sort(player_id)), .groups = "drop") %>%
    arrange(match_date) %>%
    mutate(cambios_xi = map2_int(xi, lag(xi), ~ if (is.null(.y)) NA_integer_ else length(setdiff(.x, .y))),
           cambios_xi = ifelse(entrenador != lag(entrenador), NA_integer_, cambios_xi))
  # Continuidad: minutos de jugadores que ya estaban antes del entrenador
  inicio_dt <- fechas %>% group_by(entrenador) %>% summarise(desde = min(match_date), .groups = "drop")
  primera <- pos %>% inner_join(fechas, by = "match_id") %>% group_by(player_id) %>% summarise(debut = min(match_date), .groups = "drop")
  m <- pos %>% inner_join(fechas, by = "match_id") %>% left_join(nac, by = "player_id") %>%
    left_join(primera, by = "player_id") %>% left_join(inicio_dt, by = "entrenador") %>%
    mutate(edad = as.numeric(match_date - as.Date(birth_date)) / 365.25, heredado = debut < desde)
  res <- m %>% group_by(entrenador) %>%
    summarise(partidos = n_distinct(match_id), jugadores = n_distinct(player_id),
              edad_media = weighted.mean(edad, min, na.rm = TRUE),
              pct_min_heredados = 100 * sum(min[heredado]) / sum(min),
              primero = min(match_date),
              .groups = "drop") %>%
    left_join(rot %>% group_by(entrenador) %>% summarise(cambios_xi = mean(cambios_xi, na.rm = TRUE), .groups = "drop"),
              by = "entrenador")
  # Concentracion: % de minutos de los 14 mas usados
  conc <- m %>% group_by(entrenador, player_id) %>% summarise(min = sum(min), .groups = "drop") %>%
    group_by(entrenador) %>% arrange(desc(min), .by_group = TRUE) %>%
    summarise(pct_min_top14 = 100 * sum(head(min, 14)) / sum(min), .groups = "drop")
  # Los de mas minutos con cada entrenador, con su z fiable en ese torneo
  top <- m %>% group_by(entrenador, player_id) %>% summarise(min = sum(min), partidos = n_distinct(match_id), .groups = "drop") %>%
    left_join(cr$jugadores %>% distinct(player_id, player_name) %>% group_by(player_id) %>% slice(1) %>% ungroup(), by = "player_id") %>%
    group_by(entrenador) %>% slice_max(min, n = 14, with_ties = FALSE) %>% ungroup()
  # El primer entrenador de la base no "hereda" a nadie que podamos ver
  res <- res %>% mutate(pct_min_heredados = ifelse(primero == min(primero), NA_real_, pct_min_heredados)) %>% select(-primero)
  list(resumen = res %>% left_join(conc, by = "entrenador"), top = top, minutos = m)
}

# Posicion (subgrupo) de los minutos con cada entrenador
uso_posiciones <- function(cr, part_club, club_id) {
  cr$posiciones %>% filter(team_id == club_id) %>% inner_join(part_club, by = "match_id") %>%
    mutate(subgrupo = unname(MAPA_POSICION[as.character(position_id)])) %>% filter(!is.na(subgrupo)) %>%
    group_by(entrenador, subgrupo) %>% summarise(min = sum(dur_min, na.rm = TRUE), .groups = "drop") %>%
    group_by(entrenador) %>% mutate(pct = 100 * min / sum(min)) %>% ungroup()
}

# Fichajes: jugadores que debutan con el club durante cada entrenador.
# Ex ante: su ultimo torneo en otro club de la liga; despues: su
# primer torneo en el club.
fichajes_por_entrenador <- function(perf_t, club_id, ent_torneo) {
  pc <- perf_t %>% filter(team_id == club_id)
  primer_orden <- min(perf_t$orden_torneo)
  llegada <- pc %>% group_by(player_id) %>% summarise(orden_llegada = min(orden_torneo), .groups = "drop") %>%
    filter(orden_llegada > primer_orden)
  antes <- perf_t %>% filter(team_id != club_id) %>% inner_join(llegada, by = "player_id") %>%
    filter(orden_torneo < orden_llegada) %>% group_by(player_id) %>% slice_max(orden_torneo, n = 1, with_ties = FALSE) %>%
    ungroup() %>% transmute(player_id, club_antes = club, z_antes = z_compuesto_fiable, min_antes = minutos_total)
  despues <- pc %>% inner_join(llegada, by = c("player_id", "orden_torneo" = "orden_llegada")) %>%
    transmute(player_id, player_name, torneo, orden_torneo, subgrupo_lab, edad,
              z_despues = z_compuesto_fiable, min_despues = minutos_total)
  despues %>% left_join(antes, by = "player_id") %>%
    left_join(ent_torneo, by = "torneo") %>%
    mutate(rindio = case_when(min_despues < 450 ~ "Jugó poco", z_despues > 0 ~ "Rindió", TRUE ~ "No rindió"))
}

# ── 7. ¿Mejoro a sus jugadores? ───────────────────────────────
# Para cada jugador que sigue en el mismo club de un torneo al siguiente
# (450+ minutos en los dos), el cambio de su indice (z compuesto). Un
# jugador que tuvo un torneo muy bueno tiende a bajar en el siguiente y
# uno muy malo a subir (regresion a la media), y los jovenes suelen
# mejorar con la edad. Por eso el cambio se compara contra lo esperado
# en la liga para un jugador de su nivel previo, su edad y su posicion:
# lo que queda es la mejora que no explica nada de eso. Se promedia por
# club y torneo, y el del club con cada entrenador se ubica entre todos.
desarrollo_jugadores <- function(perf_t, club_id, ent_torneo, min_min = 450, B = 2000) {
  p <- perf_t %>% filter(!is.na(z_compuesto), minutos_total >= min_min) %>%
    select(player_id, player_name, team_id, club, torneo, orden_torneo, subgrupo, edad, z = z_compuesto, minutos_total)
  # Torneos consecutivos de la liga (Apertura y Clausura alternados)
  ordenes <- sort(unique(perf_t$orden_torneo))
  sig <- setNames(c(ordenes[-1], NA), ordenes)
  pares <- p %>% mutate(orden_sig = sig[as.character(orden_torneo)]) %>%
    inner_join(p %>% select(player_id, team_id, orden_sig = orden_torneo, torneo_sig = torneo, z_sig = z, edad_sig = edad,
                            min_sig = minutos_total),
               by = c("player_id", "team_id", "orden_sig")) %>%
    mutate(cambio = z_sig - z)
  if (nrow(pares) < 50) return(NULL)
  m <- lm(cambio ~ z + edad + I(edad^2) + subgrupo, data = pares)
  pares$esperado <- predict(m, newdata = pares)
  pares$mejora <- pares$cambio - pares$esperado
  # Club-torneo (el torneo en que se mide la mejora es el siguiente)
  ct <- pares %>% group_by(team_id, club, torneo_sig) %>%
    summarise(jugadores = n(), mejora = mean(mejora), .groups = "drop")
  ref <- ct$mejora[ct$jugadores >= 4]
  club <- pares %>% filter(team_id == club_id) %>%
    left_join(ent_torneo %>% rename(torneo_sig = torneo), by = "torneo_sig") %>%
    filter(!is.na(entrenador_torneo))
  if (nrow(club) == 0) return(NULL)
  resumen <- club %>% group_by(entrenador = as.character(entrenador_torneo)) %>%
    group_modify(function(d, g) {
      bs <- replicate(B, mean(sample(d$mejora, replace = TRUE)))
      ct_dt <- d %>% group_by(torneo_sig) %>% summarise(v = mean(mejora), .groups = "drop")
      tibble(jugadores = n_distinct(d$player_id), casos = nrow(d), mejora = mean(d$mejora),
             lo = unname(quantile(bs, .025)), hi = unname(quantile(bs, .975)),
             cambio_bruto = mean(d$cambio),
             percentil = round(100 * mean(ref <= mean(ct_dt$v))))
    }) %>% ungroup()
  jug <- club %>% transmute(entrenador = as.character(entrenador_torneo), player_name, subgrupo, edad = round(edad),
                            torneo, torneo_sig, z_antes = z, z_despues = z_sig, mejora)
  list(resumen = resumen, jugadores = jug, n_pares_liga = nrow(pares),
       coef_regresion = unname(coef(m)["z"]), minimo = min_min)
}
