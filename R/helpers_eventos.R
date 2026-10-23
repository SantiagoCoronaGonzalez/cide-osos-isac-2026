# ============================================================
# helpers_eventos.R — CIDE OSOS
# Parseo compacto de Events v11 y 360 Frames v2 de StatsBomb.
#
# Un partido de eventos son ~3,500 filas con decenas de columnas
# anidadas. Aqui se reduce a lo que usa el analisis tactico:
#   eventos    una fila por evento, columnas planas
#   tiros_ff   posicion (con ID) de cada jugador en el momento de cada tiro
#   tacticas   formacion y posicion de cada jugador al inicio y en cada
#              cambio tactico (Starting XI / Tactical Shift)
#   resp_def   responsabilidad defensiva (v11) solo en acciones de balon
#              parado en el ultimo cuarto de cancha
# y de 360, solo los cuadros de balon parado (posicion de los 22
# jugadores, sin ID salvo el que ejecuta).
#
# Coordenadas StatsBomb: x 0-120 (porteria rival en 120), y 0-80,
# siempre en la direccion de ataque del equipo que hace la accion.
# ============================================================

# Columna de un data frame aplanado por jsonlite, probando nombres
col_ev <- function(d, opciones, defecto = NA) {
  for (o in opciones) if (o %in% names(d)) return(d[[o]])
  rep(defecto, nrow(d))
}

# Coordenada k de una columna lista de vectores [x, y(, z)]
coord <- function(v, k) {
  if (is.null(v)) return(NA_real_)
  if (!is.list(v)) return(rep(NA_real_, length(v)))
  vapply(v, function(p) if (length(p) >= k) as.numeric(p[[k]]) else NA_real_, numeric(1))
}

# Version tolerante de coord() cuando la columna puede no existir
coord_col <- function(d, opciones, k) {
  for (o in opciones) if (o %in% names(d)) return(coord(d[[o]], k))
  rep(NA_real_, nrow(d))
}

# Tipos de pase y de tiro que identifican el balon parado
TIPOS_BALON_PARADO <- c("Corner", "Free Kick", "Throw-in", "Goal Kick", "Kick Off")

parse_eventos <- function(ev, match_id) {
  if (is.null(ev) || !is.data.frame(ev) || nrow(ev) == 0) return(NULL)
  n <- nrow(ev)
  chr <- function(o) as.character(col_ev(ev, o))
  num <- function(o) suppressWarnings(as.numeric(col_ev(ev, o)))
  lgl <- function(o) { v <- col_ev(ev, o, FALSE); v[is.na(v)] <- FALSE; as.logical(v) }

  tipo <- chr(c("type.name", "type"))
  end_x <- coord_col(ev, c("pass.end_location", "carry.end_location"), 1)
  end_y <- coord_col(ev, c("pass.end_location", "carry.end_location"), 2)
  # Carry tiene su propio end_location: completar donde el pase no aplica
  if ("carry.end_location" %in% names(ev)) {
    cx <- coord(ev$carry.end_location, 1); cy <- coord(ev$carry.end_location, 2)
    end_x[is.na(end_x)] <- cx[is.na(end_x)]; end_y[is.na(end_y)] <- cy[is.na(end_y)]
  }

  rel <- col_ev(ev, "related_events", NULL)
  rel <- if (is.list(rel)) vapply(rel, function(r) paste(r, collapse = ","), character(1)) else rep(NA_character_, n)

  eventos <- tibble(
    match_id        = as.integer(match_id),
    id              = chr("id"),
    index           = as.integer(num("index")),
    period          = as.integer(num("period")),
    minute          = as.integer(num("minute")),
    second          = as.integer(num("second")),
    t               = as.integer(num("minute")) + as.integer(num("second")) / 60,
    type            = tipo,
    team_id         = as.integer(num(c("team.id"))),
    player_id       = as.integer(num(c("player.id"))),
    position_id     = as.integer(num(c("position.id"))),
    possession      = as.integer(num("possession")),
    possession_team_id = as.integer(num("possession_team.id")),
    play_pattern    = chr(c("play_pattern.name")),
    x               = coord_col(ev, "location", 1),
    y               = coord_col(ev, "location", 2),
    end_x           = end_x,
    end_y           = end_y,
    under_pressure  = lgl("under_pressure"),
    counterpress    = lgl("counterpress"),
    obv_total_net   = num("obv_total_net"),
    obv_for_net     = num("obv_for_net"),
    obv_against_net = num("obv_against_net"),
    pass_type       = chr("pass.type.name"),
    pass_height     = chr("pass.height.name"),
    pass_outcome    = chr("pass.outcome.name"),       # NA = completo
    pass_technique  = chr("pass.technique.name"),
    pass_recipient_id = as.integer(num("pass.recipient.id")),
    pass_shot_assist = lgl("pass.shot_assist"),
    pass_goal_assist = lgl("pass.goal_assist"),
    pass_aerial_won = lgl("pass.aerial_won"),
    shot_xg         = num("shot.statsbomb_xg"),
    shot_type       = chr("shot.type.name"),          # Open Play, Free Kick, Penalty, Corner
    shot_outcome    = chr("shot.outcome.name"),
    shot_body_part  = chr("shot.body_part.name"),
    shot_end_z      = coord_col(ev, "shot.end_location", 3),
    duel_type       = chr("duel.type.name"),
    duel_outcome    = chr("duel.outcome.name"),
    clearance_aerial_won = lgl("clearance.aerial_won"),
    sub_replacement_id = as.integer(num("substitution.replacement.id")),
    sub_outcome     = chr("substitution.outcome.name"),
    formation       = chr("tactics.formation"),
    related         = rel
  )

  # Posicion de cada jugador en cada tiro (con ID)
  tiros_ff <- NULL
  if ("shot.freeze_frame" %in% names(ev)) {
    idx <- which(tipo == "Shot" & vapply(ev$shot.freeze_frame, function(f) is.data.frame(f) && nrow(f) > 0, logical(1)))
    if (length(idx) > 0) tiros_ff <- bind_rows(lapply(idx, function(i) {
      f <- jsonlite::flatten(ev$shot.freeze_frame[[i]])
      tibble(match_id = as.integer(match_id), id = ev$id[i],
             player_id = as.integer(col_ev(f, c("player.id"))),
             position_id = as.integer(col_ev(f, c("position.id"))),
             teammate = as.logical(col_ev(f, "teammate")),
             x = coord_col(f, "location", 1), y = coord_col(f, "location", 2))
    }))
  }

  # Formacion y posiciones (Starting XI y cambios tacticos)
  tacticas <- NULL
  if ("tactics.lineup" %in% names(ev)) {
    idx <- which(tipo %in% c("Starting XI", "Tactical Shift") &
                   vapply(ev$tactics.lineup, function(f) is.data.frame(f) && nrow(f) > 0, logical(1)))
    if (length(idx) > 0) tacticas <- bind_rows(lapply(idx, function(i) {
      f <- jsonlite::flatten(ev$tactics.lineup[[i]])
      tibble(match_id = as.integer(match_id), id = ev$id[i], type = tipo[i],
             team_id = eventos$team_id[i], period = eventos$period[i], t = eventos$t[i],
             formation = eventos$formation[i],
             player_id = as.integer(col_ev(f, c("player.id"))),
             position_id = as.integer(col_ev(f, c("position.id"))))
    }))
  }

  # Responsabilidad defensiva: solo acciones de balon parado en el
  # ultimo cuarto (x >= 90). Es la que sirve para el marcaje.
  resp_def <- NULL
  col_rd <- grep("^defensive_responsibility", names(ev), value = TRUE)
  col_rd <- col_rd[vapply(col_rd, function(cn) is.list(ev[[cn]]), logical(1))]
  col_rd <- c(grep("players", col_rd, value = TRUE), col_rd)   # la lista de jugadores, no la de categorias
  if (length(col_rd) > 0) {
    cn <- col_rd[1]
    idx <- which(eventos$play_pattern %in% c("From Corner", "From Free Kick", "From Throw In") &
                   !is.na(eventos$x) & eventos$x >= 90 &
                   vapply(ev[[cn]], function(f) is.data.frame(f) && nrow(f) > 0, logical(1)))
    if (length(idx) > 0) resp_def <- bind_rows(lapply(idx, function(i) {
      f <- jsonlite::flatten(ev[[cn]][[i]])
      tibble(match_id = as.integer(match_id), id = ev$id[i],
             player_id = as.integer(col_ev(f, c("player_id", "player.id"))),
             prob = suppressWarnings(as.numeric(col_ev(f, "probability"))))
    }))
  }

  list(eventos = eventos, tiros_ff = tiros_ff, tacticas = tacticas, resp_def = resp_def)
}

# Eventos de balon parado cuyo cuadro 360 vale la pena guardar: el
# saque mismo (corner, tiro libre o lateral en el ultimo tercio) y los
# tiros que salen de una jugada de balon parado.
ids_balon_parado <- function(eventos) {
  e <- eventos
  saque <- e$type == "Pass" & e$pass_type %in% c("Corner", "Free Kick", "Throw-in") &
    !is.na(e$x) & (e$pass_type == "Corner" | e$x >= 80)
  tiro_bp <- e$type == "Shot" & e$play_pattern %in% c("From Corner", "From Free Kick", "From Throw In")
  e$id[saque | tiro_bp]
}

parse_360 <- function(fr, ids_guardar, match_id) {
  if (is.null(fr) || !is.data.frame(fr) || nrow(fr) == 0) return(NULL)
  idc <- as.character(col_ev(fr, c("event_uuid", "id")))
  idx <- which(idc %in% ids_guardar)
  if (length(idx) == 0 || !"freeze_frame" %in% names(fr)) return(NULL)
  bind_rows(lapply(idx, function(i) {
    f <- fr$freeze_frame[[i]]
    if (!is.data.frame(f) || nrow(f) == 0) return(NULL)
    f <- jsonlite::flatten(f)
    tibble(match_id = as.integer(match_id), id = idc[i],
           teammate = as.logical(col_ev(f, "teammate")),
           actor = as.logical(col_ev(f, "actor", FALSE)),
           keeper = as.logical(col_ev(f, "keeper", FALSE)),
           x = coord_col(f, "location", 1), y = coord_col(f, "location", 2))
  }))
}

# Tipos de evento y columnas que usa el analisis. Al cargar se
# descarta el resto (los "Ball Receipt" son ~25% de los eventos), para
# que ~1,850 partidos quepan en memoria.
TIPOS_USADOS <- c("Pass", "Shot", "Carry", "Pressure", "Duel", "Interception", "Clearance",
                  "Foul Committed", "Ball Recovery", "Block", "Goal Keeper", "Miscontrol",
                  "Dribble", "Dispossessed", "Substitution", "Starting XI", "Tactical Shift",
                  "Own Goal For", "Own Goal Against")
COLS_USADAS <- c("match_id", "id", "index", "period", "minute", "t", "type", "team_id", "player_id",
                 "possession", "play_pattern", "x", "y", "end_x", "end_y", "counterpress",
                 "obv_total_net", "pass_type", "pass_height", "pass_outcome", "pass_technique",
                 "shot_xg", "shot_type", "shot_outcome", "shot_body_part", "duel_type", "duel_outcome",
                 "sub_replacement_id", "sub_outcome", "formation", "pass_aerial_won", "clearance_aerial_won")

# Carga todos los eventos compactos de data/raw/eventos (con cache)
cargar_eventos <- function(dir_raw = "data/raw", cache = "data/processed/eventos.rds", refrescar = FALSE) {
  d <- file.path(dir_raw, "eventos")
  fs <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
  if (length(fs) == 0) stop("No hay eventos en ", d, ". Corre primero source('R/01b_descarga_eventos.R').")
  if (!refrescar && file.exists(cache) && file.mtime(cache) > max(file.mtime(fs))) return(readRDS(cache))
  partes <- lapply(fs, function(f) {
    p <- readRDS(f)
    p$eventos <- p$eventos[p$eventos$type %in% TIPOS_USADOS, intersect(COLS_USADAS, names(p$eventos))]
    p
  })
  junta <- function(k) bind_rows(lapply(partes, `[[`, k))
  out <- list(eventos = junta("eventos"), tiros_ff = junta("tiros_ff"),
              tacticas = junta("tacticas"), resp_def = junta("resp_def"))
  f360 <- list.files(file.path(dir_raw, "frames360"), pattern = "\\.rds$", full.names = TRUE)
  out$frames360 <- if (length(f360) > 0) bind_rows(lapply(f360, readRDS)) else NULL
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, cache)
  out
}
