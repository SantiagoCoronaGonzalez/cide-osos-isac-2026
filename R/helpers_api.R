# ============================================================
# helpers_api.R — CIDE OSOS
# Acceso a la API de StatsBomb y parseo de respuestas a tablas.
#
# Lo usan 01_descarga.R (y se puede usar desde 00_explorar_api.R).
# Credenciales: variables de entorno SB_USER y SB_PASS, idealmente
# en un .Renviron del proyecto (nunca dentro de un script).
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tibble)
})

# OJO: StatsBomb usa DOS hosts. No son intercambiables.
#   services -> competitions, matches, player-stats (temporada y partido),
#               team-stats de temporada
#   data     -> lineups, team-stats de partido
HOST_SERVICES <- "https://data.statsbombservices.com"
HOST_DATA     <- "https://data.statsbomb.com"

sb_credenciales <- function() {
  u <- Sys.getenv("SB_USER"); p <- Sys.getenv("SB_PASS")
  if (u == "" || p == "") {
    stop("Faltan credenciales. Pon SB_USER y SB_PASS en el .Renviron del ",
         "proyecto y reinicia R (Session > Restart R).")
  }
  list(user = u, pass = p)
}

# ── Peticion con reintentos ───────────────────────────────────
# Devuelve el JSON parseado, o NULL si no hay datos / no hay licencia.
# Reintenta ante 429 (limite de peticiones) y errores 5xx con espera
# creciente; no reintenta ante 401/403/404, que no se arreglan solos.
sb_get <- function(path, host = HOST_SERVICES, intentos = 4,
                   verbose = TRUE, flatten = TRUE) {
  cred <- sb_credenciales()
  url  <- paste0(host, path)

  for (k in seq_len(intentos)) {
    r <- tryCatch(
      GET(url, authenticate(cred$user, cred$pass), timeout(120)),
      error = function(e) e
    )

    if (inherits(r, "error")) {
      if (verbose) cat("    red:", conditionMessage(r), "(intento", k, ")\n")
      Sys.sleep(2 ^ k)
      next
    }

    st <- status_code(r)
    if (st == 200) {
      txt <- content(r, as = "text", encoding = "UTF-8")
      if (!nzchar(txt) || identical(trimws(txt), "[]")) return(NULL)
      return(tryCatch(fromJSON(txt, flatten = flatten),
                      error = function(e) {
                        if (verbose) cat("    JSON invalido en", path, "\n")
                        NULL
                      }))
    }
    if (st %in% c(401, 403, 404)) {
      if (verbose) cat("    HTTP", st, "en", path, "\n")
      return(NULL)
    }
    # 429 o 5xx: esperar y reintentar
    if (verbose) cat("    HTTP", st, "en", path, "- reintento", k, "\n")
    Sys.sleep(2 ^ k)
  }
  NULL
}

# ── Utilidades de parseo ──────────────────────────────────────

# Toma la primera columna que exista entre varios nombres posibles
# (los nombres aplanados cambian segun version de la API).
col_de <- function(df, opciones, defecto = NA) {
  for (o in opciones) if (o %in% names(df)) return(df[[o]])
  rep(defecto, nrow(df))
}

# "00:67:30.120" -> 67.502 (minutos de reloj de partido)
ts_a_min <- function(ts) {
  ts <- as.character(ts)
  out <- rep(NA_real_, length(ts))
  ok <- !is.na(ts) & nzchar(ts)
  if (any(ok)) {
    p <- strsplit(ts[ok], ":", fixed = TRUE)
    out[ok] <- vapply(p, function(x) {
      x <- as.numeric(x)
      if (length(x) == 3) x[1] * 60 + x[2] + x[3] / 60 else NA_real_
    }, numeric(1))
  }
  out
}

# Inicio y fin de cada periodo en minutos de reloj. Los timestamps de
# StatsBomb se reinician al empezar cada periodo (2T arranca en 45:00).
INICIO_PERIODO <- c(`1` = 0,  `2` = 45, `3` = 90,  `4` = 105, `5` = 120)
FIN_PERIODO    <- c(`1` = 45, `2` = 90, `3` = 105, `4` = 120, `5` = 120)

# Duracion de una ocupacion de posicion, sumando los periodos que
# atraviesa. El tiempo de descuento se ignora (es ruido de minutos).
duracion_posicion <- function(from, to, from_period, to_period) {
  ini <- ts_a_min(from)
  fin <- ts_a_min(to)
  fp  <- as.integer(from_period)
  fp[is.na(fp)] <- 1L
  # "to" y "to_period" nulos = siguio en la cancha hasta el silbatazo
  # final, que en un partido normal es el final del segundo tiempo.
  tp  <- as.integer(to_period)
  sin_fin <- is.na(tp)
  tp[sin_fin] <- pmax(2L, fp[sin_fin])

  # Sin "to": la ocupacion dura hasta el final de su periodo
  fin <- ifelse(is.na(fin), FIN_PERIODO[as.character(pmin(tp, 5))], fin)
  ini <- ifelse(is.na(ini), INICIO_PERIODO[as.character(pmin(fp, 5))], ini)

  dur <- numeric(length(ini))
  for (i in seq_along(ini)) {
    if (tp[i] <= fp[i]) {
      dur[i] <- max(0, min(fin[i], FIN_PERIODO[as.character(fp[i])]) - ini[i])
    } else {
      d <- max(0, FIN_PERIODO[as.character(fp[i])] - ini[i])
      if (tp[i] - fp[i] > 1) {
        for (q in (fp[i] + 1):(tp[i] - 1))
          d <- d + FIN_PERIODO[as.character(q)] - INICIO_PERIODO[as.character(q)]
      }
      d <- d + max(0, fin[i] - INICIO_PERIODO[as.character(tp[i])])
      dur[i] <- d
    }
  }
  dur
}

# ── Parser de partidos (v6) ───────────────────────────────────
parse_partidos <- function(m) {
  if (is.null(m) || nrow(m) == 0) return(NULL)
  tibble(
    match_id         = as.integer(m$match_id),
    match_date       = as.Date(col_de(m, "match_date")),
    kick_off         = as.character(col_de(m, "kick_off")),
    competition_id   = as.integer(col_de(m, c("competition.competition_id", "competition.id"))),
    season_id        = as.integer(col_de(m, c("season.season_id", "season.id"))),
    season_name      = as.character(col_de(m, c("season.season_name", "season.name"))),
    match_week       = as.integer(col_de(m, "match_week")),
    competition_stage = as.character(col_de(m, c("competition_stage.name", "competition_stage"))),
    home_team_id     = as.integer(col_de(m, c("home_team.home_team_id", "home_team.id"))),
    home_team_name   = as.character(col_de(m, c("home_team.home_team_name", "home_team.name"))),
    away_team_id     = as.integer(col_de(m, c("away_team.away_team_id", "away_team.id"))),
    away_team_name   = as.character(col_de(m, c("away_team.away_team_name", "away_team.name"))),
    home_score       = as.integer(col_de(m, "home_score")),
    away_score       = as.integer(col_de(m, "away_score")),
    match_status     = as.character(col_de(m, "match_status")),
    match_status_360 = as.character(col_de(m, "match_status_360")),
    collection_status = as.character(col_de(m, "collection_status"))
  )
}

# ── Parser de alineaciones (v5) ───────────────────────────────
# Devuelve cuatro tablas:
#   jugadores  — una fila por jugador en la hoja del partido
#                (convocado), con edad, pais, titularidad y si salio
#                lesionado
#   posiciones — una fila por ocupacion de posicion, con duracion
#   formaciones — formacion inicial y cambios
#   tarjetas   — amarillas y rojas por jugador
parse_lineup <- function(lu, match_id) {
  if (is.null(lu) || !is.data.frame(lu) || nrow(lu) == 0) return(NULL)

  jugadores <- list(); posiciones <- list(); formaciones <- list(); tarjetas <- list()

  for (i in seq_len(nrow(lu))) {
    team_id   <- as.integer(lu$team_id[i])
    team_name <- as.character(lu$team_name[i])
    li <- lu$lineup[[i]]

    if (is.data.frame(li) && nrow(li) > 0) {
      n <- nrow(li)
      # stats puede venir anidado o aplanado segun el JSON
      goles <- if ("stats.goals" %in% names(li)) li$stats.goals
               else if ("stats" %in% names(li) && is.data.frame(li$stats)) li$stats$goals
               else rep(NA, n)
      hops  <- col_de(li, c("skills.HOPS.rating"))
      pos_list <- if ("positions" %in% names(li)) li$positions else vector("list", n)

      tiene_pos <- vapply(pos_list, function(p) is.data.frame(p) && nrow(p) > 0, logical(1))

      # Posiciones: se apilan todas y se agregan por jugador (vectorizado)
      if (any(tiene_pos)) {
        idx <- which(tiene_pos)
        p_all <- do.call(rbind, lapply(idx, function(j) {
          p <- pos_list[[j]]
          data.frame(
            player_id    = as.integer(li$player_id[j]),
            position_id  = as.integer(col_de(p, "position_id")),
            position     = as.character(col_de(p, "position")),
            from         = as.character(col_de(p, "from")),
            to           = as.character(col_de(p, "to")),
            from_period  = as.integer(col_de(p, "from_period")),
            to_period    = as.integer(col_de(p, "to_period")),
            start_reason = as.character(col_de(p, "start_reason")),
            end_reason   = as.character(col_de(p, "end_reason")),
            stringsAsFactors = FALSE
          )
        }))
        p_all$dur_min <- duracion_posicion(p_all$from, p_all$to,
                                           p_all$from_period, p_all$to_period)
        posiciones[[length(posiciones) + 1]] <- data.frame(
          match_id = as.integer(match_id), team_id = team_id,
          p_all[, c("player_id", "position_id", "position", "start_reason",
                    "end_reason", "dur_min")])

        flag <- function(campo, valores) {
          hit <- tapply(p_all[[campo]] %in% valores, p_all$player_id, any)
          out <- rep(FALSE, n)
          m <- match(as.integer(li$player_id), as.integer(names(hit)))
          out[!is.na(m)] <- hit[m[!is.na(m)]]
          out
        }
        titular   <- flag("start_reason", "Starting XI")
        entro_les <- flag("start_reason", "Substitution - On (Injury)")
        salio_les <- flag("end_reason", "Substitution - Off (Injury)")
        expulsado <- flag("end_reason", c("Red Card", "Second Yellow Card"))
      } else {
        titular <- entro_les <- salio_les <- expulsado <- rep(FALSE, n)
      }

      jugadores[[length(jugadores) + 1]] <- data.frame(
        match_id     = as.integer(match_id),
        team_id      = team_id,
        team_name    = team_name,
        player_id    = as.integer(li$player_id),
        player_name  = as.character(li$player_name),
        player_nickname = as.character(col_de(li, "player_nickname")),
        birth_date   = as.character(col_de(li, "birth_date")),
        country_name = as.character(col_de(li, c("country.name"))),
        player_height = suppressWarnings(as.numeric(col_de(li, "player_height"))),
        player_weight = suppressWarnings(as.numeric(col_de(li, "player_weight"))),
        jugo         = tiene_pos,
        titular      = titular,
        entro_por_lesion = entro_les,
        salio_lesion = salio_les,
        expulsado    = expulsado,
        goles_lineup = suppressWarnings(as.numeric(goles)),
        hops_rating  = suppressWarnings(as.numeric(hops)),
        stringsAsFactors = FALSE
      )
    }

    fo <- if ("formations" %in% names(lu)) lu$formations[[i]] else NULL
    if (is.data.frame(fo) && nrow(fo) > 0) {
      formaciones[[length(formaciones) + 1]] <- tibble(
        match_id  = as.integer(match_id),
        team_id   = team_id,
        team_name = team_name,
        period    = as.integer(col_de(fo, "period")),
        timestamp = as.character(col_de(fo, "timestamp")),
        reason    = as.character(col_de(fo, "reason")),
        formation = as.character(col_de(fo, "formation"))
      )
    }

    ev <- if ("events" %in% names(lu)) lu$events[[i]] else NULL
    if (is.data.frame(ev) && nrow(ev) > 0) {
      out <- as.character(col_de(ev, "outcome"))
      es_tarjeta <- out %in% c("Yellow Card", "Second Yellow", "Red Card")
      if (any(es_tarjeta)) {
        tarjetas[[length(tarjetas) + 1]] <- tibble(
          match_id  = as.integer(match_id),
          team_id   = team_id,
          player_id = as.integer(col_de(ev, "player_id")[es_tarjeta]),
          tarjeta   = out[es_tarjeta]
        )
      }
    }
  }

  list(
    jugadores   = as_tibble(bind_rows(jugadores)),
    posiciones  = as_tibble(bind_rows(posiciones)),
    formaciones = as_tibble(bind_rows(formaciones)),
    tarjetas    = as_tibble(bind_rows(tarjetas))
  )
}

# ── Parser generico de stats (player-match, team-match, season) ──
# Convierte a tibble y fuerza a numerico todo lo que sea metrica.
parse_stats <- function(x, prefijo) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(NULL)
  df <- as_tibble(x)
  cols_met <- grep(paste0("^", prefijo), names(df), value = TRUE)
  df[cols_met] <- lapply(df[cols_met], function(v) suppressWarnings(as.numeric(v)))
  df
}
