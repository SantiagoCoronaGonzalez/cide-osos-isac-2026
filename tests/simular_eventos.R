# ============================================================
# tests/simular_eventos.R — CIDE OSOS
# Eventos y cuadros 360 FALSOS, coherentes con las alineaciones que
# dejo tests/simular_datos.R (mismos jugadores, minutos y cambios).
# Se generan como JSON con la estructura de Events v11 / 360 v2 y
# pasan por el mismo parser que los reales (helpers_eventos.R).
#
# Senales sembradas, para comprobar que el analisis las encuentra:
#   - cada club tiene una calidad ofensiva y defensiva en corners
#   - unos clubes defienden corners en zona y otros al hombre; los de
#     zona conceden mas al segundo palo
#   - el dominio del partido depende de la habilidad de los que estan
#     en la cancha, asi que los cambios mueven el xG
#
# Uso: Rscript tests/simular_eventos.R   (escribe en data/raw/eventos y frames360)
# NO correr en el proyecto real.
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(jsonlite); library(purrr); library(tibble) })
source("R/helpers_eventos.R")
set.seed(77)

DIR_RAW <- Sys.getenv("OSOS_DIR_RAW", "data/raw")
DESDE <- as.Date(Sys.getenv("OSOS_SIM_DESDE", "2023-07-01"))
for (d in c("eventos", "frames360")) dir.create(file.path(DIR_RAW, d), recursive = TRUE, showWarnings = FALSE)

partidos <- readRDS(file.path(DIR_RAW, "partidos.rds")) %>%
  filter(match_status == "available", match_date >= DESDE) %>% arrange(match_date)
lat <- readRDS(file.path(DIR_RAW, "_habilidad_latente_SIMULADA.rds"))
hab <- setNames(lat$habilidad, lat$player_id)

todos <- readRDS(file.path(DIR_RAW, "partidos.rds"))
clubes <- sort(unique(c(todos$home_team_id, todos$away_team_id)))
sp_of  <- setNames(rnorm(length(clubes), 0, 0.6), clubes)
sp_def <- setNames(rnorm(length(clubes), 0, 0.6), clubes)
zonal  <- setNames(runif(length(clubes)) < 0.5, clubes)
saveRDS(tibble(team_id = clubes, sp_of = sp_of, sp_def = sp_def, zonal = zonal),
        file.path(DIR_RAW, "_balon_parado_SIMULADO.rds"))

NOMBRE_POS <- c(`1` = "Goalkeeper", `2` = "Right Back", `3` = "Right Center Back", `5` = "Left Center Back",
                `6` = "Left Back", `9` = "Right Defensive Midfield", `10` = "Center Defensive Midfield",
                `11` = "Left Defensive Midfield", `13` = "Right Center Midfield", `15` = "Left Center Midfield",
                `17` = "Right Wing", `19` = "Center Attacking Midfield", `21` = "Left Wing",
                `23` = "Center Forward")
nm_pos <- function(p) unname(ifelse(is.na(NOMBRE_POS[as.character(p)]), "Unknown", NOMBRE_POS[as.character(p)]))
peso_tiro <- c(`1` = 0, `2` = .4, `3` = .5, `5` = .5, `6` = .4, `9` = .6, `10` = .6, `11` = .6,
               `13` = 1.2, `15` = 1.2, `17` = 2.5, `19` = 2, `21` = 2.5, `23` = 4)
peso_cabeza <- c(`1` = 0, `2` = .5, `3` = 2.5, `5` = 2.5, `6` = .5, `9` = 1, `10` = 1, `11` = 1,
                 `13` = .8, `15` = .8, `17` = .4, `19` = .6, `21` = .4, `23` = 3)

xg_de <- function(x, y, cabeza = FALSE) {
  d <- sqrt((120 - x)^2 + (40 - y)^2)
  ang <- abs(atan2(7.32 / 2 * 1.09, d))
  p <- plogis(1.2 - 0.16 * d + 2.2 * ang - ifelse(cabeza, 0.9, 0))
  round(pmin(pmax(p, 0.01), 0.85), 4)
}
ts_de <- function(t) { m <- floor(t); s <- floor((t - m) * 60); sprintf("00:%02d:%02d.000", m %% 45, s) }

uid <- local({ k <- 0L; function() { k <<- k + 1L; sprintf("ev-%08d", k) } })

simular_partido <- function(mid, home, away) {
  lu <- readRDS(file.path(DIR_RAW, "lineups", paste0(mid, ".rds")))
  pos <- lu$posiciones
  # Intervalo en la cancha de cada jugador
  en_cancha <- pos %>%
    mutate(ini = ifelse(start_reason == "Starting XI", 0, 90 - dur_min),
           fin = ifelse(start_reason == "Starting XI", pmin(dur_min, 95), 95)) %>%
    select(team_id, player_id, position_id, ini, fin, start_reason, end_reason)
  jugando <- function(team, t) en_cancha[en_cancha$team_id == team & en_cancha$ini <= t & en_cancha$fin > t, ]
  fuerza <- function(team, t) { j <- jugando(team, t); mean(hab[as.character(j$player_id)], na.rm = TRUE) }

  evs <- list(); frames <- list(); idx <- 0L
  nuevo <- function(e) { idx <<- idx + 1L; e$index <- idx; evs[[length(evs) + 1]] <<- e; e$id }
  base_ev <- function(tipo, team, t, posesion, pt, patron, jugador = NULL) {
    e <- list(id = uid(), period = ifelse(t < 45, 1L, 2L), timestamp = ts_de(t),
              minute = as.integer(floor(t)), second = as.integer(floor((t %% 1) * 60)),
              type = list(id = 0L, name = tipo), possession = posesion,
              possession_team = list(id = pt, name = as.character(pt)),
              play_pattern = list(id = 1L, name = patron),
              team = list(id = team, name = as.character(team)))
    if (!is.null(jugador)) {
      e$player <- list(id = jugador$player_id, name = paste("Jugador", jugador$player_id))
      e$position <- list(id = jugador$position_id, name = nm_pos(jugador$position_id))
    }
    e
  }
  elegir <- function(j, pesos) { w <- pesos[as.character(j$position_id)]; w[is.na(w)] <- 0.5
    if (sum(w) == 0) w[] <- 1; j[sample(nrow(j), 1, prob = w), ] }

  # Starting XI
  for (tm in c(home, away)) {
    st <- en_cancha %>% filter(team_id == tm, start_reason == "Starting XI")
    e <- base_ev("Starting XI", tm, 0, 1L, home, "Regular Play")
    e$tactics <- list(formation = 433L, lineup = lapply(seq_len(nrow(st)), function(i)
      list(player = list(id = st$player_id[i], name = paste("Jugador", st$player_id[i])),
           position = list(id = st$position_id[i], name = nm_pos(st$position_id[i])), jersey_number = i)))
    nuevo(e)
  }
  # Sustituciones (sale -> entra, mismo puesto)
  sale <- en_cancha %>% filter(grepl("Substitution - Off", end_reason))
  entra <- en_cancha %>% filter(grepl("Substitution - On", start_reason))
  subs <- list()
  for (i in seq_len(nrow(sale))) {
    s <- sale[i, ]
    r <- entra %>% filter(team_id == s$team_id, abs(ini - s$fin) < 0.5) %>% slice(1)
    if (nrow(r) == 0) r <- entra %>% filter(team_id == s$team_id, position_id == s$position_id) %>% slice(1)
    if (nrow(r) == 0) next
    subs[[length(subs) + 1]] <- list(t = s$fin, s = s, r = r)
  }
  # Cambio tactico ocasional
  shift_t <- setNames(ifelse(runif(2) < 0.15, runif(2, 55, 75), NA), c(home, away))

  # Posesiones
  n_pos <- 130
  tiempos <- sort(runif(n_pos, 0.5, 94))
  pend_subs <- subs[order(vapply(subs, `[[`, numeric(1), "t"))]
  for (k in seq_len(n_pos)) {
    t <- tiempos[k]
    while (length(pend_subs) > 0 && pend_subs[[1]]$t <= t) {
      sb <- pend_subs[[1]]; pend_subs <- pend_subs[-1]
      e <- base_ev("Substitution", sb$s$team_id, sb$t, k, home, "Regular Play", sb$s)
      e$substitution <- list(outcome = list(id = 103L, name = ifelse(grepl("Injury", sb$s$end_reason), "Injury", "Tactical")),
                             replacement = list(id = sb$r$player_id, name = paste("Jugador", sb$r$player_id)))
      nuevo(e)
    }
    for (tm in names(shift_t)) if (!is.na(shift_t[[tm]]) && shift_t[[tm]] <= t) {
      j <- jugando(as.integer(tm), shift_t[[tm]])
      e <- base_ev("Tactical Shift", as.integer(tm), shift_t[[tm]], k, home, "Regular Play")
      pid <- j$position_id; pid[pid == 13] <- 9L; pid[pid == 15] <- 11L; pid[pid == 10] <- 19L
      e$tactics <- list(formation = 4231L, lineup = lapply(seq_len(nrow(j)), function(i)
        list(player = list(id = j$player_id[i], name = "x"), position = list(id = pid[i], name = nm_pos(pid[i])),
             jersey_number = i)))
      nuevo(e); shift_t[[tm]] <- NA
    }
    fh <- fuerza(home, t); fa <- fuerza(away, t)
    at <- if (runif(1) < plogis(0.15 + 0.5 * (fh - fa))) home else away
    df <- if (at == home) away else home
    dif <- if (at == home) fh - fa else fa - fh
    ja <- jugando(at, t); jd <- jugando(df, t)
    if (nrow(ja) < 7 || nrow(jd) < 7) next
    patron <- sample(c("Regular Play", "From Throw In", "From Free Kick", "From Corner", "From Goal Kick"),
                     1, prob = c(.74, .11, .06, .05, .04))
    tt <- t

    tiro <- function(x, y, cabeza, tipo_tiro, xg_mult = 1) {
      sh <- elegir(ja, if (cabeza) peso_cabeza else peso_tiro)
      xg <- round(min(0.9, xg_de(x, y, cabeza) * xg_mult), 4)
      gol <- runif(1) < xg
      e <- base_ev("Shot", at, tt, k, at, patron, sh)
      e$location <- c(x, y)
      e$obv_total_net <- round(xg - 0.03, 4); e$obv_for_net <- e$obv_total_net; e$obv_against_net <- 0
      gk <- jd[jd$position_id == 1, ]; de <- jd[jd$position_id != 1, ]
      de <- de[sample(nrow(de), min(6, nrow(de))), ]
      tm8 <- ja[ja$player_id != sh$player_id & ja$position_id != 1, ]
      tm8 <- tm8[sample(nrow(tm8), min(3, nrow(tm8))), ]
      ff <- c(lapply(seq_len(nrow(gk)), function(i) list(location = c(119, 40), player = list(id = gk$player_id[i], name = "x"),
                                                         position = list(id = 1L, name = "Goalkeeper"), teammate = FALSE)),
              lapply(seq_len(nrow(de)), function(i) list(location = c(runif(1, x, 119), y + rnorm(1, 0, 4)),
                                                         player = list(id = de$player_id[i], name = "x"),
                                                         position = list(id = de$position_id[i], name = nm_pos(de$position_id[i])),
                                                         teammate = FALSE)),
              lapply(seq_len(nrow(tm8)), function(i) list(location = c(runif(1, x - 8, 118), runif(1, 25, 55)),
                                                          player = list(id = tm8$player_id[i], name = "x"),
                                                          position = list(id = tm8$position_id[i], name = nm_pos(tm8$position_id[i])),
                                                          teammate = TRUE)))
      e$shot <- list(statsbomb_xg = xg, end_location = c(120, 40 + rnorm(1, 0, 3), runif(1, 0, 2.5)),
                     outcome = list(id = 0L, name = if (gol) "Goal" else sample(c("Saved", "Off T", "Blocked"), 1)),
                     type = list(id = 0L, name = tipo_tiro),
                     body_part = list(id = 0L, name = if (cabeza) "Head" else "Right Foot"),
                     freeze_frame = ff)
      nuevo(e)
    }
    presion <- function(x, y) {
      pr <- elegir(jd, c(`23` = 2, `17` = 2, `21` = 2, `13` = 1.5, `15` = 1.5, `10` = 1))
      e <- base_ev("Pressure", df, tt, k, at, patron, pr)
      e$location <- c(round(120 - x, 1), round(80 - y, 1)); e$counterpress <- runif(1) < 0.2
      nuevo(e)
    }
    pase <- function(x, y, ex, ey, tipo = NULL, tecnica = NULL, alto = FALSE, ok = TRUE, quien = NULL, asist = FALSE) {
      p <- if (is.null(quien)) elegir(ja, c(`1` = .3)) else quien
      rec <- ja[ja$player_id != p$player_id, ][1, ]
      e <- base_ev("Pass", at, tt, k, at, patron, p)
      e$location <- c(round(x, 1), round(y, 1))
      e$obv_total_net <- round(rnorm(1, 0.001 + 0.0009 * (ex - x) / 10, 0.006), 4)
      e$obv_for_net <- max(e$obv_total_net, 0); e$obv_against_net <- min(e$obv_total_net, 0)
      e$pass <- list(recipient = list(id = rec$player_id, name = "x"), end_location = c(round(ex, 1), round(ey, 1)),
                     height = list(id = 0L, name = if (alto) "High Pass" else "Ground Pass"))
      if (!is.null(tipo)) e$pass$type <- list(id = 0L, name = tipo)
      if (!is.null(tecnica)) e$pass$technique <- list(id = 0L, name = tecnica)
      if (!ok) e$pass$outcome <- list(id = 9L, name = "Incomplete")
      if (asist) e$pass$shot_assist <- TRUE
      nuevo(e)
    }
    frame360 <- function(ev_id, ex, ey) {
      # Atacantes en el area, defensores en zona o al hombre, portero
      n_at <- 5
      atq <- cbind(runif(n_at, 104, 116), runif(n_at, 28, 52))
      dz <- if (zonal[[as.character(df)]])
        cbind(c(114, 114, 114, 110, 110, 106, 117) + rnorm(7, 0, .6), c(34, 40, 46, 37, 43, 40, 37) + rnorm(7, 0, .6))
      else cbind(atq[c(1:5, 1, 2), 1] + rnorm(7, 0.8, .4), atq[c(1:5, 1, 2), 2] + rnorm(7, 0, .5))
      ff <- c(list(list(location = c(ex0, ey0), teammate = TRUE, actor = TRUE, keeper = FALSE)),
              lapply(seq_len(n_at), function(i) list(location = atq[i, ], teammate = TRUE, actor = FALSE, keeper = FALSE)),
              lapply(seq_len(nrow(dz)), function(i) list(location = dz[i, ], teammate = FALSE, actor = FALSE, keeper = FALSE)),
              list(list(location = c(118.5, 40), teammate = FALSE, actor = FALSE, keeper = TRUE)))
      frames[[length(frames) + 1]] <<- list(event_uuid = ev_id, visible_area = c(80, 0, 120, 0, 120, 80, 80, 80, 80, 0),
                                            freeze_frame = ff)
    }

    if (patron == "From Corner") {
      lado <- sample(c(0.1, 79.9), 1); ex0 <- 120; ey0 <- lado
      tec <- sample(c("Inswinging", "Outswinging", "Straight"), 1, prob = c(.5, .35, .15))
      corto <- runif(1) < 0.2
      ex <- if (corto) runif(1, 100, 110) else runif(1, 103, 117)
      ey <- if (corto) ifelse(lado < 40, runif(1, 5, 18), runif(1, 62, 75)) else runif(1, 28, 52)
      lejano <- (lado < 40 & ey > 44) | (lado > 40 & ey < 36)
      toma <- elegir(ja, c(`17` = 3, `21` = 3, `13` = 1, `15` = 1, `19` = 2))
      p_tiro <- plogis(-1.4 + 0.6 * sp_of[[as.character(at)]] - 0.5 * sp_def[[as.character(df)]] +
                         ifelse(zonal[[as.character(df)]] & lejano, 0.7, 0) - ifelse(corto, 0.8, 0))
      hay_tiro <- runif(1) < p_tiro
      id_c <- pase(120, lado, ex, ey, tipo = "Corner", tecnica = tec, alto = !corto, quien = toma, asist = hay_tiro)
      # Responsabilidad defensiva
      evs[[length(evs)]]$defensive_responsibility <- list(
        categories = list("interception_like"),
        players = lapply(seq_len(min(4, nrow(jd))), function(i)
          list(player_id = jd$player_id[i], probability = round(runif(1, 0.05, 0.5), 3),
               pressure_like_probability = 0.01, interception_like_probability = 0.1)))
      if (runif(1) < 0.8) frame360(id_c, ex, ey)
      if (hay_tiro) tiro(ex + rnorm(1, 0, 1), ey + rnorm(1, 0, 1), cabeza = !corto && runif(1) < 0.7, "Open Play",
                         xg_mult = exp(0.25 * sp_of[[as.character(at)]]))
      else {
        dd <- elegir(jd, peso_cabeza)
        e <- base_ev("Clearance", df, tt, k, at, patron, dd); e$location <- c(120 - ex, 80 - ey)
        e$clearance <- list(aerial_won = !corto); nuevo(e)
      }
      next
    }
    if (patron == "From Free Kick") {
      x0 <- runif(1, 35, 100); y0 <- runif(1, 5, 75); ex0 <- x0; ey0 <- y0
      if (x0 > 78 && runif(1) < 0.7) {
        if (x0 > 88 && abs(y0 - 40) < 20 && runif(1) < 0.3) { tiro(x0, y0, FALSE, "Free Kick", 0.8); next }
        ex <- runif(1, 102, 116); ey <- runif(1, 28, 52)
        p_tiro <- plogis(-1.7 + 0.4 * sp_of[[as.character(at)]] - 0.4 * sp_def[[as.character(df)]])
        hay <- runif(1) < p_tiro
        id_f <- pase(x0, y0, ex, ey, tipo = "Free Kick", alto = TRUE, asist = hay)
        if (runif(1) < 0.8) frame360(id_f, ex, ey)
        if (hay) tiro(ex, ey, runif(1) < 0.6, "Open Play")
        next
      }
      pase(x0, y0, x0 + runif(1, 0, 25), runif(1, 5, 75), tipo = "Free Kick")
      next
    }
    if (patron == "From Throw In") {
      x0 <- runif(1, 10, 115); y0 <- sample(c(0.1, 79.9), 1); ex0 <- x0; ey0 <- y0
      ex <- min(119, x0 + rnorm(1, 3, 6)); ey <- ifelse(y0 < 40, runif(1, 3, 20), runif(1, 60, 77))
      id_t <- pase(x0, y0, ex, ey, tipo = "Throw-in")
      if (x0 >= 80) {
        if (runif(1) < 0.6) frame360(id_t, ex, ey)
        if (runif(1) < plogis(-2.4 + 0.3 * sp_of[[as.character(at)]])) {
          pase(ex, ey, runif(1, 104, 115), runif(1, 30, 50), alto = runif(1) < 0.5, asist = TRUE)
          tiro(runif(1, 104, 115), runif(1, 30, 50), runif(1) < 0.4, "Open Play")
        }
      }
      next
    }
    # Juego abierto (y saque de meta)
    x <- if (patron == "From Goal Kick") 6 else runif(1, 15, 60); y <- runif(1, 10, 70)
    n_p <- 1 + rpois(1, 3)
    for (q in seq_len(n_p)) {
      ex <- min(118, x + rnorm(1, 8 + 3 * dif, 10)); ey <- min(78, max(2, y + rnorm(1, 0, 15)))
      if (runif(1) < 0.45) presion(x, y)
      ult <- q == n_p
      llega <- ult && ex > 95 && runif(1) < plogis(-0.6 + 0.6 * dif)
      ok <- !ult || llega
      pase(x, y, ex, ey, tipo = if (q == 1 && patron == "From Goal Kick") "Goal Kick" else NULL, ok = ok, asist = llega)
      if (runif(1) < 0.3) {
        car <- elegir(ja, c(`17` = 2, `21` = 2, `2` = 1.5, `6` = 1.5))
        e <- base_ev("Carry", at, tt, k, at, patron, car); e$location <- c(ex, ey)
        cx <- min(118, ex + abs(rnorm(1, 4, 4))); e$carry <- list(end_location = c(cx, ey)); nuevo(e); ex <- cx
      }
      x <- ex; y <- ey; tt <- tt + 0.05
    }
    if (x > 95) tiro(min(117, x), y, runif(1) < 0.15, "Open Play")
  }
  list(ev = toJSON(evs, auto_unbox = TRUE, digits = 6, null = "null"),
       fr = toJSON(frames, auto_unbox = TRUE, digits = 6))
}

cat("Simulando eventos de", nrow(partidos), "partidos (desde", format(DESDE), ")...\n")
t0 <- Sys.time()
for (i in seq_len(nrow(partidos))) {
  m <- partidos[i, ]
  js <- simular_partido(m$match_id, m$home_team_id, m$away_team_id)
  ev <- fromJSON(js$ev, flatten = TRUE)
  p <- parse_eventos(ev, m$match_id)
  saveRDS(p, file.path(DIR_RAW, "eventos", paste0(m$match_id, ".rds")))
  fr <- fromJSON(js$fr, flatten = TRUE)
  f <- parse_360(fr, ids_balon_parado(p$eventos), m$match_id)
  saveRDS(if (is.null(f)) tibble() else f, file.path(DIR_RAW, "frames360", paste0(m$match_id, ".rds")))
  if (i %% 100 == 0) cat("  ", i, "/", nrow(partidos), round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
}
cat("Listo.\n")
