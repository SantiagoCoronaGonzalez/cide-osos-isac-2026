# ============================================================
# tests/simular_datos.R — CIDE OSOS
# Genera datos FALSOS con la misma estructura que deja
# 01_descarga.R en data/raw/, para probar el pipeline, la
# validacion y la app sin tocar la API.
#
# Las alineaciones se generan como JSON identico al de la API v5 y
# pasan por el mismo parser (parse_lineup), asi que tambien prueban
# esa parte.
#
# Hay una habilidad latente por jugador que mueve sus metricas y sus
# minutos. Si el modelo funciona, el backtest debe detectarla.
#
# Uso: Rscript tests/simular_datos.R  (escribe en data/raw/)
# NO correr en el proyecto real: sobrescribe data/raw/.
# ============================================================

source("R/helpers_api.R")
set.seed(2026)

DIR_RAW <- Sys.getenv("OSOS_DIR_RAW", "data/raw")
for (s in c("temporada", "lineups", "player_match", "team_match"))
  dir.create(file.path(DIR_RAW, s), recursive = TRUE, showWarnings = FALSE)

clubes <- c("América", "Cruz Azul", "Toluca", "Tigres UANL", "Monterrey", "Guadalajara",
            "Pachuca", "León", "Pumas UNAM", "Santos Laguna", "Necaxa", "Atlas",
            "Atlético San Luis", "Tijuana", "Puebla", "Juárez", "Querétaro", "Mazatlán")
team_ids <- setNames(seq_along(clubes) + 100L, clubes)

TEMP_ID <- c("2021/2022" = 108L, "2022/2023" = 235L, "2023/2024" = 281L,
             "2024/2025" = 317L, "2025/2026" = 318L, "2026/2027" = 351L)

torneos <- tibble(nombre = c("Apertura 2021", "Clausura 2022", "Apertura 2022", "Clausura 2023",
                             "Apertura 2023", "Clausura 2024", "Apertura 2024", "Clausura 2025",
                             "Apertura 2025", "Clausura 2026", "Apertura 2026")) %>%
  mutate(tipo = sub(" .*", "", nombre), anio = as.integer(sub(".* ", "", nombre)),
         temporada = ifelse(tipo == "Apertura", paste0(anio, "/", anio + 1),
                            paste0(anio - 1, "/", anio)),
         inicio = as.Date(ifelse(tipo == "Apertura", paste0(anio, "-07-12"),
                                 paste0(anio, "-01-08"))),
         jornadas = ifelse(nombre == "Apertura 2026", 10L, 17L))

# ── Jugadores ─────────────────────────────────────────────────
# Puestos del 4-3-3: GK RB RCB LCB LB CDM RCM LCM RW ST LW
PUESTOS <- c(1, 2, 3, 5, 6, 10, 13, 15, 17, 23, 21)
GRUPO_PUESTO <- c(`1` = "GK", `2` = "FB", `3` = "CB", `5` = "CB", `6` = "FB", `10` = "DM",
                  `13` = "CM", `15` = "CM", `17` = "W", `23` = "ST", `21` = "W")
paises <- c("Mexico", "Mexico", "Mexico", "Mexico", "Argentina", "Colombia", "Uruguay",
            "Chile", "Paraguay", "Brasil", "Ecuador")

sig_id <- 5000L
nuevo_jugador <- function(n, club, edad_min = 18, edad_max = 31, fecha = as.Date("2021-07-01"),
                          extranjero = FALSE, puesto = NULL) {
  ids <- sig_id + seq_len(n); sig_id <<- sig_id + n
  edad <- runif(n, edad_min, edad_max)
  tibble(
    player_id = ids,
    player_name = paste("Jugador", ids),
    birth_date = fecha - round(edad * 365.25),
    country = if (extranjero) sample(paises[-1], n, TRUE) else sample(paises, n, TRUE,
                                                     prob = c(rep(.2, 4), rep(.2 / 7, 7))),
    puesto = if (is.null(puesto)) sample(PUESTOS, n, TRUE,
                                         prob = c(.06, rep(.1, 4), .1, .1, .1, .08, .08, .08))
             else puesto,
    habilidad = rnorm(n, 0, 1) + ifelse(extranjero, 0.3, 0),
    club = club,
    lesion_hasta = 0L
  )
}

plantilla <- bind_rows(lapply(clubes, function(cl) {
  # Garantiza al menos dos por puesto
  bind_rows(nuevo_jugador(length(PUESTOS) * 2, cl, puesto = rep(PUESTOS, 2)),
            nuevo_jugador(4, cl))
}))
# Clubes mas ricos tienen mejores jugadores
fuerza_base <- setNames(seq(0.6, -0.6, length.out = length(clubes)), clubes)
plantilla$habilidad <- plantilla$habilidad + fuerza_base[plantilla$club]

# ── Utilidades de JSON de alineacion ──────────────────────────
ts <- function(min) sprintf("00:%02d:%02d.000", floor(min), round((min %% 1) * 60))

pos_obj <- function(pid, from, to, fp, tp, sr, er) {
  list(position_id = pid, position = paste("Pos", pid), from = ts(from),
       to = if (is.na(to)) NA else ts(to), from_period = fp,
       to_period = if (is.na(tp)) NA else tp, start_reason = sr, end_reason = er,
       counterpart_id = NA, counterpart_name = NA)
}

partidos <- list()
jornada_global <- 0L

for (ti in seq_len(nrow(torneos))) {
  tor <- torneos[ti, ]
  cat("Simulando", tor$nombre, "\n")

  # ── Mercado antes del torneo ────────────────────────────────
  if (ti > 1) {
    # envejecimiento y desarrollo
    edad_hoy <- as.numeric(tor$inicio - plantilla$birth_date) / 365.25
    plantilla$habilidad <- plantilla$habilidad +
      ifelse(edad_hoy < 24, 0.12, ifelse(edad_hoy > 30, -0.10, 0)) + rnorm(nrow(plantilla), 0, 0.12)
    # retiros
    plantilla <- plantilla[edad_hoy < 35.5, ]
    # traspasos dentro de la liga: ~3 por club
    mov <- sample(seq_len(nrow(plantilla)), 3 * length(clubes))
    for (i in mov) {
      # los buenos tienden a ir a clubes fuertes
      p <- exp(1.2 * plantilla$habilidad[i] * fuerza_base)
      plantilla$club[i] <- sample(clubes, 1, prob = p / sum(p))
    }
    # llegadas del extranjero y canteranos
    for (cl in clubes) {
      plantilla <- bind_rows(plantilla,
                             nuevo_jugador(1, cl, 22, 29, tor$inicio, extranjero = TRUE),
                             nuevo_jugador(1, cl, 17, 19, tor$inicio) %>%
                               mutate(habilidad = habilidad - 0.8))
    }
    # que ningun club se quede sin dos jugadores por puesto
    for (cl in clubes) for (pz in PUESTOS) {
      n <- sum(plantilla$club == cl & plantilla$puesto == pz)
      if (n < 2) plantilla <- bind_rows(plantilla, nuevo_jugador(2 - n, cl, 20, 28, tor$inicio,
                                                                 puesto = rep(pz, 2 - n)))
    }
  }

  # ── Calendario round robin ──────────────────────────────────
  eq <- clubes
  for (j in seq_len(tor$jornadas)) {
    jornada_global <- jornada_global + 1L
    fecha <- tor$inicio + 7 * (j - 1)
    rot <- c(eq[1], eq[-1][((seq_along(eq[-1]) + j - 2) %% (length(eq) - 1)) + 1])
    pares <- cbind(rot[1:9], rev(rot)[1:9])

    for (k in seq_len(nrow(pares))) {
      mid <- 3000000L + length(partidos) + 1L
      local_cl <- pares[k, 1]; visita_cl <- pares[k, 2]
      fuerza <- sapply(c(local_cl, visita_cl), function(cl) {
        h <- plantilla$habilidad[plantilla$club == cl]
        mean(sort(h, decreasing = TRUE)[1:11])
      })

      lu_json <- list(); pm_rows <- list(); tm_rows <- list(); xg_eq <- c(0, 0)

      for (lado in 1:2) {
        cl <- c(local_cl, visita_cl)[lado]
        dif <- fuerza[lado] - fuerza[3 - lado]
        disp <- which(plantilla$club == cl & plantilla$lesion_hasta < jornada_global)

        # Titulares: el mejor disponible por puesto (con ruido)
        usados <- integer(0); titulares <- integer(0)
        for (pz in PUESTOS) {
          cand <- setdiff(disp[plantilla$puesto[disp] == pz], usados)
          if (length(cand) == 0) cand <- setdiff(disp[GRUPO_PUESTO[as.character(plantilla$puesto[disp])] ==
                                                        GRUPO_PUESTO[as.character(pz)]], usados)
          if (length(cand) == 0) cand <- setdiff(disp[plantilla$puesto[disp] != 1], usados)
          sel <- cand[which.max(plantilla$habilidad[cand] + rnorm(length(cand), 0, 0.5))]
          titulares <- c(titulares, sel); usados <- c(usados, sel)
        }
        banca <- setdiff(disp, usados)
        banca <- banca[order(-plantilla$habilidad[banca])][seq_len(min(9, length(banca)))]

        # Tres cambios; 3% de lesion por titular
        lineup <- list(); minutos <- setNames(numeric(0), character(0))
        cambios <- sample(2:11, 3)
        min_cambio <- sort(runif(3, 55, 85))
        entra <- banca[seq_len(min(3, length(banca)))]

        for (s in seq_along(titulares)) {
          i <- titulares[s]; pz <- PUESTOS[s]
          lesion <- runif(1) < 0.03
          c_idx <- match(s, cambios)
          if (lesion) {
            m_sal <- runif(1, 10, 85)
            plantilla$lesion_hasta[i] <- jornada_global + sample(1:6, 1)
            pos <- list(pos_obj(pz, 0, m_sal, 1L, ifelse(m_sal > 45, 2L, 1L),
                                "Starting XI", "Substitution - Off (Injury)"))
            mins <- m_sal
          } else if (!is.na(c_idx) && c_idx <= length(entra)) {
            m_sal <- min_cambio[c_idx]
            pos <- list(pos_obj(pz, 0, m_sal, 1L, 2L, "Starting XI", "Substitution - Off (Tactical)"))
            mins <- m_sal
          } else {
            pos <- list(pos_obj(pz, 0, NA, 1L, NA, "Starting XI", "Final Whistle"))
            mins <- 90
          }
          lineup[[length(lineup) + 1]] <- list(i = i, pos = pos, min = mins)
        }
        for (e in seq_along(entra)) {
          i <- entra[e]
          pz <- PUESTOS[cambios[e]]
          m_ent <- min_cambio[e]
          pos <- list(pos_obj(pz, m_ent, NA, 2L, NA, "Substitution - On (Tactical)", "Final Whistle"))
          lineup[[length(lineup) + 1]] <- list(i = i, pos = pos, min = 90 - m_ent)
        }
        for (i in setdiff(banca, entra)) lineup[[length(lineup) + 1]] <- list(i = i, pos = list(), min = 0)

        # JSON del lineup
        tarj <- list()
        lu_players <- lapply(lineup, function(L) {
          p <- plantilla[L$i, ]
          if (L$min > 0 && runif(1) < 0.12)
            tarj[[length(tarj) + 1]] <<- list(player_id = p$player_id, player_name = p$player_name,
                                               period = 2L, timestamp = "00:70:00.000",
                                               type = "Foul Committed", outcome = "Yellow Card")
          list(player_id = p$player_id, player_name = p$player_name, player_nickname = NA,
               birth_date = format(p$birth_date), player_gender = "male",
               player_height = 178, player_weight = 74, jersey_number = 10L,
               country = list(id = 1L, name = p$country),
               positions = L$pos,
               stats = list(goals = 0L, assists = 0L, own_goals = 0L,
                            penalties_missed = 0L, penalties_saved = 0L, penalties_scored = 0L),
               skills = list(HOPS = list(rating = round(plogis(p$habilidad), 3), raw_rating = 400)))
        })
        lu_json[[lado]] <- list(team_id = team_ids[[cl]], team_name = cl, lineup = lu_players,
                                formations = list(list(period = 1L, timestamp = "00:00:00.000",
                                                       reason = "Starting XI", formation = 433L)),
                                events = tarj)

        # Stats jugador-partido
        for (L in lineup) {
          if (L$min <= 0) next
          p <- plantilla[L$i, ]
          g <- GRUPO_PUESTO[as.character(p$puesto)]
          f <- L$min / 90
          a <- p$habilidad
          esc <- exp(0.35 * a + 0.15 * dif)
          base_of <- switch(g, ST = 3, W = 2.2, CM = 1.2, DM = 0.6, FB = 0.6, CB = 0.4, GK = 0)
          base_def <- switch(g, ST = 8, W = 10, CM = 14, DM = 18, FB = 14, CB = 12, GK = 0)
          base_pas <- switch(g, ST = 22, W = 30, CM = 50, DM = 55, FB = 42, CB = 48, GK = 25)
          shots <- rpois(1, base_of * f * esc)
          xg <- sum(rbeta(shots, 1.2, 9) * exp(0.15 * a))
          goals <- rbinom(1, shots, min(0.9, 0.11 * exp(0.25 * a)))
          xg_eq[lado] <- xg_eq[lado] + xg
          pm_rows[[length(pm_rows) + 1]] <- list(
            player_id = p$player_id, player_name = p$player_name,
            team_id = team_ids[[cl]], team_name = cl, match_id = mid,
            player_match_minutes = round(L$min),
            player_match_360_minutes = round(L$min),
            player_match_np_xg = xg, player_match_np_shots = shots,
            player_match_np_goals = goals, player_match_goals = goals,
            player_match_touches_inside_box = rpois(1, base_of * 1.5 * f * esc),
            player_match_xa = rgamma(1, 1 + base_of * f * esc, 12),
            player_match_key_passes = rpois(1, 0.6 * base_of * f * esc),
            player_match_passes_into_box = rpois(1, 0.5 * base_of * f * esc),
            player_match_op_f3_passes = rpois(1, 0.25 * base_pas * f * esc),
            player_match_assists = rbinom(1, 2, 0.06 * f * esc),
            player_match_fouls_won = rpois(1, 1.2 * f),
            player_match_through_balls = rpois(1, 0.2 * f * esc),
            player_match_pressures = rpois(1, base_def * f * esc * exp(-0.1 * dif)),
            player_match_pressure_regains = rpois(1, 0.25 * base_def * f * esc),
            player_match_interceptions = rpois(1, 0.1 * base_def * f * esc),
            player_match_tackles = rpois(1, 0.1 * base_def * f * esc),
            player_match_fhalf_ball_recoveries = rpois(1, 0.08 * base_def * f * esc),
            player_match_ball_recoveries = rpois(1, 0.25 * base_def * f * esc),
            player_match_defensive_actions_above_expectation = rnorm(1, 0.6 * a * f, 1),
            player_match_fouls = rpois(1, 1.1 * f / exp(0.2 * a)),
            player_match_dribbled_past = rpois(1, 0.6 * f / exp(0.3 * a)),
            player_match_deep_progressions = rpois(1, 0.08 * base_pas * f * esc),
            player_match_forward_passes = rpois(1, 0.4 * base_pas * f * esc),
            player_match_passes = rpois(1, base_pas * f * esc * exp(0.2 * dif)),
            player_match_successful_long_balls = rpois(1, 0.05 * base_pas * f * esc),
            player_match_dribbles = rpois(1, 0.3 * base_of * f * esc),
            player_match_lbp_completed = rpois(1, 0.06 * base_pas * f * esc),
            player_match_turnovers = rpois(1, 1.3 * f / exp(0.3 * a)),
            player_match_dispossessions = rpois(1, 0.9 * f / exp(0.3 * a)),
            player_match_touches = rpois(1, 1.3 * base_pas * f * esc),
            player_match_obv = rnorm(1, 0.08 * a * f, 0.15 * sqrt(f)),
            player_match_obv_pass = rnorm(1, 0.04 * a * f, 0.08),
            player_match_obv_dribble_carry = rnorm(1, 0.02 * a * f, 0.05),
            player_match_obv_defensive_action = rnorm(1, 0.02 * a * f, 0.05),
            player_match_obv_shot = rnorm(1, 0.02 * a * f, 0.05),
            player_match_passing_ratio = plogis(1.4 + 0.3 * a + rnorm(1, 0, 0.3)),
            player_match_aerial_ratio = plogis(0.2 * a + rnorm(1, 0, 0.6))
          )
        }
      }

      # Stats equipo-partido
      d1 <- fuerza[1] - fuerza[2]
      pos1 <- min(75, max(25, 50 + 9 * d1 + rnorm(1, 0, 5)))
      tm <- tibble(
        account_id = 1L, match_id = mid,
        team_id = team_ids[c(local_cl, visita_cl)], team_name = c(local_cl, visita_cl),
        opposition_id = team_ids[c(visita_cl, local_cl)],
        opposition_name = c(visita_cl, local_cl),
        competition_id = 73L, competition_name = "Liga MX",
        team_match_possession = c(pos1, 100 - pos1) / 100,
        team_match_ppda = pmax(4, c(10 - 2 * d1, 10 + 2 * d1) + rnorm(2, 0, 1.5)),
        team_match_np_xg = xg_eq, team_match_np_xg_conceded = rev(xg_eq),
        team_match_np_xgd = xg_eq - rev(xg_eq),
        team_match_directness = runif(2, 0.8, 0.92)
      )

      # Guardar como lo hace 01_descarga.R
      txt <- jsonlite::toJSON(lu_json, auto_unbox = TRUE, na = "null", null = "null")
      lu <- jsonlite::fromJSON(txt, flatten = TRUE)
      saveRDS(parse_lineup(lu, mid), file.path(DIR_RAW, "lineups", paste0(mid, ".rds")))
      saveRDS(parse_stats(bind_rows(pm_rows), "player_match_"),
              file.path(DIR_RAW, "player_match", paste0(mid, ".rds")))
      saveRDS(parse_stats(tm, "team_match_"), file.path(DIR_RAW, "team_match", paste0(mid, ".rds")))

      partidos[[length(partidos) + 1]] <- data.frame(
        match_id = mid, match_date = format(fecha), kick_off = "19:00:00.000",
        competition.competition_id = 73L, competition.competition_name = "Liga MX",
        season.season_id = TEMP_ID[[tor$temporada]], season.season_name = tor$temporada,
        match_week = j, competition_stage.name = "Regular Season",
        home_team.home_team_id = team_ids[[local_cl]], home_team.home_team_name = local_cl,
        away_team.away_team_id = team_ids[[visita_cl]], away_team.away_team_name = visita_cl,
        home_score = 1L, away_score = 1L, match_status = "available",
        match_status_360 = "available", collection_status = "Complete",
        check.names = FALSE
      )
    }
  }
}

partidos_df <- parse_partidos(bind_rows(partidos))
partidos_df$competition_name <- "Liga MX"
saveRDS(partidos_df, file.path(DIR_RAW, "partidos.rds"))
saveRDS(plantilla, file.path(DIR_RAW, "_habilidad_latente_SIMULADA.rds"))
cat("Listo:", nrow(partidos_df), "partidos simulados en", DIR_RAW, "\n")
