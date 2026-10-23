
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)


HOST_SERVICES <- "https://data.statsbombservices.com"
HOST_DATA     <- "https://data.statsbomb.com"

SB_USER <- Sys.getenv("SB_USER")
SB_PASS <- Sys.getenv("SB_PASS")

if (SB_USER == "" || SB_PASS == "") {
  stop("Faltan credenciales. Corre primero:\n",
       '  Sys.setenv(SB_USER = "usuario", SB_PASS = "password")\n',
       "Si la competencia te dio otro metodo de autenticacion (token, API key\n",
       "en header), avisa y ajustamos sb_get().")
}

DIR_OUT <- "data/api_exploracion"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

# ── 1. HELPER DE PETICION ─────────────────────────────────────


sb_get <- function(path, host = HOST_SERVICES, verbose = TRUE) {
  url <- paste0(host, path)
  r <- tryCatch(
    GET(url, authenticate(SB_USER, SB_PASS), timeout(90)),
    error = function(e) {
      if (verbose) cat("    ERROR de red:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(r)) return(NULL)

  if (status_code(r) != 200) {
    if (verbose) cat("    HTTP", status_code(r), "en", path, "\n")
    return(NULL)
  }

  txt <- content(r, as = "text", encoding = "UTF-8")
  if (!nzchar(txt) || txt == "[]") return(NULL)

  tryCatch(
    fromJSON(txt, flatten = TRUE),
    error = function(e) {
      if (verbose) cat("    ERROR parseando JSON:", conditionMessage(e), "\n")
      NULL
    }
  )
}

# Convierte a tibble de forma segura algunos endpoints devuelven
# listas anidadas que fromJSON no aplana a data.frame
como_tbl <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(as_tibble(x))
  tryCatch(as_tibble(x), error = function(e) NULL)
}

cat("\n============================================\n")
cat("CIDE OSOS — Exploracion de licencia StatsBomb\n")
cat("============================================\n\n")

# ── 2. COMPETICIONES Y TEMPORADAS ─────────────────────────────

cat("[1/5] Competiciones con licencia...\n")

comps <- como_tbl(sb_get("/api/v4/competitions"))

if (is.null(comps) || nrow(comps) == 0) {
  stop("El endpoint de competiciones no devolvio nada.\n",
       "Revisa credenciales o si la licencia esta activa.")
}

saveRDS(comps, file.path(DIR_OUT, "competitions.rds"))

cat("  Filas competicion-temporada:", nrow(comps), "\n")
cat("  Competiciones unicas:       ", n_distinct(comps$competition_id), "\n")
cat("  Paises/regiones:            ", n_distinct(comps$country_name), "\n\n")

# Resumen legible: una fila por competicion, con sus temporadas
resumen_comps <- comps %>%
  group_by(competition_id, competition_name, country_name,
           competition_gender) %>%
  summarise(
    n_temporadas = n(),
    temporadas   = paste(sort(unique(season_name)), collapse = ", "),
    youth         = if ("competition_youth" %in% names(comps))
                      any(competition_youth, na.rm = TRUE) else NA,
    internacional = if ("competition_international" %in% names(comps))
                      any(competition_international, na.rm = TRUE) else NA,
    con_360 = if ("match_available_360" %in% names(comps))
                sum(!is.na(match_available_360)) else NA_integer_,
    .groups = "drop"
  ) %>%
  arrange(desc(n_temporadas), competition_name)

cat("--- COMPETICIONES DISPONIBLES ---\n")
print(as.data.frame(resumen_comps), right = FALSE)
cat("\n")

# 2b. ligas con 2+ temporadas 
# Sin dos temporadas consecutivas no se puede evaluar un fichaje:
# necesitamos el perfil en t-1 y el rendimiento en t.

multi_temp <- resumen_comps %>% filter(n_temporadas >= 2)

cat("--- VIABILIDAD DEL BACKTEST DE FICHAJES ---\n")
if (nrow(multi_temp) == 0) {
  cat("  NINGUNA competicion tiene 2+ temporadas.\n")
  cat("  Sin esto no hay validacion longitudinal posible:\n")
  cat("  habria que validar dentro de temporada (split muestral)\n")
  cat("  o pedir acceso a temporadas anteriores.\n\n")
} else {
  cat("  ", nrow(multi_temp), "competicion(es) permiten backtest:\n")
  for (i in seq_len(nrow(multi_temp))) {
    cat("   -", multi_temp$competition_name[i],
        "(", multi_temp$country_name[i], "):",
        multi_temp$n_temporadas[i], "temporadas ->",
        multi_temp$temporadas[i], "\n")
  }
  cat("\n")
}

# Flags utiles para el caso America / canteranos
cat("--- FLAGS RELEVANTES ---\n")
if ("competition_youth" %in% names(comps)) {
  n_youth <- sum(comps$competition_youth, na.rm = TRUE)
  cat("  Competiciones juveniles:     ", n_youth,
      if (n_youth > 0) " <- sirve para el angulo de canteranos\n" else "\n")
}
if ("competition_international" %in% names(comps)) {
  n_intl <- sum(comps$competition_international, na.rm = TRUE)
  cat("  Competiciones internacionales:", n_intl,
      if (n_intl > 0)
        " <- permite anclar la escala entre ligas sin tribble manual\n"
      else
        " <- sin partidos cruzados: la comparabilidad entre ligas sigue siendo un supuesto\n")
}
if ("match_available_360" %in% names(comps)) {
  comp_360 <- comps %>% filter(!is.na(match_available_360))
  cat("  Competicion-temporadas con 360:", nrow(comp_360), "\n")
  if (nrow(comp_360) > 0) {
    cat("    ", paste(unique(comp_360$competition_name), collapse = ", "), "\n")
  }
}
cat("\n")

# ¿Esta Liga MX? Es el eje del caso America.
idx_mx <- grep("mexic|liga mx", paste(comps$country_name, comps$competition_name),
               ignore.case = TRUE)
cat("--- LIGA MX ---\n")
if (length(idx_mx) == 0) {
  cat("  NO aparece en la licencia.\n")
  cat("  El caso America tendria que replantearse: por ejemplo,\n")
  cat("  scouting de jugadores EXTRANJEROS para el America usando\n")
  cat("  las ligas que si estan, sin evaluar al plantel actual.\n\n")
} else {
  print(as.data.frame(comps[idx_mx, c("competition_id","season_id",
                                      "competition_name","season_name")]),
        right = FALSE)
  cat("\n")
}

# 3. SONDEO POR COMPETICION-TEMPORADA
# Para cada par (comp, season) bajamos partidos y contamos equipos.
# Es la forma barata de saber si la temporada esta completa o si
# solo hay un puñado de partidos sueltos.

cat("[2/5] Partidos y equipos por competicion-temporada...\n")
cat("  (una peticion por par; puede tardar)\n\n")

pares <- comps %>% distinct(competition_id, season_id,
                            competition_name, season_name, country_name)

sondear_par <- function(cid, sid, cname, sname) {
  m <- como_tbl(sb_get(sprintf("/api/v6/competitions/%s/seasons/%s/matches",
                               cid, sid), verbose = FALSE))
  if (is.null(m) || nrow(m) == 0) {
    return(tibble(competition_id = cid, season_id = sid,
                  competition_name = cname, season_name = sname,
                  n_partidos = 0L, n_equipos = 0L,
                  disponibles = 0L, con_360 = 0L,
                  fecha_min = NA_character_, fecha_max = NA_character_))
  }

  equipos <- unique(c(m$home_team.home_team_name, m$away_team.away_team_name))
  equipos <- equipos[!is.na(equipos)]

  tibble(
    competition_id = cid, season_id = sid,
    competition_name = cname, season_name = sname,
    n_partidos  = nrow(m),
    n_equipos   = length(equipos),
    disponibles = if ("match_status" %in% names(m))
                    sum(m$match_status == "available", na.rm = TRUE) else NA_integer_,
    con_360     = if ("match_status_360" %in% names(m))
                    sum(m$match_status_360 == "available", na.rm = TRUE) else NA_integer_,
    fecha_min   = as.character(min(m$match_date, na.rm = TRUE)),
    fecha_max   = as.character(max(m$match_date, na.rm = TRUE))
  )
}

inventario <- pmap_dfr(
  list(pares$competition_id, pares$season_id,
       pares$competition_name, pares$season_name),
  function(cid, sid, cname, sname) {
    cat("   .", cname, "-", sname, "\n")
    sondear_par(cid, sid, cname, sname)
  }
)

saveRDS(inventario, file.path(DIR_OUT, "inventario_temporadas.rds"))

cat("\n--- INVENTARIO ---\n")
print(as.data.frame(
  inventario %>%
    select(competition_name, season_name, n_partidos, n_equipos,
           disponibles, con_360) %>%
    arrange(competition_name, season_name)
), right = FALSE)
cat("\n")

#  4. MUESTRA DE STATS: que campos llegan de verdad 
# Los specs listan ~250 campos, pero la licencia decide cuales
# vienen poblados. Bajamos UNA temporada y contamos NAs.

cat("[3/5] Muestra de player-stats (una temporada)...\n")

# Preferimos Liga MX si existe; si no, la temporada con mas partidos
par_muestra <- if (length(idx_mx) > 0) {
  comps[idx_mx[1], ]
} else {
  mejor <- inventario %>% arrange(desc(n_partidos)) %>% slice(1)
  comps %>% filter(competition_id == mejor$competition_id,
                   season_id == mejor$season_id) %>% slice(1)
}

cat("  Usando:", par_muestra$competition_name[1],
    "-", par_muestra$season_name[1], "\n")

pstats <- como_tbl(sb_get(sprintf(
  "/api/v7/competitions/%s/seasons/%s/player-stats",
  par_muestra$competition_id[1], par_muestra$season_id[1])))

if (is.null(pstats) || nrow(pstats) == 0) {
  cat("  Sin acceso a player-stats para esta temporada.\n\n")
} else {
  saveRDS(pstats, file.path(DIR_OUT, "muestra_player_stats.rds"))

  cat("  Jugadores:", nrow(pstats), "| Campos:", ncol(pstats), "\n")

  cobertura <- tibble(
    campo = names(pstats),
    pct_no_na = round(100 * colMeans(!is.na(pstats)), 1)
  ) %>% arrange(pct_no_na)

  saveRDS(cobertura, file.path(DIR_OUT, "cobertura_player_stats.rds"))

  vacios <- cobertura %>% filter(pct_no_na == 0)
  cat("  Campos 100% vacios:", nrow(vacios), "\n")
  if (nrow(vacios) > 0 && nrow(vacios) <= 40) {
    cat("   ", paste(vacios$campo, collapse = ", "), "\n")
  }

  # Las metricas que nos interesan especificamente para el modelo
  cat("\n  --- CAMPOS CLAVE PARA EL MODELO ---\n")
  clave <- c(
    "player_season_minutes", "player_season_90s_played",
    "primary_position", "secondary_position",
    # insumos directos de tus 5 indices
    "player_season_np_xg_90", "player_season_xa_90",
    "player_season_np_shots_90", "player_season_deep_progressions_90",
    "player_season_pressures_90", "player_season_interceptions_90",
    "player_season_tackles_and_interceptions_90",
    "player_season_passing_ratio", "player_season_aerial_ratio",
    # posesion-ajustadas: OJO con el doble ajuste
    "player_season_padj_tackles_90", "player_season_padj_pressures_90",
    "player_season_padj_interceptions_90",
    # OBV — validacion externa del z_compuesto
    "player_season_obv_90", "player_season_obv_pass_90",
    "player_season_obv_defensive_action_90",
    "player_season_obv_dribble_carry_90",
    # responsabilidad defensiva — resuelve el sesgo de oportunidad
    "player_season_defensive_actions_above_expectation_90",
    "player_season_expected_defensive_actions_90",
    "player_season_obv_conceded_responsibility_weighted_90",
    # Phases of Play (julio 2026)
    "player_season_transition_xg_90", "player_season_transition_obv_90",
    "player_season_avg_player_possession_directness",
    # 360
    "player_season_360_minutes", "player_season_lbp_90",
    "player_season_average_space_received_in"
  )
  est_clave <- tibble(campo = clave) %>%
    left_join(cobertura, by = "campo") %>%
    mutate(estado = case_when(
      is.na(pct_no_na)   ~ "NO EXISTE",
      pct_no_na == 0     ~ "vacio",
      pct_no_na < 50     ~ paste0("parcial (", pct_no_na, "%)"),
      TRUE               ~ paste0("ok (", pct_no_na, "%)")
    ))
  print(as.data.frame(est_clave[, c("campo","estado")]), right = FALSE)
  cat("\n")
}

# ── 5. MUESTRA DE TEAM-STATS ──────────────────────────────────
# Esto es lo que sustituye al tribble `calidad_equipos` y a las
# columnas posesion / ppda que hoy salen de Clubs_Statistics.

cat("[4/5] Muestra de team-stats (misma temporada)...\n")

tstats <- como_tbl(sb_get(sprintf(
  "/api/v4/competitions/%s/seasons/%s/team-stats",
  par_muestra$competition_id[1], par_muestra$season_id[1])))

if (is.null(tstats) || nrow(tstats) == 0) {
  cat("  Sin acceso a team-stats.\n")
  cat("  Sin esto hay que seguir con el tribble manual de calidad_equipos.\n\n")
} else {
  saveRDS(tstats, file.path(DIR_OUT, "muestra_team_stats.rds"))
  cat("  Equipos:", nrow(tstats), "| Campos:", ncol(tstats), "\n")

  cols_ctx <- c("team_season_possession", "team_season_ppda",
                "team_season_np_xgd_pg", "team_season_obv_pg",
                "team_season_directness", "team_season_matches",
                "team_season_defensive_distance")
  disp <- intersect(cols_ctx, names(tstats))
  if (length(disp) > 0) {
    cat("\n  --- CONTEXTO DE EQUIPO (insumo del ajuste por splines) ---\n")
    print(as.data.frame(
      tstats %>%
        select(team_name, all_of(disp)) %>%
        arrange(desc(.data[[disp[min(3, length(disp))]]]))
    ), right = FALSE)
  }
  faltan <- setdiff(cols_ctx, names(tstats))
  if (length(faltan) > 0)
    cat("\n  No disponibles:", paste(faltan, collapse = ", "), "\n")
  cat("\n")
}

# ── 6. MUESTRA DE LINEUPS: posiciones reales ──────────────────
# Sustituye al regex sobre `posicion_raw` y al Ajuste D (centrales
# contados como laterales). Aqui los minutos por posicion son exactos.

cat("[5/5] Muestra de lineups (un partido)...\n")

m_muestra <- como_tbl(sb_get(sprintf(
  "/api/v6/competitions/%s/seasons/%s/matches",
  par_muestra$competition_id[1], par_muestra$season_id[1]), verbose = FALSE))

if (!is.null(m_muestra) && nrow(m_muestra) > 0) {
  mid <- m_muestra$match_id[1]
  lu <- sb_get(sprintf("/api/v5/lineups/%s", mid), host = HOST_DATA)
  if (is.null(lu)) {
    cat("  Sin acceso a lineups (probar tambien host services).\n\n")
  } else {
    saveRDS(lu, file.path(DIR_OUT, "muestra_lineups.rds"))
    cat("  Partido", mid, "— equipos:", paste(lu$team_name, collapse = " vs "), "\n")
    ej <- lu$lineup[[1]]
    if (!is.null(ej) && nrow(ej) > 0) {
      cat("  Jugadores en la hoja:", nrow(ej), "\n")
      cat("  Campos del jugador:", paste(names(ej), collapse = ", "), "\n")
      if ("positions" %in% names(ej)) {
        cat("  positions[] disponible -> minutos por posicion exactos,\n")
        cat("  se elimina el regex sobre posicion_raw y el Ajuste D.\n")
      }
    }
    cat("\n")
  }
}

# ── 7. VEREDICTO ──────────────────────────────────────────────

cat("============================================\n")
cat("RESUMEN\n")
cat("============================================\n")
cat("Competicion-temporadas:", nrow(comps), "\n")
cat("Con 2+ temporadas:     ", nrow(multi_temp),
    if (nrow(multi_temp) > 0) " -> backtest viable\n" else " -> backtest NO viable\n")
cat("Liga MX:               ", if (length(idx_mx) > 0) "SI" else "NO", "\n")
cat("\nArchivos guardados en", DIR_OUT, "\n")
cat("  competitions.rds, inventario_temporadas.rds,\n")
cat("  muestra_player_stats.rds, cobertura_player_stats.rds,\n")
cat("  muestra_team_stats.rds, muestra_lineups.rds\n\n")
cat("Siguiente paso: mandar el output de consola para decidir\n")
cat("la reescritura del pipeline y el diseno del backtest.\n")
cat("============================================\n\n")
