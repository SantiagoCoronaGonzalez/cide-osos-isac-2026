# ============================================================
# 01_descarga.R — CIDE OSOS
# Descarga incremental de la licencia StatsBomb (Liga MX).
#
# Que baja:
#   Por temporada:  partidos (v6), stats jugador-temporada (v7),
#                   stats equipo-temporada (v4)
#   Por partido:    alineaciones (v5), stats jugador-partido (v8),
#                   stats equipo-partido (v4)
#
# Cache: cada partido se guarda en su propio .rds. Si el script se
# interrumpe, al volver a correrlo solo baja lo que falta. La
# temporada en curso se vuelve a listar siempre para recoger los
# partidos nuevos.
#
# Volumen aproximado: ~1,850 partidos x 3 endpoints. La primera
# corrida tarda (del orden de una a dos horas, depende de la red).
# Las siguientes, minutos.
#
# Uso (desde D:/isac):
#   source("R/01_descarga.R")
# ============================================================

source("R/helpers_api.R")

# ── Configuracion ─────────────────────────────────────────────
COMPETICIONES  <- c(73)        # 73 = Liga MX. Agregar 1438 para la Femenil.
PAUSA_SEG      <- 0.15         # pausa entre peticiones, para no saturar la API
DIR_RAW        <- "data/raw"

dirs <- file.path(DIR_RAW, c("temporada", "lineups", "player_match", "team_match"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

ruta <- function(tipo, id) file.path(DIR_RAW, tipo, paste0(id, ".rds"))

cat("\n============================================\n")
cat("CIDE OSOS — Descarga StatsBomb\n")
cat("============================================\n\n")

# ── 1. Competiciones y temporadas ─────────────────────────────
comps <- as_tibble(sb_get("/api/v4/competitions"))
if (nrow(comps) == 0) stop("No se pudo leer /competitions. Revisa credenciales.")

temporadas <- comps %>%
  filter(competition_id %in% COMPETICIONES) %>%
  distinct(competition_id, competition_name, season_id, season_name) %>%
  arrange(competition_id, season_name)

saveRDS(comps, file.path(DIR_RAW, "competitions.rds"))
cat("Temporadas a descargar:\n")
print(as.data.frame(temporadas), right = FALSE)
cat("\n")

# ── 2. Nivel temporada ────────────────────────────────────────
partidos_todos <- list()

for (i in seq_len(nrow(temporadas))) {
  cid <- temporadas$competition_id[i]
  sid <- temporadas$season_id[i]
  etq <- paste(temporadas$competition_name[i], temporadas$season_name[i])
  cat("[temporada]", etq, "\n")

  # Partidos: siempre se re-listan (la temporada en curso cambia)
  m <- parse_partidos(sb_get(sprintf("/api/v6/competitions/%s/seasons/%s/matches", cid, sid),
                             verbose = FALSE))
  if (is.null(m)) { cat("   sin partidos\n"); next }
  m$competition_name <- temporadas$competition_name[i]
  saveRDS(m, ruta("temporada", paste0("matches_", cid, "_", sid)))
  partidos_todos[[etq]] <- m

  # Stats de temporada: se re-bajan siempre (son una sola peticion)
  ps <- parse_stats(sb_get(sprintf("/api/v7/competitions/%s/seasons/%s/player-stats", cid, sid),
                           verbose = FALSE), "player_season_")
  if (!is.null(ps)) saveRDS(ps, ruta("temporada", paste0("player_season_", cid, "_", sid)))

  ts <- parse_stats(sb_get(sprintf("/api/v4/competitions/%s/seasons/%s/team-stats", cid, sid),
                           verbose = FALSE), "team_season_")
  if (!is.null(ts)) saveRDS(ts, ruta("temporada", paste0("team_season_", cid, "_", sid)))

  cat("   partidos:", nrow(m),
      "| disponibles:", sum(m$match_status == "available", na.rm = TRUE),
      "| jugadores temporada:", if (is.null(ps)) 0 else nrow(ps), "\n")
  Sys.sleep(PAUSA_SEG)
}

partidos <- bind_rows(partidos_todos)
saveRDS(partidos, file.path(DIR_RAW, "partidos.rds"))

# ── 3. Nivel partido (incremental) ────────────────────────────
pendientes <- partidos %>% filter(match_status == "available") %>% pull(match_id)

faltan <- function(tipo) pendientes[!file.exists(ruta(tipo, pendientes))]
f_lu  <- faltan("lineups")
f_pm  <- faltan("player_match")
f_tm  <- faltan("team_match")
ids   <- sort(unique(c(f_lu, f_pm, f_tm)))

cat("\nPartidos disponibles:", length(pendientes),
    "| por descargar:", length(ids), "\n")
if (length(ids) > 0) cat("(se puede interrumpir y volver a correr; retoma donde quedo)\n\n")

errores <- tibble(match_id = integer(), tipo = character())
t0 <- Sys.time()

# Cada partido va protegido: si uno trae una estructura inesperada se
# registra como fallo y la descarga sigue con el siguiente.
bajar <- function(tipo, mid, expr) {
  tryCatch(expr, error = function(e) {
    cat("    fallo", tipo, mid, ":", conditionMessage(e), "\n")
    errores <<- add_row(errores, match_id = mid, tipo = tipo)
  })
}

for (k in seq_along(ids)) {
  mid <- ids[k]

  if (mid %in% f_lu) bajar("lineups", mid, {
    lu <- sb_get(sprintf("/api/v5/lineups/%s", mid), host = HOST_DATA, verbose = FALSE)
    pl <- parse_lineup(lu, mid)
    if (is.null(pl)) errores <- add_row(errores, match_id = mid, tipo = "lineups")
    else saveRDS(pl, ruta("lineups", mid))
    Sys.sleep(PAUSA_SEG)
  })

  if (mid %in% f_pm) bajar("player_match", mid, {
    pm <- parse_stats(sb_get(sprintf("/api/v8/matches/%s/player-stats", mid),
                             verbose = FALSE), "player_match_")
    if (is.null(pm)) errores <- add_row(errores, match_id = mid, tipo = "player_match")
    else saveRDS(pm, ruta("player_match", mid))
    Sys.sleep(PAUSA_SEG)
  })

  if (mid %in% f_tm) bajar("team_match", mid, {
    tm <- parse_stats(sb_get(sprintf("/api/v4/matches/%s/team-stats", mid),
                             host = HOST_DATA, verbose = FALSE), "team_match_")
    if (is.null(tm)) errores <- add_row(errores, match_id = mid, tipo = "team_match")
    else saveRDS(tm, ruta("team_match", mid))
    Sys.sleep(PAUSA_SEG)
  })

  if (k %% 25 == 0 || k == length(ids)) {
    trans <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    resta <- trans / k * (length(ids) - k)
    cat(sprintf("  %d / %d partidos  (%.1f min, faltan ~%.0f min)\n",
                k, length(ids), trans, resta))
  }
}

# ── 4. Resumen ────────────────────────────────────────────────
saveRDS(errores, file.path(DIR_RAW, "errores_descarga.rds"))

cat("\n============================================\n")
cat("Descarga terminada\n")
cat("  Alineaciones:   ", length(list.files(file.path(DIR_RAW, "lineups"))), "\n")
cat("  Jugador-partido:", length(list.files(file.path(DIR_RAW, "player_match"))), "\n")
cat("  Equipo-partido: ", length(list.files(file.path(DIR_RAW, "team_match"))), "\n")
if (nrow(errores) > 0) {
  cat("  Fallos:", nrow(errores), "(ver data/raw/errores_descarga.rds;",
      "vuelve a correr el script para reintentarlos)\n")
  print(count(errores, tipo))
}
cat("Siguiente paso: source('R/02_pipeline.R')\n")
cat("============================================\n\n")
