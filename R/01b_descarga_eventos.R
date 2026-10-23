# ============================================================
# 01b_descarga_eventos.R — CIDE OSOS
# Descarga incremental de Events v11 y 360 Frames v2.
#
# Va aparte de 01_descarga.R porque pesa mucho mas: cada partido son
# ~3,500 eventos (unos 3-5 MB) y su 360 unos 10 MB. Se guarda SOLO la
# version compacta (helpers_eventos.R), no el JSON:
#   data/raw/eventos/{match_id}.rds     eventos, tiros, formaciones
#   data/raw/frames360/{match_id}.rds   solo cuadros de balon parado
#
# Necesita que 01_descarga.R ya haya corrido (usa data/raw/partidos.rds).
# Se puede interrumpir: al volver a correrlo, retoma donde quedo.
#
# Uso (desde D:/isac):
#   PRUEBA <- TRUE;  source("R/01b_descarga_eventos.R")   # un partido, para ver que la licencia los incluye
#   PRUEBA <- FALSE; source("R/01b_descarga_eventos.R")   # todo
# ============================================================

source("R/helpers_api.R")
source("R/helpers_eventos.R")

if (!exists("PRUEBA")) PRUEBA <- FALSE
CON_360      <- TRUE     # FALSE para bajar solo eventos (mucho mas rapido)
TEMPORADAS   <- NULL     # p. ej. c("2024/2025", "2025/2026") para limitar; NULL = todas
PAUSA_SEG    <- 0.15
DIR_RAW      <- "data/raw"

for (d in c("eventos", "frames360")) dir.create(file.path(DIR_RAW, d), recursive = TRUE, showWarnings = FALSE)
ruta <- function(tipo, id) file.path(DIR_RAW, tipo, paste0(id, ".rds"))

partidos <- readRDS(file.path(DIR_RAW, "partidos.rds"))
disp <- partidos %>% filter(match_status == "available")
if (!is.null(TEMPORADAS)) disp <- disp %>% filter(season_name %in% TEMPORADAS)
disp <- disp %>% arrange(desc(match_date))

# ── Prueba: un partido, con diagnostico ───────────────────────
if (PRUEBA) {
  mid <- disp$match_id[1]
  cat("\nPrueba con el partido", mid, "\n")
  ev <- sb_get(sprintf("/api/v11/events/%s", mid), verbose = TRUE)
  if (is.null(ev)) {
    cat("\n  Events v11 NO respondio. Puede que la licencia no lo incluya.\n",
        " Prueba v10: sb_get('/api/v10/events/", mid, "')\n", sep = "")
  } else {
    cat("  Eventos:", nrow(ev), "| columnas:", ncol(ev), "\n")
    cat("  Tipos mas comunes:\n"); print(head(sort(table(ev$type.name), decreasing = TRUE), 12))
    cat("  ¿Trae OBV?", "obv_total_net" %in% names(ev),
        "| ¿freeze frame en tiros?", "shot.freeze_frame" %in% names(ev),
        "| ¿responsabilidad defensiva?", any(grepl("^defensive_responsibility", names(ev))), "\n")
    p <- parse_eventos(ev, mid)
    cat("  Compacto: eventos", nrow(p$eventos), "| filas de tiros", NROW(p$tiros_ff),
        "| filas de formacion", NROW(p$tacticas), "\n")
    print(p$eventos %>% filter(type == "Pass", pass_type == "Corner") %>%
            select(minute, team_id, x, y, end_x, end_y, pass_technique) %>% head(3))
  }
  fr <- sb_get(sprintf("/api/v2/360-frames/%s", mid), verbose = TRUE)
  cat("\n  360:", if (is.null(fr)) "NO respondio" else paste(nrow(fr), "cuadros"), "\n")
  if (!is.null(ev) && !is.null(fr)) {
    f <- parse_360(fr, ids_balon_parado(p$eventos), mid)
    cat("  Cuadros 360 de balon parado guardables:", n_distinct(f$id), "\n")
  }
  cat("\nSi todo respondio, corre: PRUEBA <- FALSE; source('R/01b_descarga_eventos.R')\n")
} else {

  # ── Descarga completa (incremental) ─────────────────────────
  falta_ev  <- disp$match_id[!file.exists(ruta("eventos", disp$match_id))]
  falta_360 <- if (CON_360) disp$match_id[!file.exists(ruta("frames360", disp$match_id))] else integer()
  ids <- sort(unique(c(falta_ev, falta_360)), decreasing = TRUE)
  cat("\nPartidos:", nrow(disp), "| por descargar:", length(ids),
      if (CON_360) "(eventos + 360)" else "(solo eventos)", "\n")
  cat("(se puede interrumpir y volver a correr; retoma donde quedo)\n\n")

  fallos <- tibble(match_id = integer(), tipo = character())
  t0 <- Sys.time()
  for (k in seq_along(ids)) {
    mid <- ids[k]
    p <- NULL
    if (mid %in% falta_ev) {
      tryCatch({
        ev <- sb_get(sprintf("/api/v11/events/%s", mid), verbose = FALSE)
        p <- parse_eventos(ev, mid)
        if (is.null(p)) fallos <- add_row(fallos, match_id = mid, tipo = "eventos")
        else saveRDS(p, ruta("eventos", mid))
      }, error = function(e) {
        cat("    fallo eventos", mid, ":", conditionMessage(e), "\n")
        fallos <<- add_row(fallos, match_id = mid, tipo = "eventos")
      })
      Sys.sleep(PAUSA_SEG)
    } else if (file.exists(ruta("eventos", mid))) {
      p <- readRDS(ruta("eventos", mid))
    }

    if (CON_360 && mid %in% falta_360 && !is.null(p)) {
      tryCatch({
        fr <- sb_get(sprintf("/api/v2/360-frames/%s", mid), verbose = FALSE)
        f <- parse_360(fr, ids_balon_parado(p$eventos), mid)
        # Se guarda aunque venga vacio, para no volver a pedirlo
        saveRDS(if (is.null(f)) tibble() else f, ruta("frames360", mid))
      }, error = function(e) {
        cat("    fallo 360", mid, ":", conditionMessage(e), "\n")
        fallos <<- add_row(fallos, match_id = mid, tipo = "360")
      })
      Sys.sleep(PAUSA_SEG)
    }

    if (k %% 25 == 0 || k == length(ids)) {
      min_t <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("  %d / %d  (%.0f min, faltan ~%.0f min)\n", k, length(ids), min_t,
                  min_t / k * (length(ids) - k)))
    }
  }
  cat("\nListo. Fallos:", nrow(fallos), "\n")
  if (nrow(fallos) > 0) { print(count(fallos, tipo)); cat("Vuelve a correr el script para reintentarlos.\n") }
}
