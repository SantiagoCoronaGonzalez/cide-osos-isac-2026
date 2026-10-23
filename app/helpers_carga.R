# ============================================================
# helpers_carga.R — CIDE OSOS (app)
# Constantes y carga de datos compartidas por los modulos.
# Reutiliza R/funciones_modelo.R para que la app y el pipeline
# usen exactamente los mismos pesos, posiciones y formulas.
# ============================================================

# La app se lanza con shiny::runApp("app") desde D:/isac, asi que el
# directorio de trabajo es app/. La raiz del proyecto es la de arriba.
RAIZ <- if (dir.exists("R") && dir.exists("data")) "." else ".."
source(file.path(RAIZ, "R", "funciones_modelo.R"), local = FALSE)

ruta_proc <- function(f) file.path(RAIZ, "data", "processed", f)

MIN_MINUTOS_UI <- MIN_MINUTOS

DIMS_INDICES <- c("z_idx_ofensivo", "z_idx_creacion", "z_idx_defensivo",
                  "z_idx_progresion", "z_idx_consistencia")

ETIQUETAS_INDICES <- c(
  z_idx_ofensivo     = "Ofensivo",
  z_idx_creacion     = "Creación",
  z_idx_defensivo    = "Defensivo",
  z_idx_progresion   = "Progresión",
  z_idx_consistencia = "Consistencia"
)

SUBS_ETIQ <- c(CB = "Centrales", FB = "Laterales", DM = "Pivotes",
               CM = "Interiores", AM = "Mediapuntas",
               W  = "Extremos",  ST = "Delanteros centro")

COLORES_OSOS <- list(
  primario = "#2E6DA4", fondo = "#F8F9FA", texto = "#212529",
  verde = "#E1ECF6", amarillo = "#F3F5F8", rojo = "#F6E3E3"
)

FIRMA <- FIRMA_OSOS

# Temporada que muestra la app. Se puede cambiar sin tocar codigo:
#   Sys.setenv(OSOS_TEMPORADA = "2024/2025"); shiny::runApp("app")
# Por defecto: la temporada completa mas reciente.
temporada_app <- function(perfiles_hist) {
  t <- Sys.getenv("OSOS_TEMPORADA", "")
  if (nzchar(t) && t %in% perfiles_hist$temporada) return(t)
  compl <- unique(perfiles_hist$temporada[!perfiles_hist$en_curso])
  if (length(compl) == 0) return(max(perfiles_hist$temporada))
  max(compl)
}

fuente_texto <- function(temporada) paste0("Fuente: StatsBomb, Liga MX ", temporada, ".")

cargar_perfiles_hist <- function() {
  p <- ruta_proc("perfiles_jugadores.rds")
  if (!file.exists(p))
    stop("No se encontro '", p, "'. Corre primero source('R/02_pipeline.R') desde D:/isac.")
  readRDS(p)
}

# "12345_Ralph Orquin" -> "Ralph Orquin"
nombre_display <- function(jugador_id) sub("^[^_]+_", "", jugador_id)

# Estilo de club por temporada, desde los partidos reales de StatsBomb
cargar_estilo_clubes <- function(temporada = TEMPORADA_APP) {
  p <- ruta_proc("contexto_equipos.rds")
  if (!file.exists(p)) return(NULL)
  ctx <- readRDS(p) %>% dplyr::filter(.data$temporada == !!temporada)

  # Nivel del club: promedio de su nivel por torneo (sale de 02_pipeline.R)
  pn <- ruta_proc("nivel_equipos.rds")
  niv <- if (file.exists(pn)) {
    readRDS(pn) %>%
      dplyr::inner_join(dplyr::distinct(ctx, team_id, torneo), by = c("team_id", "torneo")) %>%
      dplyr::group_by(team_id) %>%
      dplyr::summarise(nivel_club = mean(nivel, na.rm = TRUE), .groups = "drop")
  } else dplyr::tibble(team_id = integer(), nivel_club = numeric())

  ctx %>%
    dplyr::group_by(club, team_id, liga = liga_eq) %>%
    dplyr::summarise(
      posesion_club   = mean(posesion,   na.rm = TRUE),
      ppda_club       = mean(ppda,       na.rm = TRUE),
      directo_club    = mean(directness, na.rm = TRUE),
      n_partidos_eq   = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(niv, by = "team_id") %>%
    dplyr::filter(!is.na(posesion_club), !is.na(ppda_club))
}

cargar_formaciones <- function(temporada = TEMPORADA_APP) {
  p <- ruta_proc("formacion_club.rds")
  if (!file.exists(p)) return(NULL)
  f <- readRDS(p)
  if (is.null(f)) return(NULL)
  dplyr::filter(f, .data$temporada == !!temporada)
}

cargar_validacion <- function() {
  p <- ruta_proc(file.path("validacion", "resultados.rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

cargar_modelos <- function() {
  p <- ruta_proc(file.path("validacion", "modelos_finales.rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

# Probabilidad de que un fichaje se consolide (modelo M3 del backtest).
# Las variables se arman igual que en 03_validacion.R, pero con el
# perfil de la temporada que muestra la app.
prob_consolidarse <- function(modelo, j, afinidad, necesidad, titular_proy,
                              nivel_origen, nivel_destino) {
  if (is.null(modelo)) return(NA_real_)
  nd <- data.frame(
    pct_min_pre   = min(100, j$pct_minutos[1]),
    nivel_origen  = ifelse(is.na(nivel_origen), 70, nivel_origen),
    z_fiable      = ifelse(is.na(j$z_compuesto_fiable[1]), 0, j$z_compuesto_fiable[1]),
    edad          = ifelse(is.na(j$edad[1]), 26, j$edad[1]),
    pct_conv_pre  = ifelse(is.na(j$pct_convocatorias[1]), 60, j$pct_convocatorias[1]),
    lesion_previa = as.integer(isTRUE(j$lesiones_relevantes[1] > 0)),
    afinidad      = ifelse(is.na(afinidad), 50, afinidad),
    necesidad     = ifelse(is.na(necesidad), 0, necesidad),
    titular_proy  = as.integer(isTRUE(titular_proy)),
    salto_nivel   = ifelse(is.na(nivel_destino - nivel_origen), 0, nivel_destino - nivel_origen)
  )
  as.numeric(suppressWarnings(predict(modelo, newdata = nd, type = "response")))
}
