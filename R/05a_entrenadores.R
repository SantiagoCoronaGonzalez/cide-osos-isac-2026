# ============================================================
# 05a_entrenadores.R — CIDE OSOS
# Quien dirigio a cada club en cada partido.
#
# Fuente 1: la lista de partidos de la API (matches v6) trae
#           home_team.managers / away_team.managers. Una peticion por
#           temporada.
# Fuente 2 (respaldo): config/entrenadores_manual.csv, con el club, el
#           entrenador y las fechas. Se usa para los partidos que la
#           API no cubra, y para corregir a mano un interinato.
#
# Resultado: data/processed/entrenadores_partido.rds
#   match_id, team_id, entrenador, fuente
#
# Uso: source("R/05a_entrenadores.R")   (credenciales en .Renviron)
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(purrr) })
source("R/helpers_api.R")

DIR_RAW  <- "data/raw"
DIR_PROC <- "data/processed"
partidos <- readRDS(file.path(DIR_RAW, "partidos.rds"))

# ── 1. Managers desde la API ─────────────────────────────────
managers_de <- function(m, lado) {
  col <- paste0(lado, "_team.managers")
  idc <- paste0(lado, "_team.", lado, "_team_id")
  if (!col %in% names(m)) return(NULL)
  map_dfr(seq_len(nrow(m)), function(i) {
    x <- m[[col]][[i]]
    if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(NULL)
    nom <- if ("nickname" %in% names(x) && !is.na(x$nickname[1]) && nzchar(x$nickname[1])) x$nickname[1] else x$name[1]
    tibble(match_id = as.integer(m$match_id[i]), team_id = as.integer(m[[idc]][i]),
           entrenador = as.character(nom), entrenador_id = as.integer(x$id[1]))
  })
}

api <- NULL
if (nzchar(Sys.getenv("SB_USER"))) {
  temporadas <- partidos %>% distinct(competition_id, season_id, season_name)
  api <- map_dfr(seq_len(nrow(temporadas)), function(i) {
    cat("[managers]", temporadas$season_name[i], "\n")
    m <- tryCatch(sb_get(sprintf("/api/v6/competitions/%s/seasons/%s/matches",
                                 temporadas$competition_id[i], temporadas$season_id[i]), verbose = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !is.data.frame(m)) return(NULL)
    bind_rows(managers_de(m, "home"), managers_de(m, "away"))
  })
  if (nrow(api) == 0) api <- NULL
  if (!is.null(api)) {
    api$fuente <- "API"
    # Nombres completos de la API -> nombre corto (config/entrenadores_alias.csv)
    if (file.exists("config/entrenadores_alias.csv")) {
      al <- read.csv("config/entrenadores_alias.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
      k <- match(api$entrenador, al$nombre_api)
      api$entrenador[!is.na(k)] <- al$entrenador[k[!is.na(k)]]
    }
  }
} else {
  cat("Sin credenciales: se usa solo config/entrenadores_manual.csv\n")
}

# ── 2. Respaldo manual por fechas ────────────────────────────
manual <- read.csv("config/entrenadores_manual.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
manual$desde <- as.Date(manual$desde); manual$hasta <- as.Date(manual$hasta)
# Solo partidos ya jugados: los programados no llevan entrenador
jugados <- partidos %>% filter(match_date <= Sys.Date(), !is.na(home_score))
lados <- bind_rows(
  jugados %>% transmute(match_id, match_date, team_id = home_team_id, club = home_team_name),
  jugados %>% transmute(match_id, match_date, team_id = away_team_id, club = away_team_name))
por_fecha <- lados %>%
  inner_join(manual, by = "club", relationship = "many-to-many") %>%
  filter(match_date >= desde, is.na(hasta) | match_date <= hasta) %>%
  transmute(match_id, team_id, entrenador, fuente = "manual")

# La API manda; el manual llena huecos. Si el CSV marca forzar = 1, el
# manual reemplaza a la API en esas fechas (interinatos mal cargados).
forzados <- manual %>% filter(forzar == 1)
por_fecha_forzado <- lados %>%
  inner_join(forzados, by = "club", relationship = "many-to-many") %>%
  filter(match_date >= desde, is.na(hasta) | match_date <= hasta) %>%
  transmute(match_id, team_id, entrenador, fuente = "manual (forzado)")

api_sel <- if (is.null(api)) NULL else select(api, any_of(c("match_id", "team_id", "entrenador", "fuente")))
ent <- bind_rows(por_fecha_forzado, api_sel, por_fecha) %>%
  distinct(match_id, team_id, .keep_all = TRUE)

saveRDS(ent, file.path(DIR_PROC, "entrenadores_partido.rds"))

# Resumen del club que nos importa
cat("\nEntrenadores del America en la base:\n")
am <- lados %>% filter(club == "América") %>% inner_join(ent, by = c("match_id", "team_id")) %>%
  group_by(entrenador, fuente) %>%
  summarise(desde = min(match_date), hasta = max(match_date), partidos = n(), .groups = "drop") %>%
  arrange(desde)
print(as.data.frame(am))
sin <- lados %>% filter(club == "América") %>% anti_join(ent, by = c("match_id", "team_id"))
if (nrow(sin) > 0) cat("\nOJO:", nrow(sin), "partidos del America sin entrenador. Agregalos al CSV.\n")
cat("\nSiguiente paso: source('R/05_entrenadores.R')\n")
