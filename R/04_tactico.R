# ============================================================
# 04_tactico.R — CIDE OSOS
# Analisis tactico con eventos: metricas de equipo, cambios,
# formaciones y balon parado. Deja todo en
# data/processed/tactico.rds para la app.
#
# Requiere: 01b_descarga_eventos.R y 02_pipeline.R ya corridos.
# La validacion de "calidad del XI" usa solo alineaciones y perfiles,
# asi que corre aunque falten eventos de algunas temporadas.
#
# Uso (desde D:/isac): source("R/04_tactico.R")
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble); library(ggplot2)
})
source("R/funciones_modelo.R")
source("R/helpers_eventos.R")
source("R/funciones_tactico.R")
set.seed(1438)

DIR_PROC <- "data/processed"
DIR_FIG  <- "outputs/tactico"
dir.create(DIR_FIG, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================\n")
cat("CIDE OSOS — Analisis tactico\n")
cat("============================================\n\n")

cr     <- cargar_crudos()
part   <- preparar_partidos(cr$partidos)
niv    <- readRDS(file.path(DIR_PROC, "nivel_equipos.rds"))
perf_t <- readRDS(file.path(DIR_PROC, "perfiles_torneo.rds"))
nombres_eq <- cr$tm %>% distinct(team_id = as.integer(team_id), club = as.character(team_name)) %>%
  group_by(team_id) %>% slice_tail(n = 1) %>% ungroup()

cat("[1/6] Eventos...\n")
evs <- cargar_eventos()
e   <- preparar_eventos(evs, part)
cat("  Partidos con eventos:", n_distinct(e$match_id), "| eventos:", nrow(e),
    "| torneos:", paste(sort(unique(e$torneo)), collapse = ", "), "\n")
cat("  Cuadros 360 de balon parado:", if (is.null(evs$frames360)) 0 else n_distinct(evs$frames360$id), "\n\n")
ac  <- acumulados(e)

cat("[2/6] Metricas de equipo (xG, OBV, progresivos, presion, PPDA, field tilt)...\n")
mp <- metricas_equipo_partido(e)
mt <- metricas_equipo_torneo(mp)

cat("[3/6] Cambios...\n")
cb <- analizar_cambios(e, ac)
cat("  Ventanas de cambio:", if (is.null(cb)) 0 else nrow(cb$ventanas), "\n")
ct <- analizar_cambios_tacticos(evs, e, ac)
cat("  Cambios de formacion en el partido:", if (is.null(ct)) 0 else nrow(ct), "\n")

cat("[4/6] Formaciones y calidad del XI...\n")
tf <- tramos_formacion(evs, e, ac, niv)
vxi <- validar_calidad_xi(cr, part, perf_t)
if (!is.null(vxi))
  cat(sprintf("  Calidad del XI vs diferencia de xG: rho = %.2f [%.2f, %.2f], %d partidos\n",
              vxi$rho, vxi$rho_lo, vxi$rho_hi, vxi$partidos))

cat("[5/6] Balon parado...\n")
sq  <- saques_balon_parado(e)
org <- organizacion_360(evs$frames360, sq)
cat("  Saques:", nrow(sq), "| corners:", sum(sq$tipo == "Córner"), "\n")

cat("[6/6] Persistencia de un torneo al siguiente...\n")
por_torneo <- function(f, ...) bind_rows(lapply(sort(unique(e$torneo)), function(t)
  f(..., filtro_torneos = t) %>% mutate(torneo = t))) %>%
  left_join(part %>% distinct(torneo, orden_torneo), by = "torneo")
bp_fav_t <- por_torneo(resumir_bp, sq, "favor") %>% filter(tipo == "Córner")
bp_con_t <- por_torneo(resumir_bp, sq, "contra") %>% filter(tipo == "Córner")
cb_club_t <- if (!is.null(cb)) bind_rows(lapply(sort(unique(e$torneo)), function(t)
  resumir_cambios(cb, t)$club %>% mutate(torneo = t))) %>%
  left_join(part %>% distinct(torneo, orden_torneo), by = "torneo") else NULL
pers <- bind_rows(
  persistencia(mt, "xgd", "Diferencia de xG por partido"),
  persistencia(mt, "field_tilt", "Field tilt"),
  persistencia(mt, "ppda", "PPDA"),
  persistencia(mt, "pases_prog", "Pases progresivos por partido"),
  persistencia(mt, "presiones_altas", "Presiones en campo rival"),
  persistencia(bp_fav_t, "xg_por_saque", "xG por córner a favor"),
  persistencia(bp_con_t, "xg_por_saque", "xG por córner en contra"),
  persistencia(bp_fav_t, "pct_primer_contacto", "% primer contacto en córner a favor"),
  if (!is.null(cb_club_t)) persistencia(cb_club_t, "impacto_obv", "Impacto medio de los cambios del club"))
print(as.data.frame(pers %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)

# Señal sembrada (solo existe con datos simulados)
f_sim <- file.path("data/raw", "_balon_parado_SIMULADO.rds")
chequeo_sim <- NULL
if (file.exists(f_sim)) {
  sim <- readRDS(f_sim)
  a <- resumir_bp(sq, "favor") %>% filter(tipo == "Córner") %>% inner_join(sim, by = "team_id")
  b <- resumir_bp(sq, "contra") %>% filter(tipo == "Córner") %>% inner_join(sim, by = "team_id")
  z <- if (!is.null(org)) org$defensa %>% filter(tipo == "Córner") %>% inner_join(sim, by = "team_id") else NULL
  chequeo_sim <- tibble(
    prueba = c("xG por córner a favor vs calidad sembrada", "xG por córner en contra vs defensa sembrada",
               "% defensores pegados: al hombre vs zonal"),
    valor = c(cor(a$xg_por_saque, a$sp_of, method = "spearman"),
              cor(b$xg_por_saque, b$sp_def, method = "spearman"),
              if (!is.null(z)) mean(z$pct_pegados[!z$zonal]) - mean(z$pct_pegados[z$zonal]) else NA))
  cat("\n  (datos simulados) ¿El analisis recupera lo sembrado?\n"); print(as.data.frame(chequeo_sim), row.names = FALSE)
}

cat("\nMapas de calor y tiros por jugador...\n")
mapas <- mapas_jugador(e)
tiros_j <- tiros_jugador(e)
cat("  Celdas:", nrow(mapas), "| tiros:", nrow(tiros_j), "\n")
saveRDS(list(mapas = mapas, tiros = tiros_j), file.path(DIR_PROC, "mapas_jugador.rds"))

tactico <- list(
  nombres_eq = nombres_eq,
  torneos = sort(unique(e$torneo)),
  orden_torneos = part %>% distinct(torneo, orden_torneo) %>% filter(torneo %in% unique(e$torneo)) %>% arrange(orden_torneo),
  metricas_partido = mp, metricas_torneo = mt,
  cambios = cb, cambios_tacticos = ct, tramos_formacion = tf, calidad_xi = vxi,
  saques = sq, tiros_bp = tiros_balon_parado(e), org360 = org,
  resp_def = evs$resp_def,
  persistencia = pers, chequeo_sim = chequeo_sim,
  parametros = list(ventana_cambio = VENTANA_CAMBIO, castigo_vecino = CASTIGO_VECINO)
)
saveRDS(tactico, file.path(DIR_PROC, "tactico.rds"))

# ── Graficas para la presentacion ─────────────────────────────
AZUL <- "#2E6DA4"; DORADO <- "#B8862B"; TINTA <- "#0A1628"; GRIS <- "#6B7280"
tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = TINTA), plot.subtitle = element_text(color = GRIS),
        plot.caption = element_text(color = GRIS, hjust = 0), panel.grid.minor = element_blank())

g1 <- ggplot(pers %>% filter(!is.na(r)) %>% mutate(indicador = reorder(indicador, r)),
             aes(y = indicador, x = r)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = GRIS) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, color = AZUL, linewidth = 1, orientation = "y") +
  geom_point(color = DORADO, size = 3) +
  labs(title = "¿Qué indicadores son un rasgo del club y cuáles son ruido?",
       subtitle = "Correlación entre clubes de cada indicador en un torneo y en el siguiente (Spearman, IC 95%)",
       x = "Correlación torneo a torneo", y = NULL, caption = paste(FUENTE, FIRMA_OSOS)) + tema
ggsave(file.path(DIR_FIG, "01_persistencia.png"), g1, width = 10, height = 5, dpi = 300, bg = "white")

if (!is.null(cb)) {
  lt <- resumir_cambios(cb)$liga_tramo
  g2 <- ggplot(lt, aes(x = tramo, y = cambio_obv_medio, fill = estado)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(Perdiendo = "#C0392B", Empatando = GRIS, Ganando = AZUL)) +
    labs(title = "Qué le pasa al partido después de un cambio", fill = NULL,
         subtitle = paste0("Cambio en la diferencia de OBV por 90 (", VENTANA_CAMBIO,
                           " min después menos ", VENTANA_CAMBIO, " min antes), promedio de la liga.\n",
                           "Es la referencia que se resta para medir el impacto de cada cambio"),
         x = "Minuto del cambio", y = "Cambio en OBV dif. por 90", caption = paste(FUENTE, FIRMA_OSOS)) + tema
  ggsave(file.path(DIR_FIG, "02_cambios_referencia.png"), g2, width = 10, height = 5, dpi = 300, bg = "white")
}

cat("\n============================================\n")
cat("Analisis tactico terminado\n")
cat("  Resultados:", file.path(DIR_PROC, "tactico.rds"), "\n  Graficas:  ", DIR_FIG, "\n")
cat("============================================\n")
