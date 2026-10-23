# ============================================================
# 02_pipeline.R — CIDE OSOS
# Perfiles de jugador para Liga MX, temporada por temporada y
# torneo por torneo, a partir de lo que bajo 01_descarga.R.
#
# Equivale al pipeline de la version anterior. Lo que cambia esta
# documentado al inicio de R/funciones_modelo.R.
#
# Salidas en data/processed/:
#   perfiles_jugadores.rds   — jugador x club x temporada (lo usa la app)
#   perfiles_torneo.rds      — jugador x club x torneo (validacion)
#   datos_ajustados_partido.rds — jugador x partido con residuos
#   base_partido.rds         — jugador x partido sin ajustar (validacion)
#   contexto_equipos.rds, formacion_club.rds, nivel_equipos.rds
#
# Uso (desde D:/isac):
#   source("R/02_pipeline.R")
# ============================================================

source("R/funciones_modelo.R")
source("R/helpers_eventos.R")
source("R/funciones_tactico.R")

DIR_PROC <- "data/processed"
dir.create(DIR_PROC, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================\n")
cat("CIDE OSOS — Pipeline de perfiles (Liga MX)\n")
cat("============================================\n\n")

# ── 1. Datos ──────────────────────────────────────────────────
cat("[1/6] Cargando crudos...\n")
cr <- cargar_crudos()
cat("  Partidos:", nrow(cr$partidos),
    "| filas jugador-partido:", nrow(cr$pm),
    "| convocatorias:", nrow(cr$jugadores), "\n\n")

# ── 2. Base jugador-partido ───────────────────────────────────
cat("[2/6] Base jugador-partido...\n")
base <- construir_base(cr)

# Eventos (opcionales): metricas de balon parado por jugador y partido
# (indice 6) y entradas de cambio con su impacto (indice 7).
part <- preparar_partidos(cr$partidos)
hay_eventos <- length(list.files(file.path("data/raw", "eventos"), pattern = "\\.rds$")) > 0
entradas <- NULL; bp_pm <- NULL
if (hay_eventos) {
  evs <- cargar_eventos()
  e_ev <- preparar_eventos(evs, part)
  bp_pm <- metricas_bp_partido(e_ev, evs)
  entradas <- entradas_revulsivo(analizar_cambios(e_ev), part)
  cat("  Eventos:", n_distinct(e_ev$match_id), "partidos | entradas de cambio:", NROW(entradas), "\n")
  rm(e_ev, evs); invisible(gc())
} else cat("  Sin eventos: los indices 6 (balon parado) y 7 (revulsivo) quedan vacios.\n")
base <- agregar_bp(base, bp_pm)
saveRDS(entradas, file.path(DIR_PROC, "entradas_revulsivo.rds"))
cat("  Filas:", nrow(base), "| jugadores:", n_distinct(base$player_id),
    "| temporadas:", paste(sort(unique(base$temporada)), collapse = ", "), "\n")
cat("  Por subgrupo de la posicion jugada:\n")
print(table(factor(base$sub_partido, levels = SUBS_ORDEN)))
cat("\n")
ventanas_torneo <- part %>%
  group_by(torneo, orden_torneo) %>%
  summarise(ini = min(match_date), fin = max(match_date) + 1, n = n(), .groups = "drop") %>%
  mutate(completo = n >= 0.85 * median(n)) %>%
  arrange(orden_torneo)

perfiles_por_torneo <- function(b_aj, indices, con_bp = FALSE) {
  map_dfr(seq_len(nrow(ventanas_torneo)), function(i) {
    v <- ventanas_torneo[i, ]
    p <- construir_perfiles(b_aj, cr, desde = v$ini, hasta = v$fin, por_club = TRUE, min_ref = 200,
                            indices = indices, con_bp = con_bp, entradas = entradas)
    if (is.null(p)) return(NULL)
    p %>% mutate(torneo = v$torneo, orden_torneo = v$orden_torneo, torneo_completo = v$completo)
  })
}

# ── 3. Ajuste contextual ──────────────────────────────────────
# Metodo: el que eligio 03_validacion.R (data/processed/metodo_ajuste.rds),
# o el A (nivel del equipo) si todavia no hay eleccion.
METODO <- metodo_ajuste_vigente(DIR_PROC)
INDICES_P <- indices_vigentes(DIR_PROC)
BP_P <- bp_vigente(DIR_PROC)
cat("[3/6] Modelo 1 — ajuste contextual por subgrupo\n")
cat("  Metodo:", METODOS_AJUSTE[[METODO]], "| Indices:", VERSIONES_INDICES[[INDICES_P]],
    "| Balon parado en el compuesto:", if (BP_P) "si" else "no", "\n")

# La calidad de la alineacion (metodo B) se mide siempre con perfiles
# del metodo A del torneo anterior, para que no dependa del metodo que
# se este evaluando (y con los indices completos, por la misma razon).
base_aj_A <- ajustar_contexto(base, metodo = "nivel")
perfiles_torneo_A <- perfiles_por_torneo(base_aj_A, "completo")
saveRDS(perfiles_torneo_A, file.path(DIR_PROC, "perfiles_torneo_nivel.rds"))
base <- agregar_calidad_alineacion(base, cr, perfiles_torneo_A)
saveRDS(base, file.path(DIR_PROC, "base_partido.rds"))

base_aj <- if (METODO == "nivel") base_aj_A else ajustar_contexto(base, metodo = METODO)
saveRDS(base_aj, file.path(DIR_PROC, "datos_ajustados_partido.rds"))
saveRDS(METODO, file.path(DIR_PROC, "metodo_pipeline.rds"))
saveRDS(INDICES_P, file.path(DIR_PROC, "indices_pipeline.rds"))
saveRDS(BP_P, file.path(DIR_PROC, "bp_pipeline.rds"))
cat("  OK:", nrow(base_aj), "filas ajustadas\n\n")

# ── 4. Perfiles por temporada ─────────────────────────────────
cat("[4/6] Modelo 2 — perfiles por temporada...\n")

# Una temporada esta "en curso" si tiene claramente menos partidos
# disponibles que las demas.
ventanas_temp <- part %>%
  group_by(temporada) %>%
  summarise(ini = min(match_date), fin = max(match_date) + 1,
            n = n(), .groups = "drop") %>%
  mutate(en_curso = n < 0.85 * median(n)) %>%
  arrange(ini)

perfiles <- map_dfr(seq_len(nrow(ventanas_temp)), function(i) {
  v <- ventanas_temp[i, ]
  cat("  ", v$temporada, "(", v$n, "partidos", if (v$en_curso) "- en curso" else "", ")\n")
  p <- construir_perfiles(base_aj, cr, desde = v$ini, hasta = v$fin, por_club = TRUE,
                          fecha_edad = v$ini + as.numeric(v$fin - v$ini) / 2, indices = INDICES_P,
                          con_bp = BP_P, entradas = entradas)
  if (is.null(p)) return(NULL)
  p %>% mutate(temporada = v$temporada, en_curso = v$en_curso)
})

saveRDS(perfiles, file.path(DIR_PROC, "perfiles_jugadores.rds"))
write.csv(perfiles, file.path(DIR_PROC, "perfiles_jugadores.csv"), row.names = FALSE)

# ── 5. Perfiles por torneo (para la validacion) ───────────────
cat("\n[5/6] Perfiles por torneo...\n")
perfiles_torneo <- if (METODO == "nivel" && INDICES_P == "completo" && !BP_P) perfiles_torneo_A else
  perfiles_por_torneo(base_aj, INDICES_P, BP_P)
saveRDS(perfiles_torneo, file.path(DIR_PROC, "perfiles_torneo.rds"))
saveRDS(ventanas_torneo, file.path(DIR_PROC, "torneos.rds"))
cat("  Torneos:", nrow(ventanas_torneo), "| filas:", nrow(perfiles_torneo), "\n\n")

# ── 6. Insumos de la app: estilo, formacion, nivel ────────────
cat("[6/6] Estilo de clubes, formaciones y nivel...\n")
saveRDS(contexto_equipos_app(cr), file.path(DIR_PROC, "contexto_equipos.rds"))
saveRDS(formaciones_club(cr),     file.path(DIR_PROC, "formacion_club.rds"))
saveRDS(nivel_equipos(contexto_equipo_partido(cr$tm), part),
        file.path(DIR_PROC, "nivel_equipos.rds"))

# ── Resumen ───────────────────────────────────────────────────
cat("\n============================================\n")
cat("Pipeline completado\n")
resumen <- perfiles %>%
  group_by(temporada) %>%
  summarise(jugadores = n_distinct(player_id), cumple_minimo = sum(cumple_minimo),
            clubes = n_distinct(club), m0 = first(m0_contraccion), .groups = "drop")
print(as.data.frame(resumen), row.names = FALSE)
cat("\nm0 = minutos a los que el z compuesto es 50% senal y 50% ruido.\n")
cat("La columna z_compuesto_fiable ya trae esa contraccion aplicada.\n")

# ── Chequeo de correlacion entre componentes (como en la version anterior) ──
cat("\n--- Pares de componentes con |r| > 0.75 (temporada mas reciente completa) ---\n")
ult <- perfiles %>% filter(!en_curso, cumple_minimo) %>%
  filter(temporada == max(temporada))
for (sg in SUBS_ORDEN) {
  g <- ult %>% filter(subgrupo == sg)
  if (nrow(g) < 20) next
  for (idx in INDICES) {
    cols <- paste0(setdiff(names(COMPONENTES_DE[[INDICES_P]][[idx]]), "var_obv_resid"), "_res")
    cols <- intersect(cols, names(g))
    if (length(cols) < 2) next
    m <- suppressWarnings(cor(g[, cols], use = "pairwise.complete.obs"))
    alt <- which(abs(m) > 0.75 & upper.tri(m), arr.ind = TRUE)
    if (nrow(alt) > 0) for (k in seq_len(nrow(alt)))
      cat(sprintf("  %s | %s: %s vs %s  r = %.2f\n", sg, idx,
                  rownames(m)[alt[k, 1]], colnames(m)[alt[k, 2]], m[alt[k, 1], alt[k, 2]]))
  }
}
cat("\nSiguiente paso: source('R/03_validacion.R')\n")
cat("============================================\n\n")
