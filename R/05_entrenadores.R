# ============================================================
# 05_entrenadores.R — CIDE OSOS
# "La historia de un entrenador a traves de los datos":
# todos los entrenadores del America desde que empieza la base.
#
# Requiere: 02_pipeline.R, 04_tactico.R y 05a_entrenadores.R.
# Deja:     data/processed/entrenadores.rds   (lo lee el reporte)
#           data/processed/posesiones.rds     (tabla de posesiones)
#
# Uso (desde D:/isac): source("R/05_entrenadores.R")
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
})
source("R/funciones_modelo.R")
source("R/helpers_eventos.R")
source("R/funciones_tactico.R")
source("R/funciones_entrenador.R")
set.seed(2026)

DIR_PROC <- "data/processed"
CLUB     <- Sys.getenv("OSOS_CLUB", "América")

cat("\n============================================\n")
cat("CIDE OSOS — Los entrenadores del", CLUB, "\n")
cat("============================================\n\n")

cr   <- cargar_crudos()
part <- preparar_partidos(cr$partidos)
niv  <- readRDS(file.path(DIR_PROC, "nivel_equipos.rds"))
perf_t <- readRDS(file.path(DIR_PROC, "perfiles_torneo.rds"))
tac  <- readRDS(file.path(DIR_PROC, "tactico.rds"))
ent  <- readRDS(file.path(DIR_PROC, "entrenadores_partido.rds"))

club_id <- part %>% filter(home_team_name == CLUB) %>% pull(home_team_id) %>% unique()
if (length(club_id) != 1) stop("No encuentro un solo team_id para ", CLUB)

# ── 1. Posesiones y KPIs ─────────────────────────────────────
cat("[1/7] Posesiones (marco analitico)...\n")
evs <- cargar_eventos()
e   <- preparar_eventos(evs, part)
pos <- construir_posesiones(e, niv)
saveRDS(pos, file.path(DIR_PROC, "posesiones.rds"))
cat("  Posesiones:", format(nrow(pos), big.mark = ","), "en", n_distinct(pos$match_id), "partidos\n")
cat("  Inicio:\n"); print(round(100 * prop.table(table(pos$inicio)), 1))

mp <- metricas_equipo_partido(e)
kp <- kpis_equipo_partido(pos, e, mp)
liga_t <- liga_equipo_torneo(kp)

# Partidos del club con su entrenador
part_club <- part %>%
  filter(home_team_id == club_id | away_team_id == club_id) %>%
  select(match_id, match_date, torneo, orden_torneo, competition_stage, home_team_id, away_team_id) %>%
  inner_join(ent %>% filter(team_id == club_id) %>% select(match_id, entrenador), by = "match_id")
orden_dt <- part_club %>% group_by(entrenador) %>% summarise(desde = min(match_date), hasta = max(match_date), .groups = "drop") %>%
  arrange(desde)
ENTRENADORES <- orden_dt$entrenador
part_club$entrenador <- factor(part_club$entrenador, levels = ENTRENADORES)
kp_club <- kp %>% filter(team_id == club_id) %>% inner_join(part_club %>% select(match_id, entrenador, match_date, competition_stage), by = "match_id")
# Solo entrenadores con suficientes partidos con eventos. Un interinato
# de uno o dos partidos no alcanza para hablar de una idea de juego: se
# menciona en el reporte pero no se analiza aparte.
MIN_PARTIDOS_DT <- 8
n_dt <- kp_club %>% count(entrenador = as.character(entrenador))
excluidos <- n_dt %>% filter(n < MIN_PARTIDOS_DT) %>%
  left_join(orden_dt %>% mutate(entrenador = as.character(entrenador)), by = "entrenador")
if (nrow(excluidos) > 0) cat("  Con menos de", MIN_PARTIDOS_DT, "partidos (no se analizan aparte):",
                             paste0(excluidos$entrenador, " (", excluidos$n, ")", collapse = ", "), "\n")
ENTRENADORES <- ENTRENADORES[ENTRENADORES %in% n_dt$entrenador[n_dt$n >= MIN_PARTIDOS_DT]]
kp_club$entrenador <- factor(as.character(kp_club$entrenador), levels = ENTRENADORES)
part_club <- part_club %>% filter(as.character(entrenador) %in% ENTRENADORES) %>%
  mutate(entrenador = factor(as.character(entrenador), levels = ENTRENADORES))
orden_dt <- orden_dt %>% filter(entrenador %in% ENTRENADORES)
kp_club <- kp_club %>% filter(!is.na(entrenador))
cat("  Partidos del club con eventos y entrenador:", nrow(kp_club), "\n")
print(as.data.frame(count(kp_club, entrenador)))

# Entrenador de cada torneo (el que dirigio mas partidos)
ent_torneo <- part_club %>% count(torneo, entrenador) %>% group_by(torneo) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(torneo, entrenador_torneo = entrenador)

# ── 2. Contexto de cada era ─────────────────────────────────
cat("[2/7] Contexto: resultados, nivel del plantel y de los rivales, simulacion...\n")
sim <- simular_partidos(e, club_id) %>%
  inner_join(kp_club %>% select(match_id, entrenador, torneo, orden_torneo, puntos, goles, goles_contra, np_xg, np_xg_contra,
                                competition_stage, rival_id, local), by = "match_id")
nivel_club <- niv %>% filter(team_id == club_id) %>% select(torneo, orden_torneo, nivel_club = nivel)
# Calidad individual del plantel: z fiable medio de sus jugadores, ponderado
# por minutos. El indice ya descuenta el nivel del equipo, asi que mide a
# los jugadores y no el resultado del entrenador.
calidad_t <- perf_t %>% filter(team_id == club_id, !is.na(z_compuesto_fiable)) %>%
  group_by(torneo) %>% summarise(z_plantel = weighted.mean(z_compuesto_fiable, minutos_total), .groups = "drop")
calidad_plantel <- part_club %>% count(entrenador, torneo) %>% mutate(entrenador = as.character(entrenador)) %>%
  inner_join(calidad_t, by = "torneo") %>% group_by(entrenador) %>%
  summarise(z_plantel = weighted.mean(z_plantel, n), .groups = "drop")
nivel_riv  <- sim %>% left_join(niv %>% select(rival_id = team_id, torneo, nivel_rival = nivel), by = c("rival_id", "torneo"))
contexto <- sim %>%
  group_by(entrenador) %>%
  summarise(partidos = n(),
            ganados = sum(puntos == 3), empatados = sum(puntos == 1), perdidos = sum(puntos == 0),
            pts_partido = mean(puntos), xpts_partido = mean(xpts),
            ee_dif_pts = sd(puntos - xpts) / sqrt(n()),
            goles = mean(goles), goles_contra = mean(goles_contra),
            np_xg = mean(np_xg), np_xg_contra = mean(np_xg_contra),
            liguilla = sum(competition_stage != "Regular Season", na.rm = TRUE),
            .groups = "drop") %>%
  left_join(nivel_riv %>% group_by(entrenador) %>% summarise(nivel_rival = mean(nivel_rival, na.rm = TRUE), .groups = "drop"),
            by = "entrenador") %>%
  left_join(part_club %>% distinct(entrenador, torneo) %>% left_join(nivel_club, by = "torneo") %>%
              group_by(entrenador) %>% summarise(nivel_club = mean(nivel_club, na.rm = TRUE),
                                                 torneos = paste(unique(torneo), collapse = ", "), .groups = "drop"),
            by = "entrenador") %>%
  left_join(orden_dt, by = "entrenador") %>%
  left_join(calidad_plantel, by = "entrenador") %>%
  mutate(entrenador = factor(entrenador, levels = ENTRENADORES)) %>% arrange(entrenador)
torneos_sim <- simular_torneos(sim %>% filter(competition_stage == "Regular Season" | is.na(competition_stage)))
torneos_sim <- torneos_sim %>% left_join(distinct(part_club, torneo, orden_torneo), by = "torneo") %>% arrange(orden_torneo)

# ── 3. Huella e identidad ───────────────────────────────────
cat("[3/7] Huella de cada entrenador frente a la liga...\n")
huella <- huella_entrenadores(kp_club, liga_t)
recon  <- reconocer_entrenador(kp_club)
efecto <- efecto_entrenador(kp_club)
if (!is.null(recon))
  cat(sprintf("  Acierto balanceado al adivinar el entrenador: %.0f%% (azar: %.0f%%, p = %.3f)\n",
              100 * recon$acierto, 100 * recon$azar, recon$p_valor))

# ── 4. Ajustes por contexto ─────────────────────────────────
cat("[4/7] Ajustes segun marcador, localia, rival y minuto (contra la liga)...\n")
ajustes <- ajustes_contexto(pos, part_club %>% transmute(match_id, team_id = club_id, entrenador = as.character(entrenador)))

# ── 5. Evolucion torneo a torneo ────────────────────────────
cat("[5/7] Evolucion por torneo...\n")
evol_club <- kp_club %>% group_by(torneo, orden_torneo, entrenador) %>%
  summarise(partidos = n(), across(all_of(KPIS$kpi), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(orden_torneo)
evol_liga <- liga_t %>% group_by(torneo, orden_torneo) %>%
  summarise(across(all_of(KPIS$kpi), list(q25 = ~ quantile(.x, .25, na.rm = TRUE), q50 = ~ median(.x, na.rm = TRUE),
                                          q75 = ~ quantile(.x, .75, na.rm = TRUE))), .groups = "drop")
# Partido a partido, para la serie suavizada
serie <- kp_club %>% arrange(match_date) %>% mutate(n_partido = row_number()) %>%
  select(match_id, match_date, n_partido, entrenador, torneo, all_of(KPIS$kpi), puntos)

# ── 6. Jugadores, cambios, formaciones y balon parado ───────
cat("[6/7] Jugadores, cambios, formaciones y balon parado...\n")
pc <- part_club %>% mutate(entrenador = as.character(entrenador))
uso   <- uso_jugadores(cr, pc, club_id, perf_t)
pos_u <- uso_posiciones(cr, pc, club_id)
fich  <- fichajes_por_entrenador(perf_t, club_id, ent_torneo)
desarrollo <- desarrollo_jugadores(perf_t, club_id, ent_torneo %>% filter(entrenador_torneo %in% ENTRENADORES))
if (!is.null(desarrollo)) {
  cat("  ¿Mejoro a sus jugadores? (cambio del indice sobre lo esperado; 0 = lo normal en la liga)\n")
  print(as.data.frame(desarrollo$resumen %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
}
cambios <- tac$cambios$ventanas %>% filter(team_id == club_id) %>% inner_join(pc %>% select(match_id, entrenador), by = "match_id")
cambios_res <- cambios %>% group_by(entrenador) %>%
  summarise(ventanas = n(), por_partido = n() / n_distinct(match_id),
            minuto_primero = median(tapply(t0, match_id, min)),
            pct_perdiendo = 100 * mean(estado == "Perdiendo", na.rm = TRUE),
            ee_obv = sd(impacto_obv, na.rm = TRUE) / sqrt(sum(!is.na(impacto_obv))), impacto_obv = mean(impacto_obv, na.rm = TRUE),
            impacto_xg = mean(impacto_xg, na.rm = TRUE), .groups = "drop")
cambios_estado <- cambios %>% group_by(entrenador, estado) %>%
  summarise(n = n(), impacto_obv = mean(impacto_obv, na.rm = TRUE), .groups = "drop")
formaciones <- tac$tramos_formacion %>% filter(team_id == club_id) %>% inner_join(pc %>% select(match_id, entrenador), by = "match_id") %>%
  group_by(entrenador, formacion) %>%
  summarise(xgd90_aj = weighted.mean(xgd90_aj, minutos, na.rm = TRUE), minutos = sum(minutos), .groups = "drop") %>%
  group_by(entrenador) %>% mutate(pct = 100 * minutos / sum(minutos)) %>% ungroup() %>% arrange(entrenador, desc(pct))
sq <- tac$saques
bp <- bind_rows(
  sq %>% filter(team_id == club_id) %>% mutate(lado = "A favor"),
  sq %>% filter(rival_id == club_id) %>% mutate(lado = "En contra")) %>%
  inner_join(pc %>% select(match_id, entrenador), by = "match_id") %>%
  group_by(entrenador, lado, tipo) %>%
  summarise(saques = n(), por_partido = n() / n_distinct(match_id), xg_saque = mean(xg, na.rm = TRUE),
            goles = sum(goles, na.rm = TRUE), pct_primer_contacto = 100 * mean(primer_contacto_ataque, na.rm = TRUE),
            .groups = "drop")
bp_liga <- sq %>% group_by(tipo) %>% summarise(xg_saque_liga = mean(xg, na.rm = TRUE), .groups = "drop")
bp <- bp %>% left_join(bp_liga, by = "tipo") %>% mutate(indice = 100 * xg_saque / xg_saque_liga)

# ── 7. Tablero de KPIs y perfil que falta ───────────────────
cat("[7/7] Tablero de KPIs y perfiles que faltan...\n")
ultimo <- tail(ENTRENADORES, 1)
mejor_ref <- liga_t %>% summarise(across(all_of(KPIS$kpi), list(q25 = ~ quantile(.x, .25, na.rm = TRUE),
                                                                q75 = ~ quantile(.x, .75, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("kpi", ".value"), names_pattern = "^(.*)_(q25|q75)$")
tablero <- huella %>% select(entrenador, kpi, valor) %>%
  pivot_wider(names_from = entrenador, values_from = valor) %>%
  left_join(mejor_ref, by = "kpi") %>%
  mutate(sentido = SENTIDO[kpi], etiqueta = etiqueta_kpi(kpi), fase = KPIS$fase[match(kpi, KPIS$kpi)],
         definicion = KPIS$definicion[match(kpi, KPIS$kpi)],
         objetivo = case_when(sentido == 1 ~ q75, sentido == -1 ~ q25, TRUE ~ NA_real_))

perfiles <- readRDS(file.path(DIR_PROC, "perfiles_jugadores.rds"))
temp_ult <- perfiles %>% filter(cumple_minimo) %>% count(temporada) %>% filter(n > 100) %>% pull(temporada) %>% max()
pl <- perfiles %>% filter(temporada == temp_ult)
plantel <- pl %>% filter(team_id == club_id, minutos_total >= 450) %>%
  group_by(subgrupo) %>% summarise(z_club = max(z_compuesto_fiable, na.rm = TRUE), jugadores = n(), .groups = "drop")
liga_sub <- pl %>% filter(minutos_total >= 450) %>% group_by(team_id, subgrupo) %>%
  summarise(z = max(z_compuesto_fiable, na.rm = TRUE), .groups = "drop") %>%
  group_by(subgrupo) %>% summarise(z_mediana_liga = median(z), .groups = "drop")
carencias <- tibble(subgrupo = SUBS_ORDEN) %>% left_join(plantel, by = "subgrupo") %>% left_join(liga_sub, by = "subgrupo") %>%
  mutate(brecha = coalesce(z_club, -1) - z_mediana_liga, etiqueta = ETIQUETA_SUB[subgrupo]) %>% filter(!is.na(brecha)) %>% arrange(brecha)
peor <- carencias$subgrupo[1:2]
candidatos <- pl %>% filter(subgrupo %in% peor, team_id != club_id, cumple_minimo, edad <= 28) %>%
  group_by(subgrupo) %>% slice_max(z_compuesto_fiable, n = 5, with_ties = FALSE) %>% ungroup() %>%
  transmute(subgrupo, posicion = ETIQUETA_SUB[subgrupo], player_name, club, edad, minutos_total,
            z_fiable = round(z_compuesto_fiable, 2))

out <- list(club = CLUB, club_id = club_id, entrenadores = ENTRENADORES, periodo = orden_dt, excluidos = excluidos,
            n_posesiones = nrow(pos), n_partidos_liga = n_distinct(pos$match_id),
            inicio_pos = round(100 * prop.table(table(pos$inicio)), 1),
            contexto = contexto, sim = sim, torneos_sim = torneos_sim,
            kpis = KPIS, huella = huella, reconocer = recon, efecto = efecto,
            ajustes = ajustes, evol_club = evol_club, evol_liga = evol_liga, serie = serie,
            uso = uso[c("resumen", "top")], posiciones = pos_u, fichajes = fich, desarrollo = desarrollo,
            cambios = cambios_res, cambios_estado = cambios_estado, formaciones = formaciones,
            balon_parado = bp, tablero = tablero, carencias = carencias, candidatos = candidatos,
            temporada_plantel = temp_ult, ultimo = ultimo,
            persistencia = tac$persistencia,
            liga_t = liga_t %>% left_join(tac$nombres_eq, by = "team_id"),
            parametros = list(largo_yd = LARGO_YD, seg_rapida_ult3 = SEG_RAPIDA_ULT3,
                              seg_rapida_tiro = SEG_RAPIDA_TIRO, seg_contrapresion = SEG_CONTRAPRESION))
if (file.exists(file.path(DIR_PROC, "validacion", "resultados.rds")))
  out$validacion <- readRDS(file.path(DIR_PROC, "validacion", "resultados.rds"))[c("rendimiento", "contraste", "retest")]
saveRDS(out, file.path(DIR_PROC, "entrenadores.rds"))

cat("\n--- Resultados por entrenador ---\n")
print(as.data.frame(contexto %>% select(entrenador, partidos, pts_partido, xpts_partido, np_xg, np_xg_contra, nivel_club, nivel_rival)), digits = 3)
cat("\n============================================\n")
cat("Listo: data/processed/entrenadores.rds\n")
cat("Reporte: rmarkdown::render('reporte/historia_america.Rmd')\n")
cat("============================================\n")
