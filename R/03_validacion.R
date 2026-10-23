# ============================================================
# 03_validacion.R — CIDE OSOS
# Backtest del modelo contra cinco anos de Liga MX.
#
# Pregunta central: de los traspasos que SI ocurrieron entre clubes
# de Liga MX, ¿el modelo distingue ex ante los que funcionaron de
# los que no?
#
# Diseno (sin fuga de informacion futura):
#   - Una fecha de corte por torneo: el primer partido del torneo.
#   - Perfil EX ANTE: los 365 dias previos al corte, con el ajuste
#     contextual re-estimado SOLO con partidos anteriores al corte.
#   - Traspaso: el club del jugador en el torneo nuevo (por
#     convocatorias) es distinto a su ultimo club antes del corte.
#   - Exito: jugar al menos UMBRAL_EXITO % de los minutos posibles
#     de su nuevo club en ese torneo.
#   - Evaluacion de origen rodante: para predecir el torneo k, los
#     modelos solo se estiman con torneos anteriores a k.
#
# Modelos, de menos a mas informacion:
#   M0 Mercado      — minutos que jugaba antes + nivel de su club
#                     (lo que ya ve cualquier scout)
#   M1 + Rendimiento — z compuesto con contraccion por minutos
#   M2 + Edad y fisico — edad, edad^2, convocatorias, lesiones previas
#   M3 + Encaje     — afinidad de estilo, necesidad del club destino,
#                     si entraria al once, salto de nivel
#
# Tres validaciones complementarias:
#   V1 Fiabilidad test-retest del indice entre torneos
#   V2 Validez externa contra OBV (modelo independiente de StatsBomb)
#   V3 Canteranos: ¿el modelo detecta a los 21 anos quien se
#      consolida un ano despues?
#
# Sesgo que NO se elimina y hay que declarar: solo se observa el
# rendimiento de los jugadores que SI fueron fichados. Los que el
# modelo recomendaba y nadie fichó no tienen contrafactual.
#
# Uso (desde D:/isac, despues de 02_pipeline.R):
#   source("R/03_validacion.R")
# ============================================================

source("R/funciones_modelo.R")
suppressPackageStartupMessages(library(ggplot2))

UMBRAL_EXITO   <- 50    # % de minutos posibles en el torneo de llegada
MIN_POST_REND  <- 450   # minutos para medir rendimiento despues del traspaso
VENTANA_PRE    <- 365   # dias de historia para el perfil ex ante
EDAD_CANTERA   <- 21
N_BOOT         <- 1000

DIR_PROC <- "data/processed"
DIR_VAL  <- file.path(DIR_PROC, "validacion")
DIR_FIG  <- "outputs/validacion"
dir.create(DIR_VAL, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_FIG, recursive = TRUE, showWarnings = FALSE)

set.seed(1438)

cat("\n============================================\n")
cat("CIDE OSOS — Validacion del modelo (backtest)\n")
cat("============================================\n\n")

# ── 0. Insumos ────────────────────────────────────────────────
cr      <- cargar_crudos()
base    <- readRDS(file.path(DIR_PROC, "base_partido.rds"))
perf_t  <- readRDS(file.path(DIR_PROC, "perfiles_torneo.rds"))
torneos <- readRDS(file.path(DIR_PROC, "torneos.rds")) %>%
  arrange(orden_torneo) %>% mutate(k = row_number())
niv     <- readRDS(file.path(DIR_PROC, "nivel_equipos.rds"))
part    <- preparar_partidos(cr$partidos)

ctx <- contexto_equipo_partido(cr$tm) %>%
  inner_join(part %>% select(match_id, match_date), by = "match_id")

# Club dominante de cada jugador en cada torneo y % de minutos posibles
part_eq <- bind_rows(
  part %>% transmute(match_id, torneo, team_id = home_team_id),
  part %>% transmute(match_id, torneo, team_id = away_team_id)
)
partidos_club_torneo <- part_eq %>% count(team_id, torneo, name = "partidos_club")

club_torneo <- cr$jugadores %>%
  inner_join(part %>% select(match_id, torneo), by = "match_id") %>%
  count(player_id, torneo, team_id, name = "convocatorias") %>%
  group_by(player_id, torneo) %>%
  slice_max(convocatorias, n = 1, with_ties = FALSE) %>%
  ungroup()

minutos_club_torneo <- cr$pm %>%
  transmute(match_id = as.integer(match_id), team_id = as.integer(team_id),
            player_id = as.integer(player_id), minutos = player_match_minutes) %>%
  inner_join(part %>% select(match_id, torneo), by = "match_id") %>%
  group_by(player_id, team_id, torneo) %>%
  summarise(minutos = sum(minutos, na.rm = TRUE), .groups = "drop")

resultado_torneo <- club_torneo %>%
  left_join(minutos_club_torneo, by = c("player_id", "team_id", "torneo")) %>%
  left_join(partidos_club_torneo, by = c("team_id", "torneo")) %>%
  mutate(minutos = replace_na(minutos, 0),
         pct_min = 100 * minutos / pmax(partidos_club * 90, 1))

# Formacion modal de cada club en una ventana
formacion_ventana <- function(desde, hasta) {
  if (nrow(cr$formaciones) == 0) return(tibble(team_id = integer(), formacion = character()))
  cr$formaciones %>%
    filter(reason == "Starting XI") %>%
    inner_join(part %>% filter(match_date >= desde, match_date < hasta) %>%
                 select(match_id), by = "match_id") %>%
    mutate(formacion = formacion_a_texto(formation)) %>%
    group_by(team_id) %>%
    summarise(formacion = moda(formacion), .groups = "drop")
}

# ── 1. Cortes ─────────────────────────────────────────────────
cortes <- torneos %>% filter(k >= 3, completo)
cat("Cortes evaluables:", nrow(cortes), "->",
    paste(cortes$torneo, collapse = ", "), "\n\n")
if (nrow(cortes) < 3) stop("Hacen falta al menos 3 torneos completos con un ano de historia previa.")

auc <- function(y, p) {
  ok <- !is.na(y) & !is.na(p); y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ── 1b. Carrera de especificaciones del ajuste por contexto ───
# Las tres formas de limpiar el contexto (R/funciones_modelo.R) se
# estiman en cada corte SOLO con partidos anteriores. Criterio fijado
# de antemano: gana la que mejor anticipe el OBV SIN AJUSTAR del jugador
# en su club nuevo, entre traspasos con MIN_POST_REND+ minutos. Se usa
# OBV crudo porque no depende de ninguna de las tres especificaciones
# (con el z posterior, cada una se calificaria a si misma).
# Regla: B o C solo reemplazan a A si su ventaja tiene IC 95% > 0.
cat("[0/5] Carrera de especificaciones del ajuste por contexto...\n")
if (!all(c("cal_comp_alin", "cal_rival_alin") %in% names(base)))
  stop("La base no trae la calidad de alineacion. Corre de nuevo R/02_pipeline.R (version nueva).")

# Evalua versiones en competencia. `pre` trae una fila por jugador,
# corte y version (columna `version`, z ex ante en `z_pre`). `base_v`
# es la version por defecto (la mas simple): un retador solo la
# reemplaza si su ventaja en rho tiene IC 95% > 0.
rho_s <- function(x, y) suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs"))
correr_carrera <- function(pre, etiquetas, base_v) {
  vs <- names(etiquetas)
  ev <- pre %>%
    filter(cumple_minimo) %>%
    inner_join(resultado_torneo %>% select(player_id, torneo, team_post = team_id, pct_min_post = pct_min),
               by = c("player_id", "torneo")) %>%
    left_join(perf_t %>% select(player_id, team_post = team_id, torneo,
                                obv_post_crudo = obv_p90_crudo, min_post_perf = minutos_total),
              by = c("player_id", "team_post", "torneo")) %>%
    mutate(traspaso = team_pre != team_post,
           jugo = !is.na(min_post_perf) & min_post_perf >= MIN_POST_REND,
           exito = as.integer(pct_min_post >= UMBRAL_EXITO))
  ancho <- ev %>%
    select(version, player_id, orden, traspaso, jugo, exito, obv_post_crudo, z_pre) %>%
    pivot_wider(names_from = version, values_from = z_pre) %>%
    filter(if_all(all_of(vs), ~ !is.na(.x)))
  med <- function(d) {
    t <- d[d$traspaso & d$jugo, ]; a <- d[d$jugo, ]; tm <- d[d$traspaso, ]
    unlist(lapply(vs, function(m) c(
      rho_traspasos = rho_s(t[[m]], t$obv_post_crudo),
      rho_todos     = rho_s(a[[m]], a$obv_post_crudo),
      auc_minutos   = auc(tm$exito, tm[[m]]))))
  }
  nm <- as.vector(outer(c("rho_traspasos", "rho_todos", "auc_minutos"), vs,
                        function(a, b) paste(b, a, sep = ".")))
  punto <- setNames(med(ancho), nm)
  boot <- replicate(N_BOOT, setNames(med(ancho[sample(nrow(ancho), replace = TRUE), ]), nm))
  kb <- paste0(base_v, ".rho_traspasos")
  tab <- map_dfr(vs, function(m) {
    k <- paste0(m, ".rho_traspasos"); dif <- boot[k, ] - boot[kb, ]
    tibble(
      metodo = m, especificacion = etiquetas[[m]],
      n_traspasos = sum(ancho$traspaso & ancho$jugo),
      rho_traspasos = punto[[k]],
      rho_lo = quantile(boot[k, ], 0.025, na.rm = TRUE),
      rho_hi = quantile(boot[k, ], 0.975, na.rm = TRUE),
      dif_vs_A = punto[[k]] - punto[[kb]],
      dif_lo = quantile(dif, 0.025, na.rm = TRUE),
      dif_hi = quantile(dif, 0.975, na.rm = TRUE),
      n_todos = sum(ancho$jugo),
      rho_todos = punto[[paste0(m, ".rho_todos")]],
      auc_minutos = punto[[paste0(m, ".auc_minutos")]])
  })
  ret <- tab %>% filter(metodo != base_v) %>% arrange(desc(rho_traspasos))
  gana <- if (nrow(ret) > 0 && !is.na(ret$dif_lo[1]) && ret$dif_lo[1] > 0) ret$metodo[1] else base_v
  tab$elegido <- tab$metodo == gana
  list(tabla = tab, ganador = gana)
}
imprimir_carrera <- function(tab, titulo) {
  cat("\n---", titulo, "---\n")
  print(as.data.frame(tab %>% select(-metodo) %>%
                        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
}

# Carrera 1 (con los indices completos, fijos)
carrera_pre <- map_dfr(seq_len(nrow(cortes)), function(i) {
  fila <- cortes[i, ]
  cat("   .", fila$torneo, "\n")
  map_dfr(names(METODOS_AJUSTE), function(m) {
    aj  <- ajustar_contexto(base, hasta = fila$ini, metodo = m)
    pre <- construir_perfiles(aj, cr, desde = fila$ini - VENTANA_PRE, hasta = fila$ini,
                              por_club = FALSE, fecha_edad = fila$ini, indices = "completo")
    if (is.null(pre)) return(NULL)
    pre %>% transmute(player_id, team_pre = team_id, cumple_minimo,
                      z_pre = z_compuesto_fiable, version = m,
                      torneo = fila$torneo, orden = fila$orden_torneo)
  })
})
c1 <- correr_carrera(carrera_pre, METODOS_AJUSTE, "nivel")
tabla_carrera <- c1$tabla; GANADOR <- c1$ganador
imprimir_carrera(tabla_carrera, "Carrera 1: ¿que ajuste anticipa mejor el OBV en el club nuevo?")
cat("\n  Elegido:", METODOS_AJUSTE[[GANADOR]],
    if (GANADOR == "nivel") "(ningun retador supero a A con IC > 0; se queda el mas simple)" else "", "\n")
saveRDS(GANADOR, file.path(DIR_PROC, "metodo_ajuste.rds"))
METODO_VAL <- GANADOR

# ── 1c. Carrera de indices: completos vs parsimoniosos ────────
# Con el ajuste elegido. Mismo criterio y misma regla, con la version
# mas simple como base: los indices completos solo se quedan si
# anticipan mejor el OBV con IC 95% > 0 (parsimonia).
cat("\n[0b/5] Carrera de indices: completos vs parsimoniosos...\n")
carrera_pre_ind <- map_dfr(seq_len(nrow(cortes)), function(i) {
  fila <- cortes[i, ]
  cat("   .", fila$torneo, "\n")
  aj <- ajustar_contexto(base, hasta = fila$ini, metodo = METODO_VAL)
  map_dfr(names(VERSIONES_INDICES), function(v) {
    pre <- construir_perfiles(aj, cr, desde = fila$ini - VENTANA_PRE, hasta = fila$ini,
                              por_club = FALSE, fecha_edad = fila$ini, indices = v)
    if (is.null(pre)) return(NULL)
    pre %>% transmute(player_id, team_pre = team_id, cumple_minimo,
                      z_pre = z_compuesto_fiable, version = v,
                      torneo = fila$torneo, orden = fila$orden_torneo)
  })
})
c2 <- correr_carrera(carrera_pre_ind, VERSIONES_INDICES, "parsimonioso")
tabla_carrera_ind <- c2$tabla; GANADOR_IND <- c2$ganador
imprimir_carrera(tabla_carrera_ind, "Carrera 2: ¿indices completos o parsimoniosos?")
cat("\n  Elegidos:", VERSIONES_INDICES[[GANADOR_IND]],
    if (GANADOR_IND == "parsimonioso") "(los completos no superaron a los simples con IC > 0)" else "", "\n")
saveRDS(GANADOR_IND, file.path(DIR_PROC, "indices_elegidos.rds"))
INDICES_VAL <- GANADOR_IND
leer_o <- function(f, defecto) if (file.exists(file.path(DIR_PROC, f))) readRDS(file.path(DIR_PROC, f)) else defecto

# ── 1d. Indice 6 (balon parado): solo informativo ─────────────
# Con datos reales, sumarlo al compuesto empeoro un poco la prediccion
# (-0.014, IC [-0.034, 0.005]). Se decidio dejarlo como dato: se
# calcula y se muestra, pero nunca pesa en el compuesto.
tabla_carrera_bp <- NULL; GANADOR_BP <- FALSE
saveRDS(GANADOR_BP, file.path(DIR_PROC, "balon_parado_elegido.rds"))
BP_VAL <- FALSE

# ── 1e. Indice 7 (revulsivo): ¿se repite? ─────────────────────
# Impacto contraido al entrar de cambio en los 365 dias previos a cada
# corte contra el impacto medio de sus entradas en el torneo siguiente.
# Si no se repite, el indice 7 describe el pasado pero no sirve para
# decidir a quien meter.
entradas <- leer_o("entradas_revulsivo.rds", NULL)
tabla_rev <- NULL
if (!is.null(entradas) && nrow(entradas) > 100) {
  rv <- map_dfr(seq_len(nrow(cortes)), function(i) {
    fila <- cortes[i, ]
    r <- revulsivo_ventana(entradas, fila$ini - VENTANA_PRE, fila$ini)
    if (is.null(r)) return(NULL)
    post <- entradas %>% filter(torneo == fila$torneo) %>% group_by(player_id) %>%
      summarise(n_post = n(), imp_post = mean(impacto_obv), .groups = "drop")
    r %>% filter(entradas >= MIN_ENTRADAS_REV) %>% inner_join(post %>% filter(n_post >= 2), by = "player_id") %>%
      mutate(torneo = fila$torneo)
  })
  if (nrow(rv) >= 20) {
    est <- function(x) { b <- replicate(N_BOOT, { k <- sample(nrow(rv), replace = TRUE)
      suppressWarnings(cor(rv[[x]][k], rv$imp_post[k], method = "spearman")) })
      c(rho = suppressWarnings(cor(rv[[x]], rv$imp_post, method = "spearman")),
        lo = unname(quantile(b, .025, na.rm = TRUE)), hi = unname(quantile(b, .975, na.rm = TRUE))) }
    tabla_rev <- bind_rows(
      c(predictor = "Índice 7 (contraído)", n = nrow(rv), est("revulsivo")),
      c(predictor = "Impacto bruto previo", n = nrow(rv), est("revulsivo_bruto"))) %>%
      mutate(across(c(n, rho, lo, hi), as.numeric))
    cat("\n--- Indice 7: ¿el revulsivo de antes anticipa el de despues? ---\n")
    print(as.data.frame(tabla_rev %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
  } else cat("\n  Indice 7: muy pocos jugadores con entradas antes y despues para validarlo.\n")
}

metodo_pipe  <- leer_o("metodo_pipeline.rds", "nivel")
indices_pipe <- leer_o("indices_pipeline.rds", "completo")
bp_pipe      <- isTRUE(leer_o("bp_pipeline.rds", FALSE))
if (metodo_pipe != GANADOR || indices_pipe != GANADOR_IND || bp_pipe != GANADOR_BP) {
  cat("\n  AVISO: el pipeline se corrio con", METODOS_AJUSTE[[metodo_pipe]], "+",
      VERSIONES_INDICES[[indices_pipe]], "+", if (bp_pipe) "balon parado" else "sin balon parado", "\n",
      " Lo elegido es", METODOS_AJUSTE[[GANADOR]], "+", VERSIONES_INDICES[[GANADOR_IND]], "+",
      if (GANADOR_BP) "balon parado" else "sin balon parado", "\n",
      " Para que la app y los resultados posteriores lo usen, corre otra vez:\n",
      "   source('R/02_pipeline.R'); source('R/03_validacion.R')\n\n")
}

# ── 2. Variables ex ante por corte ────────────────────────────
cat("[1/5] Perfiles ex ante por corte (re-estimando el ajuste en cada uno)...\n")

evaluar_corte <- function(fila) {
  corte <- fila$ini
  cat("   .", fila$torneo, "\n")

  aj  <- ajustar_contexto(base, hasta = corte, metodo = METODO_VAL)
  pre <- construir_perfiles(aj, cr, desde = corte - VENTANA_PRE, hasta = corte,
                            por_club = FALSE, fecha_edad = corte, indices = INDICES_VAL, con_bp = BP_VAL)
  if (is.null(pre)) return(NULL)

  # Contexto y nivel de los clubes ANTES del corte
  ctx_pre <- ctx %>%
    filter(match_date >= corte - VENTANA_PRE, match_date < corte) %>%
    group_by(team_id) %>%
    summarise(pos_club = mean(posesion, na.rm = TRUE),
              ppda_club = mean(ppda, na.rm = TRUE),
              dir_club = mean(directness, na.rm = TRUE), .groups = "drop")
  sd_pos  <- sd(ctx_pre$pos_club,  na.rm = TRUE)
  sd_ppda <- sd(ctx_pre$ppda_club, na.rm = TRUE)

  tor_prev <- torneos$torneo[torneos$k == fila$k - 1]
  niv_prev <- niv %>% filter(torneo == tor_prev) %>% select(team_id, nivel)
  form_pre <- formacion_ventana(corte - VENTANA_PRE, corte)

  post <- resultado_torneo %>%
    filter(torneo == fila$torneo) %>%
    select(player_id, team_post = team_id, pct_min_post = pct_min, min_post = minutos)

  d <- pre %>%
    inner_join(post, by = "player_id") %>%
    mutate(traspaso = team_id != team_post) %>%
    left_join(ctx_pre %>% rename(team_post = team_id, pos_dest = pos_club, ppda_dest = ppda_club),
              by = "team_post") %>%
    left_join(niv_prev %>% rename(nivel_origen = nivel), by = "team_id") %>%
    left_join(niv_prev %>% rename(team_post = team_id, nivel_destino = nivel), by = "team_post") %>%
    left_join(form_pre %>% rename(team_post = team_id), by = "team_post")

  if (nrow(d) == 0) return(NULL)

  d$afinidad <- afinidad_contexto(d$posesion_media, d$ppda_media,
                                  d$pos_dest, d$ppda_dest, sd_pos, sd_ppda)

  # Necesidad y jerarquia en el club destino, con la plantilla ex ante
  ref <- pre %>% filter(cumple_minimo)
  d$necesidad <- NA_real_; d$puesto <- NA_integer_; d$cupo <- NA_integer_
  for (i in seq_len(nrow(d))) {
    riv <- ref %>% filter(team_id == d$team_post[i], subgrupo == d$subgrupo[i],
                          player_id != d$player_id[i])
    z_club <- if (nrow(riv) == 0) rep(-1, length(DIMS_FIT))
              else vapply(DIMS_FIT, function(z) mean(riv[[z]], na.rm = TRUE), numeric(1))
    d$necesidad[i] <- necesidad_club(as.numeric(d[i, DIMS_FIT]), z_club)
    d$puesto[i] <- sum(riv$z_compuesto > d$z_compuesto[i], na.rm = TRUE) + 1L
    d$cupo[i]   <- as.integer(titulares_de_formacion(d$formacion[i])[d$subgrupo[i]])
  }

  # Rendimiento despues (torneo k, en el club destino)
  rend <- perf_t %>%
    filter(torneo == fila$torneo) %>%
    select(player_id, team_post = team_id, z_post = z_compuesto,
           obv_post = obv_p90_res, min_post_perf = minutos_total)

  d <- d %>%
    left_join(rend, by = c("player_id", "team_post")) %>%
    mutate(
      torneo_corte = fila$torneo, orden = fila$orden_torneo, k = fila$k,
      titular_proy = as.integer(puesto <= cupo),
      salto_nivel  = nivel_destino - nivel_origen,
      exito        = as.integer(pct_min_post >= UMBRAL_EXITO)
    )

  # Lista de candidatos que el modelo le habria dado a CADA club, por
  # posicion, con lo que se sabia antes del torneo. Despues se marca
  # a quien fichó cada club y como le fue a cada candidato.
  clubes_k <- unique(post$team_post)
  listas <- map_dfr(clubes_k, function(D) {
    map_dfr(SUBS_ORDEN, function(sg) {
      cand <- ref %>% filter(subgrupo == sg, team_id != D)
      if (nrow(cand) < 10) return(NULL)
      riv <- ref %>% filter(team_id == D, subgrupo == sg)
      sc <- puntuar_para_club(cand, riv, D, ctx_pre)
      sc %>% mutate(team_dest = D, subgrupo = sg, n_lista = nrow(sc))
    })
  })
  if (nrow(listas) > 0) {
    listas <- listas %>%
      left_join(post, by = "player_id") %>%
      left_join(rend, by = c("player_id", "team_post")) %>%
      mutate(torneo_corte = fila$torneo, orden = fila$orden_torneo,
             fichado_por_club = !is.na(team_post) & team_post == team_dest & team_post != team_id,
             cambio_club = !is.na(team_post) & team_post != team_id)
  }

  list(d = d, listas = listas)
}

# Puntua a los candidatos de una posicion para un club concreto.
#   calidad   — percentil del z compuesto con contraccion dentro de la lista
#   necesidad — fortaleza del candidato en los indices donde el club
#               esta por debajo de la media en esa posicion
#   afinidad  — cercania entre el contexto del candidato y el del club
#   estilo    — si el perfil es del tipo que el estilo del club aprovecha
# Dos puntuaciones fijadas de antemano (no se ajustan a resultados):
#   CIDE OSOS = promedio de calidad, necesidad y afinidad
#   Solo encaje = promedio de afinidad, necesidad y estilo (logica del modulo Scouting)
puntuar_para_club <- function(cand, riv, D, ctx_pre) {
  Z <- as.matrix(cand[, DIMS_FIT]); Z[is.na(Z)] <- 0
  z_club <- if (nrow(riv) == 0) rep(-1, length(DIMS_FIT))
            else vapply(DIMS_FIT, function(z) mean(riv[[z]], na.rm = TRUE), numeric(1))
  z_club[is.na(z_club)] <- 0
  necesidad <- round(100 * (1 - exp(-drop(pmax(Z, 0) %*% pmax(-z_club, 0)) / 1.5)), 1)

  cr_d <- ctx_pre[ctx_pre$team_id == D, ]
  sd_pos <- sd(ctx_pre$pos_club, na.rm = TRUE); sd_ppda <- sd(ctx_pre$ppda_club, na.rm = TRUE)
  afinidad <- if (nrow(cr_d) == 0) rep(50, nrow(cand))
              else afinidad_contexto(cand$posesion_media, cand$ppda_media,
                                     cr_d$pos_club[1], cr_d$ppda_club[1], sd_pos, sd_ppda)
  afinidad[is.na(afinidad)] <- 50

  zc <- function(x, v) { s <- sd(x, na.rm = TRUE); if (is.na(s) || s == 0 || is.na(v)) 0
                          else (v - mean(x, na.rm = TRUE)) / s }
  estilo <- rep(50, nrow(cand))
  if (nrow(cr_d) > 0) {
    zp <- zc(ctx_pre$pos_club, cr_d$pos_club[1]); zq <- zc(ctx_pre$ppda_club, cr_d$ppda_club[1])
    zd <- zc(ctx_pre$dir_club, cr_d$dir_club[1])
    deseo <- pmax(0, c(0.35 * zd + 0.15 * (-zp), 0.45 * zp - 0.20 * zd, 0.40 * (-zq), 0.45 * zp, 0))
    if (sum(deseo) > 0) estilo <- round(100 * pnorm(drop(Z %*% (deseo / sum(deseo)))), 1)
  }

  zf <- ifelse(is.na(cand$z_compuesto_fiable), 0, cand$z_compuesto_fiable)
  calidad <- round(100 * (rank(zf) - 0.5) / length(zf), 1)

  tibble(player_id = cand$player_id, player_name = cand$player_name,
         team_id = cand$team_id, club_origen = cand$club, edad = cand$edad,
         minutos_pre = cand$minutos_total, z_fiable_pre = zf,
         calidad, necesidad, afinidad, estilo) %>%
    mutate(score_osos  = (calidad + necesidad + afinidad) / 3,
           score_encaje = (afinidad + necesidad + estilo) / 3,
           rank_osos   = rank(-score_osos,  ties.method = "min"),
           rank_encaje  = rank(-score_encaje, ties.method = "min"),
           pct_osos    = round(100 * (rank(score_osos)  - 0.5) / n(), 1),
           pct_encaje   = round(100 * (rank(score_encaje) - 0.5) / n(), 1))
}

res_cortes  <- map(split(cortes, seq_len(nrow(cortes))), evaluar_corte)
panel       <- map_dfr(res_cortes, "d")
listas_club <- map_dfr(res_cortes, "listas")

# Imputacion simple para que todos los modelos usen las mismas filas
med <- function(x, d) { m <- median(x, na.rm = TRUE); ifelse(is.na(x), ifelse(is.na(m), d, m), x) }
panel <- panel %>%
  mutate(
    pct_min_pre   = pmin(100, replace_na(pct_minutos, 0)),
    z_fiable      = replace_na(z_compuesto_fiable, 0),
    edad          = med(edad, 26),
    pct_conv_pre  = med(pct_convocatorias, 60),
    lesion_previa = as.integer(replace_na(lesiones_relevantes, 0) > 0),
    afinidad      = med(afinidad, 50),
    necesidad     = med(necesidad, 0),
    nivel_origen  = med(nivel_origen, 70),
    salto_nivel   = med(salto_nivel, 0),
    titular_proy  = replace_na(titular_proy, 0L)
  )
saveRDS(panel, file.path(DIR_VAL, "panel_cortes.rds"))

tras <- panel %>% filter(traspaso)
cat("\n  Jugadores evaluados en cortes:", nrow(panel),
    "| traspasos dentro de Liga MX:", nrow(tras),
    "| tasa de exito:", round(100 * mean(tras$exito), 1), "%\n\n")

# ── 3. Modelos con origen rodante ─────────────────────────────
cat("[2/5] Modelos de fichajes con evaluacion de origen rodante...\n")

FORMULAS <- list(
  "M0 Mercado"          = exito ~ pct_min_pre + nivel_origen,
  "M1 + Rendimiento"    = exito ~ pct_min_pre + nivel_origen + z_fiable,
  "M2 + Edad y fisico"  = exito ~ pct_min_pre + nivel_origen + z_fiable +
                                  edad + I(edad^2) + pct_conv_pre + lesion_previa,
  "M3 + Encaje"         = exito ~ pct_min_pre + nivel_origen + z_fiable +
                                  edad + I(edad^2) + pct_conv_pre + lesion_previa +
                                  afinidad + necesidad + titular_proy + salto_nivel
)


predecir_rodante <- function(datos, formulas, min_train = 40) {
  ord <- sort(unique(datos$orden))
  map_dfr(names(formulas), function(mn) {
    map_dfr(ord, function(o) {
      tr <- datos[datos$orden < o, ]; te <- datos[datos$orden == o, ]
      if (nrow(tr) < min_train || sum(tr$exito) < 8 || sum(1 - tr$exito) < 8 || nrow(te) == 0)
        return(NULL)
      fit <- suppressWarnings(glm(formulas[[mn]], family = binomial, data = tr))
      tibble(modelo = mn, orden = o, player_id = te$player_id, exito = te$exito,
             p = suppressWarnings(predict(fit, te, type = "response")))
    })
  })
}

oos <- predecir_rodante(tras, FORMULAS)
if (nrow(oos) == 0) stop("No hubo suficientes traspasos para estimar los modelos.")

# Mismas observaciones para todos los modelos
comunes <- oos %>% count(orden, player_id) %>% filter(n == length(FORMULAS))
oos <- oos %>% semi_join(comunes, by = c("orden", "player_id"))

resumir_modelos <- function(oos, base_mod) {
  claves <- oos %>% distinct(orden, player_id)
  ancho <- oos %>% select(modelo, orden, player_id, exito, p) %>%
    pivot_wider(names_from = modelo, values_from = p)
  mods <- unique(oos$modelo)
  boot <- replicate(N_BOOT, {
    b <- ancho[sample(nrow(ancho), replace = TRUE), ]
    vapply(mods, function(m) auc(b$exito, b[[m]]), numeric(1))
  })
  map_dfr(mods, function(m) {
    p <- ancho[[m]]; y <- ancho$exito
    dif <- boot[m, ] - boot[base_mod, ]
    tibble(
      modelo = m,
      n = length(y),
      auc = auc(y, p),
      auc_lo = quantile(boot[m, ], 0.025, na.rm = TRUE),
      auc_hi = quantile(boot[m, ], 0.975, na.rm = TRUE),
      brier = mean((p - y)^2),
      dif_auc_vs_base = auc(y, p) - auc(y, ancho[[base_mod]]),
      dif_lo = quantile(dif, 0.025, na.rm = TRUE),
      dif_hi = quantile(dif, 0.975, na.rm = TRUE)
    )
  })
}

tabla_modelos <- resumir_modelos(oos, "M0 Mercado")
cat("\n--- FICHAJES: desempeno fuera de muestra (origen rodante) ---\n")
print(as.data.frame(tabla_modelos %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

# Tasa de exito por quintil de probabilidad predicha
lift <- oos %>%
  filter(modelo %in% c("M0 Mercado", "M3 + Encaje")) %>%
  group_by(modelo) %>%
  mutate(quintil = ntile(p, 5)) %>%
  group_by(modelo, quintil) %>%
  summarise(tasa_exito = mean(exito), p_media = mean(p), n = n(), .groups = "drop")

# ── 4. Modelo final con errores robustos por jugador ──────────
cat("\n[3/5] Modelo final (muestra completa) con errores agrupados por jugador...\n")

se_cluster <- function(fit, cluster) {
  X <- model.matrix(fit)
  e <- fit$y - fit$fitted.values
  pan <- vcov(fit)
  sc <- rowsum(X * e, cluster)
  G <- nrow(sc)
  V <- (G / (G - 1)) * pan %*% crossprod(sc) %*% pan
  sqrt(diag(V))
}

fit_final <- suppressWarnings(glm(FORMULAS[["M3 + Encaje"]], family = binomial, data = tras))
se_cl <- se_cluster(fit_final, tras$player_id)
coefs <- tibble(
  variable = names(coef(fit_final)),
  coef = coef(fit_final),
  se_cluster = se_cl,
  z = coef / se_cluster,
  p_valor = 2 * pnorm(-abs(z)),
  odds_ratio = exp(coef)
)
cat("\n--- M3: coeficientes (errores agrupados por jugador) ---\n")
print(as.data.frame(coefs %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# Rendimiento despues del traspaso (entre quienes jugaron lo suficiente)
rend <- tras %>% filter(!is.na(z_post), min_post_perf >= MIN_POST_REND)
cor_boot <- function(x, y) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 15) return(c(NA, NA, NA, length(x)))
  r <- cor(x, y, method = "spearman")
  b <- replicate(N_BOOT, { i <- sample(length(x), replace = TRUE)
                           suppressWarnings(cor(x[i], y[i], method = "spearman")) })
  c(r, quantile(b, c(.025, .975), na.rm = TRUE), length(x))
}
tabla_rend <- bind_rows(
  setNames(as.list(cor_boot(rend$z_fiable,    rend$z_post)),   c("rho", "lo", "hi", "n")) %>%
    as_tibble() %>% mutate(predictor = "z compuesto ex ante", resultado = "z compuesto en el club nuevo"),
  setNames(as.list(cor_boot(rend$pct_min_pre, rend$z_post)),   c("rho", "lo", "hi", "n")) %>%
    as_tibble() %>% mutate(predictor = "% minutos ex ante", resultado = "z compuesto en el club nuevo"),
  setNames(as.list(cor_boot(rend$z_fiable,    rend$obv_post)), c("rho", "lo", "hi", "n")) %>%
    as_tibble() %>% mutate(predictor = "z compuesto ex ante", resultado = "OBV en el club nuevo"),
  setNames(as.list(cor_boot(rend$pct_min_pre, rend$obv_post)), c("rho", "lo", "hi", "n")) %>%
    as_tibble() %>% mutate(predictor = "% minutos ex ante", resultado = "OBV en el club nuevo")
) %>% select(predictor, resultado, rho, lo, hi, n)
cat("\n--- Rendimiento tras el traspaso (Spearman, IC 95% bootstrap) ---\n")
print(as.data.frame(tabla_rend %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# ── 4b. ¿Rinden los fichajes que el modelo recomendaba? ───────
cat("\n[3b] Fichajes recomendados vs no recomendados...\n")

nombres_eq <- cr$tm %>%
  distinct(team_id = as.integer(team_id), team_name = as.character(team_name)) %>%
  group_by(team_id) %>% slice_tail(n = 1) %>% ungroup()

# Recomendacion GENERAL: tercio del z con contraccion entre los jugadores
# de su posicion en toda la liga, en ese corte.
ref_pct <- panel %>% filter(cumple_minimo) %>% select(orden, subgrupo, z_fiable)
pct_liga <- mapply(function(o, sg, z) {
  r <- ref_pct$z_fiable[ref_pct$orden == o & ref_pct$subgrupo == sg]
  if (length(r) < 10) NA_real_ else 100 * mean(r <= z)
}, tras$orden, tras$subgrupo, tras$z_fiable)

tercio <- function(p, n_ok = TRUE) {
  case_when(is.na(p) | !n_ok ~ "Sin datos suficientes",
            p >= 200 / 3 ~ "Recomendado",
            p <= 100 / 3 ~ "No recomendado",
            TRUE ~ "Neutral")
}

tras <- tras %>%
  mutate(pct_liga = pct_liga) %>%
  left_join(listas_club %>%
              select(orden, team_post = team_dest, subgrupo, player_id,
                     pct_osos, rank_osos, pct_encaje, rank_encaje, n_lista,
                     calidad, necesidad_lista = necesidad, estilo),
            by = c("orden", "team_post", "subgrupo", "player_id")) %>%
  left_join(nombres_eq %>% rename(team_post = team_id, club_destino = team_name), by = "team_post") %>%
  mutate(
    jugo      = !is.na(min_post_perf) & min_post_perf >= MIN_POST_REND,
    rindio    = jugo & !is.na(z_post) & z_post > 0,
    resultado = case_when(!jugo ~ "No jugó lo suficiente",
                          rindio ~ "Jugó y rindió", TRUE ~ "Jugó y no rindió"),
    rec_liga  = tercio(pct_liga, cumple_minimo),
    rec_osos  = tercio(pct_osos),
    rec_encaje = tercio(pct_encaje)
  )

resumir_rec <- function(d, col, esquema) {
  d %>%
    mutate(grupo = .data[[col]]) %>%
    group_by(grupo) %>%
    summarise(
      n = n(),
      pct_jugo = 100 * mean(jugo),
      n_jugaron = sum(jugo),
      pct_rindio_si_jugo = if (sum(jugo) > 0) 100 * mean(z_post[jugo] > 0, na.rm = TRUE) else NA_real_,
      z_post_medio = if (sum(jugo) > 0) mean(z_post[jugo], na.rm = TRUE) else NA_real_,
      pct_rindio_total = 100 * mean(rindio),
      pct_min_post_medio = mean(pct_min_post, na.rm = TRUE),
      .groups = "drop") %>%
    mutate(esquema = esquema, .before = 1)
}

# Diferencia Recomendado - No recomendado con IC bootstrap y prueba de Fisher
contraste_rec <- function(d, col, esquema) {
  r <- d[d[[col]] == "Recomendado", ]; nr <- d[d[[col]] == "No recomendado", ]
  if (nrow(r) < 5 || nrow(nr) < 5) return(NULL)
  f <- function(a, b, fn) fn(a) - fn(b)
  m_jugo   <- function(x) 100 * mean(x$jugo)
  m_rinde  <- function(x) { j <- x[x$jugo, ]; if (nrow(j) == 0) NA else 100 * mean(j$z_post > 0, na.rm = TRUE) }
  m_total  <- function(x) 100 * mean(x$rindio)
  boot <- replicate(N_BOOT, {
    rb <- r[sample(nrow(r), replace = TRUE), ]; nb <- nr[sample(nrow(nr), replace = TRUE), ]
    c(f(rb, nb, m_jugo), f(rb, nb, m_rinde), f(rb, nb, m_total))
  })
  fisher_p <- function(a, b) tryCatch(fisher.test(matrix(c(sum(a), length(a) - sum(a),
                                                           sum(b), length(b) - sum(b)), 2))$p.value,
                                      error = function(e) NA_real_)
  rj <- r[r$jugo, ]; nj <- nr[nr$jugo, ]
  tibble(
    esquema = esquema,
    medida = c("Jugó 450+ min (pp)", "Rindió, entre los que jugaron (pp)", "Rindió, del total (pp)"),
    diferencia = c(f(r, nr, m_jugo), f(r, nr, m_rinde), f(r, nr, m_total)),
    ic_inf = apply(boot, 1, quantile, 0.025, na.rm = TRUE),
    ic_sup = apply(boot, 1, quantile, 0.975, na.rm = TRUE),
    p_fisher = c(fisher_p(r$jugo, nr$jugo),
                 fisher_p(rj$z_post > 0, nj$z_post > 0),
                 fisher_p(r$rindio, nr$rindio))
  )
}

ESQUEMAS <- c(rec_osos = "Para el club: calidad + encaje (calidad + necesidad + afinidad)",
              rec_encaje = "Para el club: solo encaje (afinidad + necesidad + estilo)",
              rec_liga = "General: calidad en la liga")

tabla_rec <- map_dfr(names(ESQUEMAS), function(cn) resumir_rec(tras, cn, ESQUEMAS[[cn]])) %>%
  mutate(grupo = factor(grupo, levels = c("Recomendado", "Neutral", "No recomendado",
                                          "Sin datos suficientes"))) %>%
  arrange(esquema, grupo)
tabla_contraste <- map_dfr(names(ESQUEMAS), function(cn) contraste_rec(tras, cn, ESQUEMAS[[cn]]))

cat("\n--- ¿Rinden los fichajes que el modelo recomendaba? ---\n")
print(as.data.frame(tabla_rec %>% mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)
cat("\n--- Recomendado menos No recomendado (puntos porcentuales, IC 95%) ---\n")
print(as.data.frame(tabla_contraste %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# Casos con nombre: para la presentacion
ejemplos_rec <- tras %>%
  filter(rec_osos %in% c("Recomendado", "No recomendado")) %>%
  transmute(torneo = torneo_corte, jugador = player_name, posicion = subgrupo_lab,
            edad = round(edad), club_origen = club, club_destino,
            puesto_en_lista = paste0(rank_osos, " de ", n_lista),
            recomendacion = rec_osos, z_ex_ante = round(z_fiable, 2),
            min_despues = min_post_perf, pct_min_despues = round(pct_min_post),
            z_despues = round(z_post, 2), resultado) %>%
  arrange(desc(torneo), recomendacion, desc(z_despues))

# ── 4c. Heckman en dos etapas ─────────────────────────────────
# Etapa 1 (probit): ¿juega 450+ minutos en el club nuevo?
# Etapa 2 (MCO): cuanto rinde, con la razon inversa de Mills.
# Restricciones de exclusion — mueven la OPORTUNIDAD de jugar pero no
# la calidad: minutos previos, si entraria al once segun la plantilla
# ex ante y la necesidad del club destino en esa posicion.
# Errores: bootstrap por jugador (re-estima ambas etapas).
cat("\n[3c] Heckman en dos etapas...\n")

hk <- tras %>%
  mutate(s = as.integer(jugo),
         encaje_club = replace_na(pct_osos, 50))

F_SEL <- s ~ z_fiable + edad + I(edad^2) + nivel_origen + salto_nivel + afinidad +
             pct_min_pre + titular_proy + necesidad
F_OUT <- z_post ~ z_fiable + edad + I(edad^2) + nivel_origen + salto_nivel + afinidad

heckman_2e <- function(d) {
  pr  <- suppressWarnings(glm(F_SEL, family = binomial(link = "probit"), data = d))
  xb  <- pmax(-8, predict(pr, newdata = d, type = "link"))
  d$imr <- dnorm(xb) / pnorm(xb)
  out <- lm(update(F_OUT, . ~ . + imr), data = d[d$s == 1, ])
  ing <- lm(F_OUT, data = d[d$s == 1, ])
  list(probit = coef(pr), heckman = coef(out), ingenuo = coef(ing))
}

hk_est <- heckman_2e(hk)
filas_jug <- split(seq_len(nrow(hk)), hk$player_id)
hk_boot <- replicate(N_BOOT %/% 2, {
  b <- sample(names(filas_jug), replace = TRUE)
  db <- hk[unlist(filas_jug[b], use.names = FALSE), ]
  e <- tryCatch(heckman_2e(db), error = function(e) NULL)
  if (is.null(e)) return(rep(NA_real_, length(unlist(hk_est))))
  unlist(e)[names(unlist(hk_est))]
})
ic <- t(apply(hk_boot, 1, quantile, c(0.025, 0.975), na.rm = TRUE))
ee <- apply(hk_boot, 1, sd, na.rm = TRUE)

hk_tabla <- tibble(
  nombre = names(unlist(hk_est)),
  coef = unlist(hk_est), ee_boot = ee, ic_inf = ic[, 1], ic_sup = ic[, 2]
) %>%
  mutate(etapa = case_when(startsWith(nombre, "probit") ~ "1. Probit: juega 450+ min",
                           startsWith(nombre, "heckman") ~ "2. Rendimiento (Heckman)",
                           TRUE ~ "2. Rendimiento (MCO ingenuo)"),
         variable = sub("^(probit|heckman|ingenuo)\\.", "", nombre),
         p_valor = 2 * pnorm(-abs(coef / ee_boot))) %>%
  select(etapa, variable, coef, ee_boot, ic_inf, ic_sup, p_valor)

cat("\n--- Heckman: coeficientes con IC bootstrap por jugador ---\n")
print(as.data.frame(hk_tabla %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("  imr = razon inversa de Mills. Si su IC no incluye 0, hay seleccion y el MCO ingenuo esta sesgado.\n")

# ── 5. V1 y V2: fiabilidad y validez externa ──────────────────
cat("\n[4/5] Fiabilidad test-retest y validez externa contra OBV...\n")

pt <- perf_t %>% filter(minutos_total >= 600) %>%
  select(player_id, team_id, orden_torneo, subgrupo, all_of(DIMS_FIT), z_compuesto, obv_p90_res)
pares <- pt %>%
  inner_join(pt %>% mutate(orden_torneo = orden_torneo - 1L),
             by = c("player_id", "team_id", "orden_torneo"), suffix = c("", "_sig"))

tabla_retest <- map_dfr(c(DIMS_FIT, "z_compuesto", "obv_p90_res"), function(v) {
  tibble(indice = v, r = cor(pares[[v]], pares[[paste0(v, "_sig")]], use = "complete.obs"),
         n = sum(complete.cases(pares[[v]], pares[[paste0(v, "_sig")]])))
}) %>%
  mutate(etiqueta = recode(indice,
    z_idx_ofensivo = "Ofensivo", z_idx_creacion = "Creación", z_idx_defensivo = "Defensivo",
    z_idx_progresion = "Progresión", z_idx_consistencia = "Consistencia",
    z_compuesto = "Z compuesto", obv_p90_res = "OBV (referencia)"))
cat("\n--- V1: correlacion entre torneos consecutivos, mismo club, 600+ min ---\n")
print(as.data.frame(tabla_retest %>% select(etiqueta, r, n) %>% mutate(r = round(r, 3))), row.names = FALSE)

tabla_obv <- bind_rows(
  perf_t %>% filter(minutos_total >= 600) %>%
    group_by(subgrupo) %>%
    summarise(tipo = "Concurrente: z compuesto vs OBV mismo torneo",
              r = cor(z_compuesto, obv_p90_res, use = "complete.obs"), n = n(), .groups = "drop"),
  pares %>% group_by(subgrupo) %>%
    summarise(tipo = "Predictiva: z compuesto (k) vs OBV (k+1)",
              r = cor(z_compuesto, obv_p90_res_sig, use = "complete.obs"), n = n(), .groups = "drop"),
  pares %>% group_by(subgrupo) %>%
    summarise(tipo = "Referencia: OBV (k) vs OBV (k+1)",
              r = cor(obv_p90_res, obv_p90_res_sig, use = "complete.obs"), n = n(), .groups = "drop")
)
cat("\n--- V2: validez externa contra OBV (por subgrupo) ---\n")
print(as.data.frame(tabla_obv %>% mutate(r = round(r, 3)) %>%
                      pivot_wider(id_cols = subgrupo, names_from = tipo, values_from = r)),
      row.names = FALSE)

# ── 6. V3: canteranos ─────────────────────────────────────────
cat("\n[5/5] Backtest de canteranos...\n")

ult_completo <- max(torneos$k[torneos$completo])
consolidacion <- resultado_torneo %>%
  inner_join(torneos %>% select(torneo, k), by = "torneo") %>%
  select(player_id, k, pct_min)

cant <- panel %>%
  filter(edad <= EDAD_CANTERA, k + 3 <= ult_completo) %>%
  select(player_id, k, orden, pct_min_pre, z_fiable, edad, minutos_total, subgrupo) %>%
  distinct(player_id, k, .keep_all = TRUE)

# Exito: 50%+ de los minutos de su club en alguno de los torneos k+2 o
# k+3 (un ano despues del corte), en cualquier club de la liga.
fut <- consolidacion %>% rename(k_fut = k)
cant$exito <- vapply(seq_len(nrow(cant)), function(i) {
  f <- fut$pct_min[fut$player_id == cant$player_id[i] &
                   fut$k_fut %in% (cant$k[i] + 2):(cant$k[i] + 3)]
  as.integer(length(f) > 0 && max(f, na.rm = TRUE) >= UMBRAL_EXITO)
}, integer(1))

FORM_CANT <- list(
  "C0 Minutos y edad" = exito ~ pct_min_pre + edad,
  "C1 + Rendimiento"  = exito ~ pct_min_pre + edad + z_fiable
)
tabla_cant <- NULL; oos_cant <- NULL; fit_cant <- NULL
if (nrow(cant) >= 60 && sum(cant$exito) >= 10) {
  oos_cant <- predecir_rodante(cant, FORM_CANT, min_train = 30)
  if (nrow(oos_cant) > 0) {
    com <- oos_cant %>% count(orden, player_id) %>% filter(n == length(FORM_CANT))
    oos_cant <- oos_cant %>% semi_join(com, by = c("orden", "player_id"))
    tabla_cant <- resumir_modelos(oos_cant, "C0 Minutos y edad")
    cat("\n--- CANTERANOS: ¿quien se consolida un ano despues? ---\n")
    cat("  Jugadores <=", EDAD_CANTERA, "anos evaluados:", nrow(cant),
        "| se consolidaron:", sum(cant$exito), "\n")
    print(as.data.frame(tabla_cant %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
          row.names = FALSE)
  }
  fit_cant <- suppressWarnings(glm(FORM_CANT[["C1 + Rendimiento"]], family = binomial, data = cant))
} else {
  cat("  Muestra insuficiente de canteranos para un backtest.\n")
}

# ── 7. Guardar ────────────────────────────────────────────────
recortar_glm <- function(f) {
  if (is.null(f)) return(NULL)
  f$data <- NULL; f$model <- NULL; f$residuals <- NULL; f$weights <- NULL
  f$prior.weights <- NULL; f$linear.predictors <- NULL; f$y <- NULL
  f$fitted.values <- NULL; f$effects <- NULL; f$qr$qr <- NULL
  f
}

resultados <- list(
  parametros = list(umbral_exito = UMBRAL_EXITO, ventana_pre = VENTANA_PRE,
                    edad_cantera = EDAD_CANTERA, cortes = cortes$torneo),
  resumen = list(n_panel = nrow(panel), n_traspasos = nrow(tras),
                 tasa_exito = mean(tras$exito)),
  modelos = tabla_modelos, lift = lift, coeficientes = coefs,
  rendimiento = tabla_rend, retest = tabla_retest, obv = tabla_obv,
  canteranos = tabla_cant, n_canteranos = nrow(cant),
  tasa_cant = if (nrow(cant) > 0) mean(cant$exito) else NA,
  recomendacion = tabla_rec, contraste = tabla_contraste, ejemplos = ejemplos_rec,
  heckman = hk_tabla, min_post_rend = MIN_POST_REND,
  carrera = tabla_carrera, metodo_elegido = GANADOR, metodo_pipeline = metodo_pipe,
  carrera_indices = tabla_carrera_ind, indices_elegidos = GANADOR_IND, indices_pipeline = indices_pipe,
  carrera_bp = tabla_carrera_bp, bp_elegido = GANADOR_BP, bp_pipeline = bp_pipe, revulsivo = tabla_rev
)
# Listas por club con nombres, para el explorador de la app
saveRDS(listas_club %>%
          left_join(nombres_eq %>% rename(team_dest = team_id, club_lista = team_name), by = "team_dest") %>%
          left_join(nombres_eq %>% rename(team_post = team_id, club_despues = team_name), by = "team_post") %>%
          mutate(posicion = unname(ETIQUETA_SUB[subgrupo])),
        file.path(DIR_VAL, "listas_por_club.rds"))
saveRDS(resultados, file.path(DIR_VAL, "resultados.rds"))
saveRDS(list(fichajes = recortar_glm(fit_final), canteranos = recortar_glm(fit_cant),
             umbral_exito = UMBRAL_EXITO),
        file.path(DIR_VAL, "modelos_finales.rds"))
saveRDS(oos, file.path(DIR_VAL, "predicciones_oos.rds"))

# ── 8. Graficas para la presentacion ──────────────────────────
AZUL  <- "#2E6DA4"; DORADO <- "#B8862B"
TINTA <- "#0A1628"; GRIS <- "#6B7280"; REJILLA <- "#E5E7EB"

tema_osos <- function() {
  theme_minimal(base_size = 12) +
    theme(text = element_text(color = TINTA),
          plot.title = element_text(face = "bold", size = 15, margin = margin(b = 4)),
          plot.subtitle = element_text(color = GRIS, size = 10.5, margin = margin(b = 14)),
          plot.caption = element_text(color = GRIS, size = 8.5, hjust = 0, margin = margin(t = 12)),
          plot.title.position = "plot", plot.caption.position = "plot",
          axis.text = element_text(color = GRIS), axis.title = element_text(color = GRIS, size = 10),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = REJILLA, linewidth = 0.3),
          legend.position = "top", legend.justification = "left", legend.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(18, 22, 14, 18))
}
pie <- paste0(FUENTE, " Backtest de origen rodante sobre ", nrow(tras),
              " traspasos entre clubes de Liga MX (", paste(range(cortes$torneo), collapse = " a "),
              "). ", FIRMA_OSOS)

# G1 — AUC por modelo
g1 <- tabla_modelos %>%
  mutate(modelo = factor(modelo, levels = rev(names(FORMULAS))),
         rol = ifelse(modelo == "M0 Mercado", "Referencia: lo que ya ve el mercado",
                      "Modelo CIDE OSOS")) %>%
  ggplot(aes(y = modelo, x = auc, color = rol)) +
  geom_vline(xintercept = 0.5, color = GRIS, linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(xmin = auc_lo, xmax = auc_hi), width = 0, linewidth = 1.1, orientation = "y") +
  geom_point(size = 3.6) +
  geom_text(aes(label = sprintf("%.2f", auc)), nudge_y = 0.32, size = 3.6,
            color = TINTA, fontface = "bold") +
  scale_color_manual(values = c("Referencia: lo que ya ve el mercado" = AZUL,
                                "Modelo CIDE OSOS" = DORADO)) +
  scale_x_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  labs(title = "Qué tan bien separa cada modelo los fichajes que funcionan de los que no",
       subtitle = paste0("AUC fuera de muestra con IC 95%. 0.5 = azar. Éxito: jugar ",
                         UMBRAL_EXITO, "% o más de los minutos del nuevo club en su primer torneo"),
       x = "AUC", y = NULL, caption = pie) +
  tema_osos()
ggsave(file.path(DIR_FIG, "01_auc_modelos.png"), g1, width = 10, height = 5.2, dpi = 300, bg = "white")

# G2 — Tasa de exito por quintil de probabilidad. El titulo se decide
# con los datos: solo afirma que el modelo separa mejor si es cierto.
sup_m3 <- lift$tasa_exito[lift$modelo == "M3 + Encaje" & lift$quintil == 5]
sup_m0 <- lift$tasa_exito[lift$modelo == "M0 Mercado"  & lift$quintil == 5]
titulo_g2 <- if (length(sup_m3) && length(sup_m0) && sup_m3 > sup_m0 + 0.05) {
  paste0("Entre los fichajes que el modelo marca como más seguros, se consolida el ",
         round(100 * sup_m3), "%")
} else "Tasa real de éxito según la probabilidad que asignó cada modelo"

g2 <- lift %>%
  mutate(modelo = ifelse(modelo == "M0 Mercado", "Referencia: M0 Mercado", "CIDE OSOS: M3 completo")) %>%
  ggplot(aes(x = quintil, y = 100 * tasa_exito, color = modelo)) +
  geom_hline(yintercept = 100 * mean(oos$exito[oos$modelo == "M0 Mercado"]),
             color = GRIS, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 3.2) +
  geom_text(data = . %>% filter(quintil %in% c(1, 5)) %>%
              mutate(dy = ifelse(modelo == "Referencia: M0 Mercado", -4.5, 4.5)),
            aes(y = 100 * tasa_exito + dy, label = paste0(round(100 * tasa_exito), "%")),
            size = 3.5, show.legend = FALSE, color = TINTA) +
  scale_color_manual(values = c("Referencia: M0 Mercado" = AZUL, "CIDE OSOS: M3 completo" = DORADO)) +
  scale_x_continuous(breaks = 1:5, labels = c("20% menos\nprobable", "2", "3", "4", "20% más\nprobable")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = titulo_g2,
       subtitle = "Tasa real de éxito por quintil de probabilidad predicha (fuera de muestra). Línea punteada: tasa promedio",
       x = "Quintil de probabilidad predicha", y = "Tasa de éxito", caption = pie) +
  tema_osos()
ggsave(file.path(DIR_FIG, "02_exito_por_quintil.png"), g2, width = 10, height = 5.6, dpi = 300, bg = "white")

# G3 — Fiabilidad test-retest
g3 <- tabla_retest %>%
  mutate(etiqueta = factor(etiqueta, levels = etiqueta[order(r)])) %>%
  ggplot(aes(y = etiqueta, x = r)) +
  geom_col(width = 0.55, fill = AZUL) +
  geom_text(aes(label = sprintf("%.2f", r)), hjust = -0.2, size = 3.6, color = TINTA) +
  scale_x_continuous(limits = c(min(0, min(tabla_retest$r, na.rm = TRUE)), 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Cuánto del índice es rasgo del jugador y cuánto es ruido",
       subtitle = "Correlación entre torneos consecutivos, mismo jugador y mismo club, 600+ minutos en ambos",
       x = "Correlación test-retest", y = NULL,
       caption = paste(FUENTE, FIRMA_OSOS)) +
  tema_osos() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(DIR_FIG, "03_fiabilidad.png"), g3, width = 10, height = 5, dpi = 300, bg = "white")

# G4 — Rendimiento en el club nuevo segun cada predictor ex ante.
# Es la comparacion central: los minutos previos (lo que usa el mercado)
# frente al indice. El titulo solo afirma una diferencia si la hay.
g4_d <- tabla_rend %>%
  filter(!is.na(rho)) %>%
  mutate(predictor = ifelse(predictor == "z compuesto ex ante",
                            "Índice CIDE OSOS ex ante", "Minutos previos (mercado)"),
         resultado = ifelse(resultado == "z compuesto en el club nuevo",
                            "Rendimiento (z compuesto)", "OBV de StatsBomb"),
         resultado = factor(resultado, levels = c("OBV de StatsBomb", "Rendimiento (z compuesto)")))
rz <- g4_d$rho[g4_d$predictor == "Índice CIDE OSOS ex ante" & g4_d$resultado == "Rendimiento (z compuesto)"]
rm <- g4_d$rho[g4_d$predictor == "Minutos previos (mercado)" & g4_d$resultado == "Rendimiento (z compuesto)"]
lo_z <- g4_d$lo[g4_d$predictor == "Índice CIDE OSOS ex ante" & g4_d$resultado == "Rendimiento (z compuesto)"]
titulo_g4 <- if (length(rz) && length(rm) && !is.na(lo_z) && lo_z > 0 && rz > rm + 0.1) {
  "El índice anticipa cómo rinde un fichaje en su club nuevo; los minutos previos no"
} else "Qué anticipa el rendimiento de un fichaje en su club nuevo"

if (nrow(g4_d) > 0) {
  g4 <- ggplot(g4_d, aes(y = resultado, x = rho, color = predictor, group = predictor)) +
    geom_vline(xintercept = 0, color = GRIS, linetype = "dashed", linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 1.1, orientation = "y",
                  position = position_dodge(width = 0.5)) +
    geom_point(size = 3.6, position = position_dodge(width = 0.5)) +
    geom_text(aes(label = sprintf("%.2f", rho)), position = position_dodge(width = 0.5),
              vjust = -1.1, size = 3.5, color = TINTA, show.legend = FALSE) +
    scale_color_manual(values = c("Minutos previos (mercado)" = AZUL,
                                  "Índice CIDE OSOS ex ante" = DORADO)) +
    scale_x_continuous(limits = c(min(-0.2, min(g4_d$lo, na.rm = TRUE)), max(0.6, max(g4_d$hi, na.rm = TRUE)))) +
    labs(title = titulo_g4,
         subtitle = paste0("Correlación de Spearman con IC 95% (bootstrap). ", max(g4_d$n),
                           " traspasos con ", MIN_POST_REND, "+ minutos en el club nuevo"),
         x = "Correlación con el rendimiento posterior", y = NULL,
         caption = paste(FUENTE, "Ambos predictores medidos antes del traspaso.", FIRMA_OSOS)) +
    tema_osos()
  ggsave(file.path(DIR_FIG, "04_rendimiento_posterior.png"), g4, width = 10, height = 4.8,
         dpi = 300, bg = "white")
}

# G5 — Fichajes recomendados vs no recomendados PARA ESE CLUB
g5_d <- tabla_rec %>%
  filter(esquema == ESQUEMAS[["rec_osos"]], grupo %in% c("Recomendado", "No recomendado")) %>%
  select(grupo, n, n_jugaron, pct_jugo, pct_rindio_si_jugo) %>%
  pivot_longer(c(pct_jugo, pct_rindio_si_jugo), names_to = "medida", values_to = "pct") %>%
  mutate(medida = ifelse(medida == "pct_jugo", paste0("Jugó ", MIN_POST_REND, "+ minutos"),
                         "Rindió (entre los que jugaron)"),
         n_lab = ifelse(grepl("Jugó", medida), n, n_jugaron),
         grupo = factor(as.character(grupo), levels = c("No recomendado", "Recomendado")))
con <- tabla_contraste %>% filter(esquema == ESQUEMAS[["rec_osos"]])
dif_r <- con$diferencia[grepl("entre los que", con$medida)]
lo_r  <- con$ic_inf[grepl("entre los que", con$medida)]
titulo_g5 <- if (length(dif_r) && !is.na(lo_r) && lo_r > 0) {
  paste0("Los fichajes que el modelo recomendaba para ese club rindieron más: +",
         round(dif_r), " puntos")
} else "¿Rinden más los fichajes que el modelo recomendaba para ese club?"

if (nrow(g5_d) > 0) {
  g5 <- ggplot(g5_d, aes(x = medida, y = pct, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    geom_text(aes(label = paste0(round(pct), "%\n(n=", n_lab, ")")),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 3.4,
              color = TINTA, lineheight = 0.9) +
    scale_fill_manual(values = c("No recomendado" = AZUL, "Recomendado" = DORADO)) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(title = titulo_g5,
         subtitle = paste0("Recomendado: tercio superior de la lista que el modelo habría armado para ese club\n",
                           "y esa posición antes del torneo (calidad + necesidad + afinidad)"),
         x = NULL, y = NULL,
         caption = paste(FUENTE, "Rindió: z compuesto mayor a 0 en el club nuevo.", FIRMA_OSOS)) +
    tema_osos() + theme(panel.grid.major.x = element_blank())
  ggsave(file.path(DIR_FIG, "05_recomendados_por_club.png"), g5, width = 10, height = 5.6,
         dpi = 300, bg = "white")
}

# G6 — Carrera de especificaciones
g6_d <- tabla_carrera %>%
  mutate(especificacion = factor(especificacion, levels = rev(especificacion)),
         rol = ifelse(elegido, "Elegida", "No elegida"))
g6 <- ggplot(g6_d, aes(y = especificacion, x = rho_traspasos, color = rol)) +
  geom_vline(xintercept = 0, color = GRIS, linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(xmin = rho_lo, xmax = rho_hi), width = 0, linewidth = 1.1, orientation = "y") +
  geom_point(size = 3.6) +
  geom_text(aes(label = sprintf("%.2f", rho_traspasos)), vjust = -1.1, size = 3.5,
            color = TINTA, show.legend = FALSE) +
  scale_color_manual(values = c("Elegida" = DORADO, "No elegida" = AZUL)) +
  labs(title = paste0("Ajuste elegido: ", METODOS_AJUSTE[[GANADOR]]),
       subtitle = paste0("Correlación del índice ex ante con el OBV sin ajustar en el club nuevo (Spearman, ",
                         "IC 95%). ", tabla_carrera$n_traspasos[1], " traspasos con ", MIN_POST_REND,
                         "+ minutos.\nUn retador solo reemplaza a A si su ventaja tiene IC 95% mayor a cero"),
       x = "Correlación con el OBV posterior", y = NULL,
       caption = paste(FUENTE, "Cada especificación se estimó en cada corte solo con partidos anteriores.",
                       FIRMA_OSOS)) +
  tema_osos()
ggsave(file.path(DIR_FIG, "06_carrera_ajuste.png"), g6, width = 10, height = 4.6, dpi = 300, bg = "white")

# G7 — Carrera de indices
g7_d <- tabla_carrera_ind %>%
  mutate(especificacion = factor(especificacion, levels = rev(especificacion)),
         rol = ifelse(elegido, "Elegida", "No elegida"))
g7 <- ggplot(g7_d, aes(y = especificacion, x = rho_traspasos, color = rol)) +
  geom_vline(xintercept = 0, color = GRIS, linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(xmin = rho_lo, xmax = rho_hi), width = 0, linewidth = 1.1, orientation = "y") +
  geom_point(size = 3.6) +
  geom_text(aes(label = sprintf("%.2f", rho_traspasos)), vjust = -1.1, size = 3.5,
            color = TINTA, show.legend = FALSE) +
  scale_color_manual(values = c("Elegida" = DORADO, "No elegida" = AZUL)) +
  labs(title = paste0("Índices elegidos: ", VERSIONES_INDICES[[GANADOR_IND]]),
       subtitle = paste0("Correlación del índice ex ante con el OBV sin ajustar en el club nuevo (Spearman, ",
                         "IC 95%). ", tabla_carrera_ind$n_traspasos[1], " traspasos con ", MIN_POST_REND,
                         "+ minutos.\nLos completos solo se quedan si superan a los parsimoniosos con IC 95% mayor a cero"),
       x = "Correlación con el OBV posterior", y = NULL,
       caption = paste(FUENTE, "Ajuste:", METODOS_AJUSTE[[GANADOR]], ". Solo partidos anteriores a cada corte.",
                       FIRMA_OSOS)) +
  tema_osos()
ggsave(file.path(DIR_FIG, "07_carrera_indices.png"), g7, width = 10, height = 3.8, dpi = 300, bg = "white")

cat("\n============================================\n")
cat("Validacion terminada\n")
cat("  Resultados: ", file.path(DIR_VAL, "resultados.rds"), "\n")
cat("  Graficas:   ", DIR_FIG, "\n")
cat("============================================\n\n")
