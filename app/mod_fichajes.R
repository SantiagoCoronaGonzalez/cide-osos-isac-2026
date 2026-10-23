# ============================================================
# mod_fichajes.R
# Modulo 10 — Analizador de fichajes
#
# Responde tres preguntas:
#   1. Si este jugador ficha por este club, seria titular?
#   2. Como rendiria en ese contexto, mas alla de la compatibilidad?
#   3. Que le pasa al once tipo si firmo hasta cinco jugadores?
#
# La plantilla titular se dimensiona con la formacion mas usada
# por el club destino en la temporada (formacion del Starting XI en
# las alineaciones de StatsBomb, moda por club).
#
# CIDE OSOS agrega una cuarta respuesta: la probabilidad de que el
# fichaje se consolide (50%+ de minutos en su primer torneo), con el
# modelo M3 estimado en el backtest de 03_validacion.R.
# ============================================================

library(ggplot2)

# ── Paleta (comparte la de visualizaciones) ───────────────────
fic_navy       <- "#0E2240"
fic_navy_mid   <- "#1B3A63"
fic_navy_soft  <- "#1B3A63"
fic_gold       <- "#B8862B"
fic_gold_light <- "#F0D080"
fic_gold_pale  <- "#FAF0D0"
fic_cream      <- "#F3F5F8"
fic_white      <- "#FFFFFF"
fic_red        <- "#B23A3A"
fic_gray       <- "#46566C"
fic_gray_light <- "#DCE1E8"
fic_green      <- "#1E7A5A"

MAX_FICHAJES <- 5

# SUBS_ORDEN, SUBS_ETIQ, TABLA_FORMACIONES, titulares_de_formacion()
# y cargar_formaciones() vienen de R/funciones_modelo.R y
# app/helpers_carga.R, para que la app y el backtest usen lo mismo.

formacion_de_club <- function(formaciones, club) {
  if (is.null(formaciones)) return(list(formacion = NA_character_, pct = NA_real_))
  fila <- formaciones[formaciones$club == club, , drop = FALSE]
  if (nrow(fila) == 0) return(list(formacion = NA_character_, pct = NA_real_))
  list(formacion = fila$formacion[1], pct = fila$pct_uso[1])
}

# ── Perfil de estilo por club, derivado de sus jugadores ──────
estilo_por_club <- function(perfiles) {
  perfiles %>%
    dplyr::filter(cumple_minimo) %>%
    dplyr::group_by(club, liga) %>%
    dplyr::summarise(
      posesion_club = mean(posesion_media, na.rm = TRUE),
      ppda_club     = mean(ppda_media,     na.rm = TRUE),
      cal_comp_club = mean(cal_comp_media, na.rm = TRUE),
      .groups = "drop"
    )
}

# ── Compatibilidad de estilo con el club ──────────────────────
# Fit de sistema entre el contexto REAL del jugador y el club
# destino. Se usan solo las dimensiones que de verdad describen al
# jugador: posesion media y PPDA media del equipo donde jugo esta
# temporada. Es el mismo criterio que la columna Afinidad del modulo
# de Scouting, asi que el numero es consistente entre modulos.
#
# NOTA de diseno: NO se incluyen intensidad de paso, longitud de
# pases ni % pase largo. Esas tres no existen a nivel jugador, y
# rellenarlas con el promedio de la liga metia una distancia
# constante enorme (los clubes extremos como el Barca quedan lejos
# de "la media") que aplastaba a todos los candidatos por igual:
# un jugador del PSG (identico al Barca) salia casi igual que uno de
# un sistema opuesto. Con posesion y PPDA la senal discrimina bien.
compat_estilo_fichaje <- function(j, club_destino, estilos5) {
  if (is.null(estilos5)) return(NA_real_)
  cr <- estilos5[estilos5$club == club_destino, , drop = FALSE]
  if (nrow(cr) == 0) return(NA_real_)
  
  pares <- list(
    list(jug = j$posesion_media[1], col = "posesion_club"),
    list(jug = j$ppda_media[1],     col = "ppda_club")
  )
  
  dist_sq <- 0; usadas <- 0
  for (pr in pares) {
    vc <- cr[[pr$col]][1]
    if (is.null(vc) || is.na(vc) || is.na(pr$jug)) next
    sigma <- sd(estilos5[[pr$col]], na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) next
    dist_sq <- dist_sq + ((vc - pr$jug) / sigma)^2
    usadas  <- usadas + 1
  }
  if (usadas == 0) return(NA_real_)
  round(pmax(0, pmin(100, 100 * exp(-sqrt(dist_sq) / sqrt(usadas)))), 1)
}

# ── Proyeccion del jugador al club destino ────────────────────
# Devuelve dos cifras:
#   z_liga   -> solo re-escalado contra la liga destino
#   z_estilo -> ademas ajustado por el estilo del club destino
proyectar_fichaje <- function(j, club_destino, perfiles, estilos) {
  liga_destino <- estilos$liga[estilos$club == club_destino][1]
  if (is.na(liga_destino)) return(NULL)
  
  sub   <- j$subgrupo[1]
  grupo <- j$grupo_posicion[1]
  # La poblacion de referencia es el SUBGRUPO en la liga destino.
  # Si esa celda queda muy corta, se recurre al grupo completo.
  pop <- perfiles %>%
    dplyr::filter(liga == liga_destino, subgrupo == sub, cumple_minimo)
  ref_usada <- unname(SUBS_ETIQ[sub])
  if (nrow(pop) < 12) {
    pop <- perfiles %>%
      dplyr::filter(liga == liga_destino, grupo_posicion == grupo, cumple_minimo)
    ref_usada <- paste0(grupo, " (muestra de ", SUBS_ETIQ[sub], " insuficiente)")
  }
  if (nrow(pop) < 10) return(NULL)
  
  indices <- c("idx_ofensivo", "idx_creacion", "idx_defensivo",
               "idx_progresion", "idx_consistencia")
  
  # --- 1. Re-escalado contra la liga destino -------------------
  # Se compara el z-score YA estandarizado del jugador (z_idx_*, en
  # escala ~N(0,1) dentro de su subgrupo de origen) contra la
  # distribucion de esos mismos z en el subgrupo de la liga destino.
  #
  # OJO: NO se usa idx_* crudo. Ese indice crudo tiene una escala
  # comprimida y, al re-estandarizarlo contra poblaciones pequenas
  # (p.ej. 2-3 delanteros de un club), la sd diminuta disparaba
  # z-scores absurdos (+6 sigma). Ahora se parte del z ya normalizado
  # y se re-centra sobre la media del subgrupo destino, dividiendo por
  # una sd con piso de 0.75 para no amplificar poblaciones homogeneas.
  # El resultado se acota a +-3.5 sigma, rango realista.
  z_cols <- paste0("z_", indices)   # z_idx_ofensivo, etc.
  z_liga <- as.numeric(vapply(z_cols, function(zc) {
    if (!(zc %in% names(j)) || !(zc %in% names(pop))) return(NA_real_)
    v <- j[[zc]][1]
    m <- mean(pop[[zc]], na.rm = TRUE)
    s <- sd(pop[[zc]],   na.rm = TRUE)
    if (is.na(v)) return(NA_real_)
    if (is.na(s) || s < 0.75) s <- max(s, 0.75, na.rm = TRUE)  # piso de sd
    z <- (v - m) / s
    max(-3.5, min(3.5, z))                                     # cota dura
  }, numeric(1)))
  
  # --- 2. Ajuste adicional por estilo del club destino ---------
  # Elasticidades heuristicas, documentadas y acotadas a +-0.5 sigma.
  e_dest <- estilos[estilos$club == club_destino, ]
  pos_act  <- j$posesion_media[1]
  ppda_act <- j$ppda_media[1]
  
  sd_pos  <- sd(estilos$posesion_club, na.rm = TRUE)
  sd_ppda <- sd(estilos$ppda_club,     na.rm = TRUE)
  sd_pos  <- ifelse(is.na(sd_pos)  || sd_pos  == 0, 1, sd_pos)
  sd_ppda <- ifelse(is.na(sd_ppda) || sd_ppda == 0, 1, sd_ppda)
  
  d_pos  <- (e_dest$posesion_club[1] - pos_act)  / sd_pos
  d_pres <- (ppda_act - e_dest$ppda_club[1])     / sd_ppda   # + = destino presiona mas
  
  d_pos  <- ifelse(is.na(d_pos),  0, d_pos)
  d_pres <- ifelse(is.na(d_pres), 0, d_pres)
  
  # Mismo orden que `indices`: ofensivo, creacion, defensivo,
  # progresion, consistencia. Se suma por posicion, no por nombre.
  ajuste <- as.numeric(c(
    0.10 * d_pos,                   # ofensivo
    0.15 * d_pos,                   # creacion
    0.12 * d_pres - 0.08 * d_pos,   # defensivo
    0.15 * d_pos,                   # progresion
    0                               # consistencia
  ))
  ajuste <- pmax(-0.5, pmin(0.5, ajuste))
  
  z_estilo <- pmax(-3.5, pmin(3.5, z_liga + ajuste))
  
  names(z_liga) <- indices
  names(z_estilo) <- indices
  names(ajuste) <- indices
  
  # Mismos pesos que usa el pipeline (PESOS_SUB en funciones_modelo.R)
  pesos <- pesos_de(sub)
  
  comp <- function(z) {
    if (any(is.na(z))) return(mean(z, na.rm = TRUE))
    sum(z * pesos)
  }
  
  list(
    liga_destino = liga_destino,
    grupo        = grupo,
    subgrupo     = sub,
    ref_usada    = ref_usada,
    z_liga       = z_liga,
    z_estilo     = z_estilo,
    ajuste       = ajuste,
    comp_liga    = comp(z_liga),
    comp_estilo  = comp(z_estilo),
    comp_actual  = j$z_compuesto[1],
    d_pos        = d_pos,
    d_pres       = d_pres,
    n_pop        = nrow(pop)
  )
}

# ── Veredicto: titular, rotacion o suplente ───────────────────
evaluar_fichaje <- function(j, club_destino, perfiles, estilos, cupos) {
  proy <- proyectar_fichaje(j, club_destino, perfiles, estilos)
  if (is.null(proy)) return(NULL)
  
  sub   <- j$subgrupo[1]
  grupo <- proy$grupo
  cupo  <- as.numeric(cupos[sub])
  if (is.na(cupo)) cupo <- 0
  
  plantel <- perfiles %>%
    dplyr::filter(club == club_destino, subgrupo == sub, cumple_minimo) %>%
    dplyr::arrange(dplyr::desc(z_compuesto_fiable))
  
  # El fichaje entra a competir con el z_compuesto proyectado por estilo
  z_nuevo <- proy$comp_estilo
  rivales <- plantel$z_compuesto
  puesto  <- sum(rivales > z_nuevo, na.rm = TRUE) + 1
  
  # Con cupo 0 el subgrupo no existe en esa formacion
  veredicto <- if (cupo == 0) "Fuera del esquema"
  else if (puesto <= cupo) "Titular"
  else if (puesto <= cupo + 2) "Rotacion"
  else "Suplente"
  
  # Mejora sobre el peor titular actual de ese perfil.
  # OJO: en R, rivales[0] devuelve numeric(0), no NA. Sin el
  # cupo >= 1 esta linea explotaba con "argumento tiene longitud cero"
  # cada vez que el perfil no existia en la formacion (cupo 0).
  peor_titular <- if (cupo >= 1 && length(rivales) >= cupo) {
    rivales[cupo]
  } else NA_real_
  if (length(peor_titular) != 1) peor_titular <- NA_real_
  
  mejora <- if (!is.na(peor_titular)) z_nuevo - peor_titular else NA_real_
  
  # El plantel de ese perfil es mas corto que el cupo: entonces
  # cualquier fichaje entra al XI por defecto y el veredicto no
  # discrimina. Solo aplica cuando el perfil si tiene cupo.
  linea_incompleta <- cupo >= 1 && length(rivales) < cupo
  
  list(
    jugador_id  = j$jugador_id[1],
    nombre      = nombre_display(j$jugador_id[1]),
    club_origen = j$club[1],
    liga_origen = j$liga[1],
    grupo       = grupo,
    subgrupo    = sub,
    sub_lab     = unname(SUBS_ETIQ[sub]),
    posicion    = j$posicion[1],
    proy        = proy,
    puesto      = puesto,
    cupo        = cupo,
    n_plantel   = nrow(plantel),
    veredicto   = veredicto,
    mejora      = mejora,
    peor_titular = peor_titular,
    linea_incompleta = linea_incompleta
  )
}

color_veredicto <- function(v) {
  switch(v,
         "Titular"           = fic_green,
         "Rotacion"          = fic_gold,
         "Suplente"          = fic_gray,
         "Fuera del esquema" = fic_red,
         fic_gray)
}

# ── E1: Depth chart con los fichajes insertados ───────────────
plot_depth_chart <- function(club_destino, perfiles, evals, cupos, formacion) {
  filas <- list()
  
  # Subgrupos a mostrar: los que tienen cupo, mas los de algun fichaje
  subs_fichaje <- unique(unlist(lapply(evals, function(e) if (!is.null(e)) e$subgrupo)))
  subs_ver <- SUBS_ORDEN[SUBS_ORDEN %in% union(names(cupos)[cupos > 0], subs_fichaje)]
  
  for (sg in subs_ver) {
    cupo <- as.numeric(cupos[sg]); if (is.na(cupo)) cupo <- 0
    
    actuales <- perfiles %>%
      dplyr::filter(club == club_destino, subgrupo == sg, cumple_minimo) %>%
      dplyr::transmute(nombre = nombre_display(jugador_id),
                       z = z_compuesto, nuevo = FALSE)
    
    nuevos <- purrr::map_dfr(evals, function(e) {
      if (is.null(e) || e$subgrupo != sg) return(NULL)
      data.frame(nombre = e$nombre, z = e$proy$comp_estilo, nuevo = TRUE)
    })
    
    todos <- dplyr::bind_rows(actuales, nuevos)
    if (nrow(todos) == 0) next
    
    todos <- todos %>%
      dplyr::arrange(dplyr::desc(z)) %>%
      dplyr::mutate(
        puesto = dplyr::row_number(),
        rol    = ifelse(puesto <= cupo, "XI titular", "Fuera del XI"),
        grupo  = sg
      ) %>%
      dplyr::slice_head(n = max(cupo, 1) + 3)
    
    filas[[sg]] <- todos
  }
  
  if (length(filas) == 0) return(NULL)
  df <- dplyr::bind_rows(filas)
  
  hacer_lab <- function(sg) {
    n <- as.numeric(cupos[sg]); if (is.na(n)) n <- 0
    if (n == 0) paste0(SUBS_ETIQ[sg], "  (no contemplados en el esquema)")
    else paste0(SUBS_ETIQ[sg], "  (", n, " titular", if (n > 1) "es" else "", ")")
  }
  niveles_lab <- vapply(subs_ver, hacer_lab, character(1))
  df$grupo_lab <- factor(vapply(df$grupo, hacer_lab, character(1)),
                         levels = niveles_lab)
  df$fill <- ifelse(df$nuevo, fic_gold,
                    ifelse(df$rol == "XI titular", fic_navy, fic_gray_light))
  df$etiqueta <- ifelse(df$nuevo, paste0(df$nombre, "  [FICHAJE]"), df$nombre)
  df$orden <- -df$puesto
  
  n_nuevos_xi <- sum(df$nuevo & df$rol == "XI titular")
  
  ggplot(df, aes(x = z, y = reorder(paste0(grupo, "_", puesto), orden),
                 fill = fill)) +
    geom_col(width = 0.62) +
    scale_fill_identity() +
    geom_text(aes(label = etiqueta, x = pmin(z, 0) - 0.06),
              hjust = 1, size = 3.3,
              color = ifelse(df$nuevo, fic_navy, fic_navy_soft),
              fontface = ifelse(df$nuevo, "bold", "plain")) +
    geom_text(aes(label = sprintf("%+.2f", z),
                  x = pmax(z, 0) + 0.06),
              hjust = 0, size = 3.1, color = fic_navy_soft) +
    geom_vline(xintercept = 0, color = fic_gray, linewidth = 0.4) +
    facet_wrap(~grupo_lab, ncol = 1, scales = "free_y") +
    scale_x_continuous(expand = expansion(mult = c(0.42, 0.22))) +
    scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
    labs(
      title = paste0(club_destino, ": ", n_nuevos_xi,
                     " de los fichajes entrarían al once titular"),
      subtitle = paste0("Jerarquia por z-score compuesto dentro de cada línea. ",
                        "Formación de referencia: ", formacion, "\n",
                        "Barra dorada: fichaje evaluado. ",
                        "Barra azul: titular actual. Barra gris: fuera del XI"),
      x = "Z-score compuesto", y = NULL,
      caption = paste0(
        "Nota: los fichajes se ubican con su z-score proyectado al contexto del club destino.\n",
        fuente_texto(TEMPORADA_APP), "\n", FIRMA)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title            = element_text(size = 15, face = "bold", color = fic_navy,
                                           margin = margin(b = 5)),
      plot.subtitle         = element_text(size = 10.5, color = fic_gray,
                                           lineheight = 1.3, margin = margin(b = 16)),
      plot.caption          = element_text(size = 8.5, color = fic_gray, hjust = 0,
                                           lineheight = 1.25, margin = margin(t = 14)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      strip.text            = element_text(size = 11.5, face = "bold", color = fic_navy,
                                           hjust = 0, margin = margin(b = 6, t = 10)),
      strip.background      = element_blank(),
      axis.text.y           = element_blank(),
      axis.text.x           = element_text(size = 9, color = fic_navy_soft),
      axis.title.x          = element_text(size = 9.5, color = fic_gray),
      axis.ticks            = element_blank(),
      panel.grid.major.y    = element_blank(),
      panel.grid.major.x    = element_line(color = fic_gray_light, linewidth = 0.3),
      panel.grid.minor      = element_blank(),
      panel.background      = element_rect(fill = fic_white, color = NA),
      plot.background       = element_rect(fill = fic_white, color = NA),
      panel.spacing.y       = unit(16, "pt"),
      plot.margin           = margin(20, 26, 16, 20)
    )
}

# ── E2: Proyeccion por indice, actual vs destino ──────────────
plot_proyeccion_indices <- function(e) {
  if (is.null(e)) return(NULL)
  indices <- c("idx_ofensivo", "idx_creacion", "idx_defensivo",
               "idx_progresion", "idx_consistencia")
  etiqs <- unname(ETIQUETAS_INDICES[paste0("z_", indices)])
  
  df <- data.frame(
    indice   = factor(etiqs, levels = rev(etiqs)),
    liga     = as.numeric(e$proy$z_liga),
    estilo   = as.numeric(e$proy$z_estilo)
  )
  df_long <- tidyr::pivot_longer(df, c("liga", "estilo"),
                                 names_to = "tipo", values_to = "z")
  df_long$tipo <- factor(df_long$tipo, levels = c("liga", "estilo"),
                         labels = c("Solo re-escalado por liga",
                                    "Ajustado también por estilo del club"))
  df_long$col <- ifelse(df_long$tipo == "Solo re-escalado por liga",
                        fic_navy_soft, fic_gold)
  
  rango <- max(2, max(abs(df_long$z), na.rm = TRUE) + 0.4)
  
  # Carril horizontal por indice, para leer cada fila de un vistazo
  df_carril <- data.frame(indice = factor(rev(etiqs), levels = rev(etiqs)))
  
  # Delta entre las dos proyecciones, anotado al final de cada fila
  df_delta <- df %>%
    dplyr::mutate(
      d     = estilo - liga,
      texto = paste0(ifelse(d >= 0, "+", ""), sprintf("%.2f", d)),
      col   = ifelse(d >= 0, fic_gold, fic_red)
    )
  
  ggplot(df_long, aes(x = z, y = indice)) +
    geom_segment(data = df_carril,
                 aes(x = -rango, xend = rango, y = indice, yend = indice),
                 inherit.aes = FALSE,
                 color = fic_gray_light, linewidth = 0.5) +
    geom_vline(xintercept = 0, color = fic_navy_soft,
               linewidth = 0.5, linetype = "dashed") +
    geom_line(aes(group = indice), color = fic_navy_soft,
              linewidth = 1.6, alpha = 0.35) +
    geom_point(aes(color = col, shape = tipo), size = 4.2) +
    geom_text(data = df_delta,
              aes(x = rango * 0.96, y = indice, label = texto, color = col),
              inherit.aes = FALSE, hjust = 1, size = 3.3, fontface = "bold") +
    scale_color_identity(guide = "none") +
    scale_shape_manual(values = c(16, 18)) +
    scale_x_continuous(limits = c(-rango, rango * 1.02)) +
    labs(
      title = "Como cambia el perfil al proyectarlo al club destino",
      subtitle = paste0("Z-score de cada índice frente a los ", e$proy$ref_usada,
                        " de ", e$proy$liga_destino, " (n = ", e$proy$n_pop,
                        ").  La cifra a la derecha es cuanto mueve el ajuste por estilo"),
      x = "Z-score proyectado", y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title          = element_text(size = 13.5, face = "bold", color = fic_navy,
                                         margin = margin(b = 4)),
      plot.subtitle       = element_text(size = 10, color = fic_gray,
                                         margin = margin(b = 14)),
      plot.title.position = "plot",
      axis.text.y         = element_text(size = 11, color = fic_navy, face = "bold"),
      axis.text.x         = element_text(size = 9, color = fic_navy_soft),
      axis.title.x        = element_text(size = 9.5, color = fic_gray),
      axis.ticks          = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.major.x  = element_line(color = fic_gray_light, linewidth = 0.3),
      panel.grid.minor    = element_blank(),
      panel.background    = element_rect(fill = fic_white, color = NA),
      plot.background     = element_rect(fill = fic_white, color = NA),
      legend.position     = "top",
      legend.justification = "left",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 9.5, color = fic_navy_soft),
      plot.margin         = margin(18, 24, 14, 18)
    ) +
    guides(shape = guide_legend(
      override.aes = list(color = c(fic_navy_soft, fic_gold), size = 4)))
}

# ── UI ────────────────────────────────────────────────────────
fichajesPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("Simulador de incorporaciones"),
        tags$p(paste0("Selecciona un club destino y hasta ", MAX_FICHAJES,
                      " jugadores. La app evalua si entrarían al once titular ",
                      "según la formación más usada por ese club."))
    ),
    
    div(class = "well well-sm", style = "margin-bottom:22px; background:#F3F5F8;
                                          border:1px solid #DCE1E8;",
        fluidRow(
          column(3, selectInput(ns("liga_destino"), "Liga destino",
                                choices = NULL, width = "100%")),
          column(3, selectInput(ns("club_destino"), "Club destino",
                                choices = NULL, width = "100%")),
          column(6, selectizeInput(ns("fichajes"),
                                   paste0("Jugadores a evaluar (máximo ", MAX_FICHAJES, ")"),
                                   choices = NULL, multiple = TRUE, width = "100%",
                                   options = list(
                                     placeholder = "Escribe para buscar jugadores...",
                                     maxItems = MAX_FICHAJES)))
        )
    ),
    
    uiOutput(ns("ficha_club")),
    br(),
    uiOutput(ns("tarjetas_veredicto")),
    br(),
    
    conditionalPanel(
      condition = paste0("input['", ns("fichajes"), "'] && input['",
                         ns("fichajes"), "'].length > 0"),
      tags$hr(style = "border-color:#DCE1E8; margin:30px 0;"),
      fluidRow(column(12, plotOutput(ns("g_depth"), height = "auto"))),
      tags$hr(style = "border-color:#DCE1E8; margin:30px 0;"),
      fluidRow(column(12,
                      tags$h4("Compatibilidad de estilo con el club",
                              style = "font-weight:700; color:#0E2240; margin-bottom:6px;"),
                      tags$p(paste0("Fit de sistema entre el contexto del jugador (posesión y PPDA ",
                                    "del equipo donde juega hoy) y el club destino. 100% = viene de ",
                                    "un sistema identico. Mide riesgo de adaptacion, no idoneidad. ",
                                    "Es el mismo criterio que la columna Afinidad de Scouting."),
                             style = "color:#46566C; font-size:0.88em; margin-bottom:14px;"),
                      uiOutput(ns("compat_estilo"))
      )),
      tags$hr(style = "border-color:#DCE1E8; margin:30px 0;"),
      fluidRow(column(12,
                      tags$h4("Proyección detallada por fichaje",
                              style = "font-weight:700; color:#0E2240; margin-bottom:6px;"),
                      tags$p(paste0("Se muestran las dos proyecciones: la que solo re-escala ",
                                    "contra la liga destino, y la que ademas ajusta por el estilo ",
                                    "de juego del club (posesión y presión)."),
                             style = "color:#46566C; font-size:0.88em; margin-bottom:18px;"),
                      uiOutput(ns("detalle_fichajes"))
      )),
      tags$hr(style = "border-color:#DCE1E8; margin:30px 0;"),
      fluidRow(column(12,
                      tags$h4("Nota metodologica",
                              style = "font-weight:700; color:#0E2240; margin-bottom:8px;"),
                      uiOutput(ns("nota_metodo"))
      )),
      br(), br()
    )
  )
}

# ── Server ────────────────────────────────────────────────────
fichajesPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {
    
    estilos <- reactive({ estilo_por_club(perfiles) })
    # Estilo con las 5 dimensiones (viene de Clubs_Statistics via
    # cargar_estilo_clubes de mod_clubes_simulador.R). Fallback al
    # de 3 columnas si no existe el .rds.
    estilos5 <- reactive({
      ec <- tryCatch(cargar_estilo_clubes(), error = function(e) NULL)
      if (is.null(ec)) estilos() else ec
    })
    formaciones <- reactive({ cargar_formaciones() })
    modelos     <- reactive({ cargar_modelos() })
    
    observe({
      ligas <- sort(unique(perfiles$liga))
      updateSelectInput(session, "liga_destino", choices = ligas)
    })
    
    observe({
      req(input$liga_destino)
      clubes <- perfiles %>%
        dplyr::filter(liga == input$liga_destino) %>%
        dplyr::pull(club) %>% unique() %>% sort()
      updateSelectInput(session, "club_destino", choices = clubes)
    })
    
    observe({
      req(input$club_destino)
      d <- perfiles %>%
        dplyr::filter(cumple_minimo, club != input$club_destino) %>%
        dplyr::arrange(dplyr::desc(z_compuesto_fiable))
      etiqueta <- paste0(nombre_display(d$jugador_id), "  |  ", d$club,
                         " (", d$liga, ")  |  ", d$posicion,
                         "  |  p", d$percentil)
      updateSelectizeInput(session, "fichajes",
                           choices = setNames(d$jugador_id, etiqueta),
                           server = TRUE)
    })
    
    info_formacion <- reactive({
      req(input$club_destino)
      formacion_de_club(formaciones(), input$club_destino)
    })
    
    cupos <- reactive({
      titulares_de_formacion(info_formacion()$formacion)
    })
    
    evaluaciones <- reactive({
      req(input$club_destino)
      ids <- input$fichajes
      if (is.null(ids) || length(ids) == 0) return(list())
      lapply(ids, function(jid) {
        j <- perfiles %>% dplyr::filter(jugador_id == jid) %>%
          dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE)
        if (nrow(j) == 0) return(NULL)
        e <- evaluar_fichaje(j, input$club_destino, perfiles, estilos(), cupos())
        if (is.null(e)) return(NULL)

        # Probabilidad de consolidarse (modelo M3 del backtest)
        e5 <- estilos5()
        z_club <- perfiles %>%
          dplyr::filter(club == input$club_destino, subgrupo == e$subgrupo, cumple_minimo) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(DIMS_FIT), ~ mean(.x, na.rm = TRUE)))
        z_club <- if (nrow(z_club) == 0 || all(is.na(z_club))) rep(-1, 5) else as.numeric(z_club[1, ])
        niv <- function(cl) { v <- e5$nivel_club[e5$club == cl]; if (length(v) == 0) NA_real_ else v[1] }
        e$prob <- prob_consolidarse(
          modelos()$fichajes, j,
          afinidad     = compat_estilo_fichaje(j, input$club_destino, e5),
          necesidad    = necesidad_club(as.numeric(j[1, DIMS_FIT]), z_club),
          titular_proy = e$veredicto == "Titular",
          nivel_origen = niv(j$club[1]), nivel_destino = niv(input$club_destino))
        e
      })
    })
    
    # ── Ficha del club destino ────────────────────────────────
    output$ficha_club <- renderUI({
      req(input$club_destino)
      inf <- info_formacion()
      cu  <- cupos()
      est <- estilos() %>% dplyr::filter(club == input$club_destino)
      
      n_plantel <- perfiles %>%
        dplyr::filter(club == input$club_destino, cumple_minimo) %>% nrow()
      
      formacion_txt <- if (is.na(inf$formacion)) {
        "sin dato (se asume 4-3-3)"
      } else {
        paste0(inf$formacion, "  ·  usada en el ", inf$pct, "% de sus partidos")
      }
      
      celda <- function(lab, val, sub = NULL, apagada = FALSE) {
        col_val <- if (apagada) "#C4C8CE" else "#0E2240"
        tags$td(style = "padding:12px 10px; vertical-align:top; text-align:center;
                         border-left:1px solid #DCE1E8;",
                tags$div(style = "font-size:0.8em; color:#46566C; margin-bottom:5px;
                            line-height:1.25;", lab),
                tags$div(style = paste0("font-size:1.3em; font-weight:700; color:", col_val, ";"), val),
                if (!is.null(sub))
                  tags$div(style = "font-size:0.74em; color:#46566C; margin-top:3px;", sub)
        )
      }
      
      div(style = "border:1px solid #DCE1E8; background:#FFFFFF;",
          tags$div(style = "background:#0E2240; color:#FFFFFF; padding:12px 18px;
                          display:flex; justify-content:space-between; align-items:baseline;",
                   tags$span(style = "font-weight:700; font-size:1.15em;", input$club_destino),
                   tags$span(style = "font-size:0.85em; color:#B8862B;", formacion_txt)),
          tags$div(style = "padding:4px 0 0 0;",
                   tags$div(style = "font-size:0.8em; color:#46566C; padding:8px 16px 0 16px;",
                            "Titulares por perfil"),
                   tags$table(style = "width:100%; border-collapse:collapse;",
                              tags$tr(lapply(SUBS_ORDEN, function(sg) {
                                n <- as.numeric(cu[sg]); if (is.na(n)) n <- 0
                                celda(SUBS_ETIQ[sg], n, apagada = (n == 0))
                              }))
                   ),
                   tags$div(style = "border-top:1px solid #DCE1E8;",
                            tags$table(style = "width:100%; border-collapse:collapse;",
                                       tags$tr(
                                         celda("Posesión media", paste0(round(est$posesion_club[1], 1), "%")),
                                         celda("PPDA media", round(est$ppda_club[1], 1)),
                                         celda("Plantel registrado", n_plantel, "con 300+ min")
                                       )
                            )
                   )
          )
      )
    })
    
    # ── Tarjetas de veredicto ─────────────────────────────────
    output$tarjetas_veredicto <- renderUI({
      evs <- Filter(Negate(is.null), evaluaciones())
      if (length(evs) == 0) {
        return(div(class = "alert alert-info",
                   "Selecciona uno o más jugadores para evaluar su encaje."))
      }
      
      tarjetas <- lapply(evs, function(e) {
        col <- color_veredicto(e$veredicto)
        mejora_txt <- if (e$cupo == 0) {
          paste0("La formación habitual del club no contempla el perfil de ",
                 tolower(e$sub_lab), ". Entraría como recurso alternativo o ",
                 "exigiría un cambio de esquema.")
        } else if (!is.na(e$mejora)) {
          if (e$mejora > 0)
            paste0("Mejora al último titular en ", sprintf("%+.2f", e$mejora), " sigma")
          else
            paste0("Queda ", sprintf("%.2f", abs(e$mejora)), " sigma por debajo del último titular")
        } else {
          paste0("El club solo tiene ", e$n_plantel, " jugador(es) de ese perfil con 300+ min, ",
                 "menos que el cupo de ", e$cupo, ". Cualquier fichaje entra al XI por ",
                 "defecto: el veredicto no discrimina.")
        }
        
        div(style = "flex:1 1 300px; min-width:290px; border:1px solid #DCE1E8;
                     background:#FFFFFF;",
            tags$div(style = paste0("background:", col, "; color:#FFFFFF; padding:9px 14px;
                                   display:flex; justify-content:space-between;
                                   align-items:baseline;"),
                     tags$span(style = "font-weight:700; font-size:1.02em;", e$nombre),
                     tags$span(style = "font-size:0.82em; ", e$veredicto)),
            tags$div(style = "padding:14px 16px;",
                     tags$div(style = "color:#46566C; font-size:0.85em; margin-bottom:12px;",
                              paste0(e$sub_lab, "  ·  ", e$posicion, "  ·  ",
                                     e$club_origen, " (", e$liga_origen, ")")),
                     tags$table(style = "width:100%; font-size:0.88em;",
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;", "Puesto entre los del perfil"),
                                  tags$td(style = "text-align:right; font-weight:700; color:#0E2240;",
                                          paste0(e$puesto, " de ", e$n_plantel + 1))),
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;", "Cupo de titulares"),
                                  tags$td(style = "text-align:right; color:#0E2240;",
                                          if (e$cupo == 0) "sin cupo en el esquema" else e$cupo)),
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;", "Z actual en su liga"),
                                  tags$td(style = "text-align:right; color:#0E2240;",
                                          sprintf("%+.2f", e$proy$comp_actual))),
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;", "Z proyectado (liga)"),
                                  tags$td(style = "text-align:right; color:#0E2240;",
                                          sprintf("%+.2f", e$proy$comp_liga))),
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;", "Z proyectado (liga + estilo)"),
                                  tags$td(style = "text-align:right; font-weight:700; color:#B8862B;",
                                          sprintf("%+.2f", e$proy$comp_estilo))),
                                tags$tr(
                                  tags$td(style = "color:#46566C; padding:4px 0;",
                                          "Prob. de consolidarse (modelo validado)"),
                                  tags$td(style = "text-align:right; font-weight:700; color:#0E2240;",
                                          if (is.null(e$prob) || is.na(e$prob)) "s/d (corre 03_validacion.R)"
                                          else paste0(round(100 * e$prob), "%")))
                     ),
                     tags$div(style = paste0("margin-top:12px; padding-top:10px;
                              border-top:1px solid #DCE1E8; font-size:0.83em;",
                                             if (isTRUE(e$linea_incompleta) || e$cupo == 0)
                                               " color:#B8862B; font-weight:600;"
                                             else " color:#46566C;"), mejora_txt)
            )
        )
      })
      
      div(style = "display:flex; flex-wrap:wrap; gap:14px;", tarjetas)
    })
    
    # ── Compatibilidad de estilo por fichaje ──────────────────
    output$compat_estilo <- renderUI({
      req(input$club_destino)
      evs <- Filter(Negate(is.null), evaluaciones())
      if (length(evs) == 0)
        return(div(class = "alert alert-info",
                   "Selecciona jugadores para ver su compatibilidad de estilo."))
      e5 <- estilos5()
      
      filas <- lapply(evs, function(e) {
        j <- perfiles %>% dplyr::filter(jugador_id == e$jugador_id) %>%
          dplyr::slice_max(minutos_total, n = 1, with_ties = FALSE)
        pct <- compat_estilo_fichaje(j, input$club_destino, e5)
        col <- if (is.na(pct)) "#46566C"
        else if (pct >= 75) "#1E7A5A"
        else if (pct >= 50) "#B8862B" else "#B23A3A"
        barra_w <- if (is.na(pct)) 0 else pct
        tags$tr(
          tags$td(style = "padding:10px 14px; color:#0E2240; font-weight:600;",
                  e$nombre),
          tags$td(style = "padding:10px 14px; color:#46566C; font-size:0.9em;",
                  paste0(e$sub_lab, " · ", e$club_origen)),
          tags$td(style = "padding:10px 14px; color:#46566C; font-size:0.9em;",
                  paste0(round(j$posesion_media[1],1), "% · PPDA ",
                         round(j$ppda_media[1],1))),
          tags$td(style = "padding:10px 14px; width:220px;",
                  tags$div(style = "background:#F1F1EC; border-radius:3px; height:20px;
                              position:relative;",
                           tags$div(style = paste0("background:", col, "; width:", barra_w,
                                                   "%; height:100%; border-radius:3px;")),
                           tags$span(style = "position:absolute; right:8px; top:1px;
                                 font-size:0.82em; font-weight:700; color:#0E2240;",
                                     if (is.na(pct)) "s/d" else paste0(pct, "%"))))
        )
      })
      
      div(style = "border:1px solid #DCE1E8;",
          tags$table(style = "width:100%; border-collapse:collapse;",
                     tags$thead(tags$tr(style = "background:#0E2240; color:#FFFFFF;",
                                        tags$th(style="padding:9px 14px; text-align:left; font-size:0.82em;", "Fichaje"),
                                        tags$th(style="padding:9px 14px; text-align:left; font-size:0.82em;", "Perfil / origen"),
                                        tags$th(style="padding:9px 14px; text-align:left; font-size:0.82em;", "Contexto propio"),
                                        tags$th(style="padding:9px 14px; text-align:left; font-size:0.82em;",
                                                paste0("Compatibilidad con ", input$club_destino)))),
                     tags$tbody(filas)))
    })
    
    # ── Depth chart ───────────────────────────────────────────
    p_depth <- reactive({
      req(input$club_destino)
      evs <- Filter(Negate(is.null), evaluaciones())
      inf <- info_formacion()
      f <- if (is.na(inf$formacion)) "4-3-3 (asumida)" else inf$formacion
      plot_depth_chart(input$club_destino, perfiles, evs, cupos(), f)
    })
    # La altura crece con el numero de jugadores y de lineas: con una
    # altura fija (antes 740 px) los nombres se enciman.
    output$g_depth <- renderPlot({ p_depth() }, res = 96, height = function() {
      p <- p_depth()
      if (is.null(p)) return(400)
      170 + 30 * nrow(p$data) + 46 * length(unique(p$data$grupo))
    })
    
    # ── Detalle por fichaje ───────────────────────────────────
    output$detalle_fichajes <- renderUI({
      evs <- Filter(Negate(is.null), evaluaciones())
      if (length(evs) == 0) return(NULL)
      ns <- session$ns
      lapply(seq_along(evs), function(i) {
        e <- evs[[i]]
        tagList(
          tags$div(style = "margin-top:24px; margin-bottom:6px;
                            font-weight:700; color:#0E2240; font-size:1.05em;",
                   paste0(e$nombre, "  —  ", e$club_origen, " a ", input$club_destino)),
          plotOutput(ns(paste0("g_proy_", i)), height = "300px")
        )
      })
    })
    
    observe({
      evs <- Filter(Negate(is.null), evaluaciones())
      for (i in seq_along(evs)) {
        local({
          idx <- i
          output[[paste0("g_proy_", idx)]] <- renderPlot({
            plot_proyeccion_indices(evs[[idx]])
          }, res = 96)
        })
      }
    })
    
    # ── Nota metodologica ─────────────────────────────────────
    output$nota_metodo <- renderUI({
      evs <- Filter(Negate(is.null), evaluaciones())
      inf <- info_formacion()
      ej <- if (length(evs) > 0) evs[[1]] else NULL
      
      div(style = "background:#F3F5F8; border-left:3px solid #B8862B;
                   padding:16px 20px; font-size:0.87em; color:#0E2240;
                   line-height:1.65;",
          tags$p(tags$strong("Cupo de titulares. "),
                 paste0("La formación más usada por el club se traduce a un cupo ",
                        "por perfil: un 4-2-3-1 son dos centrales, dos laterales, ",
                        "dos pivotes, dos extremos, un mediapunta y un delantero centro. ",
                        "Sale de la moda de la formación inicial en las alineaciones de StatsBomb",
                        if (!is.na(inf$formacion))
                          paste0(" (", inf$formacion, ", ", inf$pct, "% de sus partidos)")
                        else " (sin dato para este club; se asume 4-3-3)",
                        ". Volantes por banda y mediapuntas abiertos cuentan como extremos.")),
          tags$p(tags$strong("Perfiles, no líneas. "),
                 paste0("La comparación se hace dentro de siete perfiles: centrales, ",
                        "laterales, pivotes, interiores, mediapuntas, extremos y ",
                        "delanteros centro. Un delantero centro compite contra ",
                        "delanteros centro, no contra el bloque entero de atacantes. ",
                        "Los pesos del z-score compuesto también cambian por perfil: ",
                        "el delantero centro carga en finalizacion, el extremo reparte ",
                        "entre finalizar y generar, el lateral pesa más progresion que ",
                        "el central.")),
          tags$p(tags$strong("Que significa el veredicto. "),
                 paste0("Es puramente meritocratico: se ordena a todos los jugadores ",
                        "de esa línea por z-score compuesto y se mira en que puesto cae ",
                        "el fichaje. Titular si entra en el cupo, Rotacion si queda hasta ",
                        "dos puestos fuera, Suplente si no. Un jugador peor cae más abajo ",
                        "en la lista y cambia de veredicto. No es una prediccion de lo que ",
                        "hara el entrenador, sino donde se ubica el perfil en la jerarquia ",
                        "estadística actual del plantel.")),
          tags$p(tags$strong("Proyección por liga. "),
                 "Se toma el valor bruto de cada índice del jugador y se re-estandariza ",
                 "contra la media y la desviacion tipica de los jugadores de su mismo ",
                 "puesto en la liga destino. Responde a: donde caeria este perfil si ",
                 "compitiera contra esa poblacion."),
          tags$p(tags$strong("Ajuste por estilo del club. "),
                 paste0("Sobre la cifra anterior se aplica una elasticidad acotada a ",
                        "más o menos 0.5 sigma en funcion de dos diferenciales: posesión ",
                        "y PPDA del club destino frente al contexto actual del jugador. ",
                        "Más posesión favorece creacion y progresion; más presión favorece ",
                        "el índice defensivo. Es una heuristica declarada, no un modelo ",
                        "estimado: sirve para ordenar candidatos, no para predecir ",
                        "rendimiento puntual.")),
          if (!is.null(ej))
            tags$p(tags$strong("En este caso. "),
                   paste0("El diferencial de posesión frente al contexto de ", ej$nombre,
                          " es de ", sprintf("%+.2f", ej$proy$d_pos),
                          " desviaciones y el de presión de ",
                          sprintf("%+.2f", ej$proy$d_pres), ".")),
          tags$p(tags$strong("Limitaciones. "),
                 "El modelo no observa minutos disponibles, decisiones del cuerpo técnico, ",
                 "estado fisico, encaje de vestuario ni coste de la operacion. Solo compara ",
                 "perfiles estadisticos ajustados por contexto."),
          tags$p(style = "margin-bottom:0; color:#46566C; font-size:0.92em;",
                 paste(fuente_texto(TEMPORADA_APP), FIRMA))
      )
    })
    
  })
}