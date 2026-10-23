# ============================================================
# mod_validacion.R — CIDE OSOS
# Modulo nuevo — Resultados del backtest (03_validacion.R)
#
# Es la pagina que responde "¿y como sabemos que funciona?".
# Solo lee lo que dejo 03_validacion.R; no recalcula nada.
# ============================================================

validacionPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("¿Funciona el modelo? Backtest contra traspasos reales"),
        uiOutput(ns("resumen"))),
    uiOutput(ns("contenido"))
  )
}

validacionPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    res <- cargar_validacion()
    dir_fig <- file.path(RAIZ, "outputs", "validacion")

    tabla_dt <- function(df, dig = 3) {
      df <- df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, dig)))
      datatable(df, rownames = FALSE, class = "compact stripe",
                options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    }

    output$resumen <- renderUI({
      if (is.null(res)) return(NULL)
      p <- res$parametros
      tags$p(style = "color:#46566C; font-size:0.92em; margin-top:8px; line-height:1.6;",
             paste0(res$resumen$n_traspasos, " traspasos entre clubes de Liga MX, evaluados en ",
                    length(p$cortes), " cortes (", p$cortes[1], " a ", tail(p$cortes, 1), "). ",
                    "Perfil ex ante con los ", p$ventana_pre, " días previos a cada torneo y el ",
                    "ajuste contextual re-estimado solo con partidos anteriores. Éxito: jugar ",
                    p$umbral_exito, "% o más de los minutos del nuevo club en su primer torneo ",
                    "(tasa observada: ", round(100 * res$resumen$tasa_exito), "%)."))
    })

    output$contenido <- renderUI({
      if (is.null(res)) {
        return(div(class = "alert alert-warning",
                   "Todavía no hay resultados. Corre source('R/03_validacion.R') desde D:/isac."))
      }
      bloque <- function(titulo, texto, ...) tagList(
        tags$h3(titulo),
        tags$p(texto, style = "color:#46566C; font-size:0.88em; margin-bottom:12px;"), ...)

      difiere <- function(a, b) !is.null(a) && !is.null(b) && a != b
      aviso_metodo <- if (difiere(res$metodo_elegido, res$metodo_pipeline) ||
                          difiere(res$indices_elegidos, res$indices_pipeline) ||
                          difiere(res$bp_elegido, res$bp_pipeline))
        div(class = "alert alert-warning", style = "font-size:0.88em;",
            tags$strong("Los perfiles de la app no usan todavía el ajuste elegido. "),
            "Corre source('R/02_pipeline.R') y luego source('R/03_validacion.R') para actualizarlos.")

      tagList(
        if (!is.null(res$carrera)) bloque(
          "1. ¿Cómo se descuenta el contexto? Tres especificaciones compiten",
          paste0("A: nivel del equipo y del rival en el torneo. B: calidad de los compañeros y rivales ",
                 "que estuvieron en la cancha en cada partido (índice del torneo anterior, ponderado por ",
                 "minutos). C: efectos fijos de jugador, de equipo × torneo y de rival × torneo, ",
                 "identificados por los jugadores que cambian de club. Cada una se estimó en cada corte solo ",
                 "con partidos anteriores. Criterio fijado antes de ver resultados: correlación del índice ex ",
                 "ante con el OBV sin ajustar en el club nuevo (que no depende de ninguna de las tres). ",
                 "B o C solo reemplazan a A si su ventaja tiene IC 95% mayor a cero; si no, se queda la más simple. ",
                 "Todo lo que sigue en esta página usa la elegida."),
          aviso_metodo,
          imageOutput(ns("g_carrera"), height = "auto"), br(),
          DTOutput(ns("t_carrera"))),
        if (!is.null(res$carrera_indices)) bloque(
          "2. ¿Índices completos o parsimoniosos?",
          paste0("Con el ajuste elegido arriba. Completos: 32 métricas. Parsimoniosos: 13, con máximo tres por ",
                 "índice, una sola de cada par muy correlacionado (|r| > 0.75) y sin los conteos de eventos ",
                 "raros (asistencias, pases filtrados, tarjetas) ni la variabilidad de OBV. Mismo criterio y ",
                 "misma regla, con la versión simple como base: los completos solo se quedan si anticipan mejor ",
                 "con IC 95% mayor a cero. En las dos versiones cada componente y cada índice se topan en ±3 ",
                 "desviaciones estándar."),
          imageOutput(ns("g_carrera_ind"), height = "auto"), br(),
          DTOutput(ns("t_carrera_ind"))),
        bloque("3. Revulsivo y balón parado (informativos)",
          paste0("Los dos se muestran como dato y no entran a la calificación. Balón parado: xG de sus remates, xG de ",
                 "los saques que cobra, duelos aéreos ganados y xG concedido bajo su responsabilidad, por 90 minutos y ",
                 "sin ajuste; cuando se probó sumarlo al índice empeoró un poco la predicción del rendimiento en el club ",
                 "nuevo (−0.014, IC [−0.034, 0.005]). Revulsivo: impacto contraído al entrar de cambio. Abajo: ¿el ",
                 "de los 365 días previos anticipa el del torneo siguiente?"),
          if (!is.null(res$revulsivo)) DTOutput(ns("t_rev"))
          else div(class = "alert alert-secondary", "El índice 7 no se pudo validar: faltan entradas de cambio antes y después.")),
        bloque("4. Fichajes: cuatro modelos, de menos a más información",
               paste0("AUC: probabilidad de que el modelo le asigne más probabilidad a un fichaje ",
                      "que funcionó que a uno que no. 0.5 es azar. M0 es la referencia: lo que ",
                      "ya ve cualquier scout (minutos previos y nivel del club). Diferencia vs M0 ",
                      "con IC 95% por bootstrap. N es menor que el total de traspasos porque los ",
                      "primeros cortes solo sirven para entrenar: cada torneo se predice con un ",
                      "modelo estimado únicamente con los anteriores."),
               DTOutput(ns("t_modelos")), br(),
               imageOutput(ns("g_auc"), height = "auto"), br(),
               imageOutput(ns("g_quintil"), height = "auto")),
        bloque("5. Qué variables pesan (M3, muestra completa)",
               paste0("Regresión logística. Errores estándar agrupados por jugador, porque un mismo ",
                      "jugador puede aparecer en varios traspasos."),
               DTOutput(ns("t_coef"))),
        bloque("6. Rendimiento después del traspaso",
               paste0("Entre los fichajes que jugaron lo suficiente para medirlos: ¿el índice ex ante ",
                      "anticipa el rendimiento en el club nuevo mejor que los minutos previos?"),
               DTOutput(ns("t_rend")), br(),
               imageOutput(ns("g_rend"), height = "auto")),
        bloque("7. ¿Rinden los fichajes que el modelo recomendaba para ese club?",
               paste0("Antes de cada torneo, el modelo arma para cada club y cada posición una lista de ",
                      "candidatos con lo que se sabía el año anterior: calidad del jugador, carencias del ",
                      "club en esa posición y afinidad de estilo. Recomendado = tercio superior de esa lista; ",
                      "no recomendado = tercio inferior. Después se ve qué pasó con los que el club ",
                      "efectivamente fichó. `Rindió` = z compuesto mayor a 0 en el club nuevo con ",
                      res$min_post_rend, "+ minutos. Se reportan tres formas de recomendar, fijadas antes ",
                      "de ver los resultados. El explorador de la pestaña Backtest muestra cada lista con nombres."),
               imageOutput(ns("g_rec"), height = "auto"), br(),
               DTOutput(ns("t_rec")), br(),
               tags$h4("Recomendado menos no recomendado"),
               DTOutput(ns("t_contraste")), br(),
               tags$h4("Casos con nombre"),
               DTOutput(ns("t_ejemplos"))),
        bloque("8. Corrección por selección (Heckman en dos etapas)",
               paste0("El rendimiento solo se observa en quien juega lo suficiente. Etapa 1: probit de ",
                      "si juega ", res$min_post_rend, "+ minutos. Etapa 2: rendimiento con la razón ",
                      "inversa de Mills (imr). Restricciones de exclusión: minutos previos, si entraría ",
                      "al once según la plantilla ex ante y la necesidad del club destino, que mueven la ",
                      "oportunidad de jugar pero no la calidad. Intervalos por bootstrap por jugador. ",
                      "Si el coeficiente de z_fiable se mantiene al corregir, la relación entre índice y ",
                      "rendimiento no es un artefacto de quién llega a jugar."),
               DTOutput(ns("t_heckman"))),
        bloque("9. Fiabilidad y validez externa",
               paste0("Test-retest: correlación del índice entre torneos consecutivos. Validez externa: ",
                      "correlación con OBV, el modelo de valor de StatsBomb, que no entra a los índices."),
               imageOutput(ns("g_retest"), height = "auto"), br(),
               DTOutput(ns("t_obv"))),
        bloque("10. Canteranos",
               paste0("Jugadores de ", res$parametros$edad_cantera, " años o menos: ¿el modelo detecta ",
                      "quién juega 50%+ de los minutos de su club un año después? (",
                      res$n_canteranos, " casos, tasa ", round(100 * res$tasa_cant), "%)."),
               DTOutput(ns("t_cant"))),
        div(style = "background:#F3F5F8; border-left:3px solid #B8862B; padding:14px 18px;
                     font-size:0.86em; color:#0E2240; line-height:1.65; margin-top:30px;",
            tags$strong("Lo que esta validación no resuelve. "),
            paste0("Solo se observa el rendimiento de los jugadores que sí fueron fichados: los que ",
                   "el modelo recomendaba y nadie fichó no tienen contrafactual (sesgo de selección). ",
                   "El éxito se mide en minutos, que también dependen del entrenador, y no en ",
                   "resultados del equipo. Las lesiones se infieren de las alineaciones (salidas por ",
                   "lesión y partidos de baja posteriores), no de un historial médico."))
      )
    })

    output$t_modelos <- renderDT({ req(res); tabla_dt(res$modelos %>%
      dplyr::transmute(Modelo = modelo, N = n, AUC = auc, `IC inf` = auc_lo, `IC sup` = auc_hi,
                       Brier = brier, `Δ AUC vs M0` = dif_auc_vs_base,
                       `Δ IC inf` = dif_lo, `Δ IC sup` = dif_hi)) })
    output$t_coef <- renderDT({ req(res); tabla_dt(res$coeficientes %>%
      dplyr::transmute(Variable = variable, Coeficiente = coef, `EE agrupado` = se_cluster,
                       z, `p-valor` = p_valor, `Razón de momios` = odds_ratio)) })
    output$t_rend <- renderDT({ req(res); tabla_dt(res$rendimiento %>%
      dplyr::transmute(Predictor = predictor, Resultado = resultado, `ρ Spearman` = rho,
                       `IC inf` = lo, `IC sup` = hi, N = n)) })
    output$t_obv <- renderDT({ req(res); tabla_dt(res$obv %>%
      tidyr::pivot_wider(id_cols = subgrupo, names_from = tipo, values_from = r) %>%
      dplyr::rename(Subgrupo = subgrupo)) })
    output$t_rec <- renderDT({ req(res$recomendacion); tabla_dt(res$recomendacion %>%
      dplyr::transmute(Esquema = esquema, Grupo = as.character(grupo), N = n,
                       `% jugó` = pct_jugo, `Jugaron` = n_jugaron,
                       `% rindió (si jugó)` = pct_rindio_si_jugo, `z medio después` = z_post_medio,
                       `% rindió del total` = pct_rindio_total, `% min. medio` = pct_min_post_medio), 1) })
    output$t_contraste <- renderDT({ req(res$contraste); tabla_dt(res$contraste %>%
      dplyr::transmute(Esquema = esquema, Medida = medida, `Diferencia (pp)` = diferencia,
                       `IC inf` = ic_inf, `IC sup` = ic_sup, `p Fisher` = p_fisher), 3) })
    output$t_ejemplos <- renderDT({
      req(res$ejemplos)
      datatable(res$ejemplos %>%
                  dplyr::rename(Torneo = torneo, Jugador = jugador, `Posición` = posicion, Edad = edad,
                                Origen = club_origen, Destino = club_destino,
                                `Puesto en la lista` = puesto_en_lista, `Recomendación` = recomendacion,
                                `z ex ante` = z_ex_ante, `Min. después` = min_despues,
                                `% min. después` = pct_min_despues, `z después` = z_despues,
                                Resultado = resultado),
                rownames = FALSE, class = "compact stripe",
                options = list(dom = "ftp", pageLength = 15, scrollX = TRUE,
                               language = list(search = "Buscar (jugador, club, torneo):"))) %>%
        formatStyle("Resultado", fontWeight = "bold",
                    color = styleEqual(c("Jugó y rindió", "Jugó y no rindió", "No jugó lo suficiente"),
                                       c("#1E7A5A", "#B23A3A", "#46566C")))
    })
    output$t_heckman <- renderDT({ req(res$heckman); tabla_dt(res$heckman %>%
      dplyr::transmute(Etapa = etapa, Variable = variable, Coeficiente = coef, `EE bootstrap` = ee_boot,
                       `IC inf` = ic_inf, `IC sup` = ic_sup, `p-valor` = p_valor)) })

    output$t_carrera <- renderDT({
      req(res$carrera)
      d <- res$carrera %>% dplyr::transmute(
        `Especificación` = especificacion, `Elegida` = ifelse(elegido, "Sí", ""),
        `Traspasos` = n_traspasos, `ρ con OBV posterior` = rho_traspasos,
        `IC inf` = rho_lo, `IC sup` = rho_hi, `Δ vs A` = dif_vs_A, `Δ IC inf` = dif_lo, `Δ IC sup` = dif_hi,
        `ρ todos los jugadores` = rho_todos, `N todos` = n_todos, `AUC minutos` = auc_minutos)
      tabla_dt(d) %>% formatStyle("Elegida", target = "row",
                                  backgroundColor = styleEqual("Sí", "#FBF3DF"),
                                  fontWeight = styleEqual("Sí", "bold"))
    })

    output$t_carrera_ind <- renderDT({
      req(res$carrera_indices)
      d <- res$carrera_indices %>% dplyr::transmute(
        `Versión` = especificacion, `Elegida` = ifelse(elegido, "Sí", ""),
        `Traspasos` = n_traspasos, `ρ con OBV posterior` = rho_traspasos,
        `IC inf` = rho_lo, `IC sup` = rho_hi, `Δ vs parsimoniosos` = dif_vs_A,
        `Δ IC inf` = dif_lo, `Δ IC sup` = dif_hi,
        `ρ todos los jugadores` = rho_todos, `N todos` = n_todos, `AUC minutos` = auc_minutos)
      tabla_dt(d) %>% formatStyle("Elegida", target = "row",
                                  backgroundColor = styleEqual("Sí", "#FBF3DF"),
                                  fontWeight = styleEqual("Sí", "bold"))
    })

    output$t_carrera_bp <- renderDT({
      req(res$carrera_bp)
      tabla_dt(res$carrera_bp %>% dplyr::transmute(
        `Versión` = especificacion, `Elegida` = ifelse(elegido, "Sí", ""), Traspasos = n_traspasos,
        `ρ con OBV posterior` = rho_traspasos, `IC inf` = rho_lo, `IC sup` = rho_hi,
        `Δ vs sin balón parado` = dif_vs_A, `Δ IC inf` = dif_lo, `Δ IC sup` = dif_hi,
        `ρ todos` = rho_todos, `AUC minutos` = auc_minutos)) %>%
        formatStyle("Elegida", target = "row", backgroundColor = styleEqual("Sí", "#FBF3DF"),
                    fontWeight = styleEqual("Sí", "bold"))
    })
    output$t_rev <- renderDT({
      req(res$revulsivo)
      tabla_dt(res$revulsivo %>% dplyr::transmute(`Predictor (365 días antes)` = predictor, Jugadores = n,
                                                  `ρ con su impacto en el torneo siguiente` = rho,
                                                  `IC inf` = lo, `IC sup` = hi))
    })

    output$t_cant <- renderDT({
      req(res)
      if (is.null(res$canteranos)) return(datatable(data.frame(Aviso = "Muestra insuficiente"),
                                                    rownames = FALSE, options = list(dom = "t")))
      tabla_dt(res$canteranos %>%
        dplyr::transmute(Modelo = modelo, N = n, AUC = auc, `IC inf` = auc_lo, `IC sup` = auc_hi,
                         Brier = brier, `Δ AUC vs C0` = dif_auc_vs_base))
    })

    imagen <- function(archivo) renderImage({
      f <- file.path(dir_fig, archivo)
      if (!file.exists(f)) return(list(src = "", alt = "Gráfica no disponible"))
      list(src = f, contentType = "image/png", width = "100%", alt = archivo)
    }, deleteFile = FALSE)
    output$g_auc     <- imagen("01_auc_modelos.png")
    output$g_quintil <- imagen("02_exito_por_quintil.png")
    output$g_retest  <- imagen("03_fiabilidad.png")
    output$g_rend    <- imagen("04_rendimiento_posterior.png")
    output$g_rec     <- imagen("05_recomendados_por_club.png")
    output$g_carrera <- imagen("06_carrera_ajuste.png")
    output$g_carrera_ind <- imagen("07_carrera_indices.png")
  })
}
