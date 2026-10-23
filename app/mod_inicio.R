# ============================================================
# mod_inicio.R — CIDE OSOS
# Pagina de inicio: la tesis, la grafica que la prueba, la evidencia
# y sus limites, y por donde empezar. Todos los numeros salen de
# data/processed/validacion/resultados.rds (03_validacion.R).
# ============================================================

inicioPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
        div(class = "osos-tesis", "El mercado sabe a quién darle minutos. El modelo sabe quién va a rendir."),
        uiOutput(ns("sub"))),
      column(6, plotOutput(ns("g_tesis"), height = "330px"))),
    tags$h3("La evidencia"),
    uiOutput(ns("evidencia")),
    tags$h3("Lo que el modelo no hace"),
    uiOutput(ns("limites")),
    tags$h3("Por dónde empezar"),
    div(class = "osos-rutas",
        div(class = "osos-ruta", actionLink(ns("ir_reporte"), div(class = "osos-ruta-titulo", "Un jugador")),
            tags$p("Perfil, reporte en PDF con mapa de calor y jugadores parecidos.")),
        div(class = "osos-ruta", actionLink(ns("ir_fichajes"), div(class = "osos-ruta-titulo", "Un fichaje")),
            tags$p("Candidatos por posición para un club y cómo entrarían al once.")),
        div(class = "osos-ruta", actionLink(ns("ir_tactico"), div(class = "osos-ruta-titulo", "Un rival")),
            tags$p("Métricas de equipo, balón parado, cambios y mejor once.")),
        div(class = "osos-ruta", actionLink(ns("ir_validacion"), div(class = "osos-ruta-titulo", "La prueba")),
            tags$p("Backtest completo contra traspasos reales y sus intervalos.")))
  )
}

inicioPageServer <- function(id, sesion_padre) {
  moduleServer(id, function(input, output, session) {
    res <- cargar_validacion()
    num <- function(x, d = 2) formatC(x, format = "f", digits = d)
    fila <- function(cifra, titulo, texto) div(class = "osos-evidencia-fila",
      div(class = "osos-evidencia-cifra", cifra), div(class = "osos-evidencia-texto", tags$strong(titulo), tags$span(texto)))

    rend <- function(pred, resul) {
      if (is.null(res$rendimiento)) return(NULL)
      r <- res$rendimiento[res$rendimiento$predictor == pred & res$rendimiento$resultado == resul, ]
      if (nrow(r) == 0) NULL else r[1, ]
    }

    output$sub <- renderUI({
      if (is.null(res)) return(div(class = "alert alert-warning", "Corre source('R/03_validacion.R') para ver la evidencia."))
      p <- res$parametros
      tags$p(class = "osos-tesis-sub",
             sprintf(paste0("Probado contra %s traspasos reales entre clubes de Liga MX, de %s a %s. ",
                            "En cada torneo, el modelo solo vio los partidos anteriores."),
                     res$resumen$n_traspasos, p$cortes[1], tail(p$cortes, 1)))
    })

    output$g_tesis <- renderPlot({
      req(res$rendimiento)
      d <- res$rendimiento %>%
        mutate(pred = ifelse(predictor == "z compuesto ex ante", "Índice CIDE OSOS", "Minutos previos"),
               resul = ifelse(grepl("^z", resultado), "Rendimiento en el club nuevo (índice)", "Rendimiento en el club nuevo (OBV)"),
               pred = factor(pred, levels = c("Minutos previos", "Índice CIDE OSOS")))
      ggplot(d, aes(y = pred, x = rho, color = pred)) +
        geom_vline(xintercept = 0, color = COL$texto3, linewidth = 0.4) +
        geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 1.6, orientation = "y") +
        geom_point(size = 4.2) +
        geom_text(aes(x = hi, label = num(rho)), hjust = -0.35, size = 4.2, color = COL$tinta, fontface = "bold") +
        facet_wrap(~ resul, ncol = 1) +
        scale_x_continuous(limits = c(min(0, min(d$lo)) - 0.02, 1.08), breaks = seq(0, 1, 0.25)) +
        scale_color_manual(values = c(`Índice CIDE OSOS` = COL$tinta, `Minutos previos` = COL$neutro), guide = "none") +
        labs(title = "¿Qué anticipa cómo le va a un fichaje?",
             subtitle = "Correlación de Spearman con IC 95%",
             x = NULL, y = NULL) +
        theme_minimal(base_size = 13) +
        theme(strip.text = element_text(hjust = 0, face = "bold", color = COL$texto2, size = 11.5),
              panel.grid.major.y = element_blank(), axis.text.y = element_text(color = COL$tinta, size = 12))
    }, res = 110, bg = "transparent")

    output$evidencia <- renderUI({
      req(res)
      filas <- list()
      r1 <- rend("z compuesto ex ante", "z compuesto en el club nuevo"); r0 <- rend("% minutos ex ante", "z compuesto en el club nuevo")
      if (!is.null(r1)) filas[[length(filas) + 1]] <- fila(num(r1$rho),
        "El índice anticipa el rendimiento en el club nuevo",
        sprintf("IC 95%% [%s, %s] en %d traspasos con 450+ minutos. Los minutos previos: %s.",
                num(r1$lo), num(r1$hi), as.integer(r1$n), if (!is.null(r0)) num(r0$rho) else "s/d"))
      if (!is.null(res$contraste)) {
        k <- res$contraste %>% filter(esquema == "General: calidad en la liga", grepl("del total", medida))
        if (nrow(k) == 1) filas[[length(filas) + 1]] <- fila(sprintf("%+.0f pp", k$diferencia),
          "Los fichajes que el modelo recomendaba rindieron más",
          sprintf("Diferencia entre recomendados y no recomendados en la proporción que rindió en su club nuevo. IC 95%% [%.1f, %.1f], p = %s.",
                  k$ic_inf, k$ic_sup, formatC(k$p_fisher, format = "g", digits = 2)))
      }
      if (!is.null(res$heckman)) {
        h <- res$heckman %>% filter(variable == "z_fiable")
        hk <- h$coef[h$etapa == "2. Rendimiento (Heckman)"]; mc <- h$coef[h$etapa == "2. Rendimiento (MCO ingenuo)"]
        if (length(hk) == 1 && length(mc) == 1) filas[[length(filas) + 1]] <- fila(num(hk),
          "No es un efecto de quién llega a jugar",
          sprintf("Con la corrección de Heckman el coeficiente del índice queda en %s; sin corregir, %s.", num(hk), num(mc)))
      }
      if (!is.null(res$retest)) {
        zc <- res$retest$r[res$retest$indice == "z_compuesto"]; ob <- res$retest$r[res$retest$indice == "obv_p90_res"]
        if (length(zc) == 1 && length(ob) == 1) filas[[length(filas) + 1]] <- fila(num(zc),
          "Es un rasgo estable del jugador",
          sprintf("Correlación del índice de un torneo al siguiente; la del OBV, el modelo de valor de StatsBomb, es %s.", num(ob)))
      }
      if (!is.null(res$carrera)) filas[[length(filas) + 1]] <- fila(sprintf("%s–%s", num(min(res$carrera$rho_traspasos)), num(max(res$carrera$rho_traspasos))),
        "No depende de cómo se descuente el contexto",
        "Tres formas de quitar el efecto del equipo (nivel, alineación en la cancha, efectos fijos) dan resultados que no se distinguen entre sí.")
      div(class = "osos-evidencia", filas)
    })

    output$limites <- renderUI({
      req(res)
      l <- list()
      if (!is.null(res$modelos)) {
        a0 <- res$modelos$auc[res$modelos$modelo == "M0 Mercado"]; a1 <- res$modelos$auc[res$modelos$modelo == "M1 + Rendimiento"]
        if (length(a0) == 1 && length(a1) == 1) l[[length(l) + 1]] <- div(class = "osos-limite", tags$strong("No predice quién recibe minutos. "),
          sprintf("Con o sin el índice, el modelo de minutos acierta lo mismo (AUC %s contra %s): eso lo decide el entrenador.", num(a1), num(a0)))
      }
      if (!is.null(res$revulsivo)) {
        rv <- res$revulsivo[res$revulsivo$predictor == "Índice 7 (contraído)", ]
        if (nrow(rv) == 1) l[[length(l) + 1]] <- div(class = "osos-limite", tags$strong("El impacto al entrar de cambio no se repite. "),
          sprintf("De un torneo al siguiente, ρ = %s [%s, %s]. Se muestra como dato, no como recomendación.", num(rv$rho), num(rv$lo), num(rv$hi)))
      }
      l[[length(l) + 1]] <- div(class = "osos-limite", tags$strong("El balón parado va aparte. "),
        "Sus acciones se muestran por 90 minutos pero no entran a la calificación: sumarlas no mejoró la predicción.")
      if (!is.null(res$canteranos)) {
        d <- res$canteranos$dif_auc_vs_base[2]
        if (!is.na(d)) l[[length(l) + 1]] <- div(class = "osos-limite", tags$strong("En canteranos la señal es débil. "),
          sprintf("Para anticipar qué juvenil se consolida, el rendimiento suma %s de AUC a minutos y edad.", num(d)))
      }
      div(l)
    })

    ir <- function(pestana) updateNavbarPage(sesion_padre, "nav", selected = pestana)
    observeEvent(input$ir_reporte, ir("Reporte"))
    observeEvent(input$ir_fichajes, ir("Fichajes"))
    observeEvent(input$ir_tactico, ir("Métricas de equipo"))
    observeEvent(input$ir_validacion, ir("Validación"))
  })
}
