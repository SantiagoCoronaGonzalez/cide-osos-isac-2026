# ============================================================
# mod_canteranos.R — CIDE OSOS
# Modulo nuevo — Deteccion de jovenes promesas
#
# Ordena a los jugadores jovenes por la probabilidad de que en un
# ano jueguen la mitad o mas de los minutos de su club. Usa el
# modelo C1 estimado en el backtest de canteranos (03_validacion.R):
# minutos actuales + edad + z compuesto con contraccion por minutos.
#
# La contraccion importa aqui mas que en ningun lado: un juvenil con
# 200 minutos puede tener un z espectacular por pura suerte. El z
# "fiable" lo encoge hacia la media en proporcion a la muestra.
# ============================================================

canterasPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("Jóvenes con mayor probabilidad de consolidarse"),
        tags$p(paste0("Probabilidad de jugar 50% o más de los minutos de su club dentro de un año, ",
                      "según el modelo validado contra cinco años de Liga MX."))),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(
          column(3, selectInput(ns("club"), "Club", choices = NULL, width = "100%")),
          column(3, selectInput(ns("sub"), "Perfil", width = "100%",
                                choices = c("Todos" = "", setNames(SUBS_ORDEN, SUBS_ETIQ[SUBS_ORDEN])))),
          column(3, sliderInput(ns("edad"), "Edad máxima", min = 17, max = 23, value = 21, step = 1,
                                width = "100%")),
          column(3, numericInput(ns("min_min"), "Minutos mínimos", value = 90, min = 0, step = 45,
                                 width = "100%")))),
    uiOutput(ns("aviso")),
    DTOutput(ns("tabla")),
    br(),
    div(style = "background:#F3F5F8; border-left:3px solid #B8862B; padding:12px 16px;
                 font-size:0.84em; color:#0E2240; line-height:1.6;",
        tags$strong("Cómo leerlo. "),
        paste0("Fiabilidad = minutos / (minutos + m0), donde m0 son los minutos a los que el índice ",
               "es mitad señal y mitad ruido (se estima con la coincidencia del índice entre partidos ",
               "pares e impares). Z fiable = z compuesto × fiabilidad. Un juvenil con pocos minutos ",
               "y z alto aparece con z fiable moderado: es una señal para observarlo, no una certeza."))
  )
}

canterasPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {
    modelos <- cargar_modelos()

    observe({
      clubes <- sort(unique(perfiles$club))
      sel <- if ("América" %in% clubes) "América" else ""
      updateSelectInput(session, "club", choices = c("Toda la liga" = "", clubes), selected = sel)
    })

    datos <- reactive({
      d <- perfiles %>%
        dplyr::filter(!is.na(edad), edad <= input$edad, minutos_total >= input$min_min)
      if (!is.null(input$club) && input$club != "") d <- dplyr::filter(d, club == input$club)
      if (!is.null(input$sub) && input$sub != "") d <- dplyr::filter(d, subgrupo == input$sub)
      if (nrow(d) == 0) return(d)
      d$prob <- if (!is.null(modelos$canteranos)) {
        as.numeric(suppressWarnings(predict(modelos$canteranos, type = "response",
          newdata = data.frame(pct_min_pre = pmin(100, d$pct_minutos), edad = d$edad,
                               z_fiable = ifelse(is.na(d$z_compuesto_fiable), 0, d$z_compuesto_fiable)))))
      } else NA_real_
      d %>% dplyr::arrange(dplyr::desc(prob), dplyr::desc(z_compuesto_fiable))
    })

    output$aviso <- renderUI({
      if (is.null(modelos$canteranos))
        return(div(class = "alert alert-warning",
                   "Sin modelo de canteranos: corre source('R/03_validacion.R'). ",
                   "Mientras, la tabla se ordena por z fiable."))
      d <- datos()
      if (nrow(d) == 0) return(div(class = "alert alert-info", "Sin jugadores con esos filtros."))
      NULL
    })

    output$tabla <- renderDT({
      d <- datos()
      if (nrow(d) == 0) return(NULL)
      df <- d %>% dplyr::transmute(
        Jugador = nombre_display(jugador_id), Club = club, Edad = edad,
        Perfil = subgrupo_lab, Nac. = nacionalidad, `Min.` = minutos_total,
        `% min.` = round(pct_minutos, 0),
        `Z compuesto` = round(z_compuesto, 2), Fiabilidad = paste0(round(100 * fiabilidad), "%"),
        `Z fiable` = round(z_compuesto_fiable, 2),
        `Prob. consolidarse` = ifelse(is.na(prob), NA, round(100 * prob)))
      datatable(df, rownames = FALSE, class = "compact stripe",
                options = list(dom = "tp", pageLength = 25, scrollX = TRUE, ordering = TRUE)) %>%
        formatStyle("Prob. consolidarse", fontWeight = "bold", color = "#0E2240",
                    background = styleColorBar(c(0, 100), "#EAD7A8"),
                    backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    })
  })
}
