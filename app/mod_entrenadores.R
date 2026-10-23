# ============================================================
# mod_entrenadores.R — CIDE OSOS
# Los entrenadores del America con eventos: huella frente a la
# liga, estilo de ataque y defensa, ajustes por contexto, evolucion
# y juego contra suerte. Lee data/processed/entrenadores.rds
# (R/05_entrenadores.R). Las graficas son las mismas del reporte HTML.
# ============================================================

source(file.path(RAIZ, "R", "funciones_entrenador.R"), local = FALSE)
source(file.path(RAIZ, "reporte", "funciones_reporte.R"), local = FALSE)
local({
  fu <- file.path(RAIZ, "reporte", "fonts")
  if (requireNamespace("systemfonts", quietly = TRUE) && file.exists(file.path(fu, "barlow-latin-400-normal.ttf")))
    try(systemfonts::register_font("Barlow", plain = file.path(fu, "barlow-latin-400-normal.ttf"),
                                   bold = file.path(fu, "barlow-latin-600-normal.ttf")), silent = TRUE)
})

cargar_entrenadores <- function() {
  p <- ruta_proc("entrenadores.rds")
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

entrenadoresPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado("Los entrenadores del América",
               "Qué idea de juego tuvo cada uno, comparada siempre contra lo que hace la liga en la misma situación. Es la versión interactiva del reporte HTML."),
    uiOutput(ns("aviso")),
    uiOutput(ns("fichas")),
    tabsetPanel(type = "tabs",
      tabPanel("Huella",
               br(), plotlyOutput(ns("g_huella"), height = "640px"), uiOutput(ns("rasgos")),
               fluidRow(column(6, plotOutput(ns("g_conf"), height = "380px")),
                        column(6, plotOutput(ns("g_efecto"), height = "380px"))),
               uiOutput(ns("txt_recon"))),
      tabPanel("Ataque y defensa",
               br(), fluidRow(column(6, tags$h4("Posesión contra verticalidad"), plotlyOutput(ns("g_ataque"), height = "460px")),
                              column(6, tags$h4("Intensidad contra altura de la presión"), plotlyOutput(ns("g_defensa"), height = "460px"))),
               tags$h4("Por dónde entra al último tercio"), plotOutput(ns("g_carriles"), height = "260px")),
      tabPanel("Ajustes",
               br(), selectInput(ns("ctx"), "Situación", width = "320px",
                                 choices = c("Según el marcador" = "Marcador", "En casa" = "Localía",
                                             "Contra el 20% más fuerte" = "Rival", "Último cuarto de hora" = "Minuto")),
               plotOutput(ns("g_ajustes"), height = "620px"),
               nota("Cada punto es cuánto cambió el América en esa situación por encima o por debajo de lo que cambia cualquier equipo de la liga en la misma situación, con su intervalo del 95%.")),
      tabPanel("Evolución", br(), plotlyOutput(ns("g_evol"), height = "520px")),
      tabPanel("¿Mejoró a sus jugadores?",
               br(), plotOutput(ns("g_desarrollo"), height = "320px"),
               nota("Cambio del índice de los jugadores que siguieron en el club de un torneo al siguiente (450+ minutos en ambos), por encima de lo que le pasa a un jugador de la liga con su mismo nivel previo, edad y posición. 0 = lo normal; la línea es el intervalo del 95%."),
               DTOutput(ns("t_desarrollo"))),
      tabPanel("Juego o suerte", br(), plotlyOutput(ns("g_sim"), height = "460px"),
               nota("Cada torneo repetido 10,000 veces con las mismas ocasiones: cada tiro entra con probabilidad igual a su xG. La barra gris va del 10% al 90% de las repeticiones; el punto de color es lo que pasó."))
    )
  )
}

entrenadoresPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    o <- cargar_entrenadores()
    output$aviso <- renderUI({
      if (is.null(o)) div(class = "alert alert-warning", tags$strong("Todavía no hay análisis de entrenadores. "),
                          "Corre source('R/05a_entrenadores.R') y source('R/05_entrenadores.R') desde D:/isac.")
    })
    req_o <- function() req(!is.null(o))
    output$fichas <- renderUI({
      req_o()
      ctx <- o$contexto %>% mutate(entrenador = as.character(entrenador))
      col <- colores_de(o$entrenadores)
      do.call(fila_kpi, lapply(seq_len(nrow(ctx)), function(i) {
        r <- ctx[i, ]
        tarjeta_kpi(r$entrenador, num(r$pts_partido, 2),
                    sprintf("puntos por partido; esperados %s · %d partidos", num(r$xpts_partido, 2), r$partidos), col[[r$entrenador]])
      }))
    })
    output$g_huella <- renderPlotly({ req_o(); graf_huella(o) })
    output$rasgos <- renderUI({
      req_o(); col <- colores_de(o$entrenadores)
      tagList(lapply(o$entrenadores, function(en) tags$p(tags$strong(style = paste0("color:", col[[en]]), en), ": ", rasgos(o, en, 3), ".")))
    })
    output$g_conf <- renderPlot({ req_o(); g <- graf_confusion(o); req(g); g }, res = 110)
    output$g_efecto <- renderPlot({ req_o(); graf_efecto(o) }, res = 110)
    output$txt_recon <- renderUI({
      req_o(); r <- o$reconocer; req(r)
      nota(sprintf("Con solo el estilo de cada partido (sin resultado ni fecha), el modelo adivina al entrenador en %s de los casos; al azar, %s (prueba de permutación: p = %s).",
                   pct(100 * r$acierto), pct(100 * r$azar), formatC(r$p_valor, format = "g", digits = 2)))
    })
    output$g_ataque <- renderPlotly({ req_o(); graf_mapa_estilo(o, o$liga_t, "posesion_pct", "velocidad", "Posesión (%)", "Velocidad vertical (yd/s)") })
    output$g_defensa <- renderPlotly({ req_o(); graf_mapa_estilo(o, o$liga_t, "ppda", "altura_recup", "PPDA (menos = más presión)", "Altura de recuperación (x)") %>%
        layout(xaxis = list(autorange = "reversed")) })
    output$g_carriles <- renderPlot({ req_o(); graf_carriles(o) }, res = 110)
    output$g_ajustes <- renderPlot({ req_o(); g <- graf_ajustes(o, input$ctx)
      validate(need(!is.null(g), "No hay suficientes partidos del club en las dos situaciones para comparar.")); g }, res = 100)
    output$g_evol <- renderPlotly({ req_o(); graf_evolucion(o, c("xg_100", "xg_contra_100", "posesion_pct", "velocidad", "ppda",
                                                             "altura_recup", "recupera_5s", "salida_limpia", "pos_tiro", "field_tilt")) })
    output$g_sim <- renderPlotly({ req_o(); graf_torneos_sim(o) })
    output$g_desarrollo <- renderPlot({ req_o(); g <- graf_desarrollo(o)
      validate(need(!is.null(g), "Hacen falta perfiles por torneo para medirlo.")); g }, res = 110)
    output$t_desarrollo <- renderDT({
      req_o(); req(o$desarrollo)
      d <- o$desarrollo$jugadores %>% filter(entrenador %in% o$entrenadores) %>% arrange(desc(mejora)) %>%
        transmute(Entrenador = entrenador, Jugador = player_name, Posición = unname(ETIQUETA_SUB[subgrupo]), Edad = edad,
                  `Torneo` = paste(torneo, "→", torneo_sig), `z antes` = round(z_antes, 2), `z después` = round(z_despues, 2),
                  `Mejora sobre lo esperado` = round(mejora, 2))
      datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
    })
  })
}
