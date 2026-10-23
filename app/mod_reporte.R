# ============================================================
# mod_reporte.R — CIDE OSOS
# Reporte del jugador seleccionado arriba: indices, mapa de calor,
# tiros, fortalezas y debilidades, perfiles parecidos, clubes
# compatibles y contexto. Se descarga en PDF (sin LaTeX).
# ============================================================

reportePageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("encabezado")),
    fluidRow(
      column(6, plotOutput(ns("g_indices"), height = "330px")),
      column(6, uiOutput(ns("kpis")), uiOutput(ns("validacion")),
             div(style = "margin-top:10px;", downloadButton(ns("pdf"), "Descargar reporte en PDF", class = "btn btn-primary")))),
    br(),
    fluidRow(
      column(8, selectInput(ns("categoria"), "Mapa de calor", choices = names(CATEGORIAS_MAPA), width = "320px"),
             plotOutput(ns("g_mapa"), height = "420px")),
      column(4, br(), br(), plotOutput(ns("g_tiros"), height = "420px"))),
    fluidRow(
      column(6, tags$h4("Fortalezas"), DTOutput(ns("t_fuertes"))),
      column(6, tags$h4("Áreas a mejorar"), DTOutput(ns("t_debiles")))),
    br(),
    fluidRow(
      column(6, tags$h4("Balón parado (informativo)"),
             tags$p(style = "color:#46566C; font-size:13.5px;", "Por 90 minutos, sin ajuste. No entra a la calificación."),
             DTOutput(ns("t_bp"))),
      column(6)),
    fluidRow(
      column(6, tags$h4("Perfiles más parecidos"), DTOutput(ns("t_similares"))),
      column(6, tags$h4("Clubes compatibles"), DTOutput(ns("t_clubes")))),
    uiOutput(ns("aviso_mapas"))
  )
}

reportePageServer <- function(id, perfiles, jugador_sel) {
  moduleServer(id, function(input, output, session) {
    mapas <- cargar_mapas()
    j <- reactive({
      jid <- jugador_sel(); req(jid, jid != "")
      perfiles %>% filter(jugador_id == jid) %>% slice_max(minutos_total, n = 1, with_ties = FALSE)
    })
    tabla <- function(df) datatable(df, rownames = FALSE, class = "compact stripe",
                                    options = list(dom = "t", paging = FALSE, ordering = FALSE))

    output$encabezado <- renderUI({
      x <- j()
      div(class = "osos-ficha",
          div(div(class = "osos-ficha-nombre", x$player_name),
              div(class = "osos-ficha-datos", descripcion_jugador(x))),
          div(class = "osos-ficha-pct",
              div(class = "osos-ficha-pct-valor", paste0(round(x$percentil), "º")),
              div(class = "osos-ficha-pct-texto", "percentil en su posición")))
    })
    output$kpis <- renderUI({
      x <- j()
      fila_kpi(tarjeta_kpi("z fiable", sprintf("%+.2f", x$z_compuesto_fiable), sprintf("sin contracción %+.2f", x$z_compuesto)),
               tarjeta_kpi("Minutos", format(round(x$minutos_total), big.mark = ","), paste0("fiabilidad ", round(100 * x$fiabilidad), "%")),
               tarjeta_kpi("Goles / xA", sprintf("%d / %.1f", x$goles_tot, x$xa_tot)),
               tarjeta_kpi("Disponibilidad", paste0(x$disponibilidad, "%"), paste(x$salidas_lesion, "salidas por lesión")))
    })
    output$validacion <- renderUI({ v <- texto_validacion(); if (!is.null(v)) nota(v) })
    output$g_indices <- renderPlot({ graf_indices_jugador(j()) })
    output$g_mapa <- renderPlot({
      g <- graf_mapa_calor(mapas, j(), input$categoria)
      validate(need(!is.null(g), "Sin acciones con ubicación para este jugador en esta temporada (hacen falta eventos)."))
      g
    })
    output$g_tiros <- renderPlot({
      g <- graf_tiros_jugador(mapas, j())
      validate(need(!is.null(g), "Sin tiros en la temporada."))
      g
    })
    fd <- reactive(fortalezas_debilidades(j(), perfiles))
    fmt_fd <- function(d) d %>% transmute(`Métrica` = metrica, `Índice` = indice, Percentil = paste0(percentil, "º"))
    output$t_fuertes <- renderDT({ tabla(fmt_fd(fd()$fuertes)) })
    output$t_debiles <- renderDT({ tabla(fmt_fd(fd()$debiles)) })
    output$t_bp <- renderDT({ d <- tabla_balon_parado(j(), perfiles); validate(need(!is.null(d), "Sin eventos para esta temporada.")); tabla(d) })
    output$t_similares <- renderDT({ d <- similares_jugador(j(), perfiles); req(d); tabla(d) })
    output$t_clubes <- renderDT({ d <- clubes_jugador(j(), perfiles); req(d); tabla(d) })
    output$aviso_mapas <- renderUI({
      if (is.null(mapas)) div(class = "alert alert-warning",
                              "Los mapas de calor salen de los eventos: corre source('R/04_tactico.R') para generarlos.")
    })
    output$pdf <- downloadHandler(
      filename = function() paste0("CIDE_OSOS_", gsub("[^A-Za-z0-9]+", "_", j()$player_name), "_",
                                   gsub("/", "-", j()$temporada), ".pdf"),
      content = function(file) generar_pdf_reporte(file, j(), perfiles, mapas))
  })
}
