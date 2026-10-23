# ============================================================
# mod_backtest.R — CIDE OSOS
# Modulo nuevo — Explorador del backtest por club
#
# Eliges torneo, club y posicion. Ves la lista de candidatos que el
# modelo le habria dado a ese club ANTES del torneo (con sus
# carencias y su estilo de entonces), a quien fichó de verdad, y como
# le fue a cada candidato despues, en el club donde haya terminado.
#
# Lee data/processed/validacion/listas_por_club.rds (03_validacion.R).
# ============================================================

backtestPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        div(class = "osos-encabezado",
        tags$h2("¿A quién le habría recomendado fichar el modelo, y qué pasó?"),
        tags$p(paste0("La lista se arma solo con los 365 días previos al torneo: calidad del jugador, ",
                      "carencias del club en esa posición y afinidad de estilo. Lo de la derecha es lo que ",
                      "pasó después."))),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(
          column(3, selectInput(ns("torneo"), "Torneo", choices = NULL, width = "100%")),
          column(3, selectInput(ns("club"), "Club", choices = NULL, width = "100%")),
          column(2, selectInput(ns("sub"), "Posición", width = "100%",
                                choices = setNames(SUBS_ORDEN, SUBS_ETIQ[SUBS_ORDEN]))),
          column(2, selectInput(ns("esquema"), "Puntuación", width = "100%",
                                choices = c("Calidad + encaje" = "osos", "Solo encaje" = "encaje"))),
          column(2, numericInput(ns("top"), "Mostrar top", value = 15, min = 5, max = 80, step = 5,
                                 width = "100%")))),
    uiOutput(ns("fichajes_reales")),
    br(),
    uiOutput(ns("resumen")),
    DTOutput(ns("tabla")),
    br(),
    div(style = "background:#F3F5F8; border-left:3px solid #B8862B; padding:12px 16px;
                 font-size:0.84em; color:#0E2240; line-height:1.6;",
        tags$strong("Cómo leerlo. "),
        paste0("Calidad + encaje = promedio de calidad, necesidad y afinidad. Solo encaje = promedio de ",
               "afinidad, necesidad y estilo (la lógica del módulo de Scouting). ",
               "Rindió = z compuesto mayor a 0 en el torneo siguiente, en el club donde haya jugado, ",
               "con 450+ minutos. La lista solo incluye jugadores con 300+ minutos el año previo: los ",
               "fichajes sin historia suficiente en Liga MX no aparecen. Que un candidato rinda en otro ",
               "club no prueba que habría rendido en este, pero sí que el modelo lo identificó como bueno ",
               "antes que el mercado."))
  )
}

backtestPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    p <- ruta_proc(file.path("validacion", "listas_por_club.rds"))
    listas <- if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
    if (!is.null(listas) && nrow(listas) == 0) listas <- NULL

    observe({
      req(listas)
      tor <- listas %>% dplyr::distinct(torneo_corte, orden) %>% dplyr::arrange(dplyr::desc(orden))
      updateSelectInput(session, "torneo", choices = tor$torneo_corte)
    })
    observe({
      req(listas, input$torneo)
      cl <- sort(unique(listas$club_lista[listas$torneo_corte == input$torneo]))
      sel <- if ("América" %in% cl) "América" else cl[1]
      updateSelectInput(session, "club", choices = cl, selected = sel)
    })

    lista <- reactive({
      req(listas, input$torneo, input$club, input$sub)
      d <- listas %>%
        dplyr::filter(torneo_corte == input$torneo, club_lista == input$club, subgrupo == input$sub)
      if (nrow(d) == 0) return(d)
      if (input$esquema == "encaje") d <- d %>% dplyr::mutate(rank = rank_encaje, puntaje = score_encaje)
      else d <- d %>% dplyr::mutate(rank = rank_osos, puntaje = score_osos)
      n <- nrow(d)
      d %>%
        dplyr::arrange(rank) %>%
        dplyr::mutate(
          recomendacion = dplyr::case_when(rank <= ceiling(n / 3) ~ "Recomendado",
                                           rank > n - floor(n / 3) ~ "No recomendado",
                                           TRUE ~ "Neutral"),
          jugo = !is.na(min_post_perf) & min_post_perf >= 450,
          que_paso = dplyr::case_when(
            fichado_por_club ~ paste0("Lo fichó ", input$club),
            is.na(team_post) ~ "Sin registro en el torneo",
            cambio_club ~ paste0("Fichó por ", club_despues),
            TRUE ~ paste0("Siguió en ", club_despues)),
          resultado = dplyr::case_when(
            is.na(team_post) ~ "—",
            !jugo ~ "No jugó lo suficiente",
            z_post > 0 ~ "Rindió",
            TRUE ~ "No rindió"))
    })

    output$fichajes_reales <- renderUI({
      # Primero el archivo: si falta, decirlo (antes la pestaña quedaba en blanco)
      if (is.null(listas))
        return(div(class = "alert alert-warning",
                   tags$strong("No se encontraron las listas del backtest. "),
                   paste0("Falta o está vacío el archivo ", p, ". Corre source('R/03_validacion.R') ",
                          "con la versión más reciente y vuelve a abrir la app.")))
      if (is.null(input$torneo) || input$torneo == "" || is.null(input$club) || input$club == "")
        return(div(class = "alert alert-info", "Elige torneo, club y posición."))
      d <- lista()
      if (nrow(d) == 0) return(div(class = "alert alert-info",
                                   paste0("No hay lista para ", input$club, " en esa posición y torneo ",
                                          "(hacen falta al menos 10 candidatos con 300+ minutos).")))
      f <- d %>% dplyr::filter(fichado_por_club)
      if (nrow(f) == 0)
        return(div(class = "alert alert-secondary",
                   paste0(input$club, " no fichó en esa posición a ningún jugador de la lista ",
                          "(o fichó a alguien sin 300+ minutos previos en Liga MX).")))
      tarjeta <- function(r) {
        col <- switch(r$resultado, "Rindió" = "#1E7A5A", "No rindió" = "#B23A3A", "#46566C")
        div(style = "flex:1 1 260px; border:1px solid #DCE1E8; background:#FFFFFF;",
            tags$div(style = paste0("background:", col, "; color:#FFF; padding:8px 14px;
                                   display:flex; justify-content:space-between;"),
                     tags$strong(r$player_name), tags$span(r$resultado)),
            tags$div(style = "padding:10px 14px; font-size:0.88em; color:#0E2240; line-height:1.6;",
                     paste0("Venía de ", r$club_origen, ". Puesto ", r$rank, " de ", r$n_lista,
                            " en la lista del modelo (", r$recomendacion, ")."), tags$br(),
                     paste0("Después: ", ifelse(is.na(r$min_post_perf), 0, round(r$min_post_perf)),
                            " min, z = ", ifelse(is.na(r$z_post), "s/d", sprintf("%+.2f", r$z_post)))))
      }
      tagList(
        tags$h4(paste0("Lo que fichó ", input$club, " en esa posición"),
                style = "font-weight:700; color:#0E2240;"),
        div(style = "display:flex; flex-wrap:wrap; gap:12px;",
            lapply(seq_len(nrow(f)), function(i) tarjeta(f[i, ]))))
    })

    output$resumen <- renderUI({
      d <- lista()
      req(nrow(d) > 0)
      top <- d %>% dplyr::filter(recomendacion == "Recomendado")
      bot <- d %>% dplyr::filter(recomendacion == "No recomendado")
      tasa <- function(x) { j <- x %>% dplyr::filter(jugo)
        if (nrow(j) == 0) "s/d" else paste0(round(100 * mean(j$z_post > 0, na.rm = TRUE)), "% (de ", nrow(j), ")") }
      tags$p(style = "font-size:0.92em; color:#0E2240;",
             tags$strong("En esta lista: "),
             paste0("de los recomendados que jugaron el torneo siguiente, rindió el ", tasa(top),
                    "; de los no recomendados, el ", tasa(bot), "."))
    })

    output$tabla <- renderDT({
      d <- lista()
      req(nrow(d) > 0)
      df <- d %>%
        dplyr::slice_head(n = input$top) %>%
        dplyr::transmute(
          `#` = rank, Jugador = player_name, `Club entonces` = club_origen, Edad = round(edad),
          `Min. previos` = minutos_pre, Calidad = round(calidad), Necesidad = round(necesidad),
          Afinidad = round(afinidad), Estilo = round(estilo), Puntaje = round(puntaje, 1),
          `Recomendación` = recomendacion, `Qué pasó` = que_paso,
          `Min. después` = ifelse(is.na(min_post_perf), 0, round(min_post_perf)),
          `z después` = round(z_post, 2), Resultado = resultado)
      dt <- datatable(df, rownames = FALSE, class = "compact stripe",
                      options = list(dom = "tp", pageLength = 25, scrollX = TRUE))
      fich <- unique(df$`Qué pasó`[grepl("^Lo fichó", df$`Qué pasó`)])
      if (length(fich) > 0)
        dt <- dt %>% formatStyle("Qué pasó",
                                 fontWeight = styleEqual(fich, rep("bold", length(fich))),
                                 backgroundColor = styleEqual(fich, rep("#FBF3DF", length(fich))))
      dt %>%
        formatStyle("Resultado", fontWeight = "bold",
                    color = styleEqual(c("Rindió", "No rindió", "No jugó lo suficiente"),
                                       c("#1E7A5A", "#B23A3A", "#46566C"))) %>%
        formatStyle("Puntaje", background = styleColorBar(c(0, 100), "#EAD7A8"),
                    backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    })
  })
}
