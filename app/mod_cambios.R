# ============================================================
# mod_cambios.R — CIDE OSOS
# Cambios y alineaciones:
#   - impacto de los cambios del club y de cada jugador que entra
#   - rendimiento con cada formacion, ajustado por el rival
#   - mejor XI por formacion con la plantilla actual
# ============================================================

cambiosPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado_tactico("CAMBIOS Y ALINEACIONES", "Qué cambios funcionan y qué once conviene",
      paste0("Impacto de un cambio: diferencia de OBV del equipo contra el rival por 90 minutos, en los ",
             "15 minutos después menos los 15 minutos antes, restando lo que en promedio pasa en la liga ",
             "después de un cambio en ese mismo tramo de minutos y con el mismo marcador.")),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(column(4, selectInput(ns("torneo"), "Torneo", choices = NULL, width = "100%")),
                 column(4, selectInput(ns("club"), "Club", choices = NULL, width = "100%")))),
    uiOutput(ns("aviso")),
    tabsetPanel(type = "pills",
      tabPanel("Cambios",
        br(), uiOutput(ns("kpi_cambios")),
        fluidRow(column(6, plotOutput(ns("g_tramo"), height = "360px")),
                 column(6, tags$h4("Revulsivos: impacto de cada jugador al entrar"),
                        DTOutput(ns("t_revulsivos")))),
        uiOutput(ns("nota_cambios"))),
      tabPanel("Formaciones",
        br(), tags$h4("Rendimiento con cada formación"),
        DTOutput(ns("t_form")),
        tags$h4("Cambios de formación durante el partido"),
        tags$p(style = "color:#46566C; font-size:0.86em;",
               "Mismo método que los cambios de jugador. Si el cambio de formación se hizo junto con una sustitución, el efecto es de los dos."),
        DTOutput(ns("t_shift")),
        nota("xG dif./90 ajustado: diferencia de xG por 90 minutos en los tramos jugados con esa formación, ",
             "descontando el nivel del rival y la localía. vs su promedio: cuánto mejor o peor le va al club con esa ",
             "formación que con el resto; con pocos minutos se acerca a cero, porque con poca muestra la ",
             "diferencia es sobre todo ruido.")),
      tabPanel("Mejor XI",
        br(),
        uiOutput(ns("recomendacion")),
        fluidRow(column(5, tags$h4("Qué formación aprovecha mejor la plantilla"),
                        DTOutput(ns("t_comp_form")), br(), uiOutput(ns("valid_xi"))),
                 column(7, selectInput(ns("formacion"), "Formación", choices = NULL),
                        plotOutput(ns("g_xi"), height = "470px"), DTOutput(ns("t_xi")),
                        tags$h4("Banca recomendada"),
                        uiOutput(ns("nota_banca")),
                        DTOutput(ns("t_banca"))))))
  )
}

cambiosPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {
    tac <- cargar_tactico()
    observe({
      req(tac)
      updateSelectInput(session, "torneo", choices = opciones_torneo(tac, TRUE))
      cl <- sort(unique(club_de(tac, tac$metricas_torneo$team_id)))
      updateSelectInput(session, "club", choices = cl, selected = if ("América" %in% cl) "América" else cl[1])
    })
    output$aviso <- renderUI({ if (is.null(tac)) aviso_sin_tactico() })
    tid <- reactive({ req(tac, input$club); id_de_club(tac, input$club) })
    res <- reactive({ req(tac, tac$cambios); resumir_cambios(tac$cambios, filtro_t(input$torneo)) })
    nom_jug <- function(ids) { n <- perfiles$player_name[match(ids, perfiles$player_id)]
      p <- get0("perfiles_hist", envir = globalenv())
      if (!is.null(p)) n[is.na(n)] <- p$player_name[match(ids[is.na(n)], p$player_id)]
      ifelse(is.na(n), as.character(ids), n) }

    output$kpi_cambios <- renderUI({
      r <- res(); c <- r$club; f <- c[c$team_id == tid(), ]; req(nrow(f) == 1)
      sin_senal <- all(abs(c$impacto_obv_contraido) < 1e-9, na.rm = TRUE)
      rk <- rank(-c$impacto_obv_contraido, ties.method = "min")[c$team_id == tid()]
      col <- if (sin_senal) "#46566C" else if (f$impacto_obv_contraido >= 0) "#1E7A5A" else "#B23A3A"
      fila_kpi(tarjeta_kpi("Ventanas de cambio", f$ventanas),
               if (sin_senal)
                 tarjeta_kpi("Impacto medio (contraído)", "≈ 0",
                             sprintf("Bruto %+.3f. Las diferencias entre clubes no se distinguen del azar", f$impacto_obv), col)
               else tarjeta_kpi("Impacto medio (contraído)", sprintf("%+.3f", f$impacto_obv_contraido),
                                sprintf("%dº de %d en la liga · bruto %+.3f", rk, nrow(c), f$impacto_obv), col),
               tarjeta_kpi("Impacto en xG", sprintf("%+.2f", f$impacto_xg), "dif. de xG por 90, ajustada"),
               tarjeta_kpi("Minuto medio del primer cambio", round(f$minuto_medio)),
               tarjeta_kpi("Cambios con el partido perdido", paste0(round(f$pct_perdiendo), "%")))
    })
    output$g_tramo <- renderPlot({
      r <- res(); d <- r$club_tramo[r$club_tramo$team_id == tid(), ]; req(nrow(d) > 0)
      ggplot(d, aes(tramo, impacto_obv, fill = impacto_obv >= 0)) +
        geom_col(width = 0.6, alpha = 0.85) + geom_hline(yintercept = 0, color = "#46566C") +
        geom_point(aes(y = impacto_obv_contraido), shape = 23, size = 3.5, fill = "#0E2240") +
        geom_text(aes(label = paste0(ventanas, " cambios")), vjust = ifelse(d$impacto_obv >= 0, -0.5, 1.4), size = 3.6) +
        scale_fill_manual(values = c(`TRUE` = "#1E7A5A", `FALSE` = "#B23A3A"), guide = "none") +
        labs(title = "¿En qué momento le funcionan los cambios?",
             subtitle = "Barras: impacto medio bruto por tramo. Rombo: contraído (lo que se puede sostener con esa muestra)",
             x = NULL, y = "Impacto en OBV dif. por 90") +
        theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
    })
    output$t_revulsivos <- renderDT({
      r <- res(); d <- r$jugador[r$jugador$team_id == tid(), ] %>% filter(entradas >= 2) %>%
        arrange(desc(impacto_obv_contraido))
      req(nrow(d) > 0)
      datatable(d %>% transmute(Jugador = nom_jug(player_id), Entradas = entradas, `Minuto medio` = round(minuto_medio),
                                `Impacto OBV (contraído)` = round(impacto_obv_contraido, 3),
                                `Impacto OBV (bruto)` = round(impacto_obv, 3), `Impacto xG` = round(impacto_xg, 2)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "tp", pageLength = 10, scrollX = TRUE)) %>%
        formatStyle("Impacto OBV (contraído)", fontWeight = "bold",
                    color = styleInterval(0, c("#B23A3A", "#1E7A5A")))
    })
    output$nota_cambios <- renderUI({
      req(tac)
      nota(tags$strong("Cómo leerlo. "),
           paste0("Los cambios se hacen cuando el partido ya va de cierta forma: el que pierde mete delanteros y ",
                  "el rival se repliega. Por eso no se mide el antes y después a secas, sino contra lo que en ",
                  "promedio pasa en la liga en esa misma situación. Contraído: con pocas entradas el número se ",
                  "acerca a cero. Qué tan estable es el impacto medio de los cambios de un club de un torneo al ",
                  "siguiente: ", persist_de(tac, "Impacto medio de los cambios del club"),
                  ". Si es cercano a cero, la diferencia entre clubes es sobre todo azar."))
    })

    output$t_form <- renderDT({
      req(tac, tac$tramos_formacion)
      d <- resumir_formaciones(tac$tramos_formacion, filtro_t(input$torneo))
      d <- d[d$team_id == tid(), ] %>% arrange(desc(minutos)); req(nrow(d) > 0)
      datatable(d %>% transmute(`Formación` = formacion, Tramos = tramos, Minutos = round(minutos),
                                `% minutos` = round(pct_minutos, 1), `xG/90` = round(xg90, 2), `xG contra/90` = round(xga90, 2),
                                `xG dif./90 ajustado` = round(xgd90_aj, 2),
                                `vs su promedio (contraído)` = round(vs_club_contraido, 2),
                                `OBV dif./90 ajustado` = round(obvd90_aj, 2)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE)) %>%
        formatStyle("vs su promedio (contraído)", fontWeight = "bold", color = styleInterval(0, c("#B23A3A", "#1E7A5A")))
    })

    output$t_shift <- renderDT({
      d <- resumir_cambios_tacticos(tac$cambios_tacticos, filtro_t(input$torneo))
      if (is.null(d)) return(datatable(data.frame(Aviso = "Sin cambios de formación registrados."), rownames = FALSE, options = list(dom = "t")))
      d <- d[d$team_id == tid(), ] %>% arrange(desc(veces))
      if (nrow(d) == 0) return(datatable(data.frame(Aviso = "Este club no cambió de formación durante los partidos."), rownames = FALSE, options = list(dom = "t")))
      datatable(d %>% transmute(De = de, A = a, Veces = veces, `Minuto medio` = round(minuto_medio),
                                `% perdiendo` = round(pct_perdiendo), `% junto con sustitución` = round(pct_con_sustitucion),
                                `Impacto OBV (contraído)` = round(impacto_obv_contraido, 3), `Impacto xG` = round(impacto_xg, 2)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE)) %>%
        formatStyle("Impacto OBV (contraído)", fontWeight = "bold", color = styleInterval(0, c("#B23A3A", "#1E7A5A")))
    })

    plantilla <- reactive({ req(input$club); perfiles %>% filter(club == input$club, cumple_minimo) %>%
        group_by(jugador_id) %>% slice_max(minutos_total, n = 1, with_ties = FALSE) %>% ungroup() })
    # Recomendacion automatica. Puntaje en xG de diferencia por partido:
    #   calidad del XI x lo que vale un punto de calidad (pendiente de la
    #   validacion de los onces)  +  lo que el club rinde con esa formacion
    #   frente a su promedio (contraido; 0 si nunca la uso).
    comp <- reactive({
      pl <- plantilla(); req(nrow(pl) >= 10)
      usadas <- if (!is.null(tac$tramos_formacion)) unique(tac$tramos_formacion$formacion[tac$tramos_formacion$team_id == tid()]) else character(0)
      cf <- comparar_formaciones(pl, unique(c(names(TABLA_FORMACIONES), usadas[!is.na(usadas)])))
      pend <- if (!is.null(tac$calidad_xi) && is.finite(tac$calidad_xi$pendiente)) tac$calidad_xi$pendiente else 1
      hist <- if (!is.null(tac$tramos_formacion)) {
        h <- resumir_formaciones(tac$tramos_formacion, filtro_t(input$torneo)); h[h$team_id == tid(), ]
      } else NULL
      hay <- !is.null(hist) && nrow(hist) > 0
      cf$historico <- if (hay) coalesce(hist$vs_club_contraido[match(cf$formacion, hist$formacion)], 0) else 0
      cf$min_usada <- if (hay) coalesce(hist$minutos[match(cf$formacion, hist$formacion)], 0) else 0
      cf$aporte_plantilla <- pend * (cf$calidad_xi - max(cf$calidad_xi))
      cf$puntaje <- cf$aporte_plantilla + cf$historico
      # Formaciones que piden los mismos puestos dan el mismo XI: se
      # agrupan y se muestra la que el club mas usa.
      cf$clave <- vapply(cf$formacion, function(f) paste(titulares_de_formacion(f)[SUBS_ORDEN], collapse = "-"), character(1))
      cf %>% arrange(desc(puntaje), desc(min_usada)) %>% group_by(clave, puntaje) %>%
        mutate(equivalentes = paste(formacion[-1], collapse = ", ")) %>% slice(1) %>% ungroup() %>%
        arrange(desc(puntaje), desc(min_usada))
    })
    observe({ cf <- comp()
      updateSelectInput(session, "formacion", choices = unique(c(cf$formacion, names(TABLA_FORMACIONES))),
                        selected = cf$formacion[1]) })
    output$recomendacion <- renderUI({
      cf <- comp(); req(nrow(cf) > 0); r <- cf[1, ]
      seg <- if (nrow(cf) > 1) cf[2, ] else NULL
      div(class = "osos-destacado",
          div(class = "osos-destacado-titulo", "Formación recomendada"),
          div(class = "osos-destacado-valor", r$formacion,
                   if (nzchar(r$equivalentes)) tags$span(style = "font-size:0.5em; color:#46566C; font-weight:400; margin-left:8px;",
                                                       paste0("(mismo XI que ", r$equivalentes, ")"))),
          tags$p(style = "margin:6px 0 0 0; font-size:0.9em; color:#0E2240;",
                 sprintf("XI con calidad media %+.2f%s. ", r$calidad_xi,
                         if (r$fuera_de_puesto > 0) paste0(" (", r$fuera_de_puesto, " fuera de puesto)") else ", todos en su puesto"),
                 if (r$min_usada > 0) sprintf("El club la ha usado %d minutos y rinde %+.2f xG por 90 frente a su promedio (contraído). ",
                                              round(r$min_usada), r$historico)
                 else "El club no la ha usado: la recomendación sale solo de la plantilla. ",
                 if (!is.null(seg)) sprintf("Segunda opción: %s (%.2f xG por partido menos). ", seg$formacion, r$puntaje - seg$puntaje),
                 "Puedes ver cualquier otra en el selector de la derecha."))
    })
    output$t_comp_form <- renderDT({
      cf <- comp(); req(nrow(cf) > 0)
      datatable(cf %>% head(10) %>% transmute(`Formación` = formacion, `Calidad XI` = round(calidad_xi, 2),
                                              `Fuera de puesto` = fuera_de_puesto, Banca = round(profundidad, 2),
                                              `Histórico` = round(historico, 2),
                                              Puntaje = round(puntaje - max(puntaje), 2)),
                rownames = FALSE, class = "compact stripe",
                caption = "Puntaje: xG por partido contra la recomendada. Histórico: lo que rinde el club con esa formación frente a su promedio (contraído).",
                options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
        formatStyle("Formación", target = "row", backgroundColor = styleEqual(cf$formacion[1], "#FBF3DF"),
                    fontWeight = styleEqual(cf$formacion[1], "bold"))
    })
    xi <- reactive({ req(input$formacion); mejor_xi(plantilla(), input$formacion) })
    output$g_xi <- renderPlot({
      r <- xi(); d <- coords_xi(r$xi); req(nrow(d) > 0)
      d$etq <- ifelse(is.na(d$z), d$player_name, paste0(d$player_name, "\n", sprintf("%+.2f", d$z)))
      ggplot(d) + cancha(media = FALSE) +
        geom_point(aes(x, y, color = tipo == "Natural"), size = 12) +
        geom_text(aes(x, y, label = puesto), color = "white", size = 3.8, fontface = "bold") +
        geom_text(aes(x, y + ifelse(y >= 65, -7.5, 7.5), label = etq), size = 4.2, color = "#0E2240",
                  lineheight = 0.9, fontface = "bold") +
        scale_color_manual(values = c(`TRUE` = "#2E6DA4", `FALSE` = "#B8862B"),
                           labels = c(`TRUE` = "En su puesto", `FALSE` = "Fuera de puesto o vacante"), name = NULL) +
        labs(title = paste("Mejor XI en", r$formacion, "—", input$club),
             subtitle = sprintf("Calidad media %+.2f (z con contracción). Portero no evaluado. Ataque hacia la derecha", r$calidad))
    })
    output$t_xi <- renderDT({
      r <- xi(); pl <- plantilla()
      rev <- if (DIM_REV %in% names(pl)) pl[[DIM_REV]][match(r$xi$jugador_id, pl$jugador_id)] else NA
      datatable(r$xi %>% transmute(Puesto = puesto, Jugador = player_name, `z fiable` = round(z, 2),
                                   `Revulsivo (informativo)` = round(rev, 2), Nota = tipo),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE))
    })
    output$nota_banca <- renderUI({
      v <- cargar_validacion()
      rv <- if (!is.null(v$revulsivo)) v$revulsivo[v$revulsivo$predictor == "Índice 7 (contraído)", ] else NULL
      tags$p(style = "color:#46566C; font-size:0.86em;",
             "Los mejores jugadores fuera del XI por calidad (el índice validado), uno o dos por línea. ",
             "El revulsivo (impacto al entrar de cambio) va como dato informativo",
             if (!is.null(rv) && nrow(rv) == 1)
               sprintf(": no se repite de un torneo al siguiente (ρ = %.2f [%.2f, %.2f], %d jugadores).", rv$rho, rv$lo, rv$hi, as.integer(rv$n))
             else ".")
    })
    output$t_banca <- renderDT({
      r <- xi(); pl <- plantilla()
      b <- pl %>% filter(!jugador_id %in% r$xi$jugador_id, !is.na(z_compuesto_fiable)) %>%
        arrange(desc(z_compuesto_fiable)) %>% group_by(grupo_posicion) %>% slice_head(n = 2) %>% ungroup() %>%
        arrange(desc(z_compuesto_fiable)) %>% head(7)
      if (nrow(b) == 0) return(datatable(data.frame(Aviso = "No hay más jugadores con minutos suficientes."),
                                         rownames = FALSE, options = list(dom = "t")))
      rev <- if (DIM_REV %in% names(b)) b[[DIM_REV]] else NA
      datatable(b %>% transmute(Jugador = player_name, Perfil = subgrupo_lab, `z fiable` = round(z_compuesto_fiable, 2),
                                Percentil = paste0(percentil, "º"), Minutos = round(minutos_total),
                                `Entradas de cambio` = entradas_cambio, `Revulsivo (informativo)` = round(rev, 2)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE, scrollX = TRUE))
    })
    output$valid_xi <- renderUI({
      v <- tac$calidad_xi
      if (is.null(v)) return(NULL)
      nota(tags$strong("¿Sirve la calidad del XI para elegir el once? "),
           "La recomendación suma la calidad del XI (convertida a xG con esta relación) y lo que el club ha rendido con cada formación. ",
           sprintf(paste0("En %d partidos, la diferencia de calidad entre los dos onces (índice de cada titular en ",
                          "el torneo anterior) se correlaciona %.2f [%.2f, %.2f] con la diferencia de xG del partido. ",
                          "Cada punto de calidad media vale %.2f de xG por partido."),
                   v$partidos, v$rho, v$rho_lo, v$rho_hi, v$pendiente),
           " Llenar un puesto con alguien de un subgrupo vecino cuesta ", CASTIGO_VECINO, " puntos de z.")
    })
  })
}
