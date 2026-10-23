# ============================================================
# mod_equipo.R — CIDE OSOS
# Metricas de equipo con eventos: xG, OBV, pases progresivos,
# presion, PPDA y field tilt, con el lugar del club en la liga.
# ============================================================

equipoPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado_tactico("MÉTRICAS DE EQUIPO", "Cómo juega cada club, con métricas de eventos",
      paste0("Promedios por partido en el torneo. Entre paréntesis, el lugar entre los clubes de la liga ",
             "(1º = mejor; en xG en contra y PPDA, 1º = el que menos concede o el que más presiona).")),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(column(4, selectInput(ns("torneo"), "Torneo", choices = NULL, width = "100%")),
                 column(4, selectInput(ns("club"), "Club", choices = NULL, width = "100%")))),
    uiOutput(ns("kpis")),
    fluidRow(column(6, plotOutput(ns("g_mapa"), height = "420px")),
             column(6, plotOutput(ns("g_tendencia"), height = "420px"))),
    br(), tags$h4("Tabla de la liga"),
    DTOutput(ns("tabla")),
    uiOutput(ns("nota"))
  )
}

equipoPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    tac <- cargar_tactico()
    observe({
      req(tac)
      updateSelectInput(session, "torneo", choices = opciones_torneo(tac, FALSE))
      cl <- sort(unique(club_de(tac, tac$metricas_torneo$team_id)))
      updateSelectInput(session, "club", choices = cl, selected = if ("América" %in% cl) "América" else cl[1])
    })
    datos <- reactive({ req(tac, input$torneo); tac$metricas_torneo %>% filter(torneo == input$torneo) %>%
        mutate(club = club_de(tac, team_id)) })
    fila <- reactive({ d <- datos(); req(input$club); d[d$club == input$club, ] })

    output$kpis <- renderUI({
      if (is.null(tac)) return(aviso_sin_tactico())
      f <- fila(); req(nrow(f) == 1); n <- nrow(datos())
      lug <- function(r) paste0(r, "º de ", n)
      fila_kpi(
        tarjeta_kpi("xG a favor", sprintf("%.2f", f$np_xg), lug(f$rk_np_xg)),
        tarjeta_kpi("xG en contra", sprintf("%.2f", f$np_xg_contra), lug(f$rk_np_xg_contra)),
        tarjeta_kpi("Diferencia de xG", sprintf("%+.2f", f$xgd), lug(f$rk_xgd), if (f$xgd >= 0) "#1E7A5A" else "#B23A3A"),
        tarjeta_kpi("Diferencia de OBV", sprintf("%+.2f", f$obv_dif), lug(f$rk_obv_dif)),
        tarjeta_kpi("Field tilt", paste0(round(f$field_tilt), "%"), lug(f$rk_field_tilt)),
        tarjeta_kpi("PPDA", sprintf("%.1f", f$ppda), lug(f$rk_ppda)),
        tarjeta_kpi("Pases progresivos", round(f$pases_prog), lug(f$rk_pases_prog)),
        tarjeta_kpi("Presiones en campo rival", round(f$presiones_altas), lug(f$rk_presiones_altas)))
    })

    output$g_mapa <- renderPlot({
      d <- datos(); req(nrow(d) > 0)
      d$sel <- d$club == input$club
      ggplot(d, aes(field_tilt, xgd)) +
        geom_hline(yintercept = 0, color = "#C9CED6") + geom_vline(xintercept = 50, color = "#C9CED6") +
        geom_point(aes(color = sel, size = sel)) +
        geom_text(aes(label = club), vjust = -1, size = 3.2, color = "#0E2240", check_overlap = TRUE) +
        scale_color_manual(values = c(`TRUE` = "#B8862B", `FALSE` = "#2E6DA4"), guide = "none") +
        scale_size_manual(values = c(`TRUE` = 5, `FALSE` = 2.5), guide = "none") +
        labs(title = "Dominio territorial y diferencia de xG",
             subtitle = "Field tilt: % de los pases en el último tercio que son del club",
             x = "Field tilt (%)", y = "Diferencia de xG por partido") +
        theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
    })

    output$g_tendencia <- renderPlot({
      req(tac, input$club)
      tid <- id_de_club(tac, input$club)
      d <- tac$metricas_partido %>% filter(team_id == tid, torneo == input$torneo) %>%
        left_join(tac$metricas_partido %>% distinct(match_id), by = "match_id") %>%
        arrange(match_id) %>% mutate(partido = row_number())
      req(nrow(d) > 0)
      dl <- d %>% select(partido, `xG a favor` = np_xg, `xG en contra` = np_xg_contra) %>%
        pivot_longer(-partido)
      ggplot(dl, aes(partido, value, color = name)) + geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
        scale_color_manual(values = c(`xG a favor` = "#2E6DA4", `xG en contra` = "#B23A3A")) +
        labs(title = paste(input$club, "partido a partido"), x = "Partido del torneo", y = "xG sin penales", color = NULL) +
        theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
    })

    output$tabla <- renderDT({
      d <- datos(); req(nrow(d) > 0)
      df <- d %>% arrange(desc(xgd)) %>% transmute(
        Club = club, PJ = partidos, `xG` = round(np_xg, 2), `xG contra` = round(np_xg_contra, 2),
        `Dif. xG` = round(xgd, 2), `Dif. OBV` = round(obv_dif, 2), `Field tilt %` = round(field_tilt, 1),
        PPDA = round(ppda, 1), `Pases prog.` = round(pases_prog), `Presiones campo rival` = round(presiones_altas),
        `Contrapresiones` = round(contrapresiones), `% pase` = round(pct_pase, 1))
      datatable(df, rownames = FALSE, class = "compact stripe",
                options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
        formatStyle("Club", target = "row", fontWeight = styleEqual(input$club, "bold"),
                    backgroundColor = styleEqual(input$club, "#FBF3DF"))
    })

    output$nota <- renderUI({
      req(tac)
      nota(tags$strong("Qué tan estable es cada métrica. "),
           paste0("Correlación entre clubes de un torneo al siguiente: diferencia de xG ",
                  persist_de(tac, "Diferencia de xG por partido"), ", field tilt ", persist_de(tac, "Field tilt"),
                  ", PPDA ", persist_de(tac, "PPDA"), ", pases progresivos ",
                  persist_de(tac, "Pases progresivos por partido"),
                  ". Cerca de 1, la métrica describe al club; cerca de 0, cambia de torneo a torneo. ",
                  "Pase progresivo: pase en juego completo que avanza 10+ yardas y termina en los 60% de ",
                  "cancha más cercanos al arco rival. PPDA: pases del rival en su 60% de cancha por cada acción ",
                  "defensiva (presión, duelo, intercepción o falta) en esa zona."))
    })
  })
}
