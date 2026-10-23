# ============================================================
# mod_balon_parado.R — CIDE OSOS
# Balon parado ofensivo y defensivo: corners, tiros libres y
# laterales en el ultimo tercio. Zonas de entrega y de remate,
# primer contacto, organizacion y marcaje (360) y quien concede.
# ============================================================

balonParadoPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado_tactico("BALÓN PARADO", "Córners, tiros libres y laterales en zona de peligro",
      paste0("xG por saque: xG sin penales de todos los tiros que salen de la jugada de balón parado, ",
             "dividido entre el número de saques. Índice 100 = promedio de la liga; 130 = 30% más.")),
    div(class = "well well-sm", style = "background:#F3F5F8; border:1px solid #DCE1E8;",
        fluidRow(column(4, selectInput(ns("torneo"), "Torneo", choices = NULL, width = "100%")),
                 column(4, selectInput(ns("club"), "Club", choices = NULL, width = "100%")))),
    uiOutput(ns("aviso")),
    tabsetPanel(type = "pills",
      tabPanel("Ofensivo",
        br(), DTOutput(ns("t_of")), br(),
        fluidRow(column(6, plotOutput(ns("g_entrega_of"), height = "380px")),
                 column(6, plotOutput(ns("g_remate_of"), height = "380px"))),
        fluidRow(column(6, tags$h4("Rematadores"), DTOutput(ns("t_rem"))),
                 column(6, tags$h4("Lanzadores de córner"), DTOutput(ns("t_lan")))),
        uiOutput(ns("org_of"))),
      tabPanel("Defensivo",
        br(), DTOutput(ns("t_def")), br(),
        fluidRow(column(6, plotOutput(ns("g_vulnerable"), height = "380px")),
                 column(6, plotOutput(ns("g_remate_def"), height = "380px"))),
        uiOutput(ns("org_def")),
        tags$h4("Quién concede: responsabilidad defensiva"),
        tags$p(style = "color:#46566C; font-size:0.86em;",
               paste0("En cada saque, StatsBomb estima para cada defensor la probabilidad de que le toque ",
                      "responder. El xG que sale de ese saque se reparte según esa probabilidad.")),
        DTOutput(ns("t_resp")))),
    uiOutput(ns("nota"))
  )
}

balonParadoPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    tac <- cargar_tactico()
    observe({
      req(tac)
      updateSelectInput(session, "torneo", choices = opciones_torneo(tac, TRUE))
      cl <- sort(unique(club_de(tac, tac$saques$team_id)))
      updateSelectInput(session, "club", choices = cl, selected = if ("América" %in% cl) "América" else cl[1])
    })
    ft  <- reactive(filtro_t(input$torneo))
    tid <- reactive({ req(tac, input$club); id_de_club(tac, input$club) })
    nom_jug <- function(ids) {
      p <- get0("perfiles_hist", envir = globalenv())
      if (is.null(p)) return(as.character(ids))
      n <- p$player_name[match(ids, p$player_id)]; ifelse(is.na(n), as.character(ids), n)
    }

    output$aviso <- renderUI({ if (is.null(tac)) aviso_sin_tactico() })

    tabla_bp <- function(lado) {
      d <- resumir_bp(tac$saques, lado, ft())
      d <- d[d$team_id == tid(), ]
      req(nrow(d) > 0)
      orden <- c("Córner", "Tiro libre indirecto", "Tiro libre directo", "Lateral en último tercio")
      d <- d[order(match(d$tipo, orden)), ]
      n <- n_distinct(tac$saques$team_id)
      df <- d %>% transmute(Tipo = tipo, Saques = saques, `Por partido` = round(saques / partidos, 1),
                            Tiros = tiros, Goles = goles, `xG por saque` = round(xg_por_saque, 3),
                            `Índice (100 = liga)` = round(indice), Lugar = paste0(rk, "º de ", n),
                            `% con tiro` = round(pct_con_tiro, 1),
                            `% primer contacto` = ifelse(tipo == "Tiro libre directo", NA,
                                                         round(if (lado == "favor") pct_primer_contacto else 100 - pct_primer_contacto, 1)),
                            `% cortos` = ifelse(tipo == "Córner", round(pct_cortos, 1), NA),
                            `% cerrados` = ifelse(tipo == "Córner", round(pct_cerrado, 1), NA))
      datatable(df, rownames = FALSE, class = "compact stripe",
                options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
        formatStyle("Índice (100 = liga)", fontWeight = "bold",
                    color = styleInterval(c(85, 115), if (lado == "favor") c("#B23A3A", "#0E2240", "#1E7A5A")
                                                      else c("#1E7A5A", "#0E2240", "#B23A3A")))
    }
    output$t_of  <- renderDT({ req(tac); tabla_bp("favor") })
    output$t_def <- renderDT({ req(tac); tabla_bp("contra") })

    graf_zonas <- function(lado, titulo) {
      z <- zonas_corner(tac$saques, lado, ft()); z <- z[z$team_id == tid() & z$zona != "Sin dato", ]
      req(nrow(z) > 0)
      z$zona <- factor(z$zona, levels = rev(ZONAS_ENTREGA))
      ggplot(z, aes(y = zona)) +
        geom_col(aes(x = pct, fill = "Club"), width = 0.35, position = position_nudge(y = 0.19)) +
        geom_col(aes(x = pct_liga, fill = "Liga"), width = 0.35, position = position_nudge(y = -0.19)) +
        geom_text(aes(x = pct, label = paste0("xG/saque ", sprintf("%.3f", xg_por_saque), "  (", round(indice), ")")),
                  hjust = -0.05, size = 3.2, nudge_y = 0.19, color = "#0E2240") +
        scale_fill_manual(values = c(Club = "#B8862B", Liga = "#C9CED6")) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +
        labs(title = titulo, subtitle = "% de los córners que van a cada zona; entre paréntesis, índice de xG vs liga",
             x = "% de los córners", y = NULL, fill = NULL) +
        theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
    }
    output$g_entrega_of <- renderPlot({ req(tac); graf_zonas("favor", "Dónde manda sus córners") })
    output$g_vulnerable <- renderPlot({ req(tac); graf_zonas("contra", "Dónde le hacen daño: zonas de vulnerabilidad") })

    graf_remates <- function(lado, titulo) {
      d <- tac$tiros_bp; if (!is.null(ft())) d <- d[d$torneo == ft(), ]
      d <- if (lado == "favor") d[d$team_id == tid(), ] else d[d$rival_id == tid(), ]
      req(nrow(d) > 0)
      ggplot(d) + cancha(media = TRUE) +
        geom_point(aes(x, y, size = xg, color = patron, shape = gol), alpha = 0.75) +
        scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), labels = c(`TRUE` = "Gol", `FALSE` = "Tiro"), name = NULL) +
        scale_color_manual(values = c(Córner = "#2E6DA4", `Tiro libre` = "#B8862B", Lateral = "#1E7A5A"), name = NULL) +
        scale_size_continuous(range = c(1.5, 7), name = "xG") +
        labs(title = titulo, subtitle = paste0(nrow(d), " tiros, ", sprintf("%.1f", sum(d$xg, na.rm = TRUE)),
                                                " xG, ", sum(d$gol), " goles. Ataque hacia la derecha"))
    }
    output$g_remate_of  <- renderPlot({ req(tac); graf_remates("favor", "Zonas de remate a favor") })
    output$g_remate_def <- renderPlot({ req(tac); graf_remates("contra", "Desde dónde le rematan") })

    output$t_rem <- renderDT({
      req(tac); d <- tac$tiros_bp; if (!is.null(ft())) d <- d[d$torneo == ft(), ]
      d <- d[d$team_id == tid(), ] %>% group_by(player_id) %>%
        summarise(Tiros = n(), xG = round(sum(xg, na.rm = TRUE), 2), Goles = sum(gol), Cabezazos = sum(cabeza), .groups = "drop") %>%
        arrange(desc(xG)) %>% head(10) %>% mutate(Jugador = nom_jug(player_id)) %>% select(Jugador, Tiros, xG, Goles, Cabezazos)
      datatable(d, rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE))
    })
    output$t_lan <- renderDT({
      req(tac); d <- lanzadores_bp(tac$saques, ft()); d <- d[d$team_id == tid(), ] %>% arrange(desc(corners)) %>% head(8)
      req(nrow(d) > 0)
      datatable(d %>% transmute(Jugador = nom_jug(player_id), `Córners` = corners, `xG generado` = round(xg_generado, 2),
                                `xG por córner` = round(xg_por_corner, 3), `% primer contacto` = round(pct_primer_contacto),
                                `% cerrados` = round(pct_cerrado)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE))
    })
    output$t_resp <- renderDT({
      req(tac); d <- responsabilidad_bp(list(resp_def = tac$resp_def), tac$saques, ft())
      if (is.null(d)) return(datatable(data.frame(Aviso = "Sin datos de responsabilidad defensiva (campo nuevo de Events v11)."),
                                       rownames = FALSE, options = list(dom = "t")))
      d <- d[d$team_id == tid(), ] %>% filter(saques >= 5) %>% arrange(desc(xg_resp_por_saque)) %>% head(10)
      datatable(d %>% transmute(Jugador = nom_jug(player_id), Saques = saques, `Prob. media de responder` = round(prob_media, 2),
                                `xG bajo su responsabilidad` = round(xg_responsable, 2),
                                `xG por saque` = round(xg_resp_por_saque, 3)),
                rownames = FALSE, class = "compact stripe", options = list(dom = "t", paging = FALSE))
    })

    org_bloque <- function(lado) {
      o <- tac$org360
      if (is.null(o)) return(nota("Sin cuadros 360 de balón parado. Corre la descarga con CON_360 <- TRUE."))
      if (lado == "def") {
        d <- o$defensa[o$defensa$team_id == tid() & o$defensa$tipo == "Córner", ]
        liga <- o$defensa[o$defensa$tipo == "Córner", ]
        if (nrow(d) == 0) return(NULL)
        tagList(tags$h4("Organización defensiva en córners (360)"),
          fila_kpi(tarjeta_kpi("Estilo de marcaje", d$estilo_marcaje, paste0(round(d$pct_pegados), "% de defensores a < 2 yd de un atacante")),
                   tarjeta_kpi("Defensores en el área", sprintf("%.1f", d$defensores_area), sprintf("Liga: %.1f", mean(liga$defensores_area))),
                   tarjeta_kpi("En el área chica", sprintf("%.1f", d$defensores_chica), sprintf("Liga: %.1f", mean(liga$defensores_chica))),
                   tarjeta_kpi("Córners con 360", d$saques_360)))
      } else {
        d <- o$ataque[o$ataque$team_id == tid() & o$ataque$tipo == "Córner", ]
        liga <- o$ataque[o$ataque$tipo == "Córner", ]
        if (nrow(d) == 0) return(NULL)
        tagList(tags$h4("Cómo ocupa el área en sus córners (360)"),
          fila_kpi(tarjeta_kpi("Atacantes en el área", sprintf("%.1f", d$atacantes_area), sprintf("Liga: %.1f", mean(liga$atacantes_area))),
                   tarjeta_kpi("Primer palo", sprintf("%.1f", d$at_primer_palo), sprintf("Liga: %.1f", mean(liga$at_primer_palo))),
                   tarjeta_kpi("Centro", sprintf("%.1f", d$at_centro), sprintf("Liga: %.1f", mean(liga$at_centro))),
                   tarjeta_kpi("Segundo palo", sprintf("%.1f", d$at_segundo_palo), sprintf("Liga: %.1f", mean(liga$at_segundo_palo)))),
          tags$p(style = "color:#46566C; font-size:0.84em;",
                 "Posición de los atacantes en el momento del saque. Los movimientos (desmarques, bloqueos) no se ven en datos de eventos: harían falta datos de tracking."))
      }
    }
    output$org_of  <- renderUI({ req(tac); org_bloque("of") })
    output$org_def <- renderUI({ req(tac); org_bloque("def") })

    output$nota <- renderUI({
      req(tac)
      nota(tags$strong("¿Es un rasgo del club o suerte? "),
           paste0("Correlación entre clubes de un torneo al siguiente: xG por córner a favor ",
                  persist_de(tac, "xG por córner a favor"), "; en contra ", persist_de(tac, "xG por córner en contra"),
                  "; % primer contacto ", persist_de(tac, "% primer contacto en córner a favor"),
                  ". Con unos 80 córners por torneo, un club puede salir arriba o abajo por azar: lo que más ",
                  "se repite es lo más confiable para preparar un partido. Primer contacto: el primer evento con ",
                  "balón después del saque es del equipo que saca. Zonas del córner medidas desde la banda del ",
                  "saque: primer palo, área chica, punto penal, segundo palo, borde del área, corto."))
    })
  })
}
