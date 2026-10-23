# ============================================================
# mod_scanner.R
# Módulo 8 — Scanner de carencias por equipo (CIDE OSOS: Liga MX)
#
# LÓGICA:
# 1. El usuario selecciona un equipo (independiente del jugador global)
# 2. Calculamos el perfil promedio del equipo en cada grupo posicional
#    (DEF, MID, ATT) usando los z-scores de sus jugadores
# 3. Comparamos contra el benchmark de la liga (o global)
# 4. Detectamos automáticamente las dimensiones donde el equipo
#    está por debajo del umbral (percentil configurable)
# 5. Generamos un "perfil objetivo" = lo que le falta al equipo
# 6. Buscamos los jugadores del pool que más se acercan a ese perfil
#    usando distancia euclidiana sobre los índices deficitarios
# ============================================================

# ── FUNCIONES DE CÁLCULO ─────────────────────────────────────

# Calcula el perfil promedio del equipo por grupo posicional
perfil_equipo <- function(perfiles, equipo) {
  perfiles %>%
    filter(club == equipo, cumple_minimo) %>%
    group_by(grupo_posicion) %>%
    summarise(
      n_jugadores        = n(),
      z_idx_ofensivo     = mean(z_idx_ofensivo,     na.rm = TRUE),
      z_idx_creacion     = mean(z_idx_creacion,     na.rm = TRUE),
      z_idx_defensivo    = mean(z_idx_defensivo,    na.rm = TRUE),
      z_idx_progresion   = mean(z_idx_progresion,   na.rm = TRUE),
      z_idx_consistencia = mean(z_idx_consistencia, na.rm = TRUE),
      z_compuesto        = mean(z_compuesto,         na.rm = TRUE),
      .groups = "drop"
    )
}

# Detecta carencias: índices del equipo por debajo del umbral
detectar_carencias <- function(perfil_eq, umbral_z = -0.5) {
  # umbral_z: z-score por debajo del cual consideramos carencia
  # -0.5 ≈ percentil 31 — debajo de la media de su grupo
  carencias <- list()
  for (i in seq_len(nrow(perfil_eq))) {
    grupo <- perfil_eq$grupo_posicion[i]
    for (dim in DIMS_INDICES) {
      val <- perfil_eq[[dim]][i]
      if (!is.na(val) && val < umbral_z) {
        carencias[[length(carencias) + 1]] <- list(
          grupo     = grupo,
          dimension = dim,
          z_actual  = val
        )
      }
    }
  }
  carencias
}

# Busca los mejores candidatos para cubrir una carencia específica
# usando distancia en el índice deficitario + z_compuesto como desempate
buscar_candidatos <- function(perfiles, grupo, dims_deficit, top_n = 10) {
  pool <- perfiles %>%
    filter(grupo_posicion == grupo, cumple_minimo)

  if (nrow(pool) == 0) return(NULL)

  # Score de candidato: promedio de z-scores en las dimensiones deficitarias
  dims_disp <- intersect(dims_deficit, names(pool))
  if (length(dims_disp) == 0) return(NULL)

  pool %>%
    rowwise() %>%
    mutate(
      score_carencia = mean(c_across(all_of(dims_disp)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(desc(score_carencia)) %>%
    slice_head(n = top_n) %>%
    select(jugador_id, club, liga, grupo_posicion,
           all_of(DIMS_INDICES), z_compuesto, percentil,
           score_carencia, minutos_total)
}

# ── UI — Módulo 8 Scanner ─────────────────────────────────────
scannerPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    encabezado("Carencias del plantel",
               "Índices en los que un club queda por debajo del umbral en alguna línea, y candidatos de la liga para cubrirlos."),
    # Controles propios — independientes del selector global
    div(class = "well well-sm",
      style = "margin-bottom:16px;",
      fluidRow(
        column(3,
          selectInput(ns("liga_eq"), "Liga del equipo",
                      choices = NULL, width = "100%")
        ),
        column(3,
          selectInput(ns("equipo"), "Equipo a analizar",
                      choices = NULL, width = "100%")
        ),
        column(3,
          selectInput(ns("liga_pool"), "Pool de candidatos",
                      choices = NULL, width = "100%")
        ),
        column(3,
          sliderInput(ns("umbral_z"), "Umbral de carencia (z-score)",
                      min = -1.5, max = 0, value = -0.5, step = 0.25,
                      width = "100%")
        )
      )
    ),

    # Perfil del equipo
    uiOutput(ns("aviso_equipo")),
    br(),

    fluidRow(
      column(12, h4(uiOutput(ns("titulo_equipo"))))
    ),

    # Mapa de calor
    fluidRow(
      column(12,
        h5("Perfil del equipo por línea e índice (z)"),
        plotlyOutput(ns("heatmap"), height = "280px")
      )
    ),
    br(),

    # Tabla de carencias
    fluidRow(
      column(12,
        h5("Índices por debajo del umbral"),
        DTOutput(ns("tabla_carencias"))
      )
    ),
    br(),

    # Candidatos por grupo posicional
    fluidRow(
      column(12,
        h4("Candidatos recomendados por posición"),
        uiOutput(ns("tabs_candidatos"))
      )
    )
  )
}

# ── Server — Módulo 8 Scanner ─────────────────────────────────
scannerPageServer <- function(id, perfiles) {
  moduleServer(id, function(input, output, session) {

    # Poblar selector de liga
    observe({
      ligas <- sort(unique(perfiles$liga))
      updateSelectInput(session, "liga_eq", choices = ligas)
      updateSelectInput(session, "liga_pool",
                        choices = c("Toda la liga" = "global"))
    })

    # Poblar selector de equipo según liga
    observe({
      req(input$liga_eq)
      equipos <- perfiles %>%
        filter(liga == input$liga_eq) %>%
        pull(club) %>%
        unique() %>%
        sort()
      updateSelectInput(session, "equipo", choices = equipos)
    })

    # Perfil del equipo seleccionado
    perfil_eq <- reactive({
      req(input$equipo)
      perfil_equipo(perfiles, input$equipo)
    })

    # Pool de candidatos según filtro de liga
    pool_candidatos <- reactive({
      if (is.null(input$liga_pool) || input$liga_pool == "global") {
        perfiles
      } else {
        filter(perfiles, liga == input$liga_pool)
      }
    })

    # Carencias detectadas
    carencias <- reactive({
      req(perfil_eq())
      detectar_carencias(perfil_eq(), umbral_z = input$umbral_z)
    })

    # ── Aviso ────────────────────────────────────────────────
    output$aviso_equipo <- renderUI({
      req(input$equipo)
      peq <- perfil_eq()
      if (is.null(peq) || nrow(peq) == 0) {
        return(div(class = "alert alert-warning",
                   paste0("No hay jugadores con minutos suficientes en ",
                          input$equipo, " en la base de datos.")))
      }
      n_car <- length(carencias())
      color <- if (n_car == 0) "alert-success" else "alert-warning"
      msg   <- if (n_car == 0) {
        paste0(input$equipo, " no presenta carencias por debajo del umbral z=",
               input$umbral_z, ".")
      } else {
        paste0(input$equipo, " — ", n_car, " carencia(s) detectada(s) ",
               "por debajo de z=", input$umbral_z, ".")
      }
      div(class = paste("alert", color), msg)
    })

    output$titulo_equipo <- renderUI({
      req(input$equipo)
      paste0("Perfil de ", input$equipo)
    })

    # ── Heatmap ──────────────────────────────────────────────
    output$heatmap <- renderPlotly({
      peq <- perfil_eq()
      req(peq, nrow(peq) > 0)

      grupos   <- peq$grupo_posicion
      etiquetas <- unname(ETIQUETAS_INDICES[DIMS_INDICES])

      # Matriz: filas = grupos posicionales, columnas = índices
      mat <- matrix(NA, nrow = length(grupos), ncol = length(DIMS_INDICES))
      for (i in seq_along(grupos)) {
        mat[i, ] <- as.numeric(unlist(peq[i, DIMS_INDICES]))
      }

      # Texto hover
      texto <- matrix("", nrow = nrow(mat), ncol = ncol(mat))
      for (i in seq_len(nrow(mat))) {
        for (j in seq_len(ncol(mat))) {
          v <- mat[i, j]
          texto[i, j] <- if (!is.na(v)) {
            paste0(grupos[i], " — ", etiquetas[j],
                   "\nZ-score: ", round(v, 2),
                   "\nPercentil: ", round(pnorm(v)*100, 1), "º")
          } else "Sin datos"
        }
      }

      plot_ly(
        x = etiquetas, y = grupos,
        z = mat, text = texto,
        type = "heatmap",
        colorscale = list(
          c(0,   "#B23A3A"),   # rojo: muy por debajo
          c(0.4, "#F3F5F8"),   # amarillo: cerca de la media
          c(0.6, "#F3F5F8"),
          c(1,   "#2E6DA4")    # verde: por encima
        ),
        zmin = -2, zmax = 2,
        hovertemplate = "%{text}<extra></extra>",
        showscale = TRUE,
        colorbar = list(title = "Z-score",
                        tickvals = c(-2,-1,0,1,2),
                        ticktext = c("-2","-1","0","1","2"))
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size=12)),
          yaxis = list(title = "", tickfont = list(size=12)),
          margin = list(t=20, b=20, l=60, r=20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })

    # ── Tabla de carencias ────────────────────────────────────
    output$tabla_carencias <- renderDT({
      peq <- perfil_eq()
      req(peq, nrow(peq) > 0)

      # Construir tabla larga: una fila por (grupo, índice)
      filas <- list()
      for (i in seq_len(nrow(peq))) {
        grupo <- peq$grupo_posicion[i]
        for (dim in DIMS_INDICES) {
          val <- peq[[dim]][i]
          if (!is.na(val)) {
            filas[[length(filas)+1]] <- data.frame(
              Posición  = grupo,
              Índice    = unname(ETIQUETAS_INDICES[dim]),
              `Z-score` = round(val, 2),
              Percentil = paste0(round(pnorm(val)*100, 1), "º"),
              Estado    = if (val < input$umbral_z) "⚠ Carencia" else "✓ OK",
              check.names = FALSE
            )
          }
        }
      }

      if (length(filas) == 0) return(NULL)
      df <- bind_rows(filas) %>% arrange(Posición, `Z-score`)

      datatable(df, rownames = FALSE,
                options = list(dom = "t", paging = FALSE,
                               ordering = TRUE, scrollX = TRUE),
                class = "compact stripe") %>%
        formatStyle("Estado",
          color = styleEqual(c("⚠ Carencia", "✓ OK"),
                             c("#B23A3A", "#2E6DA4")),
          fontWeight = "bold"
        ) %>%
        formatStyle("Z-score",
          backgroundColor = styleInterval(
            c(input$umbral_z, 0),
            c("#F6E3E3", "#F3F5F8", "#E1ECF6")
          )
        )
    })

    # ── Tabs de candidatos por grupo posicional ───────────────
    output$tabs_candidatos <- renderUI({
      peq <- perfil_eq()
      req(peq, nrow(peq) > 0)

      car <- carencias()
      if (length(car) == 0) {
        return(div(class = "alert alert-success",
                   "No hay carencias detectadas con el umbral actual."))
      }

      # Agrupar carencias por grupo posicional
      grupos_con_carencia <- unique(sapply(car, function(x) x$grupo))

      tabs <- lapply(grupos_con_carencia, function(g) {
        tabPanel(g, DTOutput(session$ns(paste0("cand_", g))))
      })

      do.call(tabsetPanel, tabs)
    })

    # Renderizar tabla de candidatos para cada grupo
    observe({
      peq <- perfil_eq()
      req(peq, nrow(peq) > 0)

      car <- carencias()
      if (length(car) == 0) return()

      grupos_con_carencia <- unique(sapply(car, function(x) x$grupo))

      for (g in grupos_con_carencia) {
        local({
          grupo_local <- g
          dims_deficit <- sapply(
            Filter(function(x) x$grupo == grupo_local, car),
            function(x) x$dimension
          )

          output[[paste0("cand_", grupo_local)]] <- renderDT({
            pool  <- pool_candidatos()
            cands <- buscar_candidatos(pool, grupo_local, dims_deficit, top_n = 10)
            if (is.null(cands) || nrow(cands) == 0) return(NULL)

            # Resaltar dimensiones deficitarias
            df <- cands %>%
              transmute(
                Jugador      = nombre_display(jugador_id),
                Club         = club,
                Liga         = liga,
                Minutos      = minutos_total,
                `Score`      = round(score_carencia, 2),
                Percentil    = paste0(percentil, "º"),
                Ofensivo     = round(z_idx_ofensivo,    2),
                Creación     = round(z_idx_creacion,     2),
                Defensivo    = round(z_idx_defensivo,    2),
                Progresión   = round(z_idx_progresion,   2),
                Consistencia = round(z_idx_consistencia, 2)
              )

            # Columnas deficitarias para resaltar
            cols_deficit <- unname(ETIQUETAS_INDICES[dims_deficit])
            cols_deficit <- intersect(cols_deficit, names(df))

            dt <- datatable(df, rownames = FALSE,
                            options = list(dom = "t", paging = FALSE,
                                           ordering = TRUE, scrollX = TRUE),
                            class = "compact stripe")

            # Resaltar columnas deficitarias en azul
            for (col in cols_deficit) {
              dt <- dt %>% formatStyle(col,
                backgroundColor = "#e8f4fd",
                fontWeight = "bold"
              )
            }

            dt %>% formatStyle("Score",
              fontWeight = "bold", color = "#2E6DA4"
            )
          })
        })
      }
    })

  })
}
