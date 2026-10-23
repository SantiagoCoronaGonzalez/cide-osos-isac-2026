# ============================================================
# app.R — CIDE OSOS
# Scouting para el Club América sobre datos de StatsBomb (Liga MX)
#
# Uso (desde D:/isac, despues de 02_pipeline.R y 03_validacion.R):
#   shiny::runApp("app")
#
# Para ver otra temporada:
#   Sys.setenv(OSOS_TEMPORADA = "2024/2025"); shiny::runApp("app")
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(DT)
  library(plotly)
  library(ggplot2)
})

source("helpers_carga.R")
source("helpers_cancha.R")
source("helpers_reporte.R")
source("helpers_diseno.R")

# ── Datos ─────────────────────────────────────────────────────
perfiles_hist <- cargar_perfiles_hist()

# Textos de las tablas en español (DT combina esto con las opciones de cada tabla)
options(DT.options = list(language = list(
  paginate = list(previous = "Anterior", `next` = "Siguiente"), search = "Buscar:",
  info = "_START_ a _END_ de _TOTAL_", infoEmpty = "Sin registros", infoFiltered = "(de _MAX_)",
  lengthMenu = "Mostrar _MENU_", zeroRecords = "Sin resultados", emptyTable = "Sin datos")))
# Va al entorno global porque los modulos (cargados ahi) la usan en
# sus pies de grafica; runApp evalua este archivo en otro entorno.
assign("TEMPORADA_APP", temporada_app(perfiles_hist), envir = globalenv())
perfiles      <- perfiles_hist %>% filter(temporada == TEMPORADA_APP)
assign("perfiles_hist", perfiles_hist, envir = globalenv())   # nombres de jugadores en las pestañas tácticas

# Ajuste por contexto con el que se calcularon los perfiles
METODO_APP <- local({
  f <- ruta_proc("metodo_pipeline.rds")
  m <- if (file.exists(f)) readRDS(f) else "nivel"
  c(nivel = "Ajuste A: nivel del equipo", alineacion = "Ajuste B: alineación en la cancha",
    efectos_fijos = "Ajuste C: efectos fijos en dos vías")[[m]]
})
local({
  f <- ruta_proc("indices_pipeline.rds")
  v <- if (file.exists(f)) readRDS(f) else "completo"
  METODO_APP <<- paste0(METODO_APP, ", índices ", if (v == "parsimonioso") "parsimoniosos" else "completos")
})

message("CIDE OSOS — temporada ", TEMPORADA_APP, " · ", METODO_APP, ": ",
        n_distinct(perfiles$player_id), " jugadores, ",
        n_distinct(perfiles$club), " clubes")

# ── Modulos ───────────────────────────────────────────────────
for (m in c("mod_perfil.R", "mod_similares_rankings.R", "mod_clubes.R",
            "mod_visualizaciones.R", "mod_scanner.R", "mod_fichajes.R",
            "mod_scouting.R", "mod_comparacion.R", "mod_canteranos.R",
            "mod_validacion.R", "mod_backtest.R", "mod_ranking_historico.R",
            "mod_equipo.R", "mod_balon_parado.R", "mod_cambios.R", "mod_reporte.R", "mod_inicio.R",
            "mod_entrenadores.R"))
  source(m, local = FALSE)

# ── UI ────────────────────────────────────────────────────────
marca <- tags$span(class = "osos-marca", marca_svg(),
                   tags$span(class = "osos-marca-nombre", "CIDE OSOS"),
                   tags$span(class = "osos-marca-sub", "Scouting", tags$br(), "Liga MX"))

contexto <- div(class = "osos-contexto",
  selectorJugadorUI("sel"),
  div(class = "osos-contexto-info",
      tags$strong(paste("Temporada", TEMPORADA_APP)), tags$br(), METODO_APP, tags$br(),
      "Datos: StatsBomb"))

ui <- tagList(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "osos.css")),
  navbarPage(
    title = marca, windowTitle = "CIDE OSOS · Scouting Liga MX", id = "nav", header = contexto,
    tabPanel("Resumen", inicioPageUI("inicio")),
    navbarMenu("Jugador",
      tabPanel("Perfil",       perfilPageUI("perfil")),
      tabPanel("Reporte",      reportePageUI("rep")),
      tabPanel("Gráficas",     visualizacionesPageUI("viz")),
      tabPanel("Similares",    similaresPageUI("sim")),
      tabPanel("Comparación",  comparacionPageUI("comp"))),
    navbarMenu("Mercado",
      tabPanel("Rankings",     rankingsPageUI("rank")),
      tabPanel("Fichajes",     fichajesPageUI("fich")),
      tabPanel("Scouting",     scoutingPageUI("scout")),
      tabPanel("Clubes compatibles", clubesPageUI("clubes")),
      tabPanel("Carencias",    scannerPageUI("scan")),
      tabPanel("Canteranos",   canterasPageUI("cant"))),
    navbarMenu("Táctico",
      tabPanel("Entrenadores",           entrenadoresPageUI("dt")),
      tabPanel("Métricas de equipo",     equipoPageUI("equipo")),
      tabPanel("Balón parado",           balonParadoPageUI("bp")),
      tabPanel("Cambios y alineaciones", cambiosPageUI("cambios"))),
    navbarMenu("Evidencia",
      tabPanel("Validación",           validacionPageUI("val")),
      tabPanel("Ranking vs. realidad", rankingHistPageUI("rhist")),
      tabPanel("Backtest por club",    backtestPageUI("bt")))
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  jugador_sel <- selectorJugadorServer("sel", perfiles)
  inicioPageServer("inicio", session)

  perfilPageServer("perfil", perfiles, jugador_sel)
  reportePageServer("rep", perfiles, jugador_sel)
  visualizacionesPageServer("viz", perfiles, jugador_sel)
  similaresPageServer("sim", perfiles, jugador_sel, perfiles_hist)
  rankingsPageServer("rank", perfiles, jugador_sel, perfiles_hist)
  clubesPageServer("clubes", perfiles, jugador_sel)
  fichajesPageServer("fich", perfiles)
  scoutingPageServer("scout", perfiles)
  scannerPageServer("scan", perfiles)
  canterasPageServer("cant", perfiles)
  comparacionPageServer("comp", perfiles)
  rankingHistPageServer("rhist", perfiles_hist)
  backtestPageServer("bt")
  validacionPageServer("val")
  equipoPageServer("equipo")
  entrenadoresPageServer("dt")
  balonParadoPageServer("bp")
  cambiosPageServer("cambios", perfiles)
}

shinyApp(ui, server)
