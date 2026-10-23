# ============================================================
# helpers_diseno.R — CIDE OSOS (app)
# Sistema visual: colores, tema de graficas y piezas de interfaz.
# La hoja de estilos esta en www/osos.css.
# ============================================================

COL <- list(tinta = "#0E2240", tinta2 = "#1B3A63", papel = "#F3F5F8", linea = "#DCE1E8",
            texto2 = "#46566C", texto3 = "#6E7C8F", datos = "#2E6DA4", acento = "#B8862B",
            bien = "#1E7A5A", mal = "#B23A3A", resalte = "#FBF3DF", neutro = "#C9CED6")

# Tema unico de graficas. Se enmascara theme_minimal() para que las
# graficas que ya existian en los modulos tomen el mismo estilo.
theme_osos_app <- function(base_size = 13, ...) {
  ggplot2::theme_minimal(base_size = base_size, ...) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(color = COL$tinta),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.15, hjust = 0, color = COL$tinta,
                                         margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.85, hjust = 0, color = COL$texto2,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = base_size * 0.72, hjust = 0, color = COL$texto3),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.text = ggplot2::element_text(color = COL$texto3, size = base_size * 0.8),
      axis.title = ggplot2::element_text(color = COL$texto2, size = base_size * 0.82),
      panel.grid.major = ggplot2::element_line(color = "#E8ECF1", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom", legend.title = ggplot2::element_text(size = base_size * 0.8, color = COL$texto2),
      legend.text = ggplot2::element_text(size = base_size * 0.8, color = COL$texto2),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA))
}
theme_minimal <- function(base_size = 13, ...) theme_osos_app(base_size = base_size, ...)

# Marca: rombo con la linea de medio campo, en el dorado del acento
marca_svg <- function() HTML(paste0(
  '<svg width="26" height="26" viewBox="0 0 26 26" aria-hidden="true">',
  '<rect x="4" y="4" width="18" height="18" transform="rotate(45 13 13)" fill="none" stroke="#E2B85A" stroke-width="2"/>',
  '<line x1="13" y1="3" x2="13" y2="23" stroke="#E2B85A" stroke-width="2"/>',
  '<circle cx="13" cy="13" r="3.2" fill="none" stroke="#E2B85A" stroke-width="1.6"/></svg>'))

# Encabezado de pagina: titulo y una linea de explicacion
encabezado <- function(titulo, texto = NULL) div(class = "osos-encabezado",
  tags$h2(titulo), if (!is.null(texto)) tags$p(texto))

# Linea de estadisticas
tarjeta_kpi <- function(titulo, valor, detalle = NULL, color = NULL) {
  div(class = "osos-stat",
      div(class = "osos-stat-valor", style = if (!is.null(color)) paste0("color:", color, ";"), valor),
      div(class = "osos-stat-titulo", titulo),
      if (!is.null(detalle)) div(class = "osos-stat-detalle", detalle))
}
fila_kpi <- function(...) div(class = "osos-stats", ...)

nota <- function(...) div(class = "osos-nota", ...)
