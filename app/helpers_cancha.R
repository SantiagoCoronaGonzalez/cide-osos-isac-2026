# ============================================================
# helpers_cancha.R — CIDE OSOS (app)
# Cancha en ggplot (coordenadas StatsBomb 120 x 80) y carga del
# analisis tactico.
# ============================================================

source(file.path(RAIZ, "R", "funciones_tactico.R"), local = FALSE)

cargar_tactico <- function() {
  p <- ruta_proc("tactico.rds")
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

aviso_sin_tactico <- function() div(class = "alert alert-warning",
  tags$strong("Todavía no hay análisis táctico. "),
  "Corre source('R/01b_descarga_eventos.R') y luego source('R/04_tactico.R') desde D:/isac.")

COL_LINEA <- "#C9CED6"; COL_CESPED <- "#FFFFFF"

# Cancha completa o media cancha de ataque (x de 60 a 120)
cancha <- function(media = FALSE, fondo = TRUE) {
  x0 <- if (media) 60 else 0
  l <- list(
    annotate("rect", xmin = x0, xmax = 120, ymin = 0, ymax = 80, fill = if (fondo) COL_CESPED else NA, color = COL_LINEA),
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, color = COL_LINEA),
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50, fill = NA, color = COL_LINEA),
    annotate("point", x = 108, y = 40, color = COL_LINEA, size = 1),
    annotate("segment", x = 120, xend = 120, y = 36, yend = 44, color = "#0E2240", linewidth = 1.6))
  if (!media) l <- c(l, list(
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80, color = COL_LINEA),
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62, fill = NA, color = COL_LINEA),
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, color = COL_LINEA)))
  # ylim al reves: con scale_y_reverse, c(0, 80) volvia a voltear el eje
  # y y = 0 (banda izquierda del que ataca) quedaba abajo
  c(l, list(coord_fixed(xlim = c(x0, 120), ylim = c(80, 0), expand = FALSE),
            scale_y_reverse(), theme_void(base_size = 12),
            theme(legend.position = "bottom", plot.title = element_text(face = "bold", color = "#0E2240"),
                  plot.subtitle = element_text(color = "#46566C"))))
}

# Coordenadas de dibujo de un XI segun sus puestos
coords_xi <- function(xi) {
  fila_x <- c(CB = 22, FB = 28, DM = 45, CM = 58, AM = 74, W = 84, ST = 100)
  out <- xi; out$x <- NA_real_; out$y <- NA_real_
  for (s in unique(xi$puesto)) {
    k <- which(xi$puesto == s); n <- length(k)
    ys <- if (s == "FB") c(8, 72)[seq_len(n)] else if (s == "W") c(10, 70)[seq_len(n)]
          else if (n == 1) 40 else seq(40 - 9 * (n - 1), 40 + 9 * (n - 1), length.out = n)
    out$x[k] <- fila_x[[s]]; out$y[k] <- ys
  }
  out
}

# tarjeta_kpi(), fila_kpi(), nota() y encabezado() viven en helpers_diseno.R
encabezado_tactico <- function(etiqueta, titulo, texto) encabezado(titulo, texto)

# Torneos para elegir: del mas reciente al mas viejo, con "Todos"
opciones_torneo <- function(tac, con_todos = TRUE) {
  t <- rev(tac$orden_torneos$torneo)
  if (con_todos) c("Todos los torneos" = "__todos__", setNames(t, t)) else setNames(t, t)
}
filtro_t <- function(x) if (is.null(x) || x == "__todos__") NULL else x
club_de <- function(tac, ids) tac$nombres_eq$club[match(ids, tac$nombres_eq$team_id)]
id_de_club <- function(tac, club) tac$nombres_eq$team_id[match(club, tac$nombres_eq$club)]
persist_de <- function(tac, etiqueta) {
  r <- tac$persistencia$r[tac$persistencia$indicador == etiqueta]
  if (length(r) == 0 || is.na(r)) "sin torneos suficientes para medirla" else sprintf("%.2f", r)
}
