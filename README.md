# CIDE OSOS — Scouting para el Club América (ISAC · ITAM)

Modelo de evaluación de jugadores y fichajes sobre datos de StatsBomb de Liga MX
(2021/2022 a 2026/2027), validado contra cinco años de traspasos reales.

## Cómo correrlo (desde `D:/isac`)

1. **Credenciales** en un `.Renviron` del proyecto (nunca en un script):
   ```
   SB_USER=...
   SB_PASS=...
   ```
   Reinicia R después de crearlo.
2. `source("R/01_descarga.R")` — baja todo y lo guarda partido por partido en `data/raw/`.
   La primera vez tarda (son ~1,850 partidos × 3 endpoints). Si se corta, vuelve a correrlo:
   retoma donde se quedó.
3. `source("R/02_pipeline.R")` — perfiles por temporada y por torneo (~1 min).
4. `source("R/03_validacion.R")` — backtest, tablas y gráficas en `outputs/validacion/` (~3-6 min).
   Primero corre dos carreras fuera de muestra: el **ajuste por contexto** (A, B o C) y los
   **índices** (completos o parsimoniosos). Guarda lo elegido en `data/processed/metodo_ajuste.rds`
   e `indices_elegidos.rds`. Si no coincide con lo que usó el pipeline, lo avisa: en ese caso
   corre otra vez el paso 3 y el 4 (el pipeline lee lo elegido solo).
5. `shiny::runApp("app")` — la app. Para otra temporada:
   `Sys.setenv(OSOS_TEMPORADA = "2024/2025"); shiny::runApp("app")`.

Paquetes: `httr jsonlite dplyr tidyr purrr tibble ggplot2 shiny DT plotly` (y `splines` y `Matrix`,
que ya vienen con R).

Para forzar una versión sin esperar a la validación, antes de correr el pipeline:
`Sys.setenv(OSOS_METODO = "efectos_fijos")` (o `"alineacion"`, `"nivel"`) y
`Sys.setenv(OSOS_INDICES = "completo")` (o `"parsimonioso"`).

### Umbrales (en `R/funciones_modelo.R`)

| Constante | Valor | Para qué |
|---|---|---|
| `MIN_MINUTOS` | 300 | Población de referencia contra la que se estandariza cada subgrupo |
| `MIN_MIN_RANKING` | 450 | Mínimo para aparecer en Rankings y Ranking vs. realidad (en la app se puede mover) |
| `TOPE_Z` | 3 | Tope, en desviaciones estándar, de cada componente y de cada índice antes de combinarlos |

## Estructura

| Archivo | Qué hace |
|---|---|
| `R/00_explorar_api.R` | Inventario de la licencia (ya corrido) |
| `R/helpers_api.R` | Peticiones con reintentos y parseo de la API a tablas |
| `R/01_descarga.R` | Descarga incremental con caché |
| `R/funciones_modelo.R` | Todo el modelo: ajuste contextual, índices, edad, lesiones, contracción, encaje |
| `R/02_pipeline.R` | Perfiles jugador × club × temporada y × torneo |
| `R/03_validacion.R` | Backtest de fichajes y canteranos, fiabilidad, validez externa |
| `app/` | App Shiny (13 pestañas) |
| `tests/simular_datos.R` | Datos falsos con la estructura exacta de la API, para probar sin conexión. **No correr en el proyecto real: sobrescribe `data/raw/`.** |

## Tres formas de descontar el contexto (y cómo se elige)

| | Qué controla | Ventaja | Riesgo |
|---|---|---|---|
| **A. Nivel del equipo** | Diferencial de xG del equipo y del rival en el torneo | Simple y estable | Absorbe parte del mérito del jugador bueno en equipo bueno |
| **B. Alineación en la cancha** | Índice (torneo anterior) de los compañeros y rivales que estuvieron en la cancha, ponderado por minutos | Cambia con rotaciones y lesiones, partido a partido | Depende de la calidad del índice previo |
| **C. Efectos fijos en dos vías** | Efecto de jugador + equipo × torneo + rival × torneo (tipo AKM), identificado por los que cambian de club | Separa jugador de equipo sin suponer qué hace bueno al equipo | Con pocos traspasos por equipo, los efectos se estiman con ruido |

Las tres se estiman en cada corte solo con partidos anteriores. Criterio fijado de antemano:
correlación de Spearman del índice ex ante con el **OBV sin ajustar** del jugador en su club
nuevo (traspasos con 450+ minutos). Se usa OBV crudo porque no depende de ninguna de las
tres; con el índice posterior, cada una se calificaría a sí misma. B o C solo reemplazan a A
si su ventaja tiene IC 95% (bootstrap pareado) mayor a cero. Métricas secundarias: la misma
correlación con todos los jugadores y el AUC de minutos. Gráfica `06_carrera_ajuste.png` y
sección 0 de la pestaña Validación.

## Índices completos vs parsimoniosos

Los completos usan 32 métricas. Los parsimoniosos usan 13, con una regla fija: máximo tres por
índice, una sola de cada par con |r| > 0.75 y fuera los conteos de eventos raros (asistencias,
pases filtrados, tarjetas) y la variabilidad de OBV. Compiten con el mismo criterio que los
ajustes, pero con la versión simple como base: los completos solo se quedan si anticipan mejor
el OBV en el club nuevo con IC 95% > 0. Gráfica `07_carrera_indices.png` y sección 0b de
Validación.

## Qué cambió respecto a la versión anterior del modelo

- **Sin tabla manual de calidad.** El nivel de cada equipo sale de su diferencial de xG sin
  penaltis por partido, por torneo.
- **Sin fuga temporal en el backtest.** En cada corte, el ajuste contextual se re-estima solo
  con partidos anteriores.
- **Perfil por residuos.** Rendimiento por encima de lo esperado para la posición jugada en
  cada partido y su contexto.
- **Consistencia redefinida.** La versión anterior (desviación estándar de xG/xA/presiones)
  premiaba al que produce poco. Ahora: pérdidas, desposesiones, precisión de pase y la
  variabilidad de OBV que no explica el volumen.
- **OBV fuera de los índices.** Se usa como validación externa independiente.
- **Rankings y percentiles con contracción.** Se ordena por `z_compuesto_fiable`, el índice que
  se validó; antes se ordenaba sin contracción y un jugador con 300 minutos podía salir primero.
  `percentil_sin_contraccion` queda guardado aparte.
- **Tope de ±3σ por componente y por índice**, para que un valor extremo en una sola métrica
  no arrastre el índice entero, ni un solo índice el compuesto.
- **Comparaciones por subgrupo**, no por línea (un lateral ya no sale "gemelo" de un central).
- **Subgrupo por bloque y sin tirar partidos.** Primero se elige el bloque (defensa, medio,
  ataque) con más minutos y, dentro de él, el subgrupo con más minutos. El perfil conserva
  todos los partidos: el residuo de cada uno ya se mide contra la posición que ocupó ese día.
  Antes, un mediapunta que a veces jugaba de 9 podía quedar como delantero y perder sus
  minutos de medio campo. La columna `pct_min_subgrupo` dice qué % de sus minutos jugó en su
  subgrupo; la app marca "perfil versátil" debajo de 70%.
- **Segundo delantero (SS) cuenta como mediapunta**, no como delantero centro.
- **Edad, disponibilidad y lesiones** a partir de las alineaciones.
- **Contracción por minutos**: fiabilidad estimada por mitades (partidos pares vs impares).
- **Probabilidad de consolidarse** en el módulo de Fichajes, con el modelo validado.
- Se quitó el simulador entre ligas (con una sola liga no aplica).

## Limitaciones que hay que declarar

- **Sesgo de selección**: solo se observa el rendimiento de quien sí fue fichado.
- **Éxito = minutos**, que también dependen del entrenador.
- **Lesiones inferidas**, no historial médico: salidas por lesión y partidos de baja posteriores.
- **Nivel del propio equipo como control** (método A): absorbe parte del mérito individual
  del jugador bueno en equipo bueno. Por eso compiten B y C; si ninguno gana con IC > 0,
  la diferencia no es distinguible del ruido con estos datos.
- **Método B en el backtest**: el índice de los compañeros del torneo anterior sale de los
  perfiles con el ajuste A estimado con toda la muestra. Es una fuga pequeña (solo los
  coeficientes de contexto) que favorece levemente a B; conviene declararla.
- Los resultados con `tests/simular_datos.R` **no dicen nada de Liga MX**: solo prueban que
  el código corre y que detecta una señal cuando la hay.
