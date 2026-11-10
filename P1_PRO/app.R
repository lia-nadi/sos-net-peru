# =============================================================================
#  SOS Net Perú · Telecomunicaciones de emergencia ante un sismo
#  AI for Good Sandbox Hackathon (ITU) 2026 · Prototipo R Shiny
#  Datos reales: Censo Nacional 2025 (INEI), OpenCelliD, USGS, OpenStreetMap,
#  relieve AWS/Mapzen, normativa peruana verificada (knowledge_base/).
#  Primera ejecución: source("../iniciar_app.R") desde el proyecto, o
#  Rscript preparar_datos.R dentro de P1_PRO.
# =============================================================================


suppressPackageStartupMessages({
  # markdown: lo usa includeMarkdown() de forma indirecta; se carga aquí para que
  # rsconnect lo incluya en manifest.json (Posit Connect Cloud)
  library(markdown)
  library(shiny); library(bslib); library(sf); library(dplyr)
  library(plotly); library(DT); library(jsonlite); library(readr)
})
sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/simulacion.R", encoding = "UTF-8")
source("R/normativa.R", encoding = "UTF-8")
source("R/interpretacion.R", encoding = "UTF-8")
source("R/validacion.R", encoding = "UTF-8")
source("R/informe.R", encoding = "UTF-8")
source("R/vs30_pendiente.R", encoding = "UTF-8")
source("R/malla.R", encoding = "UTF-8")

if (!file.exists("data/sitios.csv")) source("preparar_datos.R", encoding = "UTF-8")

# ------------------------------------------------------------------ datos
distritos_sf <- st_read("data/distritos_lima.geojson", quiet = TRUE)
distritos <- st_drop_geometry(distritos_sf)
distritos$costero[is.na(distritos$costero)] <- FALSE
distritos_geo <- read_json("data/distritos_lima.geojson", simplifyVector = FALSE)
costa_geo <- read_json("data/costa.geojson", simplifyVector = FALSE)
costa_xy <- st_coordinates(st_read("data/costa.geojson", quiet = TRUE))
costa_pts <- data.frame(lon = costa_xy[seq(1, nrow(costa_xy), 5), 1],
                        lat = costa_xy[seq(1, nrow(costa_xy), 5), 2])
contorno <- st_read("data/peru_contorno.geojson", quiet = TRUE)
sitios <- read_csv("data/sitios.csv", show_col_types = FALSE)
servicios <- if (file.exists("data/servicios_osm.csv"))
  read_csv("data/servicios_osm.csv", show_col_types = FALSE) else data.frame()
sismos <- read_csv("data/sismos_usgs.csv", show_col_types = FALSE)
# Relieve real (AWS Terrain Tiles) y malla de intensidad de ~450 m (se generan una vez)
DEM <- dem_lima()
MALLA <- malla_base(distritos_sf)
sitios$z <- altura_puntos(sitios$lon, sitios$lat, DEM)
if (nrow(servicios)) servicios$z <- altura_puntos(servicios$lon, servicios$lat, DEM)
LIMA <- c(lat = -12.046, lon = -77.043)
# En el proyecto completo se usan ../motor_agentes y ../knowledge_base; al publicar solo P1_PRO
# (Posit Connect Cloud) se usa la copia de P1_PRO/agentes/ (bitácoras y base de conocimiento).
DIR_MOTOR <- normalizePath(if (dir.exists("../motor_agentes/salidas")) "../motor_agentes" else "agentes", mustWork = FALSE)
ARCH_KB <- c("../knowledge_base/kb_articulos.jsonl", "agentes/kb_articulos.jsonl")
ARCH_KB <- ARCH_KB[file.exists(ARCH_KB)][1]
KB <- if (!is.na(ARCH_KB)) jsonlite::stream_in(file(ARCH_KB, encoding = "UTF-8"), verbose = FALSE) else NULL
PY <- if (.Platform$OS.type == "windows") "../.venv/Scripts/python.exe" else "../.venv/bin/python"
leer_bitacora <- function(f) {
  l <- readLines(f, encoding = "UTF-8", warn = FALSE)
  lapply(l[nzchar(l)], jsonlite::fromJSON, simplifyVector = FALSE)
}
POB_TOTAL <- sum(distritos$poblacion)

# Validación del modelo con 167 intensidades reales (80/20 por sismo, LOEO) y pruebas de coherencia: se calcula al arrancar (~3 s)
VALID <- validar_intensidad(distritos)
PRUEBAS <- suppressMessages(pruebas_coherencia(distritos, sitios, costa_pts, contorno))
VEREDICTO <- veredicto(VALID, PRUEBAS)
VEREDICTO$estado <- if (VEREDICTO$ok) "Validado" else if (VEREDICTO$n_ok >= VEREDICTO$n - 1) "Validado con reservas" else "No validado"
VEREDICTO$texto <- sprintf(paste(
  "%s (%d de %d criterios). Con %d intensidades reales de %d sismos (USGS e IGP), el error es de %.2f grados MMI al calibrar, %.2f en la prueba 80/20 por sismo (mediana de %d particiones)",
  "y %.2f al predecir un sismo que el modelo no vio. Pruebas de coherencia: %d de %d superadas, incluida la comprobación con",
  "el sismo real de Callao 2025 (OSIPTEL: sin interrupción)."),
  VEREDICTO$estado, VEREDICTO$n_ok, VEREDICTO$n, VALID$n_obs, VALID$n_ev, VALID$m_calib[["rmse"]], VALID$m_8020[["rmse_mediana"]], VALID$reps,
  VALID$m_loeo[["rmse"]], sum(PRUEBAS$resultado), nrow(PRUEBAS))

pal_nivel <- c("CRÍTICA" = "#ef4444", "ALTA" = "#f59e0b", "MEDIA" = "#eab308", "INFO" = "#38bdf8")
AGENTES <- list(
  operador_a = list(nombre = "Operador A", sub = "Concesionario móvil", icono = "tower-cell", color = "#f97316"),
  regulador = list(nombre = "Regulador", sub = "MTC / OSIPTEL", icono = "scale-balanced", color = "#a78bfa"),
  defensa_civil = list(nombre = "INDECI / COEN", sub = "Defensa civil", icono = "shield-heart", color = "#ef4444"),
  autoridad_datos = list(nombre = "Autoridad de datos", sub = "ANPD · MINJUSDH", icono = "user-shield", color = "#38bdf8"))

# ------------------------------------------------------------------ componentes de UI
ayuda <- function(texto, titulo = NULL) {
  popover(tags$span(class = "ayuda", icon("circle-question")), texto, title = titulo, placement = "right")
}
etiqueta <- function(texto, ayuda_txt) tags$span(texto, ayuda(ayuda_txt))

kpi <- function(id_out, titulo, icono, clase, ayuda_txt, id_box = NULL) {
  div(id = id_box, class = paste("kpi", clase),
      div(class = "kpi-icono", icon(icono)),
      div(class = "kpi-cuerpo",
          div(class = "kpi-titulo", titulo, ayuda(ayuda_txt)),
          div(class = "kpi-valor", textOutput(id_out, inline = TRUE)),
          div(class = "kpi-sub", textOutput(paste0(id_out, "_sub"), inline = TRUE))))
}

leyenda <- HTML(paste0(
  "<div class='p1-legend'><div class='lg-t'>Antenas</div>",
  "<span class='dot' style='background:#009E73'></span>Operativa <span class='dot' style='background:#F0E442'></span>Con batería<br>",
  "<span class='dot' style='background:#D55E00'></span>Caída <span class='dot' style='background:#56B4E9'></span>Restablecida<br>",
  "<div class='lg-t'>Intensidad (MMI)</div><div class='mmi-bar'></div>",
  "<div class='mmi-lbl'><span>IV</span><span>VI</span><span>VIII</span><span>X</span></div>",
  "<span class='dot' style='background:#fff'></span>Hospital <span class='dot' style='background:#ff7800'></span>Bomberos<br>",
  "<span style='color:#ff0080'>━</span> Ruptura &nbsp;<span style='color:#00f0ff'>◯</span> Tsunami</div>"))

mapa_div <- function(id, center, zoom, extra = NULL) {
  div(class = "p1-map-wrap",
      div(id = id, class = "p1map", `data-center` = toJSON(center), `data-zoom` = zoom),
      div(id = paste0(id, "_reloj"), class = "p1-reloj"),
      extra)
}

tarjeta_caso <- function(id, c) {
  div(class = "caso", style = sprintf("--c:%s", c$color),
      div(class = "caso-top", div(class = "caso-icono", icon(c$icono)),
          div(tags$span(class = "caso-tipo", c$tipo), tags$h5(c$titulo), tags$small(c$etiqueta))),
      tags$p(class = "caso-historia", c$historia),
      tags$p(class = "caso-pregunta", icon("circle-question"), " ", c$pregunta),
      tags$button(class = "btn btn-caso w-100", onclick = sprintf("Shiny.setInputValue('caso','%s',{priority:'event'})", id),
                  icon("play"), " Cargar y simular"))
}

# ------------------------------------------------------------------ pestañas
tab_inicio <- nav_panel(
  "Inicio", value = "inicio", icon = icon("house"),
  div(class = "hero",
      div(class = "hero-texto",
          tags$span(class = "hero-chip", icon("satellite"), " AI for Good Sandbox · UIT 2026 · Perú"),
          tags$h1("¿Qué pasa con las ", tags$span(class = "grad", "comunicaciones"), " cuando un gran sismo golpea Lima?"),
          tags$p("SOS Net simula el terremoto sobre datos reales, muestra qué antenas caen, quién recibe la alerta a tiempo y qué normas peruanas se activan… o no existen. Cuatro agentes de IA representan a las instituciones y detectan los vacíos legales."),
          div(class = "hero-botones",
              tags$button(class = "btn btn-hero", onclick = "Shiny.setInputValue('caso','p1',{priority:'event'})", icon("play"), " Simular el escenario P1"),
              tags$button(class = "btn btn-hero-sec", onclick = "iniciarTour()", icon("route"), " Recorrido guiado (2 min)"))),
      div(class = "hero-stats",
          div(class = "hstat", tags$b(fmt_m(POB_TOTAL)), tags$span("habitantes · Censo 2025 (INEI)")),
          div(class = "hstat", tags$b(fmt_n(nrow(sitios))), tags$span("antenas reales · OpenCelliD")),
          div(class = "hstat", tags$b(nrow(KB %||% data.frame())), tags$span("artículos de ley verificados")),
          div(class = "hstat", tags$b("10"), tags$span("vacíos normativos detectados")))),
  div(class = "seccion-titulo", icon("shoe-prints"), " Empieza en 3 pasos"),
  layout_column_wrap(width = "260px", fill = FALSE,
    div(class = "paso", div(class = "paso-n", "1"), tags$h6("Elige un caso"), tags$p("Usa los casos de abajo o configura tu propio sismo en el Simulador.")),
    div(class = "paso", div(class = "paso-n", "2"), tags$h6("Mira el mapa 3D y el tiempo"), tags$p("Mueve la línea de tiempo o pulsa ▶ para ver cómo caen y se recuperan las antenas.")),
    div(class = "paso", div(class = "paso-n", "3"), tags$h6("Lee las alertas"), tags$p("La campana 🔔 reúne las alertas con su norma, el vacío legal y qué se propone."))),
  div(class = "seccion-titulo", icon("flask"), " Casos de ejemplo · un clic y listo"),
  do.call(layout_column_wrap, c(list(width = "300px", fill = FALSE, id = "galeria_casos"),
                                lapply(names(CASOS), function(k) tarjeta_caso(k, CASOS[[k]])))),
  div(class = "seccion-titulo", icon("layer-group"), " ¿Qué hay en cada menú?"),
  layout_column_wrap(width = "240px", fill = FALSE,
    div(class = "modulo", icon("house-crack"), tags$h6("Simulador 3D"), tags$p("Gemelo digital: sismo, antenas, alertas, tsunami y escenarios «¿qué pasaría si?».")),
    div(class = "modulo", icon("people-group"), tags$h6("Agentes IA"), tags$p("Cuatro instituciones simuladas por IA deciden citando la ley; se auditan sus razonamientos.")),
    div(class = "modulo", icon("satellite-dish"), tags$h6("Monitoreo en vivo"), tags$p("Sismos reales del USGS actualizados cada 5 minutos y nivel de alerta.")),
    div(class = "modulo", icon("scale-balanced"), tags$h6("Vacíos legales"), tags$p("Los 10 vacíos detectados, con la norma peruana y el ejemplo internacional.")))
)

tab_sim <- nav_panel(
  "Simulador 3D", value = "sim", icon = icon("house-crack"),
  layout_sidebar(
    sidebar = sidebar(
      width = 340, open = "desktop", id = "barra_sim",
      accordion(id = "acc_sim", open = c("Sismo"), multiple = TRUE,
        accordion_panel("Sismo", icon = icon("wave-square"),
          selectInput("escenario", etiqueta("Escenario", "Sismos reales del catálogo USGS o escenarios sintéticos. Al elegir uno se cargan su epicentro, magnitud y profundidad."), names(ESCENARIOS)),
          uiOutput("nota_escenario"),
          layout_columns(
            numericInput("lat", "Latitud", -12.50, step = 0.01),
            numericInput("lon", "Longitud", -77.45, step = 0.01)),
          div(class = "tip", icon("hand-pointer"), " Haz clic en el mapa para mover el epicentro."),
          sliderInput("M", etiqueta("Magnitud (Mw)", "Desde M7 el sismo se modela como una ruptura de falla a lo largo de la fosa (línea magenta)."), 5, 9.2, 8.0, step = 0.1),
          sliderInput("prof", "Profundidad (km)", 5, 150, 30)),
        accordion_panel("¿Qué pasaría si…?", icon = icon("wand-magic-sparkles"),
          div(class = "tip", "Cada interruptor simula que se cierra un vacío legal. Luego pulsa «Simular»."),
          input_switch("alerta_auto", etiqueta("Alerta automática IGP → SISMATE", "Vacío G5: hoy el mensaje pasa por una cadena manual en el COEN. La Ley 30472 permite validar mensajes por adelantado."), FALSE),
          conditionalPanel("!input.alerta_auto",
            sliderInput("min_manual", "Minutos de la cadena manual", 1, 20, 8)),
          input_switch("activacion_auto", etiqueta("Activación automática por umbral", "Vacío G3: hoy todo depende de que INDECI comunique la emergencia (DS 051-2010-MTC, art. 23)."), FALSE),
          conditionalPanel("!input.activacion_auto",
            sliderInput("min_indeci", "Minutos hasta la comunicación de INDECI", 5, 120, 40)),
          input_switch("roaming", etiqueta("Roaming de emergencia", "Vacío G1: hoy el roaming nacional solo es obligatorio en pueblos de hasta 1 000 habitantes (DS 002-2023-MTC)."), FALSE),
          input_switch("portatiles", "Estaciones portátiles priorizadas", FALSE)),
        accordion_panel("Supuestos declarados", icon = icon("clipboard-check"),
          input_switch("supuesto_A", etiqueta("Operador A vulnerable en Lima Sur", "Supuesto del escenario P1: anillo de fibra sin respaldo y baterías de menor autonomía. Apágalo para ver el resultado sin este supuesto."), TRUE)),
        accordion_panel("Capas del mapa", icon = icon("layer-group"),
          radioButtons("modo", "Colorear el mapa por", c("Intensidad del sismo (MMI)" = "mmi", "Tiempo de aviso" = "aviso")),
          checkboxGroupInput("ops", "Operadores", c("A" = "A", "B" = "B", "C" = "C", "D" = "D"), selected = c("A", "B", "C", "D"), inline = TRUE),
          radioButtons("base", "Mapa base", c("Oscuro" = "oscuro", "Satélite (Esri)" = "satelite"), "oscuro", inline = TRUE),
          checkboxGroupInput("capas", "Capas", c("Intensidad continua y distritos" = "distritos", "Antenas" = "sitios",
                                                 "Hospitales y bomberos" = "servicios", "Costa" = "costa",
                                                 "Edificios 3D (OSM, acerca el zoom)" = "edificios"),
                             selected = c("distritos", "sitios", "servicios", "costa", "edificios")),
          input_switch("terreno", "Relieve 3D real", TRUE))),
      actionButton("simular", "Simular sismo", icon = icon("bolt"), class = "btn-simular w-100")
    ),
    div(id = "fila_kpi", class = "fila-kpi",
      kpi("kpi_pob", "Población con MMI ≥ VII", "people-group", "k-rojo", "Personas en distritos con intensidad VII o más: daño moderado o mayor en construcciones vulnerables."),
      kpi("kpi_caidos", "Antenas caídas ahora", "tower-cell", "k-naranja", "Porcentaje de sitios móviles sin servicio en el instante elegido en la línea de tiempo."),
      kpi("kpi_bloqueo", "Llamadas que fallan", "phone-slash", "k-ambar", "Intentos de llamada bloqueados por congestión: tras un sismo todos llaman a la vez."),
      kpi("kpi_aviso", "Alerta tardía", "bell-slash", "k-violeta", "Población que recibe la alerta DESPUÉS de que llega la onda S (el sacudimiento fuerte)."),
      kpi("kpi_alertas", "Alertas activas", "bell", "k-teal", "Eventos ya ocurridos en la bitácora y cuántos revelan un vacío legal. Ábrelos con la campana 🔔.")),
    layout_columns(col_widths = c(8, 4), fill = FALSE,
      card(full_screen = TRUE, id = "card_mapa", class = "card-mapa",
        card_header(class = "d-flex align-items-center gap-2",
          icon("map-location-dot"), tags$b("Lima Metropolitana y Callao · vista 3D"),
          tags$span(class = "ms-auto small text-muted", "Arrastra para mover · clic derecho para inclinar")),
        card_body(padding = 0, mapa_div("map_sim", c(-77.0, -12.15), 9.7, leyenda)),
        card_footer(div(id = "linea_tiempo",
          sliderInput("t", tags$span(icon("clock"), " Tiempo desde el sismo (horas) · pulsa ▶ para reproducir"), 0, 72, 0, step = 0.5,
                      width = "100%", animate = animationOptions(interval = 700))))),
      card(id = "card_lectura", class = "card-lectura",
        card_header(icon("lightbulb"), tags$b(" Lectura del escenario"), tags$small(class = "text-muted", " · en lenguaje claro")),
        card_body(uiOutput("lectura")))),
    navset_card_pill(id = "tabs_inferiores",
      nav_panel(tagList(icon("list-check"), " Bitácora normativa"),
                div(id = "descargas_72h", class = "barra-descargas",
                    tags$span(icon("download"), tags$b(" Descargar las 72 h:")),
                    downloadButton("dl_informe", "Informe completo (HTML/PDF)", class = "btn-sm btn-descarga", icon = icon("file-lines")),
                    downloadButton("dl_alertas", "Alertas (CSV)", class = "btn-sm btn-descarga", icon = icon("bell")),
                    downloadButton("dl_horario", "Qué pasa hora a hora (CSV)", class = "btn-sm btn-descarga", icon = icon("clock")),
                    ayuda("El informe se abre en el navegador; usa Imprimir → Guardar como PDF. Los CSV se abren en Excel. Todo corresponde al escenario simulado actual, de 0 a 72 h, aunque la línea de tiempo esté en otro instante.")),
                DTOutput("tabla_eventos")),
      nav_panel(tagList(icon("chart-line"), " Red y roaming"),
                layout_columns(plotlyOutput("plot_disp", height = 330), plotlyOutput("plot_red", height = 330))),
      nav_panel(tagList(icon("table"), " Distritos"), DTOutput("tabla_distritos")))
  )
)

tab_agentes <- nav_panel(
  "Agentes IA", value = "agentes", icon = icon("people-group"),
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectInput("corrida", "Ejecución a revisar", choices = NULL),
      actionButton("correr_live", "Ejecutar con IA (live)", icon = icon("robot"), class = "btn-simular w-100 mb-2"),
      actionButton("correr_replay", "Ejecutar sin IA (replay)", icon = icon("rotate"), class = "btn-outline-light w-100"),
      accordion(open = FALSE,
        accordion_panel("¿Cómo leer esta pantalla?", icon = icon("circle-info"),
          tags$ol(class = "small",
            tags$li("Cada tarjeta es una decisión de una institución simulada por IA."),
            tags$li("Las etiquetas celestes son los artículos citados: pasa el cursor para leer la norma."),
            tags$li("Las etiquetas amarillas (G1-G10) son vacíos legales detectados."),
            tags$li("«Traza de razonamiento» muestra cómo pensó el modelo (auditoría)."))),
        accordion_panel("Pipeline ITU-T Y.3172", icon = icon("diagram-project"),
          tags$small("SRC: director y hechos por etapa · C: filtro de visibilidad · PP: búsqueda BM25 · M: 4 agentes LLM · P: verificador de citas y detector de vacíos · D: mensajes · SINK: bitácora e informe")),
        accordion_panel("Proveedores de IA", icon = icon("key"),
          tags$small("Live usa Claude, Groq o DeepSeek según la clave en P1_PROYECT/.env (ANTHROPIC_API_KEY, GROQ_API_KEY o DEEPSEEK_API_KEY). Replay reproduce la clave de respuestas sin IA.")))
    ),
    uiOutput("resumen_corrida"),
    uiOutput("stepper_etapas"),
    navset_card_pill(
      nav_panel(tagList(icon("id-card"), " Decisiones por etapa"), uiOutput("tarjetas_agentes")),
      nav_panel(tagList(icon("magnifying-glass"), " Auditoría"), DTOutput("tabla_bitacora")),
      nav_panel(tagList(icon("book"), " Base de conocimiento"), DTOutput("tabla_kb")))
  )
)

tab_vivo <- nav_panel(
  "Monitoreo en vivo", value = "vivo", icon = icon("satellite-dish"),
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput("periodo", "Periodo", c("Últimas 24 h" = 1, "Últimos 7 días" = 7, "Últimos 30 días" = 30), 7),
      sliderInput("min_mag_vivo", "Magnitud mínima", 2.5, 6, 3.5, step = 0.5),
      input_switch("ver_hist", "Catálogo histórico 3D (1900-hoy)", TRUE),
      sliderInput("min_mag_hist", "Magnitud mínima del histórico", 4.5, 8, 6, step = 0.5),
      input_switch("terreno_vivo", "Relieve 3D", TRUE),
      actionButton("refrescar", "Actualizar ahora", icon = icon("rotate"), class = "btn-outline-light w-100"),
      tags$small(class = "text-muted", textOutput("ultima_act")),
      accordion(open = FALSE, accordion_panel("¿Cómo se calcula el nivel?", icon = icon("circle-info"),
        tags$small("VERDE: sin eventos relevantes. AMARILLO: M ≥ 4,5 a menos de 200 km de Lima en 24 h. ROJO: M ≥ 6 a menos de 300 km de Lima en 24 h. Umbrales PROPUESTOS por este prototipo; la norma vigente no los fija. Fuente: USGS, actualización cada 5 minutos.")))
    ),
    uiOutput("banner_alerta"),
    div(class = "fila-kpi",
      kpi("v_ultimo", "Último sismo", "location-crosshairs", "k-teal", "Magnitud y hora (Lima) del sismo más reciente del periodo."),
      kpi("v_n", "Sismos en el periodo", "chart-simple", "k-azul", "Cantidad de sismos que cumplen el filtro de magnitud."),
      kpi("v_max", "Mayor magnitud", "arrow-up-wide-short", "k-naranja", "La magnitud más alta registrada en el periodo."),
      kpi("v_nivel", "Nivel de alerta", "traffic-light", "k-rojo", "Nivel propuesto por el prototipo según magnitud y distancia a Lima.")),
    card(full_screen = TRUE,
         card_header(icon("earth-americas"), " Sismicidad del Perú en 3D · altura = magnitud · color = profundidad (rojo superficial → azul profundo)"),
         card_body(padding = 0, mapa_div("map_vivo", c(-75.5, -10.5), 4.7))),
    card(card_header(icon("list"), " Eventos recientes"), DTOutput("tabla_vivo"))
  )
)

tab_norm <- nav_panel(
  "Vacíos legales", value = "vacios", icon = icon("scale-balanced"),
  div(class = "seccion-titulo", icon("scale-balanced"), " 10 vacíos normativos detectados · telecomunicaciones de emergencia"),
  tags$p(class = "text-muted", "Haz clic en cada tarjeta para ver la evidencia en la norma peruana, el ejemplo internacional y la propuesta."),
  uiOutput("tarjetas_vacios"),
  accordion(open = FALSE, accordion_panel("Ver como tabla", icon = icon("table"), DTOutput("tabla_vacios")))
)

tab_valid <- nav_panel(
  "Validación", value = "valid", icon = icon("clipboard-check"),
  uiOutput("banner_validacion"),
  div(class = "fila-kpi",
    kpi("val_calib", "Error al calibrar", "bullseye", "k-teal", "Error (RMSE) con las 167 intensidades reales usando los coeficientes de la app; cada sismo pesa lo mismo. Mide el ajuste, no la capacidad de predecir."),
    kpi("val_8020", "Error en prueba 80/20", "shuffle", "k-azul", "Se ajusta con el 80 % de los sismos (22) y se mide el error en el 20 % restante (6), que el ajuste nunca vio. Se separa por sismo, no por dato, para no hacer trampa. Se repite 500 veces."),
    kpi("val_loeo", "Error con un sismo no visto", "eye-slash", "k-violeta", "Se deja fuera un sismo completo, se reajusta con los otros y se predice el que quedó fuera. Es la prueba más exigente: imita un terremoto nuevo."),
    kpi("val_pruebas", "Pruebas de coherencia", "list-check", "k-verde", "Comprobaciones automáticas de física, red, alertas, políticas, reproducibilidad y un evento real.")),
  layout_columns(col_widths = c(6, 6),
    card(card_header(icon("circle-question"), tags$b(" ¿Se usa 80/20?")),
      card_body(
        tags$p("Sí. El modelo de intensidad se calibra y valida con ", tags$b("167 intensidades reales observadas en Lima y Callao, de 28 sismos (1940-2026)"), ":"),
        tags$ul(
          tags$li(tags$b("USGS ShakeMap:"), " 95 reportes ciudadanos «¿Lo sentiste?», 39 intensidades históricas y 24 registros de estaciones sísmicas."),
          tags$li(tags$b("IGP:"), " 9 intensidades por distrito de 1940, 1974, 2007 y 2025.")),
        tags$ol(
          tags$li(tags$b("80/20 por sismo, 500 veces:"), " el 80 % de los sismos ajusta el modelo y el 20 % lo prueba. Nunca se mezclan datos de un mismo sismo en los dos lados (semilla fija 2026, reproducible)."),
          tags$li(tags$b("Dejar fuera cada sismo (LOEO, 28 pliegues):"), " se predice cada terremoto sin haberlo visto. Es lo más parecido a un sismo nuevo.")),
        tags$p(class = "small text-muted", "Cada sismo pesa lo mismo: uno con 30 reportes no domina sobre uno con 2. Los criterios se fijaron antes de medir y no se ajustan para que pasen. La red móvil (baterías, fibra) no tiene datos observados por sitio: se valida con pruebas de coherencia y con el sismo real de Callao 2025."))),
    card(card_header(icon("chart-column"), tags$b(" Error de prueba en las 500 particiones 80/20")),
         card_body(plotlyOutput("plot_val_8020", height = 300)))),
  layout_columns(col_widths = c(6, 6),
    card(card_header(icon("chart-line"), tags$b(" Observado vs. predicho")),
         card_body(plotlyOutput("plot_val_obs", height = 330))),
    card(card_header(icon("table"), tags$b(" Criterios de aceptación")),
         card_body(uiOutput("tabla_criterios"),
                   tags$h6(class = "mt-3", "Error por tipo de dato (sismo no visto)"), DTOutput("tabla_val_tipo"),
                   tags$h6(class = "mt-3", "Error por sismo (dejándolo fuera)"), DTOutput("tabla_val_evento")))),
  card(card_header(class = "d-flex align-items-center gap-2", icon("vial-circle-check"), tags$b(" Pruebas de coherencia del simulador"),
                   tags$span(class = "ms-auto", downloadButton("dl_validacion", "Descargar validación (CSV)", class = "btn-sm btn-descarga"))),
       card_body(DTOutput("tabla_pruebas"))),
  card(card_header(icon("database"), tags$b(" Las 167 observaciones reales y sus predicciones")), card_body(DTOutput("tabla_val_obs")))
)

tab_fuentes <- nav_panel(
  "Datos y fuentes", value = "fuentes", icon = icon("database"),
  card(card_body(includeMarkdown("FUENTES.md")))
)

centro_alertas <- div(class = "offcanvas offcanvas-end offcanvas-alertas", tabindex = "-1", id = "centroAlertas",
  div(class = "offcanvas-header",
      tags$h5(class = "offcanvas-title", icon("bell"), " Centro de alertas"),
      tags$button(type = "button", class = "btn-close btn-close-white", `data-bs-dismiss` = "offcanvas")),
  div(class = "offcanvas-body",
      div(class = "small text-muted mb-2", "Alertas ocurridas hasta el instante de la línea de tiempo. Despliega cada una para ver la norma, el vacío y la propuesta."),
      uiOutput("lista_alertas")))

ui <- page_navbar(
  window_title = "SOS Net Perú · Emergencias Telecom",
  id = "nav",
  title = tags$span(class = "marca", tags$span(class = "marca-logo", icon("tower-broadcast")),
                    tags$b("SOS Net"), tags$span(class = "marca-sub", " Perú")),
  theme = bs_theme(version = 5, bg = "#0b1220", fg = "#e6edf6", primary = "#14b8a6", secondary = "#334155",
                   success = "#22c55e", info = "#38bdf8", warning = "#f59e0b", danger = "#ef4444",
                   base_font = font_google("Inter", local = FALSE), heading_font = font_google("Poppins", local = FALSE),
                   "navbar-bg" = "#0b1220", "card-bg" = "#111a2e", "border-radius" = "0.9rem"),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.css"),
    tags$script(src = "https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.js"),
    tags$script(src = "https://unpkg.com/deck.gl@9.0.38/dist.min.js"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/driver.js@1.3.1/dist/driver.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/driver.js@1.3.1/dist/driver.js.iife.js"),
    tags$link(rel = "stylesheet", href = "estilos.css")
  ),
  tab_inicio, tab_sim, tab_agentes, tab_vivo, tab_norm, tab_valid, tab_fuentes,
  nav_spacer(),
  nav_item(tags$button(class = "btn btn-nav-ayuda", onclick = "iniciarTour()", icon("route"), " ¿Cómo se usa?")),
  nav_item(tags$button(id = "btn_sonido", class = "btn btn-sonido", onclick = "SismoSonido.alternar()",
                       title = "Sonido de alarmas", icon("volume-high"))),
  nav_item(tags$button(id = "btn_alertas", class = "btn btn-campana", `data-bs-toggle` = "offcanvas",
                       `data-bs-target` = "#centroAlertas", icon("bell"),
                       tags$span(class = "badge-alertas", textOutput("n_alertas", inline = TRUE)))),
  footer = tagList(centro_alertas,
                   div(class = "pie", "SOS Net Perú · Prototipo AI for Good Sandbox 2026 · Datos: INEI Censo 2025, OpenCelliD (CC BY-SA 4.0), USGS, OpenStreetMap · Modelos ilustrativos"),
                   tags$script(src = "sonidos.js"), tags$script(src = "mapa3d.js"), tags$script(src = "tour.js"))
)

# ------------------------------------------------------------------ SERVER
server <- function(input, output, session) {

  session$sendCustomMessage("p1_static", list(
    id = "map_sim", distritos = distritos_geo, costa = costa_geo,
    sitios = list(lon = sitios$lon, lat = sitios$lat, op = sitios$operador,
                  id = sitios$sitio, fuente = sitios$fuente, tec = sitios$tecnologias, cel = sitios$celdas, z = sitios$z),
    servicios = as.character(toJSON(servicios, dataframe = "rows", na = "null"))))
  session$sendCustomMessage("p1_static", list(id = "map_vivo", costa = costa_geo))

  # ---------------------------------------------------- escenarios y casos
  observeEvent(input$escenario, {
    e <- ESCENARIOS[[input$escenario]]
    updateNumericInput(session, "lat", value = e$lat)
    updateNumericInput(session, "lon", value = e$lon)
    updateSliderInput(session, "M", value = e$M)
    updateSliderInput(session, "prof", value = e$prof)
  })
  output$nota_escenario <- renderUI(div(class = "nota-esc", icon("circle-info"), " ", ESCENARIOS[[input$escenario]]$nota))
  observeEvent(input$map_sim_click, {
    updateNumericInput(session, "lat", value = round(input$map_sim_click$lat, 3))
    updateNumericInput(session, "lon", value = round(input$map_sim_click$lon, 3))
  })

  caso_p <- reactiveVal(NULL)
  disparo <- reactiveVal(0)
  POL_BASE <- list(alerta_auto = FALSE, activacion_auto = FALSE, roaming = FALSE, portatiles = FALSE,
                   min_manual = 8, min_indeci = 40, supuesto_A = TRUE)
  observeEvent(input$caso, {
    c <- CASOS[[input$caso]]; e <- ESCENARIOS[[c$escenario]]
    pol <- modifyList(POL_BASE, c$politicas)
    caso_p(c(list(lat = e$lat, lon = e$lon, M = e$M, prof = e$prof), pol))
    freezeReactiveValue(input, "escenario")
    updateSelectInput(session, "escenario", selected = c$escenario)
    updateNumericInput(session, "lat", value = e$lat); updateNumericInput(session, "lon", value = e$lon)
    updateSliderInput(session, "M", value = e$M); updateSliderInput(session, "prof", value = e$prof)
    for (k in c("alerta_auto", "activacion_auto", "roaming", "portatiles", "supuesto_A")) update_switch(k, value = pol[[k]])
    updateSliderInput(session, "min_manual", value = pol$min_manual)
    updateSliderInput(session, "min_indeci", value = pol$min_indeci)
    nav_select("nav", "sim")
    accordion_panel_open("acc_sim", "¿Qué pasaría si…?")
    showNotification(tagList(icon("flask"), tags$b(paste(" Caso cargado:", c$titulo)), tags$br(), c$pregunta),
                     type = "message", duration = 6)
    # Primero se envía el cambio de pestaña; la simulación arranca en el ciclo siguiente
    session$onFlushed(function() disparo(isolate(disparo()) + 1), once = TRUE)
  })
  observeEvent(list(input$simular, disparo()), {
    showNotification(tagList(icon("bolt"), " Simulando el sismo y la red…"), id = "simulando", duration = NULL)
  }, ignoreInit = TRUE, priority = 10)

  sim <- eventReactive(list(input$simular, disparo()), {
    p <- isolate(caso_p()) %||% list(
      lat = input$lat, lon = input$lon, M = input$M, prof = input$prof,
      alerta_auto = input$alerta_auto, min_manual = input$min_manual %||% 8,
      activacion_auto = input$activacion_auto, min_indeci = input$min_indeci %||% 40,
      roaming = input$roaming, portatiles = input$portatiles, supuesto_A = input$supuesto_A)
    caso_p(NULL)
    s <- simular_escenario(p, distritos, sitios, costa_pts, contorno)
    s$curvas <- curvas_red(s, sitios, distritos)
    s$eventos <- eventos_normativos(s, s$curvas, distritos)
    s
  }, ignoreNULL = FALSE)

  observeEvent(sim(), {
    s <- sim()
    session$sendCustomMessage("p1_update", list(
      id = "map_sim", version = as.numeric(Sys.time()),
      mmi = round(s$mmi_d, 2), mmiR = romano(s$mmi_d), pga = round(s$pga_d, 2),
      aviso = round(s$aviso_s), tsu = ifelse(is.na(s$t_tsu_min), NA, round(s$t_tsu_min)),
      epi = list(lon = s$p$lon, lat = s$p$lat), tsunami = s$tsunami,
      ruptura = if (is.null(s$ruptura)) list() else s$ruptura, L_km = round(s$L_km),
      malla = malla_escenario(s, MALLA)))
    session$sendCustomMessage("p1_fly", list(id = "map_sim", lon = 0.3 * s$p$lon + 0.7 * -77.02,
                                             lat = 0.3 * s$p$lat + 0.7 * -12.12, zoom = 9.4))
    session$sendCustomMessage("p1_animate", list(
      id = "map_sim", epi = list(lon = s$p$lon, lat = s$p$lat), prof = s$p$prof,
      t_alerta = s$t_alerta, sim_s = min(150, max(60, max(s$t_s) + 15)), dur_ms = 15000))
    updateSliderInput(session, "t", value = 0)
    alertas_vistas(0)
    removeNotification("simulando")
  })

  estado_t <- reactive(estado_sitios(input$t, sim(), sitios))

  observe({
    s <- sim(); est <- estado_t()
    tsu_km <- if (s$tsunami) min(450, V_TSUNAMI_KMH * input$t) else 0
    session$sendCustomMessage("p1_update", list(
      id = "map_sim", est = est, tver = input$t, ops = as.list(input$ops %||% character(0)),
      vis = as.list(setNames(c("distritos", "sitios", "servicios", "costa", "edificios") %in% input$capas,
                             c("distritos", "sitios", "servicios", "costa", "edificios"))),
      base = input$base %||% "oscuro", modo = input$modo, tsunami = s$tsunami, tsuKm = tsu_km, terrain = isTRUE(input$terreno)))
  })

  # ---------------------------------------------------- KPIs con interpretación
  disp_t <- reactive(disponibilidad(estado_t(), sitios, distritos))
  output$kpi_pob <- renderText(fmt_m(sim()$pob_mmi7))
  output$kpi_pob_sub <- renderText(sprintf("%.0f %% de la población · MMI máx. %s",
                                           100 * sim()$pob_mmi7 / POB_TOTAL, romano(max(sim()$mmi_d))))
  output$kpi_caidos <- renderText(sprintf("%.1f %%", 100 * mean(estado_t() == 2L)))
  output$kpi_caidos_sub <- renderText({
    cv <- sim()$curvas; sprintf("pico %.0f %% a las %.0f h", 100 * max(cv$caidos), cv$t[which.max(cv$caidos)])
  })
  output$kpi_bloqueo <- renderText(sprintf("%.0f %%", 100 * bloqueo(input$t, disp_t()$total, sim()$pico_demanda)))
  output$kpi_bloqueo_sub <- renderText(sprintf("a las %.1f h del sismo", input$t))
  output$kpi_aviso <- renderText(sprintf("%.0f %%", 100 * sim()$pob_sin_aviso / POB_TOTAL))
  output$kpi_aviso_sub <- renderText(sprintf("alerta enviada a los %.0f s", sim()$t_alerta))

  eventos_t <- reactive({
    e <- sim()$eventos
    e$estado <- ifelse(e$t_min <= input$t * 60, "Ocurrido", "Pendiente")
    e
  })
  output$kpi_alertas <- renderText(sum(eventos_t()$estado == "Ocurrido"))
  output$kpi_alertas_sub <- renderText({
    e <- eventos_t(); sprintf("%d revelan vacíos legales", sum(e$estado == "Ocurrido" & e$vacio != "" & !grepl("resuelto", e$vacio)))
  })

  # ---------------------------------------------------- lectura del escenario
  output$lectura <- renderUI({
    items <- leer_escenario(sim(), estado_t(), input$t, distritos, sitios, sim()$curvas)
    tagList(lapply(items, function(it)
      div(class = paste("lectura-item", paste0("li-", it$color)),
          div(class = "li-icono", icon(it$icono)),
          div(tags$b(it$titulo), tags$p(it$texto)))))
  })

  # ---------------------------------------------------- centro de alertas + avisos emergentes
  alertas_vistas <- reactiveVal(0)
  output$n_alertas <- renderText({
    e <- eventos_t(); n <- sum(e$estado == "Ocurrido" & e$nivel == "CRÍTICA"); if (n) n else ""
  })
  output$lista_alertas <- renderUI({
    e <- eventos_t(); e <- e[e$estado == "Ocurrido", ]; e <- e[nrow(e):1, , drop = FALSE]
    if (!nrow(e)) return(div(class = "text-muted", "Sin alertas todavía. Simula un sismo o avanza la línea de tiempo."))
    tagList(lapply(seq_len(nrow(e)), function(i) {
      r <- e[i, ]; g <- regmatches(r$vacio, regexpr("G[0-9]+", r$vacio))
      tags$details(class = "alerta", style = sprintf("--c:%s", pal_nivel[[r$nivel]] %||% "#38bdf8"),
        tags$summary(tags$span(class = "al-nivel", r$nivel), tags$span(class = "al-t", r$tiempo), tags$br(),
                     tags$b(r$actor), ": ", r$evento),
        div(class = "al-det",
            div(icon("gavel"), tags$b(" Norma: "), r$norma),
            if (length(g)) div(icon("triangle-exclamation"), tags$b(paste0(" Vacío ", g, ": ")),
                               VACIOS$Vacio[VACIOS$ID == g]),
            if (length(g) && grepl("resuelto", r$vacio)) div(class = "al-ok", icon("circle-check"), " Resuelto en esta simulación por la política activa."),
            if (length(g)) div(icon("lightbulb"), tags$b(" Propuesta: "), RECOMENDACION[[g]]),
            if (length(g)) div(icon("earth-americas"), tags$b(" Ejemplo internacional: "), VACIOS$Referente[VACIOS$ID == g]))
      )
    }))
  })
  # La insignia y el panel lateral están ocultos al inicio: se calculan igual
  outputOptions(output, "n_alertas", suspendWhenHidden = FALSE)
  outputOptions(output, "lista_alertas", suspendWhenHidden = FALSE)
  observe({
    e <- eventos_t(); oc <- e[e$estado == "Ocurrido" & e$nivel %in% c("CRÍTICA", "ALTA"), ]
    n0 <- isolate(alertas_vistas())
    if (nrow(oc) > n0 && input$t > 0) {
      nuevos <- oc[seq(n0 + 1, nrow(oc)), ]
      # Alarma sonora: suena la del nivel más grave entre las alertas nuevas
      session$sendCustomMessage("p1_sonido", list(nivel = if (any(nuevos$nivel == "CRÍTICA")) "CRÍTICA" else "ALTA"))
      for (i in seq_len(min(2, nrow(nuevos)))) {
        r <- nuevos[i, ]
        showNotification(tagList(tags$b(paste0(r$nivel, " · ", r$tiempo)), tags$br(), r$evento,
                                 if (nzchar(r$vacio)) tags$div(class = "small", paste("Vacío:", r$vacio))),
                         type = if (r$nivel == "CRÍTICA") "error" else "warning", duration = 7)
      }
    }
    alertas_vistas(nrow(oc))
  })

  # ---------------------------------------------------- tablas y gráficos
  output$tabla_eventos <- renderDT({
    e <- eventos_t()[, c("tiempo", "estado", "nivel", "actor", "evento", "norma", "vacio")]
    datatable(e, rownames = FALSE, escape = TRUE, class = "compact hover",
              colnames = c("Tiempo", "Estado", "Nivel", "Actor", "Evento / alerta", "Norma que aplica", "Vacío"),
              options = list(pageLength = 20, dom = "t", ordering = FALSE, scrollX = TRUE)) |>
      formatStyle("nivel", color = "white", fontWeight = "bold",
                  backgroundColor = styleEqual(names(pal_nivel), unname(pal_nivel))) |>
      formatStyle("estado", target = "row", opacity = styleEqual(c("Pendiente", "Ocurrido"), c(0.45, 1))) |>
      formatStyle("vacio", color = styleEqual(unique(e$vacio), ifelse(grepl("resuelto", unique(e$vacio)), "#22c55e", "#fb7185")),
                  fontWeight = "bold")
  })

  tema_plot <- function(p, titulo) {
    p |> layout(title = list(text = titulo, font = list(size = 13, color = "#e6edf6")),
                paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                font = list(color = "#cbd5e1", family = "Inter"),
                xaxis = list(title = "horas desde el sismo", gridcolor = "#1f2a44", zeroline = FALSE),
                yaxis = list(title = "%", gridcolor = "#1f2a44", zeroline = FALSE),
                shapes = list(list(type = "line", x0 = input$t, x1 = input$t, y0 = 0, y1 = 100,
                                   line = list(color = "#e6edf6", dash = "dash", width = 1))),
                legend = list(orientation = "h", font = list(size = 10)), hovermode = "x unified") |>
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  output$plot_disp <- renderPlotly({
    cv <- sim()$curvas
    cols <- c(A = "#f97316", B = "#38bdf8", C = "#22c55e", D = "#eab308")
    pl <- plot_ly()
    for (o in names(cols)) for (r in unique(cv$roaming)) {
      d <- cv[cv$operador == o & cv$roaming == r, ]
      pl <- add_lines(pl, x = d$t, y = 100 * d$disponibilidad, name = paste("Op.", o, if (grepl("Con", r)) "+ roaming" else ""),
                      line = list(color = cols[[o]], dash = if (grepl("Con", r)) "dot" else "solid", width = 2.5), legendgroup = o)
    }
    tema_plot(pl, "Población con servicio por operador · sólida: norma actual · punteada: con roaming (G1)")
  })
  output$plot_red <- renderPlotly({
    d <- sim()$curvas; d <- d[!duplicated(d$t), ]
    plot_ly(d, x = ~t) |>
      add_trace(y = ~100 * caidos, type = "scatter", mode = "lines", fill = "tozeroy", name = "Antenas caídas",
                line = list(color = "#ef4444", width = 3), fillcolor = "rgba(239,68,68,0.15)") |>
      add_lines(y = ~100 * bloqueo, name = "Llamadas bloqueadas", line = list(color = "#f59e0b", width = 3)) |>
      tema_plot("Monitoreo de la red: caídas y congestión")
  })

  output$tabla_distritos <- renderDT({
    s <- sim(); est <- estado_t()
    caido <- tapply(est == 2L, factor(sitios$IDDIST, levels = distritos$IDDIST), mean)
    df <- data.frame(Distrito = tools::toTitleCase(tolower(distritos$distrito)), Provincia = distritos$provincia,
                     Poblacion = distritos$poblacion, Suelo = distritos$suelo, MMI = romano(s$mmi_d),
                     PGA_g = round(s$pga_d, 2), Aviso_s = round(s$aviso_s),
                     Tsunami_min = round(s$t_tsu_min), Sitios_caidos = round(100 * caido, 1))
    datatable(df[order(-s$mmi_d), ], rownames = FALSE, class = "compact hover",
              colnames = c("Distrito", "Provincia", "Población (Censo 2025)", "Suelo", "MMI", "PGA (g)",
                           "Aviso antes del sacudimiento (s)", "Tsunami (min)", "Antenas caídas (%)"),
              options = list(pageLength = 10, scrollX = TRUE)) |>
      formatRound("Poblacion", 0, mark = " ") |>
      formatStyle("Aviso_s", color = styleInterval(c(0, 10), c("#fb7185", "#eab308", "#22c55e")), fontWeight = "bold") |>
      formatStyle("Sitios_caidos", background = styleColorBar(c(0, 100), "rgba(239,68,68,0.35)"),
                  backgroundSize = "95% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # ---------------------------------------------------- monitoreo en vivo
  vivo <- reactive({
    input$refrescar
    invalidateLater(5 * 60 * 1000)
    dias <- as.numeric(input$periodo)
    inicio <- format(Sys.time() - dias * 86400, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    url <- paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson",
                  "&starttime=", inicio, "&minlatitude=-20&maxlatitude=-2",
                  "&minlongitude=-83&maxlongitude=-68&minmagnitude=", input$min_mag_vivo)
    r <- tryCatch({
      j <- fromJSON(url); f <- j$features
      if (length(f) == 0 || NROW(f) == 0) data.frame() else {
        co <- do.call(rbind, f$geometry$coordinates)
        data.frame(fecha = format(as.POSIXct(f$properties$time / 1000, origin = "1970-01-01", tz = "America/Lima"), "%Y-%m-%d %H:%M"),
                   t = as.POSIXct(f$properties$time / 1000, origin = "1970-01-01"),
                   mag = round(f$properties$mag, 1), lugar = f$properties$place,
                   lon = co[, 1], lat = co[, 2], prof = round(co[, 3]), fuente = "USGS en vivo")
      }
    }, error = function(e) NULL)
    if (is.null(r)) {
      lim <- Sys.time() - dias * 86400
      r <- sismos |> mutate(t = as.POSIXct(fecha, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) |>
        filter(t >= lim, mag >= input$min_mag_vivo) |>
        transmute(fecha = format(t, "%Y-%m-%d %H:%M"), t, mag, lugar, lon, lat, prof = round(prof),
                  fuente = "Catálogo local (sin conexión)")
    }
    attr(r, "hora") <- format(Sys.time(), "%H:%M:%S")
    r
  })
  output$ultima_act <- renderText(paste("Última actualización:", attr(vivo(), "hora")))
  observe({
    v <- vivo()
    hist <- sismos |> filter(mag >= input$min_mag_hist) |>
      transmute(lon, lat, mag, prof = round(prof), fecha = substr(fecha, 1, 10), lugar)
    vv <- if (nrow(v)) transform(v[, c("lon", "lat", "mag", "prof", "fecha", "lugar")],
                                 recent = as.numeric(difftime(Sys.time(), v$t, units = "hours")) < 24) else v
    session$sendCustomMessage("p1_quakes", list(
      id = "map_vivo", historicos = as.character(toJSON(hist, dataframe = "rows")),
      vivo = as.character(toJSON(vv, dataframe = "rows")),
      qvis = isTRUE(input$ver_hist), terrain = isTRUE(input$terreno_vivo)))
  })
  nivel <- reactive({
    v <- vivo()
    if (!nrow(v)) return(list(n = "VERDE", txt = "Sin sismos relevantes en el periodo."))
    d <- dist_km(LIMA["lat"], LIMA["lon"], v$lat, v$lon)
    h <- as.numeric(difftime(Sys.time(), v$t, units = "hours"))
    if (any(v$mag >= 6 & d < 300 & h < 24))
      list(n = "ROJO", txt = "Sismo M ≥ 6 a menos de 300 km de Lima: activar el protocolo del DS 051-2010-MTC (comunicación del INDECI, RECSE y 119). Umbral propuesto.")
    else if (any(v$mag >= 4.5 & d < 200 & h < 24))
      list(n = "AMARILLO", txt = "Sismo moderado cerca de Lima: monitorear el estado de la red (OSIPTEL / COE-MTC).")
    else list(n = "VERDE", txt = "Sin eventos que activen el protocolo. La red opera con normalidad.")
  })
  # Alarma sonora cuando el nivel del monitoreo en vivo sube a AMARILLO o ROJO
  nivel_previo <- reactiveVal("VERDE")
  observeEvent(nivel()$n, {
    n <- nivel()$n
    if (n != nivel_previo() && n %in% c("ROJO", "AMARILLO"))
      session$sendCustomMessage("p1_sonido", list(nivel = if (n == "ROJO") "CRÍTICA" else "ALTA"))
    nivel_previo(n)
  })
  output$banner_alerta <- renderUI({
    n <- nivel()
    cls <- c(ROJO = "danger", AMARILLO = "warning", VERDE = "success")[[n$n]]
    ic <- c(ROJO = "circle-exclamation", AMARILLO = "triangle-exclamation", VERDE = "circle-check")[[n$n]]
    div(class = paste0("banner banner-", cls), icon(ic), tags$b(paste(" Nivel", n$n, "· ")), n$txt)
  })
  output$v_ultimo <- renderText({ v <- vivo(); if (!nrow(v)) "—" else sprintf("M%.1f", v$mag[1]) })
  output$v_ultimo_sub <- renderText({ v <- vivo(); if (!nrow(v)) "" else paste(v$fecha[1], "·", substr(v$lugar[1], 1, 32)) })
  output$v_n <- renderText(nrow(vivo()))
  output$v_n_sub <- renderText(paste("con M ≥", input$min_mag_vivo))
  output$v_max <- renderText({ v <- vivo(); if (!nrow(v)) "—" else sprintf("M%.1f", max(v$mag, na.rm = TRUE)) })
  output$v_max_sub <- renderText({ v <- vivo(); if (!nrow(v)) "" else substr(v$lugar[which.max(v$mag)], 1, 36) })
  output$v_nivel <- renderText(nivel()$n)
  output$v_nivel_sub <- renderText("umbral propuesto")
  output$tabla_vivo <- renderDT({
    v <- vivo()
    if (!nrow(v)) return(datatable(data.frame(Mensaje = "Sin eventos en el periodo")))
    datatable(v[, c("fecha", "mag", "prof", "lugar", "fuente")], rownames = FALSE, class = "compact hover",
              colnames = c("Fecha (hora de Lima)", "Magnitud", "Profundidad (km)", "Lugar", "Fuente"),
              options = list(pageLength = 8, order = list(list(0, "desc")))) |>
      formatStyle("mag", color = styleInterval(c(4.5, 6), c("#cbd5e1", "#eab308", "#ef4444")), fontWeight = "bold")
  })

  # ---------------------------------------------------- agentes IA
  corridas <- reactivePoll(3000, session,
    checkFunc = function() paste(list.files(file.path(DIR_MOTOR, "salidas"), "^bitacora_"), collapse = ""),
    valueFunc = function() sort(list.files(file.path(DIR_MOTOR, "salidas"), "^bitacora_.*\\.jsonl$"), decreasing = TRUE))
  observe({
    cs <- corridas()
    etiquetas <- setNames(cs, sub("^bitacora_(\\d{8})-(\\d{4})\\d{2}-(.*)\\.jsonl$", "\\1 \\2 · \\3", cs))
    pref <- cs[grepl("live", cs)][1] %||% cs[1]
    updateSelectInput(session, "corrida", choices = etiquetas, selected = if (length(cs)) pref else NULL)
  })
  correr_motor <- function(modo) {
    if (!file.exists(PY)) { showNotification("No se encontró el entorno de Python (.venv).", type = "error"); return() }
    if (modo == "live" && !nzchar(Sys.getenv("ANTHROPIC_API_KEY")) && !file.exists("../.env")) {
      showNotification("Falta una clave de IA en P1_PROYECT/.env. Se mantiene la última ejecución.", type = "warning", duration = 8)
      return()
    }
    showNotification(tagList(icon("robot"), paste(" Ejecutando los agentes en modo", modo, "… puede tardar varios minutos.")), id = "motor", duration = NULL)
    Sys.setenv(PYTHONIOENCODING = "utf-8")
    out <- tryCatch(system2(normalizePath(PY), c(shQuote(file.path(DIR_MOTOR, "motor.py")), "--modo", modo),
                            stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
    removeNotification("motor")
    showNotification(paste(tail(out, 1)), duration = 8)
  }
  observeEvent(input$correr_replay, correr_motor("replay"))
  observeEvent(input$correr_live, correr_motor("live"))

  bitacora <- reactive({ req(input$corrida); leer_bitacora(file.path(DIR_MOTOR, "salidas", input$corrida)) })
  resumen_bit <- reactive({
    req(input$corrida)
    f <- file.path(DIR_MOTOR, "salidas", sub("^bitacora_(.*)\\.jsonl$", "resumen_\\1.json", input$corrida))
    if (file.exists(f)) jsonlite::fromJSON(f) else NULL
  })
  output$resumen_corrida <- renderUI({
    r <- resumen_bit(); req(r)
    vac <- names(r$vacios_detectados)
    div(class = "fila-kpi",
      div(class = "kpi k-teal", div(class = "kpi-icono", icon("robot")), div(class = "kpi-cuerpo",
          div(class = "kpi-titulo", "Modelo"), div(class = "kpi-valor kv-chico", r$modelo), div(class = "kpi-sub", paste("modo", r$modo)))),
      div(class = "kpi k-verde", div(class = "kpi-icono", icon("check-double")), div(class = "kpi-cuerpo",
          div(class = "kpi-titulo", "Citas verificadas"), div(class = "kpi-valor", sprintf("%d/%d", r$citas_validas, r$citas_totales)),
          div(class = "kpi-sub", "todas existen en la base normativa"))),
      div(class = "kpi k-ambar", div(class = "kpi-icono", icon("magnifying-glass")), div(class = "kpi-cuerpo",
          div(class = "kpi-titulo", "Vacíos detectados"), div(class = "kpi-valor", sprintf("%d de %d", length(intersect(vac, r$vacios_esperados)), length(r$vacios_esperados))),
          div(class = "kpi-sub", paste(sort(vac), collapse = " · ")))))
  })
  output$stepper_etapas <- renderUI({
    regs <- bitacora(); req(length(regs))
    et <- unique(vapply(regs, function(r) paste(r$etapa, r$etapa_nombre, sep = "|"), ""))
    div(class = "stepper", lapply(et, function(x) {
      p <- strsplit(x, "|", fixed = TRUE)[[1]]
      tags$a(class = "step", href = paste0("#etapa", p[1]), div(class = "step-n", p[1]), tags$span(p[2]))
    }))
  })
  etiqueta_cita <- function(id) {
    a <- if (!is.null(KB)) KB[KB$id == id, ] else NULL
    tit <- if (!is.null(a) && nrow(a)) paste0(a$norma[1], ", art. ", a$art[1], ": ", a$texto[1]) else "No está en la base"
    tooltip(tags$span(class = "chip-cita", icon("gavel"), " ", id), tit)
  }
  output$tarjetas_agentes <- renderUI({
    regs <- bitacora(); req(length(regs))
    etapas <- unique(vapply(regs, function(r) r$etapa, 1))
    tagList(lapply(etapas, function(e) {
      rs <- Filter(function(r) r$etapa == e, regs)
      tarjetas <- lapply(rs, function(r) {
        a <- AGENTES[[r$agente]] %||% list(nombre = r$agente, sub = "", icono = "user", color = "#94a3b8")
        v <- unlist(r$vacios_confirmados)
        div(class = "agente", style = sprintf("--c:%s", a$color),
          div(class = "ag-head", div(class = "ag-avatar", icon(a$icono)),
              div(tags$b(a$nombre), tags$small(paste0(a$sub, " · ronda ", r$ronda)))),
          div(class = "ag-accion", r$accion),
          tags$p(class = "ag-decision", r$decision),
          div(class = "ag-chips", if (length(r$fundamento)) lapply(r$fundamento, function(f) etiqueta_cita(f$id))
              else tags$span(class = "chip-vacio", "sin norma habilitante")),
          if (length(v)) div(class = "ag-chips", lapply(v, function(g) tooltip(tags$span(class = "chip-gap", g), VACIOS$Vacio[VACIOS$ID == g])),
                             if (isTRUE(r$vacio_declarado$hay)) tags$small(class = "ag-tipo", sub("_.*", "", r$vacio_declarado$tipo))),
          if (isTRUE(r$vacio_declarado$hay)) tags$p(class = "ag-vacio", icon("triangle-exclamation"), " ", r$vacio_declarado$descripcion),
          if (length(r$mensajes_enviados)) tags$details(class = "ag-det", tags$summary(icon("envelope"), sprintf(" Mensajes enviados (%d)", length(r$mensajes_enviados))),
            lapply(r$mensajes_enviados, function(m) tags$p(class = "small", tags$b(paste0("→ ", m$para, ": ")), m$contenido))),
          tags$details(class = "ag-det", tags$summary(icon("brain"), " Traza de razonamiento"),
                       tags$pre(r$traza_razonamiento %||% "")),
          tags$small(class = "ag-pie", sprintf("Conocía: %s", paste(unlist(r$hechos_conocidos), collapse = ", "))))
      })
      div(id = paste0("etapa", e), class = "bloque-etapa",
          tags$h5(class = "etapa-titulo", tags$span(class = "etapa-n", e), rs[[1]]$etapa_nombre),
          do.call(layout_column_wrap, c(list(width = "330px", fill = FALSE), tarjetas)))
    }))
  })
  output$tabla_bitacora <- renderDT({
    regs <- bitacora(); req(length(regs))
    df <- do.call(rbind, lapply(regs, function(r) data.frame(
      id = r$id, etapa = r$etapa, agente = r$agente, accion = r$accion %||% "",
      fundamento = paste(vapply(r$fundamento %||% list(), function(f) f$id, ""), collapse = ", "),
      competencia = r$competencia_invocada %||% "",
      vacios = paste(unlist(r$vacios_confirmados), collapse = ", "),
      citas_rechazadas = paste(unlist(r$verificacion_citas$rechazadas_1er_intento), collapse = ", "),
      modelo = r$modelo %||% "", hash = r$hash_contexto %||% "", hora = r$timestamp %||% "")))
    datatable(df, rownames = FALSE, class = "compact hover", options = list(pageLength = 15, scrollX = TRUE))
  })
  output$tabla_kb <- renderDT({
    req(KB)
    datatable(KB[, c("id", "pais", "norma", "art", "titulo", "efecto", "confiabilidad")], rownames = FALSE,
              class = "compact hover", options = list(pageLength = 12, scrollX = TRUE))
  })

  # ---------------------------------------------------- vacíos
  output$tarjetas_vacios <- renderUI({
    v <- VACIOS[order(-VACIOS$Prioridad), ]
    do.call(layout_column_wrap, c(list(width = "340px", fill = FALSE, heights_equal = "row", class = "grid-arriba"), lapply(seq_len(nrow(v)), function(i) {
      r <- v[i, ]; col <- if (r$Prioridad >= 13) "#ef4444" else if (r$Prioridad >= 11) "#f59e0b" else "#38bdf8"
      tags$details(class = "vacio", style = sprintf("--c:%s", col),
        tags$summary(div(class = "vac-top", tags$span(class = "vac-id", r$ID), tags$span(class = "vac-tipo", r$Tipo),
                         tags$span(class = "vac-prio", sprintf("%d/15", r$Prioridad))),
                     tags$h6(r$Vacio)),
        div(class = "vac-det",
            div(icon("gavel"), tags$b(" Evidencia en el Perú: "), r$Norma_peruana),
            div(icon("earth-americas"), tags$b(" Ejemplo internacional: "), r$Referente),
            div(icon("lightbulb"), tags$b(" Propuesta: "), RECOMENDACION[[r$ID]])))
    })))
  })
  # ---------------------------------------------------- descargables de las 72 h
  etiqueta_sim <- reactive(etiqueta_escenario(sim()$p))
  alertas_dl <- reactive(alertas_72h(sim(), etiqueta_sim()))
  horario_dl <- reactive(horario_72h(sim(), sitios, distritos, etiqueta_sim()))
  nombre_dl <- function(base, ext) sprintf("SOSNet_%s_M%.1f_%s.%s", base, sim()$p$M, format(Sys.time(), "%Y%m%d_%H%M"), ext)
  output$dl_alertas <- downloadHandler(filename = function() nombre_dl("alertas_72h", "csv"),
    content = function(f) readr::write_excel_csv(alertas_dl(), f))
  output$dl_horario <- downloadHandler(filename = function() nombre_dl("hora_a_hora_72h", "csv"),
    content = function(f) readr::write_excel_csv(horario_dl(), f))
  output$dl_informe <- downloadHandler(filename = function() nombre_dl("informe_72h", "html"),
    content = function(f) informe_72h_html(f, sim(), alertas_dl(), horario_dl(), etiqueta_sim(),
                                           val = list(titulo = paste("Modelo", tolower(VEREDICTO$estado)), texto = VEREDICTO$texto)))

  # ---------------------------------------------------- validación
  output$banner_validacion <- renderUI({
    cl <- if (VEREDICTO$ok) "#22c55e" else if (VEREDICTO$estado == "Validado con reservas") "#f59e0b" else "#ef4444"
    div(class = "banner-valid", style = sprintf("--c:%s", cl),
        icon(if (VEREDICTO$ok) "circle-check" else "triangle-exclamation"),
        div(tags$b(VEREDICTO$estado), tags$p(VEREDICTO$texto),
            tags$p(class = "small mb-0", icon("arrow-trend-down"), sprintf(
              " Mejora frente a la calibración anterior (solo 9 datos), medida con los mismos 167 datos reales: error %.2f → %.2f grados; sesgo %+.2f → %+.2f (antes exageraba la intensidad de los grandes terremotos); predicciones a ±1 grado %.0f %% → %.0f %%.",
              VALID$m_antes[["rmse"]], VALID$m_calib[["rmse"]], VALID$m_antes[["sesgo"]], VALID$m_calib[["sesgo"]],
              100 * VALID$m_antes[["dentro1"]], 100 * VALID$m_calib[["dentro1"]])),
            tags$p(class = "small mb-0", icon("circle-info"),
              " Qué significa: el error típico al predecir un sismo nuevo es de menos de un grado de intensidad. Sirve para comparar políticas y estimar daños a escala de distrito; no reemplaza un estudio de microzonificación.")))
  })
  output$val_calib <- renderText(sprintf("%.2f MMI", VALID$m_calib[["rmse"]]))
  output$val_calib_sub <- renderText(sprintf("%.0f %% a ±1 grado · %d datos, %d sismos", 100 * VALID$m_calib[["dentro1"]], VALID$n_obs, VALID$n_ev))
  output$val_8020 <- renderText(sprintf("%.2f MMI", VALID$m_8020[["rmse_mediana"]]))
  output$val_8020_sub <- renderText(sprintf("mediana · 90 %% de las veces ≤ %.2f", VALID$m_8020[["rmse_p90"]]))
  output$val_loeo <- renderText(sprintf("%.2f MMI", VALID$m_loeo[["rmse"]]))
  output$val_loeo_sub <- renderText(sprintf("%.0f %% a ±1 grado · sesgo %+.2f", 100 * VALID$m_loeo[["dentro1"]], VALID$m_loeo[["sesgo"]]))
  output$val_pruebas <- renderText(sprintf("%d / %d", sum(PRUEBAS$resultado), nrow(PRUEBAS)))
  output$val_pruebas_sub <- renderText(if (all(PRUEBAS$resultado)) "todas superadas" else "revisar las fallidas")
  output$plot_val_8020 <- renderPlotly({
    r <- VALID$rep8020
    plot_ly(r, x = ~rmse_prueba, type = "histogram", nbinsx = 25, name = "prueba (20 %)",
            marker = list(color = "#38bdf8", line = list(color = "#0b1220", width = 1))) |>
      tema_plot("") |>
      layout(xaxis = list(title = "error de prueba (RMSE, grados MMI)", gridcolor = "#1f2a44"),
             yaxis = list(title = "particiones", gridcolor = "#1f2a44"), showlegend = FALSE, hovermode = "closest",
             shapes = list(
               list(type = "line", x0 = CRITERIOS$rmse_8020, x1 = CRITERIOS$rmse_8020, y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "#f59e0b", dash = "dash")),
               list(type = "line", x0 = VALID$m_8020[["rmse_mediana"]], x1 = VALID$m_8020[["rmse_mediana"]], y0 = 0, y1 = 1,
                    yref = "paper", line = list(color = "#22c55e"))),
             annotations = list(
               list(x = CRITERIOS$rmse_8020, y = 1, yref = "paper", text = "umbral", showarrow = FALSE, font = list(color = "#f59e0b"), xanchor = "left"),
               list(x = VALID$m_8020[["rmse_mediana"]], y = 0.92, yref = "paper", text = "mediana", showarrow = FALSE,
                    font = list(color = "#22c55e"), xanchor = "right")))
  })
  output$plot_val_obs <- renderPlotly({
    t <- VALID$tabla
    t$txt <- sprintf("Sismo %s · %s<br>%s<br>observado %.1f", t$evento, t$lugar, t$tipo, t$mmi_obs)
    set.seed(1); t$x <- t$mmi_obs + runif(nrow(t), -0.12, 0.12)
    plot_ly() |>
      add_ribbons(x = c(1, 9), ymin = c(0, 8), ymax = c(2, 10), name = "±1 grado", fillcolor = "rgba(20,184,166,.12)",
                  line = list(color = "transparent"), hoverinfo = "skip") |>
      add_lines(x = c(1, 9), y = c(1, 9), name = "predicción perfecta", line = list(color = "#94a3b8", dash = "dot"), hoverinfo = "skip") |>
      add_markers(data = t, x = ~x, y = ~pred_antes, name = "modelo anterior (9 datos)", text = ~txt, visible = "legendonly",
                  hovertemplate = "%{text}<br>predicho %{y:.2f}<extra>modelo anterior</extra>",
                  marker = list(color = "#fb7185", size = 7, opacity = 0.7)) |>
      add_markers(data = t, x = ~x, y = ~pred_loeo, name = "sismo no visto (LOEO)", text = ~txt,
                  hovertemplate = "%{text}<br>predicho %{y:.2f}<extra>sismo no visto</extra>",
                  marker = list(color = "#a78bfa", size = 8, symbol = "diamond", opacity = 0.85,
                                line = list(color = "#0b1220", width = 0.5))) |>
      tema_plot("") |>
      layout(xaxis = list(title = "intensidad observada (MMI)", range = c(1, 8.6), gridcolor = "#1f2a44"),
             yaxis = list(title = "intensidad predicha (MMI)", range = c(1, 8.6), gridcolor = "#1f2a44"),
             shapes = list(), hovermode = "closest")
  })
  output$tabla_criterios <- renderUI({
    c <- VEREDICTO$criterios
    tags$table(class = "tabla-criterios", lapply(seq_len(nrow(c)), function(i) tags$tr(
      tags$td(if (c$cumple[i]) tags$span(class = "ok", icon("circle-check")) else tags$span(class = "no", icon("circle-xmark"))),
      tags$td(c$criterio[i]), tags$td(tags$b(c$valor[i])))))
  })
  output$tabla_val_evento <- renderDT({
    e <- VALID$por_evento[, c("evento", "n", "obs_media", "rmse_calibracion", "rmse_sin_ver_evento", "sesgo_sin_ver_evento")]
    e[, 3:6] <- round(e[, 3:6], 2)
    datatable(e, rownames = FALSE, class = "compact",
              colnames = c("Sismo", "Datos", "MMI media obs.", "Error al calibrar", "Error sin verlo", "Sesgo sin verlo"),
              options = list(dom = "tp", pageLength = 8, ordering = FALSE))
  })
  output$tabla_val_tipo <- renderDT({
    e <- VALID$por_tipo; e$rmse_sin_ver_evento <- round(e$rmse_sin_ver_evento, 2); e$dentro1 <- sprintf("%.0f %%", 100 * e$dentro1)
    datatable(e, rownames = FALSE, class = "compact", colnames = c("Tipo de dato", "Datos", "Sismos", "Error sin verlo", "A ±1 grado"),
              options = list(dom = "t", ordering = FALSE))
  })
  output$tabla_pruebas <- renderDT({
    p <- PRUEBAS; p$resultado <- ifelse(p$resultado, "✔ Superada", "✘ Fallida")
    datatable(p, rownames = FALSE, class = "compact", colnames = c("Área", "Prueba", "Resultado", "Detalle medido"),
              options = list(dom = "t", pageLength = 50, ordering = FALSE)) |>
      formatStyle("resultado", fontWeight = "bold", color = styleEqual(c("✔ Superada", "✘ Fallida"), c("#22c55e", "#ef4444")))
  })
  output$tabla_val_obs <- renderDT({
    t <- VALID$tabla; t[, 6:9] <- round(t[, 6:9], 2)
    datatable(t, rownames = FALSE, class = "compact", filter = "top",
              colnames = c("Sismo", "Tipo", "Lugar", "Lat", "Lon", "MMI observada", "Predicha (calibración)",
                           "Predicha (sismo no visto)", "Modelo anterior"),
              options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
  })
  output$dl_validacion <- downloadHandler(filename = function() sprintf("SOSNet_validacion_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(f) {
      crit <- VEREDICTO$criterios
      out <- rbind(
        data.frame(seccion = "Veredicto", elemento = VEREDICTO$estado, valor = sprintf("%d/%d criterios", VEREDICTO$n_ok, VEREDICTO$n), detalle = VEREDICTO$texto),
        data.frame(seccion = "Criterio", elemento = crit$criterio, valor = crit$valor, detalle = ifelse(crit$cumple, "cumple", "no cumple")),
        data.frame(seccion = paste("Coherencia ·", PRUEBAS$area), elemento = PRUEBAS$prueba,
                   valor = ifelse(PRUEBAS$resultado, "superada", "fallida"), detalle = PRUEBAS$detalle),
        data.frame(seccion = paste("Observación ·", VALID$tabla$tipo),
                   elemento = sprintf("Sismo %s · %s (%.4f, %.4f)", VALID$tabla$evento, VALID$tabla$lugar, VALID$tabla$lat, VALID$tabla$lon),
                   valor = sprintf("obs %.1f", VALID$tabla$mmi_obs),
                   detalle = sprintf("calibración %.2f · sismo no visto %.2f · modelo anterior %.2f",
                                     VALID$tabla$pred_calibracion, VALID$tabla$pred_loeo, VALID$tabla$pred_antes)))
      readr::write_excel_csv(out, f)
    })

  output$tabla_vacios <- renderDT({
    datatable(VACIOS, rownames = FALSE, class = "compact",
              colnames = c("ID", "Vacío", "Tipo", "Norma peruana", "Referente internacional", "Prioridad (/15)"),
              options = list(pageLength = 10, dom = "t", order = list(list(5, "desc"))))
  })
}

shinyApp(ui, server)
