# =============================================================================
# Interpretaciones en lenguaje claro, casos de ejemplo y recomendaciones
# =============================================================================

fmt_n <- function(x) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
fmt_m <- function(x) if (x >= 1e6) sprintf("%.1f millones", x / 1e6) else fmt_n(x)

semaforo <- function(valor, umbrales, colores = c("success", "warning", "danger")) {
  colores[findInterval(valor, umbrales) + 1]
}

# Qué propone el sistema para cada vacío (resumen del informe)
RECOMENDACION <- c(
  G1 = "Habilitar una orden de roaming temporal de emergencia (por zona y plazo), con acuerdos y pruebas previas, como en EE. UU. (FCC §4.17) o Chile (RAN-E).",
  G2 = "Crear una base legal acotada (rescate, plazo, registro y supervisión de la autoridad de datos) para usar la ubicación individual; por defecto, entregar mapas agregados.",
  G3 = "Activar la red de emergencia por umbrales objetivos (magnitud del IGP o % de antenas caídas), sin depender solo de la comunicación de INDECI.",
  G4 = "Actualizar la prioridad a voz y datos (VoLTE/5G) y abrirla a funciones críticas: alcaldes, hospitales, SAMU y servicios básicos.",
  G5 = "Permitir mensajes preaprobados que el IGP o la DHN disparen automáticamente por umbral, e implementar el canal de alertas en quechua y aimara.",
  G6 = "Exigir supervisión humana y un 'modo emergencia' para la IA que gestiona redes (no despriorizar tráfico de emergencia, registros para el regulador).",
  G7 = "Firmar protocolos bilaterales o andinos de roaming y coordinación de emergencia en fronteras.",
  G8 = "Aprobar un protocolo MTC-OSIPTEL-INDECI con roles claros y un canal único de decisión en emergencias.",
  G9 = "Aprobar un Plan Nacional de Telecomunicaciones de Emergencia siguiendo las guías de la UIT.",
  G10 = "Garantizar mensajería por datos y acceso a sitios oficiales gratuitos en las zonas afectadas."
)

# ----------------------------------------------------------------- casos de ejemplo
CASOS <- list(
  p1 = list(titulo = "Terremoto frente a Lima", etiqueta = "Escenario P1", tipo = "Sintético",
            icono = "house-crack", color = "#ef4444",
            historia = "Un sismo M8.0 golpea Lima Sur. ¿Cuántas antenas caen, quién recibe la alerta a tiempo y qué normas se activan?",
            pregunta = "¿Está preparada la red para un gran sismo?",
            escenario = "P1: M8.0 frente a Lima Sur (sintético)", politicas = list()),
  peor = list(titulo = "El peor escenario oficial", etiqueta = "INDECI · M8.8", tipo = "Escenario oficial",
              icono = "triangle-exclamation", color = "#b91c1c",
              historia = "La magnitud que usa INDECI para sus simulacros nacionales. Muestra el límite de lo que la red y las normas deben soportar.",
              pregunta = "¿Qué pasa en el caso más extremo?",
              escenario = "INDECI: M8.8 costa central (escenario oficial, aprox.)", politicas = list()),
  pisco = list(titulo = "Revivir Pisco 2007", etiqueta = "Real · M8.0", tipo = "Sismo real",
               icono = "clock-rotate-left", color = "#f59e0b",
               historia = "El terremoto de Pisco colapsó las comunicaciones por congestión. Vuelve a simularlo con la red actual de Lima.",
               pregunta = "¿Cómo afectaría hoy un sismo como el de 2007?",
               escenario = "Real 15/08/2007 Pisco (USGS Mw 8.0)", politicas = list()),
  callao = list(titulo = "Callao 2025: el susto", etiqueta = "Real · M5.6", tipo = "Sismo real",
                icono = "circle-check", color = "#22c55e",
                historia = "Un sismo moderado sentido en toda Lima. OSIPTEL reportó 38 antenas afectadas y ninguna interrupción: el modelo debe coincidir.",
                pregunta = "¿El modelo reproduce lo que pasó?",
                escenario = "Real 15/06/2025 Callao (USGS Mww 5.6; IGP 6.1)", politicas = list()),
  vacios = list(titulo = "¿Y si cerramos los vacíos?", etiqueta = "P1 + 3 políticas", tipo = "Qué pasaría si",
                icono = "wand-magic-sparkles", color = "#14b8a6",
                historia = "El mismo sismo P1, pero con alerta automática, activación por umbral y roaming de emergencia. Compara el antes y el después.",
                pregunta = "¿Cuánto mejora la respuesta con normas nuevas?",
                escenario = "P1: M8.0 frente a Lima Sur (sintético)",
                politicas = list(alerta_auto = TRUE, activacion_auto = TRUE, roaming = TRUE, portatiles = TRUE)),
  lenta = list(titulo = "La alerta que llega tarde", etiqueta = "P1 · cadena de 15 min", tipo = "Qué pasaría si",
               icono = "bell-slash", color = "#8b5cf6",
               historia = "Si la aprobación manual del mensaje tarda 15 minutos y INDECI demora 2 horas en comunicar la emergencia.",
               pregunta = "¿Cuánto cuesta la burocracia en segundos?",
               escenario = "P1: M8.0 frente a Lima Sur (sintético)",
               politicas = list(min_manual = 15, min_indeci = 120))
)

# ----------------------------------------------------------------- lectura del escenario
# Devuelve una lista de items {icono, color, titulo, texto} en lenguaje claro
leer_escenario <- function(s, est, t, distritos, sitios, curvas) {
  pob_total <- sum(distritos$poblacion)
  caidos <- mean(est == 2L)
  t_cerca <- curvas$t[which.min(abs(curvas$t - t))]
  d_t <- curvas[curvas$t == t_cerca, ][1, ]
  pico <- max(curvas$caidos); t_pico <- curvas$t[which.max(curvas$caidos)]
  pct_tarde <- s$pob_sin_aviso / pob_total
  peor <- distritos$distrito[which.max(s$mmi_d)]
  mmi_max <- max(s$mmi_d)
  out <- list()
  add <- function(icono, color, titulo, texto) out[[length(out) + 1]] <<- list(icono = icono, color = color, titulo = titulo, texto = texto)

  # Intensidad
  if (round(mmi_max) >= 8) {
    add("house-crack", "danger", "Sacudimiento severo",
        sprintf("%s personas sentirían intensidad VII o más. El distrito más golpeado sería %s (MMI %s): daños importantes en construcciones vulnerables.",
                fmt_m(s$pob_mmi7), tools::toTitleCase(tolower(peor)), romano(mmi_max)))
  } else if (round(mmi_max) >= 6) {
    add("house-crack", "warning", "Sacudimiento moderado a fuerte",
        sprintf("El sismo se sentiría fuerte en %s (MMI %s). %s personas con intensidad VII o más.",
                tools::toTitleCase(tolower(peor)), romano(mmi_max), fmt_m(s$pob_mmi7)))
  } else {
    add("circle-check", "success", "Sismo moderado",
        sprintf("Intensidad máxima %s: se siente, pero no se esperan daños significativos ni caídas de la red.", romano(mmi_max)))
  }
  # Alerta temprana
  if (pct_tarde > 0.5) {
    add("bell-slash", "danger", "La alerta llega tarde",
        sprintf("El %d %% de la población recibiría la alerta DESPUÉS del sacudimiento: la detección toma %.0f s, pero la cadena de aprobación suma %.0f s más. %s",
                round(100 * pct_tarde), s$t_det, s$t_alerta - s$t_det,
                if (isTRUE(s$p$alerta_auto)) "" else "Prueba la política «Alerta automática» para ver la diferencia (vacío G5)."))
  } else {
    add("bell", "success", "Alerta a tiempo",
        sprintf("El %d %% de la población recibiría la alerta antes de la onda fuerte. Ganar segundos permite agacharse, cubrirse y sujetarse.", round(100 * (1 - pct_tarde))))
  }
  # Tsunami
  if (isTRUE(s$tsunami)) {
    tmin <- suppressWarnings(min(s$t_tsu_min, na.rm = TRUE))
    add("water", "danger", "Riesgo de tsunami",
        sprintf("Epicentro en el mar y superficial: el primer arribo a la costa se estima en %.0f minutos. Los distritos costeros deben evacuar a zonas altas.", tmin))
  }
  # Red
  if (pico > 0.2) {
    add("tower-cell", "danger", "La red móvil colapsa en parte",
        sprintf("Ahora (T+%.1f h) hay %.0f %% de antenas caídas. El peor momento sería a las %.0f h (%.0f %%), cuando se agotan las baterías.",
                t, 100 * caidos, t_pico, 100 * pico))
  } else if (pico > 0.02) {
    add("tower-cell", "warning", "Daños localizados en la red",
        sprintf("El pico de antenas caídas sería %.0f %% a las %.0f h.", 100 * pico, t_pico))
  } else {
    add("tower-cell", "success", "La red resiste",
        "Prácticamente no hay antenas caídas: coincide con lo observado en sismos moderados recientes.")
  }
  if (!is.na(d_t$bloqueo) && d_t$bloqueo > 0.4)
    add("phone-slash", "warning", "Congestión de llamadas",
        sprintf("%.0f %% de las llamadas fallan en este momento porque todos llaman a la vez. Recomendación a la población: usar mensajes de texto o el 119.", 100 * d_t$bloqueo))
  # Roaming
  ca <- curvas[curvas$t == 24 & curvas$operador == "A", ]
  if (nrow(ca) == 2 && pico > 0.05) {
    sin <- ca$disponibilidad[grepl("Sin", ca$roaming)]; con <- ca$disponibilidad[grepl("Con", ca$roaming)]
    add("right-left", if (con - sin > 0.03) "info" else "secondary", "Valor del roaming de emergencia",
        sprintf("A las 24 h, el roaming de emergencia llevaría la cobertura del Operador A de %.0f %% a %.0f %% (+%.0f puntos). Hoy la norma no permite ordenarlo en zonas urbanas (vacío G1).",
                100 * sin, 100 * con, 100 * (con - sin)))
  }
  out
}
