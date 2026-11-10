# =============================================================================
# Descargables de las 72 horas: alertas, evolución hora a hora ("qué está
# pasando") e informe imprimible (HTML → Imprimir → PDF desde el navegador)
# =============================================================================

fase_72h <- function(h) {
  ifelse(h < 1, "Impacto (0-1 h)",
  ifelse(h < 6, "Respuesta inmediata (1-6 h)",
  ifelse(h < 24, "Primer día (6-24 h)",
  ifelse(h < 48, "Segundo día (24-48 h)", "Recuperación (48-72 h)"))))
}

vacio_id <- function(v) regmatches(v, regexpr("G[0-9]+", v))

# Bitácora de alertas lista para descargar (todas las de las 72 h)
alertas_72h <- function(s, etiqueta) {
  e <- s$eventos
  rec <- vapply(e$vacio, function(v) {
    id <- vacio_id(v); if (!length(id) || grepl("resuelto", v)) "" else RECOMENDACION[[id]]
  }, character(1), USE.NAMES = FALSE)
  data.frame(escenario = etiqueta, tiempo = e$tiempo, minuto = round(e$t_min, 1),
             hora = round(e$t_min / 60, 2), fase = fase_72h(e$t_min / 60), nivel = e$nivel,
             actor = e$actor, evento = e$evento, norma = e$norma, vacio = e$vacio,
             propuesta = rec, stringsAsFactors = FALSE)
}

# Evolución hora a hora con una lectura en lenguaje claro
horario_72h <- function(s, sitios, distritos, etiqueta) {
  e <- s$eventos
  filas <- lapply(0:72, function(h) {
    est <- estado_sitios(h, s, sitios); d <- disponibilidad(est, sitios, distritos)
    op <- mean(est == 0L); ba <- mean(est == 1L); ca <- mean(est == 2L); re <- mean(est == 3L)
    bl <- bloqueo(h, d$total, s$pico_demanda)
    nuevas <- e[e$t_min > (h - 1) * 60 & e$t_min <= h * 60, ]
    if (h == 0) nuevas <- e[e$t_min <= 0, ]
    acum <- e[e$t_min <= h * 60, ]
    txt <- c(
      sprintf("%.0f %% de antenas caídas", 100 * ca),
      if (bl >= 0.05) sprintf("%.0f %% de las llamadas fallan por congestión", 100 * bl) else "las llamadas fluyen con normalidad",
      if (ba >= 0.02) sprintf("%.0f %% de los sitios funcionan solo con batería y se apagarán si la luz no vuelve", 100 * ba),
      if (re >= 0.02) sprintf("%.0f %% ya fue restablecido", 100 * re))
    lectura <- paste0(toupper(substring(paste(txt, collapse = "; "), 1, 1)), substring(paste(txt, collapse = "; "), 2), ".")
    if (nrow(nuevas)) lectura <- paste0(lectura, " Nuevas alertas: ", paste(nuevas$evento, collapse = " | "), ".")
    data.frame(escenario = etiqueta, hora = h, fase = fase_72h(h),
               antenas_operativas_pct = round(100 * op, 1), con_bateria_pct = round(100 * ba, 1),
               caidas_pct = round(100 * ca, 1), restablecidas_pct = round(100 * re, 1),
               llamadas_fallan_pct = round(100 * bl, 1), cobertura_poblacion_pct = round(100 * d$total, 1),
               opA_sin_roaming_pct = round(100 * d$sin[["A"]], 1), opA_con_roaming_pct = round(100 * d$con[["A"]], 1),
               alertas_nuevas = nrow(nuevas), alertas_acumuladas = nrow(acum),
               criticas_acumuladas = sum(acum$nivel == "CRÍTICA"),
               vacios_revelados = paste(unique(unlist(lapply(acum$vacio[acum$vacio != "" & !grepl("resuelto", acum$vacio)], vacio_id))), collapse = " "),
               que_esta_pasando = lectura, stringsAsFactors = FALSE)
  })
  do.call(rbind, filas)
}

etiqueta_escenario <- function(p) {
  pol <- c(if (isTRUE(p$alerta_auto)) "alerta automática (G5)", if (isTRUE(p$activacion_auto)) "activación por umbral (G3)",
           if (isTRUE(p$roaming)) "roaming de emergencia (G1)", if (isTRUE(p$portatiles)) "estaciones portátiles")
  sprintf("M%.1f · %.2f, %.2f · %d km de profundidad · políticas: %s", p$M, p$lat, p$lon, as.integer(p$prof),
          if (length(pol)) paste(pol, collapse = ", ") else "norma actual")
}

# Informe HTML autocontenido e imprimible
informe_72h_html <- function(archivo, s, alertas, horario, etiqueta, val = NULL) {
  col <- c("CRÍTICA" = "#dc2626", "ALTA" = "#d97706", "MEDIA" = "#ca8a04", "INFO" = "#0284c7")
  pico <- horario[which.max(horario$caidas_pct), ]
  hitos <- horario[horario$hora %in% c(0, 1, 3, 6, 12, 24, 36, 48, 60, 72), ]
  tabla <- function(df, cab) tags$table(tags$thead(tags$tr(lapply(cab, tags$th))),
                                        tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, ], function(x) tags$td(x))))))
  vac <- sort(unique(unlist(lapply(alertas$vacio[alertas$vacio != "" & !grepl("resuelto", alertas$vacio)], vacio_id))))
  css <- "
        body{font-family:Segoe UI,Arial,sans-serif;color:#0f172a;max-width:1100px;margin:24px auto;padding:0 16px;font-size:13px}
        h1{color:#0f766e;margin:0}h2{color:#0f766e;border-bottom:2px solid #14b8a6;padding-bottom:4px;margin-top:28px}
        .sub{color:#475569}.kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:16px 0}
        .k{border:1px solid #cbd5e1;border-radius:10px;padding:10px}.k b{display:block;font-size:22px;color:#0f766e}
        table{border-collapse:collapse;width:100%;margin:8px 0}th{background:#0f766e;color:#fff;text-align:left}
        th,td{border:1px solid #cbd5e1;padding:5px 7px;vertical-align:top}tr:nth-child(even) td{background:#f8fafc}
        .niv{color:#fff;font-weight:700;border-radius:6px;padding:1px 6px;font-size:11px}
        .nota{background:#f1f5f9;border-left:4px solid #14b8a6;padding:8px 12px;margin:10px 0}
        @media print{body{margin:0}h2{break-after:avoid}tr{break-inside:avoid}}"
  cuerpo <- tags$body(
      tags$h1("SOS Net Perú · Informe de las primeras 72 horas"),
      tags$div(class = "sub", etiqueta, tags$br(), "Generado: ", format(Sys.time(), "%d/%m/%Y %H:%M"),
               " · Modelo ilustrativo (no es un pronóstico oficial)"),
      tags$div(class = "kpis",
        tags$div(class = "k", tags$b(sprintf("%.1f M", s$pob_mmi7 / 1e6)), "personas con intensidad ≥ VII"),
        tags$div(class = "k", tags$b(sprintf("%.0f %%", pico$caidas_pct)), sprintf("pico de antenas caídas (hora %d)", pico$hora)),
        tags$div(class = "k", tags$b(sprintf("%.0f s", s$t_alerta)), "envío de la alerta sísmica"),
        tags$div(class = "k", tags$b(nrow(alertas)), sprintf("alertas en 72 h (%d críticas)", sum(alertas$nivel == "CRÍTICA")))),
      tags$h2("1. Qué está pasando, por momentos clave"),
      tabla(hitos[, c("hora", "fase", "caidas_pct", "con_bateria_pct", "llamadas_fallan_pct", "que_esta_pasando")],
            c("Hora", "Fase", "% caídas", "% con batería", "% llamadas fallan", "Lectura")),
      tags$h2("2. Bitácora de alertas (72 h)"),
      tags$table(tags$thead(tags$tr(lapply(c("Tiempo", "Nivel", "Actor", "Evento / alerta", "Norma", "Vacío"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(alertas)), function(i) { a <- alertas[i, ]
          tags$tr(tags$td(a$tiempo), tags$td(tags$span(class = "niv", style = sprintf("background:%s", col[[a$nivel]] %||% "#64748b"), a$nivel)),
                  tags$td(a$actor), tags$td(a$evento), tags$td(a$norma), tags$td(a$vacio)) }))),
      tags$h2("3. Vacíos legales que revela el escenario"),
      if (length(vac)) tags$ul(lapply(vac, function(g) tags$li(tags$b(g, ": "), VACIOS$Vacio[VACIOS$ID == g], tags$br(),
                                                               tags$i("Propuesta: ", RECOMENDACION[[g]]))))
      else tags$p("Con las políticas activadas, el escenario no revela vacíos abiertos."),
      tags$h2("4. Evolución hora a hora"),
      tabla(horario[, c("hora", "antenas_operativas_pct", "con_bateria_pct", "caidas_pct", "restablecidas_pct",
                        "llamadas_fallan_pct", "opA_sin_roaming_pct", "opA_con_roaming_pct", "alertas_acumuladas")],
            c("Hora", "% operativas", "% batería", "% caídas", "% restablecidas", "% llamadas fallan",
              "Op. A sin roaming", "Op. A con roaming", "Alertas acumuladas")),
      if (!is.null(val)) tagList(tags$h2("5. Confiabilidad del modelo"),
        tags$div(class = "nota", tags$b(val$titulo), tags$br(), val$texto)),
      tags$p(class = "sub", "Fuentes: INEI Censo 2025, OpenCelliD (CC BY-SA 4.0), USGS, OpenStreetMap, normativa peruana verificada. ",
             "Autonomía de baterías y redundancia de fibra son supuestos sintéticos declarados."))
  # htmltools descarta <head> al convertir: la cabecera se arma como texto
  html <- paste0("<!DOCTYPE html>
<html lang=\"es\">
<head>
<meta charset=\"utf-8\">
",
                 "<title>Informe 72 h · SOS Net Perú</title>
<style>", css, "</style>
</head>
",
                 as.character(cuerpo), "
</html>")
  con <- file(archivo, open = "w", encoding = "UTF-8"); on.exit(close(con))
  writeLines(html, con)
}
