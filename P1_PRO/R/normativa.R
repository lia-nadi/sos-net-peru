# =============================================================================
# Alertas integradas con la normativa peruana (fuente: investigacion_P1/)
# Cada evento indica el actor, la norma que lo respalda y el vacío que revela.
# =============================================================================

VACIOS <- data.frame(
  ID = c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10"),
  Vacio = c(
    "No existe roaming de emergencia activable por la autoridad",
    "No hay base legal para usar la ubicación de los teléfonos en búsqueda y rescate",
    "Toda la activación depende de la comunicación del INDECI",
    "La prioridad (RECSE) es solo para voz por circuitos y una lista cerrada de autoridades",
    "La alerta sísmica pasa por una cadena manual y no hay alertas en lenguas originarias",
    "La IA de red es de riesgo alto, pero sin supervisión humana obligatoria en telecomunicaciones",
    "No hay protocolo transfronterizo de roaming o coordinación",
    "Competencias fragmentadas entre el MTC y OSIPTEL",
    "No hay Plan Nacional de Telecomunicaciones de Emergencia",
    "No hay datos ni mensajería gratuitos en emergencias"),
  Tipo = c("Ausencia + Insuficiencia","Ausencia + Conflicto","Ejecución","Desfase + Insuficiencia",
           "Ejecución + Insuficiencia","Insuficiencia + Competencia","Ausencia","Competencia",
           "Ausencia","Desfase"),
  Norma_peruana = c(
    "DS 002-2023-MTC (roaming obligatorio solo en centros poblados de hasta 1 000 habitantes); Res. 261-2023-CD/OSIPTEL",
    "Ley 29733, art. 14 (sin excepción por interés vital); DL 1182 (solo PNP en flagrancia); Const. art. 2.10",
    "DS 051-2010-MTC, arts. 2, 23.1 y 23.2",
    "DS 051-2010-MTC, arts. 2, 9 y 10 (1/2 E1, *272)",
    "Ley 30472; Directiva INDECI 003-2024; RM 896-2021-MTC (canal 4383 'a ser evaluado')",
    "DS 115-2025-PCM, arts. 24.1.a, 31.4 y 32.1",
    "Convenio de Tampere (ratificado en 2003); Decisión 854 de la CAN, art. 20",
    "DS 051-2010-MTC, art. 41 (supervisa solo el MTC)",
    "Presentación del MTC ante la UIT (2017): plan pendiente",
    "DS 051-2010-MTC, arts. 14 y 37.c (solo SMS y llamadas)"),
  Referente = c("FCC 47 CFR §4.17 (MDRI); Japan Roaming; RAN-E de Chile",
                "RGPD art. 6.1.d; EECC art. 109; E911/AML",
                "MDRI: 3 disparadores alternativos",
                "ITU-T E.106 / Y.1271; 3GPP MPS",
                "ShakeAlert/WEA; ETWS de Japón; EECC art. 110",
                "Ley de IA de la UE, anexo III.2; ITU-T Y.3172",
                "Resolución 34 de la UIT-D",
                "Rol de la FCC con el DIRS",
                "Guías NETP de la UIT (2020)",
                "Medidas voluntarias y de la FCC"),
  Prioridad = c(14,14,12,13,13,13,11,8,11,9),
  stringsAsFactors = FALSE)

fmt_t <- function(min) {
  ifelse(min < 1, sprintf("T+%ds", round(min * 60)),
         ifelse(min < 60, sprintf("T+%dmin", round(min)),
                sprintf("T+%.1fh", min / 60)))
}

# p: parámetros (incluye las políticas); sim: resultado de simular_escenario; curvas
eventos_normativos <- function(sim, curvas, distritos) {
  p <- sim$p
  ev <- list()
  add <- function(t_min, nivel, actor, desc, norma, vacio = "") {
    ev[[length(ev) + 1]] <<- data.frame(t_min = t_min, nivel = nivel, actor = actor,
      evento = desc, norma = norma, vacio = vacio, stringsAsFactors = FALSE)
  }
  add(0, "CRÍTICA", "IGP",
      sprintf("Sismo M%.1f, profundidad %d km (%s)", p$M, round(p$prof),
              if (sim$en_mar) "epicentro en el mar" else "epicentro en tierra"),
      "Ley 29664 (SINAGERD)")

  # Alerta temprana sísmica
  pob_total <- sum(distritos$poblacion)
  pct_sin <- 100 * sim$pob_sin_aviso / pob_total
  add(sim$t_det / 60, "ALTA", "IGP (SASPe)",
      sprintf("Detección automática en la primera estación costera (%.0f s)", sim$t_det),
      "Protocolo operativo SASPe IGP-INDECI [S]")
  if (isTRUE(p$alerta_auto)) {
    add(sim$t_alerta / 60, "ALTA", "SISMATE (automático)",
        sprintf("Alerta Cell Broadcast enviada de forma automática. El %.0f%% de la población la recibe DESPUÉS de la onda S.", pct_sin),
        "PROPUESTA: vía pre-aprobada IGP→SISMATE (no existe en la norma)", "G5 (resuelto en la simulación)")
  } else {
    add(sim$t_alerta / 60, "CRÍTICA", "INDECI/COEN (SISMATE manual)",
        sprintf("Alerta Cell Broadcast tras la cadena manual Monitoreo→Evaluador→Operador (%.0f min). El %.0f%% de la población la recibe DESPUÉS del sacudimiento.",
                p$min_manual, pct_sin),
        "Ley 30472; Directiva INDECI 003-2024, num. 6.2", "G5")
  }
  add(sim$t_alerta / 60 + 0.1, "MEDIA", "INDECI/COEN",
      "Mensaje solo en castellano y sin tildes; el canal 4383 (otro idioma) no está implementado",
      "RM 896-2021-MTC/01.03, Anexo Técnico", "G5")

  if (sim$tsunami) {
    tmin <- suppressWarnings(min(sim$t_tsu_min, na.rm = TRUE))
    add(6, "CRÍTICA", "DHN / INDECI",
        sprintf("ALERTA DE TSUNAMI. Primer arribo estimado: %.0f min en los distritos costeros", tmin),
        "Ley 30472 (SISMATE); DS 051-2010-MTC, art. 27 (radiodifusores)")
  }

  # Activación del sistema de comunicaciones de emergencia
  t_indeci <- if (isTRUE(p$activacion_auto)) 2 else p$min_indeci
  if (isTRUE(p$activacion_auto)) {
    add(t_indeci, "ALTA", "MTC (automático por umbral)",
        "Activación automática: RECSE + 119 a nivel nacional al superarse el umbral de magnitud",
        "PROPUESTA: disparador objetivo (no existe en la norma)", "G3 (resuelto en la simulación)")
  } else {
    add(t_indeci, if (p$min_indeci > 15) "ALTA" else "MEDIA", "INDECI → operadores",
        sprintf("INDECI comunica la emergencia (vía satelital, %.0f min). Recién ahora se activan la RECSE y el 119", p$min_indeci),
        "DS 051-2010-MTC, arts. 23.1, 23.3 y 37", if (p$min_indeci > 15) "G3" else "")
  }
  add(t_indeci + 1, "MEDIA", "Operadores",
      "Prioridad RECSE (*272) solo para las autoridades registradas; alcaldes y hospitales quedan fuera; no hay prioridad para datos ni VoLTE",
      "DS 051-2010-MTC, arts. 2, 9 y 10", "G4")

  pico <- max(curvas$caidos)
  t_pico <- curvas$t[which.max(curvas$caidos)]
  add(20, if (pico > 0.2) "CRÍTICA" else "MEDIA", "Operadores / OSIPTEL",
      sprintf("Monitoreo: caídas de sitios en curso; pico proyectado del %.0f%% a las %.0f h (agotamiento de baterías y corte del anillo de fibra del Operador A)", 100 * pico, t_pico),
      "DS 051-2010-MTC, art. 22 (Plan de Contingencia)")
  b0 <- curvas$bloqueo[curvas$t == 0][1]
  if (b0 > 0.3)
    add(25, "ALTA", "Operadores",
        sprintf("Congestión: el %.0f%% de los intentos de llamada fallan. Se limitan las llamadas a 1-2 min (salvo la RECSE)", 100 * b0),
        "DS 051-2010-MTC, art. 22.2.a")
  if (pico > 0.1)
    add(30, "ALTA", "IA de red del Operador A",
        "El optimizador de IA despriorizó el video y apagó sitios con batería baja SIN validación humana",
        "DS 115-2025-PCM, art. 24.1.a (riesgo alto); art. 31.4 no incluye las telecomunicaciones", "G6")
  add(120, "MEDIA", "OSIPTEL",
      "Recomienda usar mensajería por datos, pero la gratuidad cubre solo SMS y llamadas",
      "DS 051-2010-MTC, arts. 14 y 37.c", "G10")
  add(360, "ALTA", "INDECI",
      "Define las zonas afectadas. Recién ahora rigen las llamadas y SMS gratuitos en la zona",
      "DS 051-2010-MTC, arts. 23.2, 31 y 32", "G3")
  add(540, "CRÍTICA", "INDECI → Operador A → ANPD",
      "Pedido de ubicación de 37 números para el rescate: el mapa agregado es lícito (art. 14.8); la ubicación individual no tiene base legal",
      "Ley 29733, art. 14; DL 1182; Const. art. 2.10", "G2")
  if (isTRUE(p$roaming)) {
    add(26 * 60, "ALTA", "MTC / operadores",
        "Roaming de emergencia activado: los usuarios del Operador A usan la red de B, C y D",
        "PROPUESTA tipo FCC 47 CFR §4.17 (no existe en la norma)", "G1 (resuelto en la simulación)")
  } else {
    add(26 * 60, "CRÍTICA", "Operador A → Operador B → OSIPTEL",
        "El Operador B rechaza el roaming; el mandato no procede (zona de más de 1 000 habitantes, procedimiento ordinario)",
        "DS 002-2023-MTC; Res. 261-2023-CD/OSIPTEL", "G1")
  }
  add(46 * 60, "MEDIA", "MTC + INDECI",
      "Vence el periodo mínimo de 48 h de activación; corresponde evaluar una extensión",
      "DS 051-2010-MTC, art. 24.2")
  out <- do.call(rbind, ev)
  out <- out[order(out$t_min), ]
  out$tiempo <- fmt_t(out$t_min)
  out
}
