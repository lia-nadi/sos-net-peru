# =============================================================================
# Validación del modelo de intensidad y pruebas de coherencia del simulador
#
# Datos: 167 intensidades REALES observadas en Lima y Callao, de 28 sismos (1940-2026)
#   · 158 del ShakeMap del USGS (históricas, «¿Lo sentiste?» y estaciones sísmicas), ver R/obs_usgs.R
#   ·   9 intensidades del IGP / catálogos históricos por distrito (1940, 1974, 2007, 2025)
# Cada sismo pesa lo mismo en el ajuste y en las métricas (un sismo con 30 reportes no
# domina sobre uno con 2).
#
# ¿Se usa 80/20? Sí, POR SISMO: se separa el 80 % de los sismos para ajustar y el 20 %
# para probar (nunca se mezclan datos de un mismo sismo en ambos lados). Se repite 500 veces.
# Además se deja fuera cada sismo completo (LOEO, 28 pliegues): es lo que ocurrirá con un
# terremoto nuevo. En cada pliegue se reajustan a, b y c y se mide el error en lo no visto.
# =============================================================================

OBS_MMI <- data.frame(
  evento = c("Pisco 2007", "Pisco 2007", "Callao 2025", "Callao 2025",
             "Lima 1974", "Lima 1974", "Lima 1974", "Lima 1940", "Lima 1940"),
  distrito = c("LIMA", "CALLAO", "LIMA", "CALLAO", "LIMA", "CALLAO", "LA MOLINA", "LIMA", "CALLAO"),
  mmi_obs = c(6, 6, 5, 5.5, 7, 8, 8, 7.5, 8),
  stringsAsFactors = FALSE)
EVENTOS_OBS <- list(
  "Pisco 2007"  = c("Real 15/08/2007 Pisco (USGS Mw 8.0)", "usp000fjta"),
  "Callao 2025" = c("Real 15/06/2025 Callao (USGS Mww 5.6; IGP 6.1)", "us6000qkg7"),
  "Lima 1974"   = c("Real 03/10/1974 Lima (USGS Ms 7.6; Mw ~8.1)", "usp0000888"),
  "Lima 1940"   = c("Real 24/05/1940 Lima (USGS Mw 8.2)", "iscgem901374"))

# Criterios de aceptación (declarados antes de medir; no se cambian para que pasen)
CRITERIOS <- list(rmse_8020 = 1.0, rmse_loeo = 1.25, dentro1 = 0.70)
COEF_ANTES <- c(a = 2.75, b = 1.00, c = 2.25)   # calibración previa (solo 9 datos), para comparar

obs_validacion <- function(distritos, archivo = "data/obs_intensidad.csv") {
  i <- match(OBS_MMI$distrito, distritos$distrito)
  igp <- do.call(rbind, lapply(seq_len(nrow(OBS_MMI)), function(k) {
    ev <- EVENTOS_OBS[[OBS_MMI$evento[k]]]; e <- ESCENARIOS[[ev[1]]]; j <- i[k]
    data.frame(evento_id = ev[2], fecha = substr(OBS_MMI$evento[k], nchar(OBS_MMI$evento[k]) - 3, 99), M = e$M,
               ev_lat = e$lat, ev_lon = e$lon, prof = e$prof, tipo = "IGP (distrito)",
               lugar = OBS_MMI$distrito[k], lat = distritos$lat[j], lon = distritos$lon[j],
               mmi_obs = OBS_MMI$mmi_obs[k], dmmi = distritos$dmmi[j], stringsAsFactors = FALSE)
  }))
  o <- if (file.exists(archivo)) readr::read_csv(archivo, show_col_types = FALSE) else NULL
  if (!is.null(o)) {
    o <- data.frame(evento_id = o$evento_id, fecha = as.character(o$fecha), M = o$M, ev_lat = o$ev_lat, ev_lon = o$ev_lon,
                    prof = o$prof, tipo = c(historica = "USGS histórica", DYFI = "USGS «¿Lo sentiste?»",
                                            instrumental = "USGS estación sísmica")[o$tipo],
                    lugar = ifelse(is.na(o$distrito), "fuera de distrito", o$distrito), lat = o$lat, lon = o$lon,
                    mmi_obs = o$mmi_obs, dmmi = o$dmmi, stringsAsFactors = FALSE)
    igp <- rbind(igp, o)
  }
  igp$logR <- log10(pmax(mapply(dist_ruptura, igp$M, igp$ev_lat, igp$ev_lon, igp$prof, igp$lat, igp$lon), 15))
  anio <- substr(igp$fecha, 1, 4)
  igp$evento <- sprintf("%s · M%.1f", anio, igp$M)
  igp$w <- 1 / ave(rep(1, nrow(igp)), igp$evento_id, FUN = sum)
  igp
}

predecir_mmi <- function(coef, X) pmin(10, pmax(1, coef[[1]] + coef[[2]] * X$M - coef[[3]] * X$logR + X$dmmi))
ajustar_mmi <- function(d) {
  m <- stats::lm(I(mmi_obs - dmmi) ~ M + logR, data = d, weights = d$w)
  c(a = unname(coef(m)[1]), b = unname(coef(m)[2]), c = -unname(coef(m)[3]))
}
metricas <- function(obs, pred, w = rep(1, length(obs))) {
  e <- pred - obs
  c(rmse = sqrt(weighted.mean(e^2, w)), mae = weighted.mean(abs(e), w), sesgo = weighted.mean(e, w),
    dentro1 = weighted.mean(abs(e) <= 1, w))
}

validar_intensidad <- function(distritos, reps = 500, semilla = 2026) {
  X <- obs_validacion(distritos); y <- X$mmi_obs
  ev <- unique(X$evento_id); n_ev <- length(ev)
  coef_todo <- ajustar_mmi(X)
  pred_app <- predecir_mmi(MMI_COEF, X)
  pred_antes <- predecir_mmi(COEF_ANTES, X)

  # 1. 80/20 por sismo, repetido
  set.seed(semilla)
  n_test <- max(1, round(0.2 * n_ev))
  rep8020 <- do.call(rbind, lapply(seq_len(reps), function(r) {
    te <- X$evento_id %in% sample(ev, n_test)
    cf <- ajustar_mmi(X[!te, ])
    data.frame(rep = r,
               rmse_ajuste = metricas(y[!te], predecir_mmi(cf, X[!te, ]), X$w[!te])[["rmse"]],
               rmse_prueba = metricas(y[te], predecir_mmi(cf, X[te, ]), X$w[te])[["rmse"]],
               a = cf[["a"]], b = cf[["b"]], c = cf[["c"]])
  }))

  # 2. Dejar fuera cada sismo (LOEO)
  pred_loeo <- rep(NA_real_, nrow(X))
  for (e in ev) { te <- X$evento_id == e; pred_loeo[te] <- predecir_mmi(ajustar_mmi(X[!te, ]), X[te, ]) }

  por_evento <- do.call(rbind, lapply(ev, function(e) {
    i <- X$evento_id == e
    data.frame(evento = X$evento[i][1], id = e, n = sum(i), obs_media = mean(y[i]),
               rmse_calibracion = sqrt(mean((pred_app[i] - y[i])^2)),
               rmse_sin_ver_evento = sqrt(mean((pred_loeo[i] - y[i])^2)),
               sesgo_sin_ver_evento = mean(pred_loeo[i] - y[i]), stringsAsFactors = FALSE)
  }))
  por_evento <- por_evento[order(-as.numeric(sub(".*M", "", por_evento$evento))), ]
  por_tipo <- do.call(rbind, lapply(unique(X$tipo), function(t) {
    i <- X$tipo == t
    data.frame(tipo = t, n = sum(i), sismos = length(unique(X$evento_id[i])),
               rmse_sin_ver_evento = sqrt(mean((pred_loeo[i] - y[i])^2)),
               dentro1 = mean(abs(pred_loeo[i] - y[i]) <= 1), stringsAsFactors = FALSE)
  }))

  tabla <- data.frame(evento = X$evento, tipo = X$tipo, lugar = X$lugar, lat = X$lat, lon = X$lon,
                      mmi_obs = y, pred_calibracion = pred_app, pred_loeo = pred_loeo, pred_antes = pred_antes)
  list(tabla = tabla, rep8020 = rep8020, por_evento = por_evento, por_tipo = por_tipo,
       n_obs = nrow(X), n_ev = n_ev, n_test = n_test, reps = reps, coef_todo = coef_todo,
       m_calib = metricas(y, pred_app, X$w), m_antes = metricas(y, pred_antes, X$w),
       m_loeo = metricas(y, pred_loeo, X$w),
       m_8020 = c(rmse_mediana = median(rep8020$rmse_prueba),
                  rmse_p90 = unname(quantile(rep8020$rmse_prueba, 0.9)),
                  rmse_ajuste_mediana = median(rep8020$rmse_ajuste)))
}

# ----------------------------------------------------------------------------
# Pruebas de coherencia del simulador completo (física, red, alerta, políticas)
pruebas_coherencia <- function(distritos, sitios, costa_pts, contorno) {
  res <- list()
  prueba <- function(area, nombre, ok, detalle) {
    res[[length(res) + 1]] <<- data.frame(area = area, prueba = nombre, resultado = isTRUE(ok),
                                          detalle = detalle, stringsAsFactors = FALSE)
  }
  base <- list(lat = -12.50, lon = -77.45, M = 8.0, prof = 30, alerta_auto = FALSE, min_manual = 8,
               activacion_auto = FALSE, min_indeci = 40, roaming = FALSE, portatiles = FALSE, supuesto_A = TRUE)
  conp <- function(...) modifyList(base, list(...))
  corre <- function(p) simular_escenario(p, distritos, sitios, costa_pts, contorno)

  # Física de la intensidad
  m_d <- mmi_de(8, c(20, 50, 100, 200, 400))
  prueba("Física", "La intensidad baja con la distancia", all(diff(m_d) <= 0),
         paste("MMI a 20, 50, 100, 200 y 400 km:", paste(round(m_d, 1), collapse = " · ")))
  m_m <- sapply(c(6, 7, 8, 9), function(M) mmi_de(M, 100))
  prueba("Física", "La intensidad sube con la magnitud", all(diff(m_m) > 0),
         paste("MMI a 100 km para M6, M7, M8 y M9:", paste(round(m_m, 1), collapse = " · ")))
  prueba("Física", "El suelo blando amplifica", mmi_de(8, 100, 0.5) > mmi_de(8, 100, 0),
         sprintf("MMI en roca %.1f · en suelo blando %.1f", mmi_de(8, 100, 0), mmi_de(8, 100, 0.5)))
  prueba("Física", "La intensidad está acotada entre I y X",
         all(mmi_de(c(4, 9.5), c(800, 1)) >= 1 & mmi_de(c(4, 9.5), c(800, 1)) <= 10), "MMI ∈ [1, 10]")

  # Comprobación con un evento real: Callao 15/06/2025
  e25 <- ESCENARIOS[["Real 15/06/2025 Callao (USGS Mww 5.6; IGP 6.1)"]]
  s25 <- corre(conp(lat = e25$lat, lon = e25$lon, M = e25$M, prof = e25$prof))
  max25 <- max(sapply(seq(0, 72, 2), function(h) mean(estado_sitios(h, s25, sitios) == 2L)))
  prueba("Evento real", "Callao 2025: la red no colapsa (OSIPTEL: 38 antenas afectadas, sin interrupción)",
         max25 < 0.01, sprintf("Máximo de sitios caídos en 72 h: %.2f %% (umbral < 1 %%)", 100 * max25))
  ePi <- ESCENARIOS[["Real 15/08/2007 Pisco (USGS Mw 8.0)"]]
  sPi <- corre(conp(lat = ePi$lat, lon = ePi$lon, M = ePi$M, prof = ePi$prof))
  mPi <- sPi$mmi_d[distritos$distrito == "LIMA"][1]
  prueba("Evento real", "Pisco 2007: Lima siente intensidad VI (IGP)", abs(mPi - 6) <= 1,
         sprintf("MMI simulada en Cercado de Lima: %.1f (observada VI)", mPi))

  # Red móvil
  s <- corre(base); cur <- curvas_red(s, sitios, distritos)
  caidos <- tapply(cur$caidos, cur$t, mean)
  prueba("Red", "Los porcentajes están entre 0 y 100 %",
         all(cur$disponibilidad >= 0 & cur$disponibilidad <= 1 & cur$bloqueo >= 0 & cur$bloqueo <= 1),
         "Disponibilidad, caídas y bloqueo dentro de [0, 1]")
  prueba("Red", "La red se recupera hacia las 72 h", tail(caidos, 1) < max(caidos),
         sprintf("Pico de caídas %.1f %% · a las 72 h %.1f %%", 100 * max(caidos), 100 * tail(caidos, 1)))
  sin <- cur$disponibilidad[grepl("^Sin", cur$roaming)]; con <- cur$disponibilidad[grepl("^Con", cur$roaming)]
  prueba("Políticas", "El roaming de emergencia (G1) nunca empeora la cobertura", all(con >= sin - 1e-9),
         sprintf("Mejora media %.1f puntos; mínima %.1f", 100 * mean(con - sin), 100 * min(con - sin)))
  sp <- corre(conp(portatiles = TRUE))
  cp <- sapply(c(24, 36, 48), function(h) mean(estado_sitios(h, sp, sitios) == 2L))
  cb <- sapply(c(24, 36, 48), function(h) mean(estado_sitios(h, s, sitios) == 2L))
  prueba("Políticas", "Las estaciones portátiles aceleran el restablecimiento", all(cp <= cb),
         sprintf("Caídos a 24/36/48 h: %s %% con portátiles vs %s %% sin",
                 paste(round(100 * cp, 1), collapse = "/"), paste(round(100 * cb, 1), collapse = "/")))
  m7 <- corre(conp(M = 7)); m9 <- corre(conp(M = 8.8))
  c7 <- max(sapply(c(1, 4, 12), function(h) mean(estado_sitios(h, m7, sitios) == 2L)))
  c9 <- max(sapply(c(1, 4, 12), function(h) mean(estado_sitios(h, m9, sitios) == 2L)))
  prueba("Red", "Un sismo mayor deja más antenas caídas", c9 > c7,
         sprintf("Pico de caídas: M7.0 %.1f %% · M8.8 %.1f %%", 100 * c7, 100 * c9))

  # Alerta temprana
  sa <- corre(conp(alerta_auto = TRUE))
  prueba("Alerta", "La alerta llega después de detectar la onda P", s$t_alerta > s$t_det && sa$t_alerta > sa$t_det,
         sprintf("Detección %.0f s · alerta manual %.0f s · automática %.0f s", s$t_det, s$t_alerta, sa$t_alerta))
  prueba("Alerta", "La alerta automática (G5) reduce a quienes reciben el aviso tarde",
         sa$pob_sin_aviso <= s$pob_sin_aviso,
         sprintf("Población con aviso tardío: %.2f M (manual) vs %.2f M (automática)",
                 s$pob_sin_aviso / 1e6, sa$pob_sin_aviso / 1e6))
  prueba("Alerta", "Sismo en el mar M8 genera alerta de tsunami; sismo en tierra no",
         s$tsunami && !corre(conp(lat = -11.9, lon = -76.9))$tsunami, "Epicentro marino vs epicentro continental")

  # Reproducibilidad
  prueba("Reproducibilidad", "Mismos datos de entrada dan el mismo resultado",
         identical(estado_sitios(6, s, sitios), estado_sitios(6, corre(base), sitios)),
         "Dos corridas idénticas del escenario P1 a las 6 h")
  do.call(rbind, res)
}

# Veredicto global según los criterios declarados
veredicto <- function(v, pc) {
  chk <- c(v$m_8020["rmse_mediana"] <= CRITERIOS$rmse_8020,
           v$m_loeo["rmse"] <= CRITERIOS$rmse_loeo,
           v$m_loeo["dentro1"] >= CRITERIOS$dentro1,
           all(pc$resultado))
  list(ok = all(chk), n_ok = sum(chk), n = length(chk),
       criterios = data.frame(
         criterio = c(sprintf("Error de prueba 80/20 por sismo (mediana de %d particiones) ≤ %.2f MMI", v$reps, CRITERIOS$rmse_8020),
                      sprintf("Error al predecir un sismo no visto (LOEO) ≤ %.2f MMI", CRITERIOS$rmse_loeo),
                      sprintf("Predicciones LOEO a ±1 grado de lo observado ≥ %.0f %%", 100 * CRITERIOS$dentro1),
                      sprintf("Pruebas de coherencia superadas (%d de %d)", sum(pc$resultado), nrow(pc))),
         valor = c(sprintf("%.2f", v$m_8020["rmse_mediana"]), sprintf("%.2f", v$m_loeo["rmse"]),
                   sprintf("%.0f %%", 100 * v$m_loeo["dentro1"]), sprintf("%d/%d", sum(pc$resultado), nrow(pc))),
         cumple = unname(chk), stringsAsFactors = FALSE))
}
