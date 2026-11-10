# =============================================================================
# Motor de simulación (ilustrativo, NO es un modelo de ingeniería sísmica)
#  - Fuente: para M >= 7 se modela una ruptura lineal a lo largo de la fosa
#    (azimut 325°) con longitud L = 10^(-2.44 + 0.59*M) km (Wells & Coppersmith, 1994)
#    centrada en el epicentro; R = distancia a la ruptura más la profundidad.
#  - Intensidad: MMI = 6.52 + 0.76*M - 3.54*log10(R) + dMMI_suelo, calibrada por
#    mínimos cuadrados ponderados (cada sismo pesa lo mismo) con 167 intensidades REALES
#    observadas en Lima y Callao de 28 sismos (1940-2026): ShakeMap del USGS (históricas,
#    «¿Lo sentiste?» y estaciones sísmicas) + 9 intensidades del IGP. Ver R/obs_usgs.R y
#    R/validacion.R (80/20 por sismo y validación dejando fuera cada sismo).
#  - Suelo: dMMI según Vs30 (pendiente, Wald & Allen 2007, corregida con el CISMID)
#    y la amplificación de Borcherdt (1994). Ver R/vs30_pendiente.R.
#  - PGA a partir de la MMI (Wald et al., 1999): log10(PGA[cm/s2]) = (MMI + 1.66) / 3.66
#  - Falla de sitios: curvas de fragilidad log-normales (estructura, energía, fibra)
#    con autonomía de baterías.
#  - Alerta temprana: Vp = 6.5 km/s, Vs = 3.5 km/s, procesamiento de 4 s.
#  - Tsunami: velocidad de aguas someras sqrt(g*h) con h = 400 m, profundidad
#    media efectiva de la plataforma; da unos 12 min a 45 km de la costa.
# =============================================================================

VP <- 6.5; VS <- 3.5; T_PROC <- 4
V_TSUNAMI_KMH <- sqrt(9.81 * 400) * 3.6

dist_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

MMI_COEF <- c(a = 6.52, b = 0.76, c = 3.54)
mmi_de <- function(M, R, dmmi = 0)
  pmin(10, pmax(1, MMI_COEF[["a"]] + MMI_COEF[["b"]] * M - MMI_COEF[["c"]] * log10(pmax(R, 15)) + dmmi))

AZ_FOSA <- 325
long_ruptura <- function(M) if (M >= 7) 10^(-2.44 + 0.59 * M) else 0

# Distancia (km) de cada punto a la ruptura lineal, más la profundidad
dist_ruptura <- function(M, lat0, lon0, prof, lat, lon, az = AZ_FOSA) {
  kx <- 111.32 * cos(lat0 * pi / 180); ky <- 110.57
  x <- (lon - lon0) * kx; y <- (lat - lat0) * ky
  L <- long_ruptura(M)
  if (L == 0) return(sqrt(x^2 + y^2 + prof^2))
  ux <- sin(az * pi / 180); uy <- cos(az * pi / 180)
  t <- pmax(-L / 2, pmin(L / 2, x * ux + y * uy))
  sqrt((x - t * ux)^2 + (y - t * uy)^2 + prof^2)
}
extremos_ruptura <- function(M, lat0, lon0, az = AZ_FOSA) {
  L <- long_ruptura(M); if (L == 0) return(NULL)
  kx <- 111.32 * cos(lat0 * pi / 180); ky <- 110.57
  dx <- sin(az * pi / 180) * L / 2 / kx; dy <- cos(az * pi / 180) * L / 2 / ky
  list(c(lon0 - dx, lat0 - dy), c(lon0 + dx, lat0 + dy))
}
pga_g  <- function(mmi) 10^((mmi + 1.66) / 3.66) / 981
frac   <- function(x) x - floor(x)

romano <- function(x) {
  r <- c("I","II","III","IV","V","VI","VII","VIII","IX","X")
  r[pmin(10, pmax(1, round(x)))]
}

ESCENARIOS <- list(
  "P1: M8.0 frente a Lima Sur (sintético)" =
    list(lat = -12.50, lon = -77.45, M = 8.0, prof = 30,
         nota = "Escenario de la Propuesta 1 (sintético)."),
  "INDECI: M8.8 costa central (escenario oficial, aprox.)" =
    list(lat = -12.30, lon = -77.60, M = 8.8, prof = 30,
         nota = "Magnitud del escenario oficial INDECI-DIPRE (2017); el epicentro es aproximado."),
  "Real 03/10/1974 Lima (USGS Ms 7.6; Mw ~8.1)" =
    list(lat = -12.265, lon = -77.795, M = 8.1, prof = 13,
         nota = "USGS id del evento: Ms 7.6 en el catálogo; en la literatura se reporta Mw ~8.1."),
  "Real 15/08/2007 Pisco (USGS Mw 8.0)" =
    list(lat = -13.386, lon = -76.603, M = 8.0, prof = 39, nota = "Catálogo USGS."),
  "Real 24/05/1940 Lima (USGS Mw 8.2)" =
    list(lat = -11.217, lon = -77.438, M = 8.2, prof = 45, nota = "Catálogo USGS."),
  "Real 15/06/2025 Callao (USGS Mww 5.6; IGP 6.1)" =
    list(lat = -12.1764, lon = -77.4434, M = 5.6, prof = 40,
         nota = "Catálogo USGS. OSIPTEL reportó 38 antenas afectadas y ninguna interrupción.")
)

# ---------------------------------------------------------------------------
# Cálculo estático por escenario (se hace una vez al pulsar "Simular")
simular_escenario <- function(p, distritos, sitios, costa_pts, contorno) {
  # Distancia a la ruptura (falla extensa si M >= 7) y amplificación por suelo
  rd <- dist_ruptura(p$M, p$lat, p$lon, p$prof, distritos$lat, distritos$lon)
  rs <- dist_ruptura(p$M, p$lat, p$lon, p$prof, sitios$lat, sitios$lon)
  mmi_d <- mmi_de(p$M, rd, distritos$dmmi)
  mmi_s <- mmi_de(p$M, rs, sitios$dmmi)
  # Las ondas sísmicas se propagan desde el hipocentro (para la alerta temprana)
  rh <- sqrt(dist_km(p$lat, p$lon, distritos$lat, distritos$lon)^2 + p$prof^2)

  # Alerta temprana: la primera estación es el punto de costa más cercano
  # (el IGP instaló estaciones en faros de la costa).
  d_st <- min(dist_km(p$lat, p$lon, costa_pts$lat, costa_pts$lon))
  r_st <- sqrt(d_st^2 + p$prof^2)
  t_det <- r_st / VP + T_PROC
  latencia <- if (isTRUE(p$alerta_auto)) 2 else p$min_manual * 60
  t_alerta <- t_det + latencia
  t_s <- rh / VS
  aviso_s <- t_s - t_alerta                    # >0: la alerta llega antes de la onda S

  # Tsunami
  en_mar <- !any(sf::st_intersects(
    sf::st_sfc(sf::st_point(c(p$lon, p$lat)), crs = 4326), contorno, sparse = FALSE))
  tsunami <- p$M >= 7 && p$prof <= 60 && en_mar
  costero <- !is.na(distritos$costero) & distritos$costero
  t_tsu_min <- ifelse(tsunami & costero,
                      dist_km(p$lat, p$lon, distritos$lat, distritos$lon) / V_TSUNAMI_KMH * 60, NA)

  # Pico de demanda de llamadas según la intensidad media sentida (ponderada por población)
  mmi_w <- sum(mmi_d * distritos$poblacion) / sum(distritos$poblacion)
  pico <- 1 + 7 * pnorm((mmi_w - 5.5) / 0.8)

  list(p = p, ruptura = extremos_ruptura(p$M, p$lat, p$lon), L_km = long_ruptura(p$M), mmi_d = mmi_d, pico_demanda = pico, mmi_s = mmi_s, pga_d = pga_g(mmi_d), t_s = t_s,
       aviso_s = aviso_s, t_det = t_det, t_alerta = t_alerta, en_mar = en_mar,
       tsunami = tsunami, t_tsu_min = t_tsu_min,
       pob_mmi7 = sum(distritos$poblacion[mmi_d >= 7]),
       pob_sin_aviso = sum(distritos$poblacion[aviso_s <= 0]))
}

# ---------------------------------------------------------------------------
# Estado de los sitios en el instante t (horas)
# 0 = operativo · 1 = con batería · 2 = caído · 3 = restablecido
estado_sitios <- function(t, sim, sitios) {
  m <- sim$mmi_s
  # Supuesto del escenario P1: el Operador A tiene menos redundancia en Lima Sur
  # (anillo de fibra sin respaldo y baterías de menor autonomía).
  # Se puede desactivar desde la app (input "supuesto_A") para ver el escenario sin este supuesto.
  opA <- sitios$operador == "A" & !isFALSE(sim$p$supuesto_A)
  redundante <- sitios$fibra_redundante & !opA
  bat <- ifelse(opA, sitios$bateria_h * 0.6, sitios$bateria_h)
  estruct <- sitios$u1 < pnorm((m - 8.7) / 0.5)
  fibra   <- frac(sitios$u1 * 7.31) < pnorm((m - 8.0) / 0.6) * ifelse(redundante, 0.25, 1)
  energia <- sitios$u2 < pnorm((m - 7.6) / 0.6)
  t_luz   <- 2 + 34 * sitios$u3
  t_rep   <- 12 + 60 * frac(sitios$u3 * 3.7)
  if (isTRUE(sim$p$portatiles)) t_rep <- t_rep * 0.6     # política: estaciones portátiles priorizadas
  # Anillo de fibra del Operador A en Lima Sur (supuesto declarado del escenario P1):
  # tendido aéreo sin respaldo por los acantilados de la Costa Verde, donde hubo derrumbes
  # con intensidades VI-VII (1974, 2007). Si la MMI media del anillo llega a 6.5, se corta
  # y el 75 % de sus sitios al sur de -12.12 queda aislado hasta su reparación.
  anillo <- opA & sitios$lat < -12.12 & frac(sitios$u2 * 5.3) < 0.75
  corte_anillo <- any(anillo) && mean(m[anillo]) >= 6.5
  t_anillo <- if (isTRUE(sim$p$portatiles)) 18 else 30
  aislado <- anillo & corte_anillo & t < t_anillo
  caido_f  <- ((estruct | fibra) & t < t_rep) | aislado
  sin_luz  <- energia & t < t_luz
  caido_b  <- sin_luz & !caido_f & t >= bat
  bateria  <- sin_luz & !caido_f & !caido_b
  afectado <- estruct | fibra | energia | (anillo & corte_anillo)
  est <- rep(0L, length(m))
  est[afectado & !caido_f & !caido_b & !bateria] <- 3L
  est[bateria] <- 1L
  est[caido_f | caido_b] <- 2L
  est
}

# Disponibilidad ponderada por población, por operador, con y sin roaming de emergencia
disponibilidad <- function(est, sitios, distritos) {
  up <- est != 2L
  pob <- setNames(distritos$poblacion, distritos$IDDIST)
  ops <- c("A", "B", "C", "D")
  # Fracción de sitios arriba por distrito y operador
  por_op <- sapply(ops, function(o) {
    s <- sitios$operador == o
    tapply(up[s], factor(sitios$IDDIST[s], levels = names(pob)), mean)
  })
  por_op[is.na(por_op)] <- 1
  todos <- tapply(up, factor(sitios$IDDIST, levels = names(pob)), mean)
  todos[is.na(todos)] <- 1
  w <- pob / sum(pob)
  sin_roaming <- colSums(por_op * w)
  # Con roaming: cada operador usa la mejor red disponible del distrito (sujeto a capacidad)
  con_roaming <- colSums(pmax(por_op, matrix(todos, nrow = length(todos), ncol = 4) * 0.9) * w)
  list(sin = sin_roaming, con = con_roaming, total = sum(todos * w))
}

bloqueo <- function(t, capacidad, pico = 8) {
  demanda <- 1 + (pico - 1) * exp(-t / 2)     # pico de demanda que decae con t = 2 h
  pmax(0, 1 - capacidad / demanda)
}

curvas_red <- function(sim, sitios, distritos, horas = c(0, 0.5, 1, 2, 3, seq(4, 72, by = 2))) {
  do.call(rbind, lapply(horas, function(h) {
    e <- estado_sitios(h, sim, sitios)
    d <- disponibilidad(e, sitios, distritos)
    data.frame(t = h,
               operador = c("A", "B", "C", "D", "A", "B", "C", "D"),
               roaming = rep(c("Sin roaming de emergencia (norma actual)",
                               "Con roaming de emergencia (propuesta G1)"), each = 4),
               disponibilidad = c(d$sin, d$con),
               caidos = mean(e == 2L),
               bloqueo = bloqueo(h, d$total, sim$pico_demanda))
  }))
}
