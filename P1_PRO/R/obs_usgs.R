# =============================================================================
# Intensidades REALES observadas en Lima y Callao (USGS ShakeMap stationlist)
#  - "historica":    intensidades macrosísmicas de los catálogos históricos (1970, 1974, 2007…)
#  - "DYFI":         reportes ciudadanos «¿Lo sentiste?» del USGS, agregados por celda
#  - "instrumental": intensidad derivada de la aceleración registrada en estaciones sísmicas
# Se descargan los ShakeMap de los sismos M ≥ 5 a menos de 400 km de Lima y se
# conservan las observaciones dentro de Lima Metropolitana y Callao.
# Salida: data/obs_intensidad.csv (la usa R/validacion.R para calibrar y validar).
# Uso: source("R/obs_usgs.R"); construir_obs_usgs()   (directorio de trabajo: P1_PRO)
# =============================================================================

BBOX_LIMA <- c(lat1 = -12.75, lat2 = -11.55, lon1 = -77.35, lon2 = -76.55)

descargar_shakemaps <- function(dir = "data_raw/usgs_shakemap", radio_km = 400, mmin = 5) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  url <- sprintf(paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&latitude=-12.05",
                        "&longitude=-77.04&maxradiuskm=%d&minmagnitude=%.1f&starttime=1900-01-01&producttype=shakemap"),
                 radio_km, mmin)
  ev <- jsonlite::fromJSON(url, simplifyVector = FALSE)$features
  for (f in ev) {
    sl <- file.path(dir, paste0(f$id, "_stationlist.json"))
    if (file.exists(sl)) next
    det <- tryCatch(jsonlite::fromJSON(f$properties$detail, simplifyVector = FALSE), error = function(e) NULL)
    sm <- det$properties$products$shakemap
    if (is.null(sm)) next
    k <- grep("stationlist.json$", names(sm[[1]]$contents), value = TRUE)[1]
    if (is.na(k)) next
    txt <- tryCatch(readLines(sm[[1]]$contents[[k]]$url, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(txt)) next
    est <- jsonlite::fromJSON(paste(txt, collapse = ""), simplifyVector = FALSE)$features
    dentro <- vapply(est, function(s) { x <- unlist(s$geometry$coordinates)
      x[2] >= BBOX_LIMA["lat1"] && x[2] <= BBOX_LIMA["lat2"] && x[1] >= BBOX_LIMA["lon1"] && x[1] <= BBOX_LIMA["lon2"] }, logical(1))
    if (any(dentro)) {
      writeLines(txt, sl, useBytes = TRUE)
      jsonlite::write_json(det, file.path(dir, paste0(f$id, "_detalle.json")), auto_unbox = TRUE, digits = NA)
    }
  }
  invisible(dir)
}

leer_shakemaps <- function(dir = "data_raw/usgs_shakemap") {
  num <- function(x) suppressWarnings(as.numeric(if (is.null(x)) NA else x))
  filas <- lapply(list.files(dir, "_stationlist.json$", full.names = TRUE), function(f) {
    id <- sub("_stationlist.json$", "", basename(f))
    det <- jsonlite::fromJSON(file.path(dir, paste0(id, "_detalle.json")), simplifyVector = FALSE)
    sm <- det$properties$products$shakemap[[1]]$properties
    g <- unlist(det$geometry$coordinates)
    ev <- list(M = num(sm$magnitude %||% det$properties$mag), lat = num(sm$latitude %||% g[2]),
               lon = num(sm$longitude %||% g[1]), prof = num(sm$depth %||% g[3]),
               fecha = format(as.POSIXct(det$properties$time / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"))
    est <- jsonlite::fromJSON(f, simplifyVector = FALSE)$features
    do.call(rbind, lapply(est, function(s) {
      x <- unlist(s$geometry$coordinates); p <- s$properties
      if (x[2] < BBOX_LIMA["lat1"] || x[2] > BBOX_LIMA["lat2"] || x[1] < BBOX_LIMA["lon1"] || x[1] > BBOX_LIMA["lon2"]) return(NULL)
      i <- num(p$intensity); if (is.na(i)) return(NULL)
      src <- p$source %||% ""
      tipo <- if (identical(p$station_type, "seismic")) "instrumental" else
        if (grepl("DYFI|Did You", src) || identical(p$network, "DYFI")) "DYFI" else "historica"
      data.frame(evento_id = id, fecha = ev$fecha, M = ev$M, ev_lat = ev$lat, ev_lon = ev$lon, prof = ev$prof,
                 tipo = tipo, fuente = paste("USGS ShakeMap ·", src), lat = round(x[2], 4), lon = round(x[1], 4),
                 mmi_obs = i, stringsAsFactors = FALSE)
    }))
  })
  o <- do.call(rbind, filas)
  # 1974: el catálogo da Ms 7.6 (magnitud saturada); la literatura reporta Mw ~8.1 (igual que el escenario de la app)
  o$M[o$evento_id == "usp0000888"] <- 8.1
  o
}

construir_obs_usgs <- function(descargar = TRUE, salida = "data/obs_intensidad.csv") {
  if (descargar) try(descargar_shakemaps(), silent = TRUE)
  o <- leer_shakemaps()
  dsf <- sf::st_read("data/distritos_lima.geojson", quiet = TRUE)
  i <- sf::st_intersects(sf::st_as_sf(o, coords = c("lon", "lat"), crs = 4326), dsf)
  o$distrito <- vapply(i, function(k) if (length(k)) dsf$distrito[k[1]] else NA_character_, character(1))
  dm <- vapply(i, function(k) if (length(k)) dsf$dmmi[k[1]] else NA_real_, numeric(1))
  vs <- terra::extract(terra::rast("data/vs30_lima.tif"), as.matrix(o[, c("lon", "lat")]))[, 1]
  # Suelo: el del distrito (Vs30 + corrección CISMID, igual que la app); fuera de los distritos, el Vs30 del punto
  o$dmmi <- round(ifelse(!is.na(dm), dm, ifelse(is.na(vs), 0, delta_mmi(vs))), 2)
  readr::write_csv(o, salida)
  message(sprintf("Observaciones reales: %d de %d sismos -> %s", nrow(o), length(unique(o$evento_id)), salida))
  invisible(o)
}
