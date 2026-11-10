# =============================================================================
# Malla continua de intensidad (estilo ShakeMap) y alturas del terreno
#  - Celdas de ~450 m sobre Lima Metropolitana y Callao (solo tierra, dentro de los distritos)
#  - Suelo por celda: Vs30 por pendiente (Wald & Allen) interpolado; donde el CISMID
#    clasifica el distrito, manda la clasificación del CISMID (igual que en los sitios)
#  - Altura: DEM AWS Terrain Tiles (terrarium z11), para poner antenas y hospitales
#    sobre el relieve 3D
# Se guarda en data/malla_lima.rds y data/dem_lima.tif (se generan una vez).
# =============================================================================

dem_lima <- function(bbox = c(xmin = -77.33, xmax = -76.65, ymin = -12.55, ymax = -11.58),
                     archivo = "data/dem_lima.tif") {
  if (file.exists(archivo)) return(terra::rast(archivo))
  if (!exists("dem_terrarium")) source("R/vs30_pendiente.R", encoding = "UTF-8")
  dem <- dem_terrarium(bbox)
  dem <- terra::crop(dem, terra::ext(bbox[["xmin"]], bbox[["xmax"]], bbox[["ymin"]], bbox[["ymax"]]))
  dem[dem < 0] <- 0
  terra::writeRaster(dem, archivo, overwrite = TRUE)
  dem
}

altura_puntos <- function(lon, lat, dem = dem_lima()) {
  z <- terra::extract(dem, cbind(lon, lat), method = "bilinear")[, 1]
  round(pmax(0, ifelse(is.na(z), 0, z)))
}

malla_base <- function(distritos_sf, res = 0.004, archivo = "data/malla_lima.rds") {
  if (file.exists(archivo)) return(readRDS(archivo))
  if (!exists("delta_mmi")) source("R/vs30_pendiente.R", encoding = "UTF-8")
  bb <- sf::st_bbox(distritos_sf)
  lon <- seq(bb[["xmin"]] + res / 2, bb[["xmax"]], by = res)
  lat <- seq(bb[["ymax"]] - res / 2, bb[["ymin"]], by = -res)          # de norte a sur (filas de imagen)
  g <- expand.grid(lon = lon, lat = lat)                                # lon varía más rápido: orden de fila
  plantilla <- terra::rast(ncols = length(lon), nrows = length(lat), crs = "EPSG:4326",
                           xmin = min(lon) - res / 2, xmax = max(lon) + res / 2,
                           ymin = min(lat) - res / 2, ymax = max(lat) + res / 2)
  v <- terra::vect(distritos_sf); v$fila <- seq_len(nrow(v))
  i_dist <- terra::values(terra::rasterize(v, plantilla, field = "fila", touches = FALSE))[, 1]
  tierra <- which(!is.na(i_dist))
  vs <- terra::extract(terra::rast("data/vs30_lima.tif"), as.matrix(g[tierra, ]), method = "bilinear")[, 1]
  d <- sf::st_drop_geometry(distritos_sf)[i_dist[tierra], ]
  vs <- ifelse(d$suelo == "pendiente (Vs30)", ifelse(is.na(vs), d$vs30, vs), d$vs30)
  m <- list(w = length(lon), h = length(lat), res = res,
            bbox = c(oeste = min(lon) - res / 2, este = max(lon) + res / 2,
                     sur = min(lat) - res / 2, norte = max(lat) + res / 2),
            idx = tierra, lon = g$lon[tierra], lat = g$lat[tierra],
            dmmi = round(delta_mmi(vs), 2), distrito = d$distrito)
  saveRDS(m, archivo)
  m
}

# Intensidad y segundos de aviso por celda para un escenario simulado
malla_escenario <- function(s, malla) {
  p <- s$p
  r <- dist_ruptura(p$M, p$lat, p$lon, p$prof, malla$lat, malla$lon)
  mmi <- mmi_de(p$M, r, malla$dmmi)
  rh <- sqrt(dist_km(p$lat, p$lon, malla$lat, malla$lon)^2 + p$prof^2)
  aviso <- rh / VS - s$t_alerta
  completo <- function(v, nd) { x <- rep(nd, malla$w * malla$h); x[malla$idx] <- v; x }
  list(w = malla$w, h = malla$h, bbox = unname(malla$bbox),
       mmi = completo(round(mmi, 1), -1), aviso = completo(round(aviso), -9999))
}
