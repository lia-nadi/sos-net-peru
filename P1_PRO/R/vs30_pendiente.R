# =============================================================================
# Vs30 a partir de la pendiente topográfica (Wald & Allen, 2007, BSSA 97(5)).
# Es el método que usa el USGS para su mapa global de Vs30 (regiones tectónicas
# activas). DEM: AWS Terrain Tiles (terrarium), zoom 11 (~75 m), remuestreado
# a ~900 m (30 arc-seg), como en el método original.
# =============================================================================
suppressPackageStartupMessages({ library(terra); library(png) })

tile_xy <- function(lon, lat, z) {
  n <- 2^z
  x <- floor((lon + 180) / 360 * n)
  y <- floor((1 - log(tan(lat * pi / 180) + 1 / cos(lat * pi / 180)) / pi) / 2 * n)
  c(x, y)
}
tile_bounds <- function(x, y, z) {
  n <- 2^z
  lon1 <- x / n * 360 - 180; lon2 <- (x + 1) / n * 360 - 180
  lat1 <- atan(sinh(pi * (1 - 2 * y / n))) * 180 / pi
  lat2 <- atan(sinh(pi * (1 - 2 * (y + 1) / n))) * 180 / pi
  c(xmin = lon1, xmax = lon2, ymin = lat2, ymax = lat1)
}

dem_terrarium <- function(bbox, z = 11, cache = "data_raw/dem_tiles") {
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  t1 <- tile_xy(bbox["xmin"], bbox["ymax"], z); t2 <- tile_xy(bbox["xmax"], bbox["ymin"], z)
  rs <- list()
  for (x in t1[1]:t2[1]) for (y in t1[2]:t2[2]) {
    f <- file.path(cache, sprintf("%d_%d_%d.png", z, x, y))
    if (!file.exists(f))
      utils::download.file(sprintf("https://s3.amazonaws.com/elevation-tiles-prod/terrarium/%d/%d/%d.png", z, x, y),
                           f, mode = "wb", quiet = TRUE)
    im <- png::readPNG(f) * 255
    elev <- (im[, , 1] * 256 + im[, , 2] + im[, , 3] / 256) - 32768
    b <- tile_bounds(x, y, z)
    r <- rast(elev, extent = ext(b["xmin"], b["xmax"], b["ymin"], b["ymax"]), crs = "EPSG:3857")
    # Los tiles son Web Mercator; las coordenadas de la extensión están en grados,
    # así que se asigna EPSG:4326 (con este tamaño de tile el error es despreciable).
    crs(r) <- "EPSG:4326"
    rs[[length(rs) + 1]] <- r
  }
  do.call(merge, rs)
}

# Tabla de Wald & Allen (2007) para regiones tectónicas activas: pendiente (m/m) -> Vs30 (m/s)
WA_SLOPE <- c(1.0e-4, 2.2e-3, 6.3e-3, 0.018, 0.050, 0.10, 0.138)
WA_VS30  <- c(180,    240,    300,    360,   490,   620,  760)

vs30_desde_pendiente <- function(s) {
  na <- is.na(s)
  s <- pmax(ifelse(na, 1, s), 1e-5)
  v <- approx(log10(WA_SLOPE), WA_VS30, xout = log10(s), rule = 2)$y
  bajo <- s < 1e-4
  v[bajo] <- 180 - (log10(1e-4) - log10(s[bajo])) * 20            # por debajo de la clase E
  v <- pmax(150, pmin(900, v))
  v[na] <- NA
  v
}

construir_vs30 <- function(bbox, salida = "data/vs30_lima.tif") {
  dem <- dem_terrarium(bbox)
  dem <- crop(dem, ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"]))
  dem[dem < -5] <- NA                                    # mar
  dem30 <- aggregate(dem, fact = 12, fun = mean, na.rm = TRUE)  # ~900 m
  pend <- terrain(dem30, v = "slope", unit = "radians")
  s <- tan(values(pend))
  v <- dem30; values(v) <- vs30_desde_pendiente(s)
  v[is.na(dem30)] <- NA
  names(v) <- "vs30"
  writeRaster(v, salida, overwrite = TRUE)
  v
}

# Amplificación: factor de sitio de periodo corto, (760/Vs30)^0.6 (Borcherdt, 1994),
# convertido a incremento de MMI con la pendiente PGA-MMI de Wald et al. (1999):
# dMMI = 3.66 * log10(F)
delta_mmi <- function(vs30) {
  vs30[is.na(vs30)] <- 760
  pmax(0, 3.66 * log10((760 / pmax(vs30, 150))^0.6))
}
