# =============================================================================
# preparar_datos.R  ·  Prototipo P1 (AI for Good Sandbox Hackathon 2026)
# Descarga (si falta) y procesa datos REALES para la app Shiny.
#   - USGS FDSN: catálogo de sismos M>=4.5 en Perú (1900-hoy)
#   - Límites distritales oficiales del Perú (IDDIST = ubigeo INEI)
#   - Población total por distrito del CENSO NACIONAL 2025 (INEI, publicado en 2026)
#   - Densidad de población 2020 (INEI) y superficie por distrito (ubigeo-peru-aumentado)
#   - OpenStreetMap: torres de telecomunicaciones, hospitales, bomberos, línea de costa
# Los sitios móviles SINTÉTICOS se generan aquí: no existe un registro público
# georreferenciado de estaciones base en Perú. Se distribuyen según la población real.
# Ejecutar desde la carpeta P1_PRO:  Rscript preparar_datos.R
# =============================================================================
suppressPackageStartupMessages({
  library(sf); library(dplyr); library(jsonlite); library(readr)
})
sf_use_s2(FALSE)
set.seed(2026)
dir.create("data", showWarnings = FALSE)
dir.create("data_raw", showWarnings = FALSE)

descargar <- function(url, destino) {
  if (!file.exists(destino)) {
    message("Descargando ", basename(destino))
    utils::download.file(url, destino, mode = "wb", quiet = TRUE)
  }
}
descargar("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=1900-01-01&minlatitude=-20&maxlatitude=-2&minlongitude=-83&maxlongitude=-68&minmagnitude=4.5&orderby=time",
          "data_raw/usgs_peru.csv")
descargar("https://raw.githubusercontent.com/jmcastagnetto/ubigeo-peru-aumentado/main/ubigeo_distrito.csv",
          "data_raw/ubigeo_distrito.csv")
descargar("https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_distrital_simple.geojson",
          "data_raw/peru_distrital.geojson")

# ---------------------------------------------------------------- distritos
ubi <- read_csv("data_raw/ubigeo_distrito.csv", col_types = cols(.default = "c")) |>
  transmute(IDDIST = inei,
            superficie = as.numeric(superficie),
            densidad   = as.numeric(pob_densidad_2020),
            poblacion  = round(superficie * densidad),
            idh_2019   = as.numeric(idh_2019),
            pobreza    = as.numeric(pct_pobreza_total))

peru <- st_read("data_raw/peru_distrital.geojson", quiet = TRUE)
vacios_geom <- peru$IDDIST[st_is_empty(peru)]            # geometrías vacías en la fuente
peru_ok <- peru[!st_is_empty(peru), ] |> st_zm() |> st_make_valid()

lima <- peru |>
  filter(IDPROV %in% c("1501", "0701")) |>
  st_zm(drop = TRUE, what = "ZM") |>          # Lima Metropolitana + Callao
  left_join(ubi, by = "IDDIST") |>
  rename(poblacion_2020 = poblacion)

# Población oficial del CENSO NACIONAL 2025 (INEI, resultados publicados en 2026):
# población total = censada + omitida estimada por la Encuesta Postcensal.
# Fuente: API pública de censos2025.inei.gob.pe (resultados/mapa-indicador, indicador 1, ámbito distrital).
censo <- read_csv("data_raw/censo2025_poblacion_total_distrital.csv", col_types = cols(.default = "c")) |>
  transmute(IDDIST = ubigeo, poblacion_2025 = as.numeric(poblacion_total_2025))
lima <- lima |>
  left_join(censo, by = "IDDIST") |>
  mutate(poblacion = coalesce(poblacion_2025, poblacion_2020, 0),
         fuente_poblacion = ifelse(is.na(poblacion_2025), "Proyección INEI 2020", "Censo 2025 (INEI)"))

# Dos distritos (La Punta y Santa Anita) vienen con geometría vacía en la fuente.
# Se aproximan con un círculo de área equivalente (radio máx. 2 km) centrado en su
# capital (coordenadas del ubigeo) y se marcan como geometría aproximada.
cap <- read_csv("data_raw/ubigeo_distrito.csv", col_types = cols(.default = "c")) |>
  transmute(IDDIST = inei, lat_c = as.numeric(latitude), lon_c = as.numeric(longitude))
lima$geom_aprox <- lima$IDDIST %in% vacios_geom
for (i in which(lima$geom_aprox)) {
  cc <- cap[which(cap$IDDIST == lima$IDDIST[i])[1], ]
  r <- min(2000, sqrt(lima$superficie[i] * 1e6 / pi))
  circ <- st_transform(st_buffer(st_transform(st_sfc(st_point(c(cc$lon_c, cc$lat_c)), crs = 4326), 32718), r), 4326)
  st_geometry(lima)[i] <- st_cast(circ, "MULTIPOLYGON")
}
lima <- st_make_valid(lima)

# Contorno del Perú (para saber si un epicentro está en el mar)
contorno <- peru_ok |> st_union() |> st_simplify(dTolerance = 0.01) |> st_as_sf()
st_write(contorno, "data/peru_contorno.geojson", delete_dsn = TRUE, quiet = TRUE)

# ---------------------------------------------------------------- costa (OSM)
costa_json <- "data_raw/osm_costa.json"
if (file.exists(costa_json)) {
  cj <- fromJSON(costa_json, simplifyVector = FALSE)$elements
  lineas <- lapply(cj, function(w) {
    m <- do.call(rbind, lapply(w$geometry, function(p) c(p$lon, p$lat)))
    st_linestring(m)
  })
  costa <- st_sf(geometry = st_sfc(lineas, crs = 4326))
  # Distrito costero = su polígono toca una franja de 400 m de la línea de costa
  # (antes se usaba una distancia de 1,5 km, que marcaba por error a San Martín de Porres).
  franja <- st_buffer(st_transform(st_union(costa), 32718), 400)
  lima$costero <- lengths(st_intersects(st_transform(lima, 32718), franja)) > 0
  st_write(costa, "data/costa.geojson", delete_dsn = TRUE, quiet = TRUE)
} else {
  lima$costero <- FALSE
}

# ---------------------------------------------------------------- suelo (Vs30)
# Vs30 por pendiente topográfica (Wald & Allen, 2007; método del mapa global del USGS)
# con DEM real, corregido con la clasificación del CISMID cuando la hay [S]:
# el método de pendiente subestima las arenas eólicas y los depósitos de ladera,
# y sobreestima la amplificación en el conglomerado ("grava de Lima").
source("R/vs30_pendiente.R", encoding = "UTF-8")
bb <- c(xmin = -77.25, xmax = -76.70, ymin = -12.55, ymax = -11.70)
vs30 <- if (file.exists("data/vs30_lima.tif")) terra::rast("data/vs30_lima.tif") else construir_vs30(bb)
lima$vs30_pendiente <- round(terra::extract(vs30, terra::vect(lima), fun = median, na.rm = TRUE)[, 2])
CISMID_DESFAVORABLE <- c("LA PUNTA", "CALLAO", "VENTANILLA", "VILLA EL SALVADOR", "CHORRILLOS",
                         "LURIN", "CARABAYLLO", "SAN JUAN DE LURIGANCHO", "COMAS", "PUENTE PIEDRA")
CISMID_FAVORABLE <- c("BREÑA", "JESUS MARIA", "MIRAFLORES", "SAN ISIDRO", "MAGDALENA DEL MAR",
                      "MAGDALENA VIEJA", "LIMA", "LINCE", "SAN BORJA", "SURQUILLO", "SAN MIGUEL")
lima$suelo <- ifelse(lima$NOMBDIST %in% CISMID_DESFAVORABLE, "desfavorable (CISMID)",
               ifelse(lima$NOMBDIST %in% CISMID_FAVORABLE, "favorable (CISMID)", "pendiente (Vs30)"))
lima$vs30 <- ifelse(lima$NOMBDIST %in% CISMID_DESFAVORABLE, pmin(coalesce(lima$vs30_pendiente, 250), 250),
              ifelse(lima$NOMBDIST %in% CISMID_FAVORABLE, pmax(coalesce(lima$vs30_pendiente, 600), 600),
                     coalesce(lima$vs30_pendiente, 450)))
lima$dmmi <- round(delta_mmi(lima$vs30), 2)

lima <- lima |>
  mutate(centro = st_centroid(st_geometry(lima))) |>
  mutate(lon = st_coordinates(centro)[, 1], lat = st_coordinates(centro)[, 2]) |>
  select(IDDIST, distrito = NOMBDIST, provincia = NOMBPROV, poblacion, poblacion_2020, fuente_poblacion, superficie,
         densidad, idh_2019, pobreza, costero, geom_aprox, vs30, dmmi, suelo, lon, lat) |>
  st_simplify(dTolerance = 0.0005)
st_write(lima, "data/distritos_lima.geojson", delete_dsn = TRUE, quiet = TRUE)

# ---------------------------------------------------------------- sismos USGS
usgs <- read_csv("data_raw/usgs_peru.csv", show_col_types = FALSE) |>
  filter(type == "earthquake") |>
  transmute(fecha = as.character(time), lat = latitude, lon = longitude,
            prof = depth, mag, tipo_mag = magType, lugar = place, id)
write_csv(usgs, "data/sismos_usgs.csv")

# ---------------------------------------------------------------- OSM puntos
leer_osm <- function(f) {
  if (!file.exists(f)) return(NULL)
  x <- fromJSON(f, simplifyVector = FALSE)$elements
  if (length(x) == 0) return(NULL)
  bind_rows(lapply(x, function(e) {
    lat <- if (!is.null(e$lat)) e$lat else e$center$lat
    lon <- if (!is.null(e$lon)) e$lon else e$center$lon
    tibble(osm_id = e$id, lat = lat, lon = lon,
           nombre = e$tags$name %||% NA_character_,
           man_made = e$tags$man_made %||% NA_character_,
           tower_type = e$tags$`tower:type` %||% NA_character_,
           movil = e$tags$`communication:mobile_phone` %||% NA_character_,
           operador = e$tags$operator %||% NA_character_,
           amenity = e$tags$amenity %||% NA_character_)
  }))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

torres <- leer_osm("data_raw/osm_towers.json")
if (!is.null(torres)) {
  torres <- torres |>
    filter(grepl("communication|telecom", tower_type, ignore.case = TRUE) |
             movil == "yes" |
             grepl("claro|movistar|telef|entel|bitel", operador, ignore.case = TRUE))
  write_csv(torres, "data/torres_osm.csv")
}
serv <- leer_osm("data_raw/osm_servicios.json")
if (!is.null(serv)) write_csv(serv, "data/servicios_osm.csv")

# ---------------------------------------------------------------- sitios móviles REALES (OpenCelliD)
# OpenCelliD (CC BY-SA 4.0): celdas GSM/UMTS/LTE del Perú (MCC 716). Las posiciones
# son estimadas por crowdsourcing. Las celdas se agrupan en "sitios" por operador
# (MNC) en una grilla de unos 100 m. Los operadores se ANONIMIZAN (A-D, de mayor a
# menor número de sitios) para no atribuir fallas simuladas a empresas reales.
# El token se lee de .Renviron (OPENCELLID_TOKEN), que no se publica.
ocid_f <- "data_raw/opencellid_716.csv.gz"
if (!file.exists(ocid_f) && nzchar(Sys.getenv("OPENCELLID_TOKEN"))) {
  utils::download.file(paste0("https://opencellid.org/ocid/downloads?token=", Sys.getenv("OPENCELLID_TOKEN"),
                              "&type=mcc&file=716.csv.gz"), ocid_f, mode = "wb", quiet = TRUE)
}
ocid <- read_csv(ocid_f, col_names = c("radio", "mcc", "mnc", "area", "cell", "unit", "lon", "lat",
                                       "range", "samples", "changeable", "created", "updated", "avg_signal"),
                 show_col_types = FALSE) |>
  filter(between(lat, -12.55, -11.70), between(lon, -77.25, -76.70), mnc %in% c(6, 10, 15, 17))
sitios <- ocid |>
  mutate(gx = round(lon, 3), gy = round(lat, 3)) |>
  group_by(mnc, gx, gy) |>
  summarise(lon = mean(lon), lat = mean(lat), celdas = n(),
            tecnologias = paste(sort(unique(radio)), collapse = "/"),
            actualizado = format(as.POSIXct(max(updated), origin = "1970-01-01"), "%Y-%m"),
            .groups = "drop")
rango <- sitios |> count(mnc, sort = TRUE) |> mutate(operador = LETTERS[seq_len(n())])
sitios <- sitios |> left_join(rango[, c("mnc", "operador")], by = "mnc")
pts <- st_as_sf(sitios, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
it <- st_intersects(pts, lima, sparse = TRUE)
sitios$IDDIST <- lima$IDDIST[vapply(it, function(i) if (length(i)) i[1] else NA_integer_, 1L)]
sitios <- filter(sitios, !is.na(IDDIST))
sitios$vs30 <- round(terra::extract(vs30, as.matrix(sitios[, c("lon", "lat")]))[, 1])
# Corrección CISMID a nivel de distrito (igual que arriba)
dist_suelo <- st_drop_geometry(lima)[, c("IDDIST", "suelo", "vs30")]
names(dist_suelo)[3] <- "vs30_dist"
sitios <- sitios |> left_join(dist_suelo, by = "IDDIST") |>
  mutate(vs30 = ifelse(suelo == "pendiente (Vs30)", coalesce(vs30, vs30_dist), vs30_dist),
         dmmi = round(delta_mmi(vs30), 2))
n <- nrow(sitios)
sitios <- sitios |> transmute(
  sitio = sprintf("C%05d", seq_len(n)), lon = round(lon, 5), lat = round(lat, 5), IDDIST, operador,
  celdas, tecnologias, actualizado, dmmi,
  # Atributos de resiliencia MODELADOS (no públicos): autonomía de baterías y redundancia
  bateria_h = round(runif(n, 2, 8), 1),
  fibra_redundante = runif(n) < 0.55,
  u1 = round(runif(n), 4), u2 = round(runif(n), 4), u3 = round(runif(n), 4),
  fuente = "OpenCelliD")
write_csv(sitios, "data/sitios.csv")

message(sprintf("Listo: %d distritos, %d sismos, %d sitios OpenCelliD (%d celdas), %d servicios",
                nrow(lima), nrow(usgs), nrow(sitios), sum(sitios$celdas),
                if (is.null(serv)) 0 else nrow(serv)))

# ---------------------------------------------------------------- intensidades reales (USGS ShakeMap)
# 167 intensidades observadas en Lima y Callao para calibrar y validar el modelo (R/validacion.R)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/obs_usgs.R", encoding = "UTF-8")
construir_obs_usgs(descargar = !dir.exists("data_raw/usgs_shakemap"))
# Relieve y malla de intensidad del mapa 3D (se regeneran si se borran)
source("R/malla.R", encoding = "UTF-8")
invisible(dem_lima()); invisible(malla_base(sf::st_read("data/distritos_lima.geojson", quiet = TRUE)))
