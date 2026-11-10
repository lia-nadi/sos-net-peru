# =============================================================================
# SOS Net Perú · Arranque con un clic
# Abre P1_PROYECT.Rproj en RStudio y ejecuta:  source("iniciar_app.R")
#  1. Verifica paquetes
#  2. Prepara los datos si faltan (Censo 2025, OpenCelliD, USGS, OSM, suelos)
#  3. Abre la aplicación en el navegador
# =============================================================================
local({
  `%||%` <- function(a, b) if (is.null(a)) b else a
  raiz <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    rstudioapi::getActiveProject() %||% getwd() else getwd()
  if (basename(raiz) != "P1_PROYECT" && dir.exists(file.path(raiz, "P1_PROYECT"))) raiz <- file.path(raiz, "P1_PROYECT")
  if (!dir.exists(file.path(raiz, "P1_PRO"))) raiz <- getwd()
  app <- file.path(raiz, "P1_PRO")
  stopifnot("No se encontró la carpeta P1_PRO" = dir.exists(app))

  necesarios <- c("shiny", "bslib", "sf", "terra", "png", "dplyr", "readr", "plotly", "DT", "jsonlite", "markdown")
  faltan <- setdiff(necesarios, rownames(installed.packages()))
  if (length(faltan)) stop("Faltan paquetes: ", paste(faltan, collapse = ", "),
                           ". Ejecuta primero source(\"instalar_paquetes.R\").")

  if (!file.exists(file.path(app, "data", "sitios.csv"))) {
    message("Preparando datos (solo la primera vez, unos minutos)...")
    old <- setwd(app); on.exit(setwd(old), add = TRUE)
    source("preparar_datos.R", encoding = "UTF-8", local = new.env())
  }
  message("Abriendo SOS Net Perú en el navegador...")
  shiny::runApp(app, launch.browser = TRUE)
})
