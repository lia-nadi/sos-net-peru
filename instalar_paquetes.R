# =============================================================================
# SOS Net Perú · Instala los paquetes de R necesarios (solo la primera vez)
# Uso: abrir P1_PROYECT.Rproj en RStudio y ejecutar source("instalar_paquetes.R")
# =============================================================================
paquetes <- c("shiny", "bslib", "sf", "terra", "png", "dplyr", "readr", "plotly",
              "DT", "jsonlite", "markdown", "htmltools")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
} else {
  message("Todos los paquetes ya están instalados.")
}
if (utils::packageVersion("bslib") < "0.7.0")
  install.packages("bslib", repos = "https://cloud.r-project.org")
message("Listo. Ahora ejecuta source(\"iniciar_app.R\").")
