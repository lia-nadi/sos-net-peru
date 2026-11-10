# SOS Net Perú · Emergencias Telecom (prototipo en R Shiny)

Prototipo de la **Propuesta 1** (AI for Good Sandbox Hackathon 2026). Simula un sismo en Lima y Callao sobre datos reales y muestra:
- el impacto en la red móvil;
- las alertas integradas con la normativa peruana;
- los vacíos legales (G1-G10);
- el monitoreo sísmico en vivo.

## Cómo ejecutarlo (con un clic)
- Primera vez: abrir `P1_PROYECT.Rproj` y ejecutar `source("instalar_paquetes.R")`.
- Luego: `source("iniciar_app.R")`. Prepara los datos si faltan y abre la app en el navegador.

## Cómo ejecutarlo (manual)
1. Abrir `P1_PROYECT.Rproj` en RStudio.
2. Solo la primera vez, o para actualizar los datos: `source("P1_PRO/preparar_datos.R")`, con el directorio de trabajo en `P1_PRO`.
3. Abrir `P1_PRO/app.R` y pulsar **Run App**.

**Paquetes:** shiny, bslib, sf, terra, png, dplyr, readr, plotly, DT, jsonlite y markdown.

**Para descargar OpenCelliD:** se necesita `OPENCELLID_TOKEN` en `P1_PRO/.Renviron`. Ese archivo no se publica.

El navegador necesita internet para:
- los mapas (MapLibre, deck.gl, CARTO y el relieve de AWS);
- el monitoreo en vivo del USGS. Si no hay conexión, usa el catálogo local.

## Pestañas
| Pestaña | Qué hace |
|---|---|
| **Simulador 3D** | Elige un escenario (real del USGS o sintético) o haz clic en el mapa para mover el epicentro. "Simular sismo" anima las ondas P y S con el reloj de la alerta. La línea de tiempo (0-72 h, con botón de reproducción) muestra la caída y el restablecimiento de 9 378 sitios móviles reales (OpenCelliD). Incluye indicadores, una bitácora normativa con norma y vacío por evento, curvas de red con y sin roaming de emergencia y una tabla por distrito |
| **Políticas (qué pasaría si…)** | Las casillas de la barra lateral simulan cerrar los vacíos **G5** (alerta automática IGP→SISMATE), **G3** (activación por umbral) y **G1** (roaming de emergencia), además de priorizar estaciones portátiles |
| **Monitoreo en vivo** | Sismos reales del USGS (24 h, 7 días o 30 días) con actualización cada 5 min. Muestra el catálogo histórico en 3D (altura según magnitud, color según profundidad) y un nivel de alerta (verde, amarillo o rojo) que remite al protocolo del DS 051-2010-MTC |
| **Ejercicio multiagente** | Bitácora de los 4 agentes (Claude o replay) por etapa, con citas verificables, vacíos, trazas de razonamiento y la base de conocimiento |
| **Validación** | Valida el modelo de intensidad fuera de muestra con 167 intensidades reales de 28 sismos, con dos esquemas: 80/20 por sismo repetido 500 veces y dejar fuera cada sismo. Corre 15 pruebas de coherencia y emite un veredicto con criterios fijados antes de medir. La validación se puede descargar en CSV |
| **Descargas de 72 h** (en la Bitácora normativa) | Informe HTML imprimible como PDF, alertas en CSV y "qué está pasando" hora a hora en CSV |
| **Normativa y vacíos** | Tabla G1-G10 con la norma peruana, el referente internacional y la prioridad |
| **Datos y fuentes** | Fuentes reales, supuestos sintéticos y modelos usados |

## Estructura
```
P1_PRO/
├─ app.R               interfaz y servidor
├─ preparar_datos.R    descarga y procesa los datos reales (USGS, INEI, OSM, OpenCelliD, Vs30)
├─ R/simulacion.R      intensidad, fallas, baterías, congestión, alerta temprana y tsunami
├─ R/normativa.R       alertas vinculadas a la norma y tabla de vacíos
├─ www/mapa3d.js       mapas 3D (MapLibre + deck.gl) y su comunicación con Shiny
├─ www/estilos.css
├─ data_raw/           descargas originales (USGS, OSM, límites, INEI)
├─ data/               datos procesados que usa la app
└─ FUENTES.md
```

## Calibración y validación (con datos reales)
- **Intensidad:** MMI = 6,52 + 0,76·M − 3,54·log10(R) + suelo.
  - Se calibró con **167 intensidades reales observadas en Lima y Callao, de 28 sismos (1940-2026)**: el ShakeMap del USGS (reportes «¿Lo sentiste?», intensidades históricas y estaciones sísmicas) más 9 intensidades del IGP.
  - Ver `R/obs_usgs.R` y `R/validacion.R`.
- **Validación fuera de muestra:**
  - Error de 0,81 grados MMI en el 80/20 por sismo (mediana de 500 particiones).
  - Error de 0,83 al predecir un sismo no visto, con el 82 % de las predicciones a ±1 grado.
  - Cumple los 4 criterios; antes cumplía 3 de 4.
  - La calibración anterior (9 datos) exageraba la intensidad de los grandes terremotos (sesgo de +0,59).
- **Real, 15/06/2025, Callao (M5.6):** el modelo da 0 % de sitios caídos. OSIPTEL reportó que no hubo interrupción.
- **P1 (M8.0, sintético):** pico del 17 % de sitios caídos. El supuesto del P1 (anillo de fibra del Operador A en Lima Sur) deja aislado el 75 % de esos sitios. El roaming de emergencia sube la cobertura del Operador A de 81 % a 88 % a las 24 h.
- **Escenario INDECI M8.8:** pico del 41 % de sitios caídos.

## Mapa 3D
- **Relieve real:** AWS Terrain Tiles con exageración de 1,6. Las antenas y los hospitales se ubican a su altura real.
- **Intensidad continua tipo ShakeMap:** celdas de ~450 m pegadas al relieve, con la escala de colores del USGS. Al pasar el cursor se ve la intensidad del punto exacto.
- **Mapa base:** oscuro o satelital (Esri World Imagery).
- **Edificios 3D reales:** de OpenStreetMap vía OpenFreeMap, visibles al acercar el zoom.

## Limitaciones
- Es un modelo **ilustrativo**, no de ingeniería sísmica.
- Los sitios son **reales** (OpenCelliD, CC BY-SA 4.0), pero sus posiciones vienen de *crowdsourcing* y los operadores están anonimizados (A-D). La autonomía de baterías y la redundancia de fibra son modeladas.
- La Punta y Santa Anita usan una geometría aproximada (círculo) porque la fuente las trae vacías.
