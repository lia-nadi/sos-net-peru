### Datos reales · Real data

| Capa | Fuente | Detalle |
|---|---|---|
| **Sitios móviles** | **OpenCelliD** (CC BY-SA 4.0), MCC 716 | 11 897 celdas GSM, UMTS y LTE reales (actualizadas en 2025-2026), agrupadas en **9 378 sitios** de Lima y Callao. Los operadores están anonimizados (A-D, del mayor al menor) |
| Sismos históricos | **USGS FDSN Event API** | 6 370 eventos de M ≥ 4.5 entre 1900 y hoy |
| Monitoreo en vivo | **USGS FDSN Event API**, consulta cada 5 min | Si no hay conexión, usa el catálogo local |
| Distritos | Límites oficiales del INEI (IDDIST = ubigeo) | 49 distritos. La Punta y Santa Anita tienen geometría aproximada porque la fuente las trae vacías |
| **Población (Censo 2025)** | **Censos Nacionales 2025, INEI** (resultados publicados en 2026; API pública de censos2025.inei.gob.pe, indicador «población total») | 11,18 millones en Lima y Callao (10,14 M en Lima Metropolitana y 1,09 M en Callao). La población total es la censada más la omitida estimada por la Encuesta Postcensal. Se conserva la proyección 2020 como comparación |
| Suelo (Vs30) | Pendiente de un modelo de elevación real (AWS Terrain Tiles), con el método **Wald & Allen (2007)** que usa el USGS en su mapa global, corregido con el **CISMID** a nivel de distrito | La amplificación sigue a Borcherdt (1994) |
| Hospitales y bomberos | **OpenStreetMap** | 135 hospitales y 57 estaciones de bomberos. El registro IPRESS del MINSA (2017) se revisó, pero solo trae coordenadas en el 9 % de los establecimientos de Lima |
| **Intensidades observadas** | **USGS ShakeMap** (lista de estaciones de 28 sismos M ≥ 5 a menos de 400 km de Lima) | 158 observaciones dentro de Lima y Callao: 95 «¿Lo sentiste?» (DYFI), 39 intensidades históricas y 24 estaciones sísmicas. Se suman 9 intensidades del IGP por distrito |
| Relieve 3D | **AWS Terrain Tiles** (terrarium, z11) | Altura de antenas y hospitales; sombreado del relieve |
| Imagen satelital | **Esri World Imagery** | Mapa base opcional |
| Edificios 3D | **OpenStreetMap** vía **OpenFreeMap** (esquema OpenMapTiles) | Alturas `render_height` |
| Costa | **OpenStreetMap** (`natural=coastline`) | Un distrito es costero si toca una franja de 400 m de la línea de costa |

### Modelos (ilustrativos) · Models (illustrative)
- **Fuente sísmica:** para M ≥ 7, una ruptura lineal en la fosa (azimut 325°) de longitud L = 10^(-2,44 + 0,59·M) km, según Wells & Coppersmith (1994).
- **Intensidad:** MMI = 6,52 + 0,76·M − 3,54·log10(R) + ΔMMI del suelo.
  - Se calibró por mínimos cuadrados ponderados (cada sismo pesa lo mismo) con **167 intensidades reales de 28 sismos** (USGS ShakeMap e IGP).
  - Antes se usaban solo 9 datos. Esa calibración exageraba la intensidad de los grandes terremotos (sesgo de +0,59 grados).
- **Validación fuera de muestra** (pestaña *Validación*, `R/validacion.R`):
  - **80/20 por sismo, repetido 500 veces**: el error de prueba tiene una mediana de 0,81 MMI.
  - **Dejar fuera cada sismo (LOEO)**: el error es de 0,83 MMI y el 82 % de las predicciones cae a ±1 grado.
  - **Veredicto: validado** (cumple 4 de 4 criterios fijados antes de medir).
  - Además pasa **15 de 15 pruebas de coherencia**.
- **Mapa:** la intensidad se calcula en una malla de ~450 m (`R/malla.R`). El suelo de cada celda es el Vs30 por pendiente; donde el CISMID clasifica el distrito, se usa su clasificación.
- **Supuesto del anillo del Operador A** (P1): fibra aérea sin respaldo por los acantilados de la Costa Verde, donde hubo derrumbes con intensidades VI-VII en 1974 y 2007. Se corta si la MMI media del anillo llega a 6,5.
- **Comprobación con un evento real:** el sismo del 15/06/2025 (M5.6 según el USGS) da 0 % de sitios caídos; OSIPTEL reportó que no hubo interrupción.
- **Fallas de sitios:** curvas de fragilidad para estructura, energía y fibra, más el agotamiento de las baterías.
- **Supuesto del escenario P1:** el Operador A tiene un anillo de fibra sin respaldo en Lima Sur. Se puede desactivar con su casilla.
- **Alerta temprana:** Vp = 6,5 km/s y Vs = 3,5 km/s, con detección en la estación costera más cercana más 4 s de procesamiento.
- **Tsunami:** velocidad de aguas someras √(g·400 m).
- **Congestión:** el pico de demanda escala con la intensidad sentida.
- **Umbral del nivel de alerta en vivo:** es una **propuesta** de este prototipo; la norma vigente no fija umbrales.

### Sintético (declarado) · Synthetic (declared)
- La autonomía de las baterías y la redundancia de fibra de cada sitio.
- Los hechos del escenario: porcentajes, horarios y número de llamadas.

### Normativa · Legal sources
La base de conocimiento de 45 artículos verificados está en `../knowledge_base/`. Los textos oficiales están en `../../investigacion_P1/fuentes/`.
