# Comparación: el mismo LLM con y sin el sistema (9 de noviembre de 2026)

| | Línea base (mismo LLM, sin base normativa) | Sistema multiagente |
|---|---|---|
| **Modelo** | `openai/gpt-oss-120b` (Groq) | `openai/gpt-oss-120b` (Groq) |
| **Archivos** | `baseline_20261109-115157.json` | `bitacora_20261109-113535-live-groq.jsonl` y `resumen_…json` |
| **Citas que existen en la normativa** | 0 de 4 respuestas sin normas inventadas | 108 de 108 citas válidas (1 de 20 decisiones fue corregida por el verificador al primer intento) |
| **Vacíos detectados** | 0 (las respuestas afirman que existe norma) | 7 de 9 esperados (G1, G2, G3, G4, G5, G6, G8). No detectó G7 ni G10 |

## Detalle de la línea base: normas inventadas o mal atribuidas
- **G1 (roaming):** responde "Sí, OSIPTEL puede ordenar roaming de emergencia". Cita una "Ley N.º 29330 – Ley General de Telecomunicaciones, art. 31" y un "DS 012-2013-EM, art. 16" con un umbral del 20 %. **Ambas son inexistentes o mal atribuidas:** la ley real es el TUO aprobado por DS 013-93-TCC, y el sufijo "EM" corresponde al sector Energía y Minas. La norma real (DS 002-2023-MTC) solo obliga en centros poblados de hasta 1 000 habitantes.
- **G5 (alerta):** responde "Sí, el IGP está autorizado a emitir alertas automáticas". Cita una "Ley N.º 30949, arts. 41 y 43" y un "DS 005-2018-MTC". **No corresponden** a la normativa real del SISMATE (Ley 30472 y Directiva INDECI 003-2024, que establece una cadena manual).
- **G6 (IA de red):** cita un "DS 009-2021-PCM, Reglamento de la Ley 29944, art. 67", que "exige supervisión humana directa y permanente". **Es inventado.** La norma real (DS 115-2025-PCM, art. 31.4) no incluye las telecomunicaciones.
- **G2 (ubicación):** llega a una conclusión prudente, pero sitúa el secreto de las comunicaciones en el art. 2, inc. 5 de la Constitución. **Es el inc. 10.** Tampoco menciona el DL 1182 ni la falta de una excepción por interés vital.

## Hallazgo del sistema: discrepancia institucional en G2
En la etapa 3, el agente de la autoridad de datos consideró lícita la entrega individual invocando el art. 14.1 de la Ley 29733 junto con el mandato de información del SINAGERD. El agente del operador la rechazó invocando el secreto de las comunicaciones (Constitución, art. 2.10). La clave del escenario esperaba "sin base habilitante". **Que dos instituciones lean la misma norma en sentidos opuestos es justamente la evidencia de G2:** la base legal no es clara. En la etapa 4, la propia autoridad terminó registrando G2.
