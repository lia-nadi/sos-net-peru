# Emergency Telecom Policy Sandbox (P1): AI for Good Sandbox Hackathon 2026

An agentic AI sandbox for **emergency telecommunications after a major earthquake**. It has three parts:

- **Multi-agent engine:** four institutional agents (mobile operator, telecom regulator, civil-protection authority and data-protection authority). They receive facts progressively and asymmetrically and reason over a **verifiable, article-level knowledge base of real law**. Every decision must cite an article that exists in the knowledge base, or the agent must declare a policy gap. Gaps are compared with global policy examples.
- **3D digital twin (R Shiny):** quantifies what each gap costs in people warned too late and population without service. It runs on real data from USGS, OpenCelliD, INEI and OpenStreetMap.
- **Audit log:** a JSONL record per decision with the facts known, the articles retrieved, the citations, a summarised reasoning trace, the model and a hash of the context.

The test country is Peru, an AI for Good Lab pilot country. The knowledge-base schema and the engine are country-agnostic.


> **Published contents:** this repository contains only the program (Shiny app, multi-agent engine, knowledge base and processed data). The official legal texts in PDF (`knowledge_base/Dataset/**/*.pdf`), API keys (`.env`, `.Renviron`) and large raw downloads are not included. The PDFs are cited by file name in `construir_kb.py`, and `P1_PRO/preparar_datos.R` downloads the raw data again.

## Repository layout
```
knowledge_base/      articulos.yaml (source) -> kb_articulos.jsonl + Dataset/<Country>/PolicyData/... (ITU reference format)
motor_agentes/       motor.py (Y.3172 pipeline: SRC, C, PP, M, P, D, SINK), escenario_p1.yaml, baseline.py, salidas/ (logs)
P1_PRO/              R Shiny app: 3D simulator, multi-agent exercise viewer, live USGS monitoring, policy gaps
```
The repository holds code, the knowledge base, data and logs. The technical report and the video script are in `../concurso_oficial/` and are submitted separately.

## Quick start
```bash
python -m venv .venv && .venv/Scripts/pip install -r motor_agentes/requirements.txt   # Windows path
.venv/Scripts/python knowledge_base/construir_kb.py          # build the KB
.venv/Scripts/python motor_agentes/motor.py --modo replay     # reference-key run, no LLM
# LLM keys go in P1_PROYECT/.env (gitignored): ANTHROPIC_API_KEY=... or DEEPSEEK_API_KEY=...
.venv/Scripts/python motor_agentes/motor.py --modo live                        # Claude if available, else DeepSeek
.venv/Scripts/python motor_agentes/motor.py --modo live --proveedor deepseek   # force DeepSeek (deepseek-v4-pro)
```

To run the Shiny app, open `P1_PROYECT.Rproj`, then `P1_PRO/app.R`, and click **Run App**.
- **R packages:** shiny, bslib, sf, terra, png, dplyr, readr, plotly, DT, jsonlite and markdown.
- **First run:** set the working directory to `P1_PRO` and run `Rscript preparar_datos.R`. It needs `OPENCELLID_TOKEN` in `P1_PRO/.Renviron`, a file that is not committed.

## How the engine maps to ITU-T Y.3172
| Node | Implementation |
|---|---|
| SRC | Exercise director releases chunked facts per stage (`escenario_p1.yaml`) |
| C | Visibility filter: each agent only sees its own facts and the messages it receives |
| PP | BM25 retrieval over article-level units with metadata (effect, actors, trigger, conditions) |
| M | Four LLM agents. Two providers: Claude (`claude-opus-5-5`: structured JSON output, summarised reasoning traces, prompt caching, refusal fallback) or DeepSeek (`deepseek-v4-pro`: JSON mode, `reasoning_content` traces) |
| P | Citation verifier (rejects IDs that are not in the KB, one retry), competence rule, gap detector (types A/I/C/K/D/E) |
| D | Inter-agent messages; a scripted second operator for roaming requests |
| SINK | Append-only audit log, run summary (valid-citation rate, gap recall) and Markdown report |

## Data and licences
- **Code:** MIT.
- **Knowledge base texts:** official public legal texts; the metadata is CC BY 4.0.
- **OpenCelliD-derived site data** (`P1_PRO/data/sitios.csv`): **CC BY-SA 4.0**, © OpenCelliD contributors. Operators are anonymised (A-D).
- **OpenStreetMap data:** ODbL. **USGS:** public domain. **INEI-derived population:** open data.
- **Synthetic elements:** battery autonomy, fibre redundancy and the scenario facts. These are declared in the app and in the report.
