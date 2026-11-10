"""Motor multiagente P1: telecomunicaciones de emergencia ante un sismo.

Arquitectura (mapeada a ITU-T Y.3172):
  SRC   Director: libera los hechos del escenario por etapas
  C     Bus de hechos con filtro de visibilidad (cada agente ve solo lo suyo)
  PP    Recuperación de artículos de la base (BM25 sobre kb_articulos.jsonl)
  M     4 agentes LLM (Claude), uno por institución
  P     Verificador de citas + detector de vacíos + reglas de competencia
  D     Mensajes entre agentes
  SINK  Bitácora JSONL auditable + informe

Modos:
  live    llama a Claude (requiere ANTHROPIC_API_KEY o un perfil de `ant auth login`)
  replay  reproduce la clave de respuestas del escenario SIN LLM; sirve para probar el
          flujo y como respaldo de la demo, y queda marcado como "replay" en la bitácora

Uso:
  python motor_agentes/motor.py --modo live
  python motor_agentes/motor.py --modo replay
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import sys
import time
import unicodedata
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

import yaml

RAIZ = Path(__file__).resolve().parent
KB_PATH = RAIZ.parent / "knowledge_base" / "kb_articulos.jsonl"
ESC_PATH = RAIZ / "escenario_p1.yaml"
SALIDAS = RAIZ / "salidas"
MODELO = "claude-opus-5-5"

# ----------------------------------------------------------------------------- KB y recuperación (PP)

def normalizar(t: str) -> list[str]:
    t = unicodedata.normalize("NFKD", t.lower())
    t = "".join(c for c in t if not unicodedata.combining(c))
    return [w for w in re.findall(r"[a-z0-9]+", t) if len(w) > 2]


class BaseConocimiento:
    def __init__(self, path: Path = KB_PATH):
        self.arts = [json.loads(l) for l in path.read_text(encoding="utf-8").splitlines() if l.strip()]
        self.por_id = {a["id"]: a for a in self.arts}
        self.docs = [normalizar(" ".join([a["norma"], a["titulo"], a["texto"], " ".join(a.get("vacios", []))]))
                     for a in self.arts]
        self.df = Counter(w for d in self.docs for w in set(d))
        self.avgdl = sum(len(d) for d in self.docs) / len(self.docs)

    def buscar(self, consulta: str, categorias: list[str] | None = None, k: int = 8) -> list[dict]:
        q = normalizar(consulta)
        n = len(self.docs)
        puntajes = []
        for a, d in zip(self.arts, self.docs):
            if categorias and a["categoria"] not in categorias:
                continue
            tf = Counter(d)
            s = 0.0
            for w in q:
                if w in tf:
                    idf = math.log(1 + (n - self.df[w] + 0.5) / (self.df[w] + 0.5))
                    s += idf * tf[w] * 2.2 / (tf[w] + 1.2 * (0.25 + 0.75 * len(d) / self.avgdl))
            puntajes.append((s, a))
        return [a for s, a in sorted(puntajes, key=lambda x: -x[0])[:k] if s > 0]

    def subconjunto(self, categorias: list[str]) -> list[dict]:
        return [a for a in self.arts if a["categoria"] in categorias]


# ----------------------------------------------------------------------------- Agentes (M)

ESQUEMA_DECISION = {
    "type": "object",
    "properties": {
        "accion": {"type": "string", "description": "Acción concreta que tomas ahora (una frase)."},
        "decision": {"type": "string", "description": "Qué decides y por qué, en 2-4 frases, citando los IDs de artículo."},
        "fundamento": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "id": {"type": "string", "description": "ID exacto de un artículo de la base (p. ej. PE-DS051-23)."},
                    "uso": {"type": "string", "description": "Para qué lo citas (habilita, obliga, prohíbe o limita)."},
                },
                "required": ["id", "uso"],
                "additionalProperties": False,
            },
        },
        "competencia_invocada": {"type": "string", "description": "Institución cuya competencia ejerces (MTC, OSIPTEL, INDECI, ANPD, Operador)."},
        "mensajes": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {"para": {"type": "string"}, "contenido": {"type": "string"}},
                "required": ["para", "contenido"],
                "additionalProperties": False,
            },
        },
        "vacio": {
            "type": "object",
            "properties": {
                "hay": {"type": "boolean"},
                "tipo": {"type": "string", "enum": ["ninguno", "A_ausencia", "I_insuficiencia", "C_conflicto",
                                                   "K_competencia", "D_desfase", "E_ejecucion"]},
                "descripcion": {"type": "string"},
                "normas_relacionadas": {"type": "array", "items": {"type": "string"}},
            },
            "required": ["hay", "tipo", "descripcion", "normas_relacionadas"],
            "additionalProperties": False,
        },
        "confianza": {"type": "string", "enum": ["alta", "media", "baja"]},
    },
    "required": ["accion", "decision", "fundamento", "competencia_invocada", "mensajes", "vacio", "confianza"],
    "additionalProperties": False,
}

TIPOS_VACIO = """Tipos de vacío normativo:
- A_ausencia: no existe ninguna norma que habilite una acción necesaria.
- I_insuficiencia: existe una norma, pero una condición la excluye del caso (población, actor, tecnología).
- C_conflicto: dos normas aplicables llevan a decisiones opuestas.
- K_competencia: no está claro qué institución decide.
- D_desfase: la norma regula una tecnología superada.
- E_ejecucion: la norma existe, pero su procedimiento es más lento que la crisis."""


def system_prompt(agente_id: str, ficha: dict, kb_sub: list[dict]) -> str:
    """Parte estable (cacheable): rol + reglas + artículos permitidos. Sin datos variables."""
    arts = "\n\n".join(
        f"[{a['id']}] {a['norma']}, art. {a['art']}: {a['titulo']} (efecto: {a['efecto']}; "
        f"confiabilidad {a['confiabilidad']})\n{a['texto']}" for a in kb_sub)
    return f"""Eres un agente que representa a: {ficha['nombre']}.
Mandato: {ficha['mandato']}
Estilo institucional: {ficha['sesgo']}

Participas en un ejercicio de simulación de una emergencia (sismo) con otras instituciones. Recibirás hechos
por etapas; solo conoces lo que te llega a ti y los mensajes que otras instituciones te envíen.

Reglas:
1. Cada decisión debe apoyarse en artículos de la BASE NORMATIVA de abajo, citados por su ID exacto en "fundamento".
   No cites normas que no estén en la base. Si una acción necesaria no tiene norma que la habilite, dilo y regístralo como vacío.
2. Actúa solo dentro de tus competencias. Si algo corresponde a otra institución, envíale un mensaje
   (destinatarios posibles: {', '.join(ficha['puede_escribir_a'])}).
3. Si detectas un vacío normativo, complétalo en "vacio" con su tipo y los IDs de las normas relacionadas.
   Los referentes internacionales de la base (EE. UU., UE, Japón, Chile, UIT) sirven para comparar, no para fundamentar tus actos en el Perú.
4. Responde en español, de forma concisa y verificable.

{TIPOS_VACIO}

BASE NORMATIVA DISPONIBLE PARA TI:
{arts}"""


class AgenteLLM:
    def __init__(self, agente_id: str, ficha: dict, kb: BaseConocimiento):
        import anthropic  # importación diferida: el modo replay no necesita el SDK
        self.anthropic = anthropic
        self.client = anthropic.Anthropic()
        self.id = agente_id
        self.ficha = ficha
        self.kb = kb
        self.kb_sub = kb.subconjunto(ficha["categorias_kb"])
        self.system = system_prompt(agente_id, ficha, self.kb_sub)
        self.ids_permitidos = {a["id"] for a in self.kb_sub}

    def decidir(self, contexto_usuario: str) -> dict:
        mensajes = [{"role": "user", "content": contexto_usuario}]
        resp = self._llamar(mensajes)
        datos, traza = self._parsear(resp)
        # Verificador de citas (nodo P): una sola reintentona con retroalimentación
        malas = [f["id"] for f in datos.get("fundamento", []) if f["id"] not in self.ids_permitidos]
        reintento = None
        if malas:
            reintento = malas
            mensajes += [
                {"role": "assistant", "content": resp.content},
                {"role": "user", "content": f"Verificador: estos IDs no existen en tu base normativa: {malas}. "
                                              "Corrige tu respuesta citando solo IDs de la base, o declara el vacío."},
            ]
            resp = self._llamar(mensajes)
            datos, traza2 = self._parsear(resp)
            traza = traza + "\n[tras verificación] " + traza2
        return {
            "salida": datos, "traza_razonamiento": traza, "citas_rechazadas_1er_intento": reintento,
            "modelo": resp.model, "request_id": getattr(resp, "_request_id", None),
            "uso": {"input": resp.usage.input_tokens, "output": resp.usage.output_tokens,
                    "cache_read": getattr(resp.usage, "cache_read_input_tokens", 0) or 0,
                    "cache_write": getattr(resp.usage, "cache_creation_input_tokens", 0) or 0},
            "stop_reason": resp.stop_reason,
        }

    def _llamar(self, mensajes):
        intentos = 0
        while True:
            try:
                return self.client.beta.messages.create(
                    model=MODELO,
                    max_tokens=16000,
                    betas=["server-side-fallback-2026-07-01"],
                    fallbacks="default",               # reintento en otro modelo si hay rechazo de seguridad
                    thinking={"type": "adaptive", "display": "summarized"},  # traza auditable resumida
                    output_config={"effort": "medium",
                                   "format": {"type": "json_schema", "schema": ESQUEMA_DECISION}},
                    system=[{"type": "text", "text": self.system, "cache_control": {"type": "ephemeral"}}],
                    messages=mensajes,
                )
            except self.anthropic.RateLimitError as e:
                intentos += 1
                if intentos > 4:
                    raise
                time.sleep(int(e.response.headers.get("retry-after", "20")))
            except self.anthropic.APIConnectionError:
                intentos += 1
                if intentos > 4:
                    raise
                time.sleep(5 * intentos)

    @staticmethod
    def _parsear(resp) -> tuple[dict, str]:
        if resp.stop_reason == "refusal":
            return ({"accion": "sin respuesta (rechazo del modelo)", "decision": "", "fundamento": [],
                     "competencia_invocada": "", "mensajes": [],
                     "vacio": {"hay": False, "tipo": "ninguno", "descripcion": "", "normas_relacionadas": []},
                     "confianza": "baja"}, "")
        traza = " ".join(getattr(b, "thinking", "") or "" for b in resp.content if b.type == "thinking").strip()
        texto = next((b.text for b in resp.content if b.type == "text"), "{}")
        return json.loads(texto), traza


class AgenteReplay:
    """Reproduce la clave de respuestas del escenario, sin LLM (marcado como replay)."""

    def __init__(self, agente_id: str, ficha: dict, kb: BaseConocimiento, escenario: dict):
        self.id = agente_id
        self.kb = kb
        self.esc = escenario
        self.etapa = 0

    def decidir(self, contexto_usuario: str) -> dict:
        etapa = next(e for e in self.esc["etapas"] if e["id"] == self.etapa)
        ds = [d for d in etapa["clave"]["decisiones"] if d["agente"] == self.id]
        vac = [v for v in etapa["clave"]["vacios"]
               if any(v in self.kb.por_id[n].get("vacios", []) for d in ds for n in d["normas"])]
        if not ds:
            salida = {"accion": "monitorear y coordinar", "decision": "Sin acción propia en esta etapa según la clave.",
                      "fundamento": [], "competencia_invocada": "", "mensajes": [],
                      "vacio": {"hay": False, "tipo": "ninguno", "descripcion": "", "normas_relacionadas": []},
                      "confianza": "media"}
        else:
            d = ds[0]
            salida = {"accion": d["accion"],
                      "decision": f"{d['accion']} (según la clave del escenario).",
                      "fundamento": [{"id": n, "uso": "clave"} for n in d["normas"]],
                      "competencia_invocada": {"regulador": "MTC", "defensa_civil": "INDECI",
                                               "autoridad_datos": "ANPD"}.get(self.id, "Operador"),
                      "mensajes": [],
                      "vacio": {"hay": bool(vac), "tipo": "A_ausencia" if vac else "ninguno",
                                "descripcion": "; ".join(self.esc["vacios"][v] for v in vac),
                                "normas_relacionadas": d["normas"]},
                      "confianza": "alta"}
        return {"salida": salida, "traza_razonamiento": "(replay: sin LLM)", "citas_rechazadas_1er_intento": None,
                "modelo": "replay", "request_id": None, "uso": {}, "stop_reason": "replay"}


# ----------------------------------------------------------------------------- Detector de vacíos (P)

def confirmar_vacios(salida: dict, kb: BaseConocimiento, esperados: list[str]) -> list[str]:
    """Un vacío declarado se confirma si alguna norma citada o relacionada está asociada en la base
    a un vacío esperado de la etapa. Se devuelve la lista de IDs (G1..G10) confirmados."""
    v = salida.get("vacio") or {}
    if not v.get("hay"):
        return []
    ids = set(v.get("normas_relacionadas", [])) | {f["id"] for f in salida.get("fundamento", [])}
    asociados = {g for i in ids if i in kb.por_id for g in kb.por_id[i].get("vacios", [])}
    return sorted(asociados & set(esperados)) or sorted(asociados)[:2]


# ----------------------------------------------------------------------------- Director y ciclo (SRC, C, D, SINK)

def correr(modo: str, proveedor: str = "claude") -> Path:
    kb = BaseConocimiento()
    esc = yaml.safe_load(ESC_PATH.read_text(encoding="utf-8"))
    SALIDAS.mkdir(exist_ok=True)
    etiqueta = modo if modo == "replay" else f"live-{proveedor}"
    corrida = datetime.now().strftime("%Y%m%d-%H%M%S") + f"-{etiqueta}"
    bitacora = SALIDAS / f"bitacora_{corrida}.jsonl"

    agentes = {}
    for aid, ficha in esc["agentes"].items():
        if modo == "replay":
            agentes[aid] = AgenteReplay(aid, ficha, kb, esc)
        elif proveedor in ("groq", "deepseek"):
            from proveedor_compatible import AgenteCompatible
            agentes[aid] = AgenteCompatible(proveedor, aid, ficha, kb, system_prompt, ESQUEMA_DECISION)
        else:
            agentes[aid] = AgenteLLM(aid, ficha, kb)

    buzones: dict[str, list] = {a: [] for a in agentes}
    conocidos: dict[str, list] = {a: [] for a in agentes}
    if modo == "replay":
        modelo_txt = "replay"
    elif proveedor in ("groq", "deepseek"):
        from proveedor_compatible import modelo_de
        modelo_txt = f"{proveedor}:{modelo_de(proveedor)}"
    else:
        modelo_txt = MODELO
    resumen = {"corrida": corrida, "modo": modo, "proveedor": proveedor if modo == "live" else "-", "modelo": modelo_txt,
               "etapas": [], "vacios_detectados": {}, "citas_totales": 0, "citas_validas": 0}
    n = 0
    with bitacora.open("w", encoding="utf-8") as f:
        for etapa in esc["etapas"]:
            # SRC/C: el director libera los hechos a cada agente según su visibilidad
            for h in etapa["hechos"]:
                for a in h["para"]:
                    conocidos[a].append(h)
            activos = [a for a in agentes if a in etapa["preguntas"]]
            for ronda in (1, 2):
                turno = activos if ronda == 1 else [a for a in agentes if buzones[a]]
                for aid in turno:
                    hechos_etapa = [h for h in conocidos[aid] if h["id"].startswith(f"H{etapa['id']}.")]
                    previos = [h for h in conocidos[aid] if not h["id"].startswith(f"H{etapa['id']}.")]
                    recibidos, buzones[aid] = buzones[aid], []
                    pregunta = etapa["preguntas"].get(aid, "Responde a los mensajes recibidos según tu mandato.")
                    consulta = " ".join([pregunta] + [h["texto"] for h in hechos_etapa] + [m["contenido"] for m in recibidos])
                    recuperados = kb.buscar(consulta, esc["agentes"][aid]["categorias_kb"], k=8)
                    ctx = (f"ETAPA {etapa['id']}: {etapa['nombre']} ({etapa['ventana']}). Ronda {ronda}.\n\n"
                           + ("Hechos de etapas anteriores que conoces:\n" + "\n".join(f"- {h['t']} {h['texto']}" for h in previos) + "\n\n" if previos else "")
                           + "Hechos nuevos que te llegan:\n" + ("\n".join(f"- {h['t']} {h['texto']}" for h in hechos_etapa) or "- (ninguno nuevo)")
                           + ("\n\nMensajes recibidos:\n" + "\n".join(f"- de {m['de']}: {m['contenido']}" for m in recibidos) if recibidos else "")
                           + "\n\nArtículos posiblemente relevantes (recuperación automática): "
                           + ", ".join(a["id"] for a in recuperados)
                           + f"\n\nPregunta: {pregunta}")
                    if modo == "replay":
                        agentes[aid].etapa = etapa["id"]
                        if ronda == 2:
                            continue
                    r = agentes[aid].decidir(ctx)
                    s = r["salida"]
                    citas = [c["id"] for c in s.get("fundamento", [])]
                    validas = [c for c in citas if c in kb.por_id]
                    confirmados = confirmar_vacios(s, kb, etapa["clave"]["vacios"])
                    for g in confirmados:
                        resumen["vacios_detectados"].setdefault(g, []).append(f"E{etapa['id']}-{aid}")
                    resumen["citas_totales"] += len(citas)
                    resumen["citas_validas"] += len(validas)
                    # D: entrega de mensajes (el Operador B es un actor de entorno guionado)
                    for m in s.get("mensajes", []):
                        dest = m["para"].strip().lower().replace(" ", "_")
                        if dest == "operador_b":
                            orden = any(x in " ".join(mm["contenido"] for mm in recibidos).lower() for x in ("orden", "mandato"))
                            buzones[aid].append({"de": "operador_b",
                                                 "contenido": esc["entorno"]["operador_b"]["con_orden" if orden else "sin_orden"]})
                        elif dest in buzones and dest != aid:
                            buzones[dest].append({"de": aid, "contenido": m["contenido"]})
                    n += 1
                    registro = {
                        "id": f"E{etapa['id']}-R{ronda}-{aid}-{n:03d}", "corrida": corrida, "modo": modo,
                        "etapa": etapa["id"], "etapa_nombre": etapa["nombre"], "ronda": ronda, "agente": aid,
                        "hechos_conocidos": [h["id"] for h in conocidos[aid]],
                        "mensajes_recibidos": recibidos,
                        "recuperados": [a["id"] for a in recuperados],
                        "accion": s.get("accion"), "decision": s.get("decision"),
                        "fundamento": s.get("fundamento"), "competencia_invocada": s.get("competencia_invocada"),
                        "mensajes_enviados": s.get("mensajes"), "vacio_declarado": s.get("vacio"),
                        "vacios_confirmados": confirmados, "confianza": s.get("confianza"),
                        "verificacion_citas": {"citadas": citas, "validas": validas,
                                               "rechazadas_1er_intento": r["citas_rechazadas_1er_intento"]},
                        "traza_razonamiento": r["traza_razonamiento"],
                        "modelo": r["modelo"], "request_id": r["request_id"], "uso": r["uso"],
                        "stop_reason": r["stop_reason"],
                        "hash_contexto": hashlib.sha256(ctx.encode("utf-8")).hexdigest()[:16],
                        "timestamp": datetime.now(timezone.utc).isoformat(),
                    }
                    f.write(json.dumps(registro, ensure_ascii=False) + "\n")
                    f.flush()
                    print(f"[E{etapa['id']} R{ronda}] {aid}: {s.get('accion')} | citas {len(validas)}/{len(citas)} | vacíos {confirmados}")
            resumen["etapas"].append({"etapa": etapa["id"], "esperados": etapa["clave"]["vacios"]})

    esperados = sorted({g for e in esc["etapas"] for g in e["clave"]["vacios"]}, key=lambda g: int(g[1:]))
    resumen["vacios_esperados"] = esperados
    resumen["recall_vacios"] = round(len(set(resumen["vacios_detectados"]) & set(esperados)) / len(esperados), 2)
    resumen["precision_citas"] = round(resumen["citas_validas"] / max(1, resumen["citas_totales"]), 3)
    (SALIDAS / f"resumen_{corrida}.json").write_text(json.dumps(resumen, ensure_ascii=False, indent=2), encoding="utf-8")
    escribir_informe(bitacora, resumen, esc, kb)
    print(json.dumps({k: resumen[k] for k in ("recall_vacios", "precision_citas", "vacios_detectados")}, ensure_ascii=False))
    return bitacora


def escribir_informe(bitacora: Path, resumen: dict, esc: dict, kb: BaseConocimiento) -> None:
    regs = [json.loads(l) for l in bitacora.read_text(encoding="utf-8").splitlines()]
    L = [f"# Informe del ejercicio {resumen['corrida']}", "",
         f"Modo: **{resumen['modo']}** · Modelo: {resumen['modelo']} · Citas válidas: {resumen['citas_validas']}/{resumen['citas_totales']}"
         f" · Cobertura de vacíos esperados: {resumen['recall_vacios']:.0%}", "", "## Línea de tiempo y decisiones", ""]
    for r in regs:
        cit = ", ".join(c["id"] for c in (r["fundamento"] or [])) or "sin norma habilitante"
        L.append(f"- **E{r['etapa']} · {r['agente']}** (ronda {r['ronda']}): {r['accion']}. *Norma:* {cit}."
                 + (f" *Vacío:* {', '.join(r['vacios_confirmados'])}" if r["vacios_confirmados"] else ""))
    L += ["", "## Vacíos detectados y referentes internacionales", ""]
    for g in sorted(resumen["vacios_detectados"], key=lambda g: int(g[1:])):
        refs = [a for a in kb.arts if g in a.get("vacios", []) and a["pais"] in ("Global", "Internacional")]
        L.append(f"- **{g}** · {esc['vacios'].get(g, '')}. Detectado en: {', '.join(resumen['vacios_detectados'][g])}. "
                 f"Referentes: {'; '.join(a['norma'] + ' ' + a['art'] for a in refs) or '-'}")
    faltan = set(resumen["vacios_esperados"]) - set(resumen["vacios_detectados"])
    if faltan:
        L += ["", f"Vacíos esperados no detectados en esta corrida: {', '.join(sorted(faltan))}."]
    (SALIDAS / f"informe_{resumen['corrida']}.md").write_text("\n".join(L), encoding="utf-8")


def cargar_env(path: Path = RAIZ.parent / ".env") -> None:
    """Carga variables (p. ej. ANTHROPIC_API_KEY) desde P1_PROYECT/.env, que no se publica."""
    import os
    if path.exists():
        for linea in path.read_text(encoding="utf-8").splitlines():
            if "=" in linea and not linea.lstrip().startswith("#"):
                k, v = linea.split("=", 1)
                os.environ.setdefault(k.strip(), v.strip())


if __name__ == "__main__":
    cargar_env()
    ap = argparse.ArgumentParser()
    ap.add_argument("--modo", choices=["live", "replay"], default="live")
    ap.add_argument("--proveedor", choices=["claude", "groq", "deepseek"], default=None,
                    help="Por defecto: claude, groq o deepseek, según la primera clave disponible en ese orden")
    args = ap.parse_args()
    import os
    prov = args.proveedor or ("claude" if os.environ.get("ANTHROPIC_API_KEY") else
                              "groq" if os.environ.get("GROQ_API_KEY") else
                              "deepseek" if os.environ.get("DEEPSEEK_API_KEY") else "claude")
    if args.modo == "live":
        try:
            if prov in ("groq", "deepseek"):
                sys.path.insert(0, str(RAIZ))
                from proveedor_compatible import comprobar_credenciales
                comprobar_credenciales(prov)
            else:
                import anthropic
                anthropic.Anthropic().models.retrieve(MODELO)   # comprueba credenciales sin generar tokens
        except Exception as e:  # sin credenciales o sin acceso al modelo
            sys.exit(f"No se pudo usar el proveedor {prov} ({e}). Revisa la clave en P1_PROYECT/.env o usa --modo replay.")
    try:
        correr(args.modo, prov)
    finally:   # no dejar bitácoras vacías si la corrida se interrumpe
        for f in SALIDAS.glob("bitacora_*.jsonl"):
            if f.stat().st_size == 0:
                f.unlink()
