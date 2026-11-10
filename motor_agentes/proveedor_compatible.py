"""Agentes institucionales sobre APIs compatibles con OpenAI: Groq y DeepSeek.

Misma interfaz que AgenteLLM (Claude): decidir(contexto) -> dict con la salida JSON,
la traza de razonamiento que devuelve el modelo, el uso de tokens y la verificación de citas.
Las claves se leen de P1_PROYECT/.env (GROQ_API_KEY, DEEPSEEK_API_KEY), que no se publica.
"""
from __future__ import annotations

import json
import os
import re
import time

PROVEEDORES = {
    "groq": {"base_url": "https://api.groq.com/openai/v1", "clave": "GROQ_API_KEY",
             "modelo": os.environ.get("GROQ_MODEL", "openai/gpt-oss-120b"), "traza": "reasoning"},
    "deepseek": {"base_url": "https://api.deepseek.com", "clave": "DEEPSEEK_API_KEY",
                 "modelo": os.environ.get("DEEPSEEK_MODEL", "deepseek-v4-pro"), "traza": "reasoning_content"},
}

CLAVES = ["accion", "decision", "fundamento", "competencia_invocada", "mensajes", "vacio", "confianza"]


def _cliente(proveedor: str):
    import openai
    cfg = PROVEEDORES[proveedor]
    return openai, openai.OpenAI(api_key=os.environ[cfg["clave"]], base_url=cfg["base_url"]), cfg


def modelo_de(proveedor: str) -> str:
    return PROVEEDORES[proveedor]["modelo"]


def comprobar_credenciales(proveedor: str) -> None:
    """Lista los modelos (sin generar tokens). Lanza una excepción si la clave o el modelo no sirven."""
    _, c, cfg = _cliente(proveedor)
    ids = [m.id for m in c.models.list().data]
    if cfg["modelo"] not in ids:
        raise RuntimeError(f"El modelo {cfg['modelo']} no está disponible; modelos: {ids}")


def preguntar(proveedor: str, pregunta: str) -> tuple[str, str]:
    """Pregunta simple sin base normativa (para la línea base)."""
    _, c, cfg = _cliente(proveedor)
    r = c.chat.completions.create(model=cfg["modelo"], max_tokens=3000,
                                  messages=[{"role": "user", "content": pregunta}])
    return r.choices[0].message.content or "", r.model


def _extraer_json(texto: str) -> dict:
    try:
        return json.loads(texto)
    except json.JSONDecodeError:
        m = re.search(r"\{.*\}", texto, re.S)
        try:
            return json.loads(m.group(0)) if m else {}
        except json.JSONDecodeError:
            return {}


def _valida(d: dict) -> bool:
    return all(k in d for k in CLAVES) and isinstance(d.get("fundamento"), list) and isinstance(d.get("vacio"), dict)


class AgenteCompatible:
    def __init__(self, proveedor: str, agente_id: str, ficha: dict, kb, system_prompt_fn, esquema: dict):
        self.openai, self.client, self.cfg = _cliente(proveedor)
        self.proveedor = proveedor
        self.id = agente_id
        self.kb_sub = kb.subconjunto(ficha["categorias_kb"])
        self.ids_permitidos = {a["id"] for a in self.kb_sub}
        # El esquema va dentro del system prompt (modo JSON: response_format json_object)
        self.system = (system_prompt_fn(agente_id, ficha, self.kb_sub)
                       + "\n\nFORMATO DE SALIDA: responde SOLO con un objeto JSON válido que cumpla este esquema:\n"
                       + json.dumps(esquema, ensure_ascii=False))

    def decidir(self, contexto_usuario: str) -> dict:
        mensajes = [{"role": "system", "content": self.system}, {"role": "user", "content": contexto_usuario}]
        r = self._llamar(mensajes)
        datos, traza, texto = self._parsear(r)
        reintento = None
        problemas = []
        if not _valida(datos):
            problemas.append("La respuesta no cumple el esquema JSON (faltan campos).")
        malas = [f.get("id") for f in datos.get("fundamento", []) if f.get("id") not in self.ids_permitidos]
        if malas:
            reintento = malas
            problemas.append(f"Estos IDs no existen en tu base normativa: {malas}. Cita solo IDs de la base o declara el vacío.")
        if problemas:   # verificador (nodo P): una reintentona con retroalimentación
            mensajes += [{"role": "assistant", "content": texto},
                         {"role": "user", "content": "Verificador: " + " ".join(problemas) + " Devuelve el JSON corregido."}]
            r = self._llamar(mensajes)
            datos, traza2, _ = self._parsear(r)
            traza = (traza + "\n[tras verificación] " + traza2).strip()
        if not _valida(datos):
            datos = {"accion": "respuesta inválida", "decision": "", "fundamento": [], "competencia_invocada": "",
                     "mensajes": [], "vacio": {"hay": False, "tipo": "ninguno", "descripcion": "", "normas_relacionadas": []},
                     "confianza": "baja", **{k: v for k, v in datos.items() if k in CLAVES}}
        u = r.usage
        return {"salida": datos, "traza_razonamiento": traza, "citas_rechazadas_1er_intento": reintento,
                "modelo": f"{self.proveedor}:{r.model}", "request_id": r.id,
                "uso": {"input": u.prompt_tokens, "output": u.completion_tokens,
                        "cache_read": getattr(u, "prompt_cache_hit_tokens", 0) or 0, "cache_write": 0},
                "stop_reason": r.choices[0].finish_reason}

    def _llamar(self, mensajes):
        for intento in range(8):
            try:
                return self.client.chat.completions.create(
                    model=self.cfg["modelo"], messages=mensajes, max_tokens=6000,
                    response_format={"type": "json_object"})
            except self.openai.RateLimitError as e:
                # Respeta retry-after (Groq limita tokens por minuto en el plan gratuito)
                espera = e.response.headers.get("retry-after")
                time.sleep(float(espera) + 1 if espera else 20 * (intento + 1))
            except self.openai.APIConnectionError:
                time.sleep(5 * (intento + 1))
            except self.openai.APIStatusError as e:
                if e.status_code == 402:
                    raise SystemExit(f"{self.proveedor}: saldo insuficiente en la cuenta (402).")
                if e.status_code == 400 and "json" in str(e).lower() and intento < 2:
                    # El modelo no produjo JSON válido: se reintenta
                    continue
                raise
        raise RuntimeError(f"{self.proveedor} no respondió tras varios intentos")

    def _parsear(self, r):
        msg = r.choices[0].message
        texto = msg.content or ""
        traza = (getattr(msg, self.cfg["traza"], None) or "").strip()
        return _extraer_json(texto), traza, texto
