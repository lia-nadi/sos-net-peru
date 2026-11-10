"""Línea base para el video: la misma pregunta a un LLM SIN base normativa ni agentes.

Guarda las respuestas en salidas/baseline_<fecha>.json para contrastarlas con la bitácora del sistema
(citas inexistentes, normas de otros países, vacíos no detectados).
Uso: python motor_agentes/baseline.py   (requiere credenciales de la API de Claude)
"""
from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path

import os
import sys

PREGUNTAS = {
    "G2_geolocalizacion": "En Perú, tras un sismo, INDECI pide a un operador móvil la ubicación de 37 números que llamaron al 119 sin completar la llamada. ¿Puede el operador entregarla? Cita la norma peruana aplicable.",
    "G1_roaming": "En Perú, tras un sismo en Lima, un operador móvil tiene el 25 % de su red caída. ¿Puede el regulador ordenar roaming de emergencia con otro operador? Cita la norma peruana.",
    "G5_alerta": "En Perú, ¿puede el IGP enviar automáticamente una alerta sísmica por Cell Broadcast a los celulares? Cita la norma.",
    "G6_ia_red": "En Perú, si la IA de optimización de un operador apaga celdas y desprioriza tráfico durante un desastre, ¿qué exige la normativa peruana sobre supervisión humana? Cita la norma.",
}


def preguntar_claude(q: str) -> tuple[str, str]:
    import anthropic
    r = anthropic.Anthropic().beta.messages.create(
        model="claude-opus-5-5", max_tokens=4000,
        betas=["server-side-fallback-2026-07-01"], fallbacks="default",
        output_config={"effort": "low"},
        messages=[{"role": "user", "content": q}],
    )
    return next((b.text for b in r.content if b.type == "text"), ""), r.model




def main() -> None:
    sys.path.insert(0, str(Path(__file__).parent))
    from motor import cargar_env
    cargar_env()
    from proveedor_compatible import preguntar
    if os.environ.get("ANTHROPIC_API_KEY"):
        usar = preguntar_claude
    else:
        prov = "groq" if os.environ.get("GROQ_API_KEY") else "deepseek"
        usar = lambda q: preguntar(prov, q)
    out = {}
    for k, q in PREGUNTAS.items():
        texto, modelo = usar(q)
        out[k] = {"pregunta": q, "respuesta": texto, "modelo": modelo}
        print(k, "listo")
    destino = Path(__file__).parent / "salidas" / f"baseline_{datetime.now():%Y%m%d-%H%M%S}.json"
    destino.parent.mkdir(exist_ok=True)
    destino.write_text(json.dumps(out, ensure_ascii=False, indent=2), encoding="utf-8")
    print(destino)


if __name__ == "__main__":
    main()
