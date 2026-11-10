"""Construye la base de conocimiento a partir de articulos.yaml.

Salidas:
  kb_articulos.jsonl                       unidades citables (las consulta el motor de agentes)
  Dataset/<Pais>/PolicyData/<cat>/*.md     una ficha por norma, con el formato del repositorio
                                           de referencia de la UIT (CrashingGuru/Sandbox_Training)
  Dataset/Peru/PolicyData/<cat>/*.pdf      copia de los textos oficiales descargados

Uso: python knowledge_base/construir_kb.py
"""
from __future__ import annotations

import json
import re
import shutil
from collections import defaultdict
from pathlib import Path

import yaml

RAIZ = Path(__file__).resolve().parent
FUENTES = RAIZ.parent.parent / "investigacion_P1" / "fuentes"

# Archivos oficiales descargados -> norma a la que pertenecen
PDF_POR_NORMA = {
    "DS 051-2010-MTC": "DS_051-2010-MTC_Marco_Comunicaciones_Emergencias_SPIJ2015.pdf",
    "DS 002-2023-MTC": "DS_002-2023-MTC_Roaming_Nacional_ElPeruano.txt",
    "Res. 261-2023-CD/OSIPTEL": "OSIPTEL_Res_261-2023-CD_mandatos_Roaming_Nacional.pdf",
    "RM 896-2021-MTC": "RM_896-2021-MTC_Anexo_Tecnico_SISMATE.pdf",
    "Directiva 003-2024": "INDECI_Directiva_003-2024_uso_SISMATE.pdf",
    "DS 115-2025-PCM": "DS_115-2025-PCM_Reglamento_Ley_31814_IA.pdf",
    "Ley 29733": "MINJUSDH_Compendio_Proteccion_Datos_Ley29733.pdf",
    "Decreto Legislativo 1182": "DL_1182_geolocalizacion.pdf",
    "Ley 30472": "Ley_30472_SISMATE_ElPeruano.txt",
    "Decisión 854": "CAN_Gaceta_3902_Decision_854_roaming.pdf",
    "Convenio de Tampere": "Convenio_Tampere_ES.pdf",
}


def slug(s: str) -> str:
    s = re.sub(r"[^\w\s-]", "", s, flags=re.UNICODE)
    return re.sub(r"[\s]+", "_", s.strip())[:80]


def main() -> None:
    arts = yaml.safe_load((RAIZ / "articulos.yaml").read_text(encoding="utf-8"))
    ids = [a["id"] for a in arts]
    assert len(ids) == len(set(ids)), "IDs duplicados en articulos.yaml"

    # 1) JSONL de unidades citables
    with (RAIZ / "kb_articulos.jsonl").open("w", encoding="utf-8") as f:
        for a in arts:
            f.write(json.dumps(a, ensure_ascii=False) + "\n")

    # 2) Fichas por norma (formato del repositorio de referencia de la UIT)
    dataset = RAIZ / "Dataset"
    if dataset.exists():
        shutil.rmtree(dataset)
    por_norma: dict[tuple, list] = defaultdict(list)
    for a in arts:
        base = a["norma"].split(" (")[0]          # agrupa variantes de la misma norma
        if base in ("UE, Reglamento", "UE, Directiva"):   # bases demasiado genéricas
            base = a["norma"]
        por_norma[(a["pais"], a["categoria"], base)].append(a)

    contador: dict[str, int] = defaultdict(int)
    for (pais, cat, norma), unidades in sorted(por_norma.items()):
        carpeta = dataset / pais / "PolicyData" / cat
        carpeta.mkdir(parents=True, exist_ok=True)
        contador[cat] += 1
        doc_id = f"{int(cat[:2])}.{contador[cat]}"
        nombres = sorted({u["norma"] for u in unidades}, key=len, reverse=True)
        norma = nombres[0]
        nombre = f"{doc_id}_{slug(norma)}"
        urls = sorted({u["url"] for u in unidades})
        vacios = sorted({v for u in unidades for v in u.get("vacios", [])})
        nodos = sorted({x for u in unidades for x in u.get("y3172", [])})
        dims = sorted({x for u in unidades for x in u.get("readiness", [])})
        lineas = [
            f"# {norma}",
            "",
            f"- **Doc ID:** {doc_id}",
            f"- **País / ámbito:** {pais}",
            f"- **Categoría:** {cat}",
            f"- **Nodos ITU-T Y.3172:** {', '.join(nodos)}",
            f"- **Dimensiones ITU AI Readiness 2.0:** {', '.join(map(str, dims))}",
            f"- **Vacíos relacionados (P1):** {', '.join(vacios) or '-'}",
            "",
            "## Abstract",
            "",
            "; ".join(u["titulo"] for u in unidades) + ".",
            "",
            "## Access links",
            "",
            *[f"- <{u}>" for u in urls],
            "",
            "## Artículos",
            "",
        ]
        for u in unidades:
            lineas += [
                f"### {u['id']} · art. {u['art']} · {u['titulo']}",
                "",
                f"- Efecto: **{u['efecto']}** · Texto: {u['texto_tipo']} · Confiabilidad: {u['confiabilidad']}",
                *( [f"- Condiciones: {', '.join(u['condiciones'])}"] if u.get("condiciones") else [] ),
                *( [f"- Disparador: {u['disparador']}"] if u.get("disparador") else [] ),
                *( [f"- Nota: {u['nota']}"] if u.get("nota") else [] ),
                "",
                f"> {u['texto']}",
                "",
            ]
        (carpeta / f"{nombre}.md").write_text("\n".join(lineas), encoding="utf-8")
        for clave, archivo in PDF_POR_NORMA.items():
            if clave in norma and (FUENTES / archivo).exists():
                shutil.copy2(FUENTES / archivo, carpeta / f"{doc_id}_{archivo}")

    print(f"KB: {len(arts)} unidades, {len(por_norma)} normas -> {dataset}")


if __name__ == "__main__":
    main()
