"""
PLSsemLab logo: builds the module icon, the jamovi ribbon icon and the
light/dark README logos as SVG, in the suite's visual system (MokkenLab,
EGAnetLab): a 256 px rounded tile (#F4F7FB), white shapes with thick strokes
in blue (#6B9DE8) and amber (#E6AC40), grey connectors (#9F9F9F) with round
caps, and the wordmark with the method name in blue.

The mark is a small PLS path model: two constructs (blue circles) predict a
third (amber circle); circles are constructs, as the nodes of EGAnetLab, and
the arrows add the direction that makes it structural equation modeling.

ES: Logo de PLSsemLab: genera el icono del módulo, el icono de la cinta de
jamovi y los logos claro y oscuro del README con el sistema visual de la
suite. El símbolo es un pequeño modelo de rutas PLS: dos constructos
(círculos azules) predicen un tercero (círculo ámbar).

Run: python3 docs/logo/build_logo.py   (writes the SVGs next to this file and
jamovi/assets/plssemlab-icon.png when rsvg-convert is available)
"""
import math
import shutil
import subprocess
from pathlib import Path

OUT = Path(__file__).resolve().parent
ASSETS = OUT.parent.parent / "jamovi" / "assets"
BLUE, AMBER, GREY = "#6B9DE8", "#E6AC40", "#9F9F9F"
FONT = "Segoe UI, Helvetica Neue, Arial, sans-serif"
THEMES = {
    "light": dict(fill="#FFFFFF", text="#1F2A44", accent="#4A86E0", tagline="#5B6B85"),
    "dark": dict(fill="#1E2533", text="#E8EDF5", accent="#7FA9EC", tagline="#A9B4C7"),
}


def arrow(a, b, r_start, r_end, sw, head):
    """A grey connector from the edge of shape a to the edge of shape b with a
    filled, rounded arrowhead. ES: Conector gris con punta de flecha."""
    (x1, y1), (x2, y2) = a, b
    length = math.hypot(x2 - x1, y2 - y1)
    ux, uy = (x2 - x1) / length, (y2 - y1) / length
    sx, sy = x1 + ux * r_start, y1 + uy * r_start
    tx, ty = x2 - ux * r_end, y2 - uy * r_end
    bx, by = tx - ux * head, ty - uy * head
    px, py, w = -uy, ux, head * 0.62
    pts = f"{tx:.1f},{ty:.1f} {bx + px * w:.1f},{by + py * w:.1f} {bx - px * w:.1f},{by - py * w:.1f}"
    return (f'  <line x1="{sx:.1f}" y1="{sy:.1f}" x2="{bx + ux * 2:.1f}" y2="{by + uy * 2:.1f}" '
            f'stroke="{GREY}" stroke-width="{sw:.1f}" stroke-linecap="round"/>\n'
            f'  <polygon points="{pts}" fill="{GREY}" stroke="{GREY}" stroke-width="{sw * 0.45:.1f}" '
            f'stroke-linejoin="round"/>\n')


def mark(a, b, c, r, sw, swl, head, fill="#FFFFFF"):
    """Two predictors (a, b) and an outcome (c). ES: Dos predictores y un resultado."""
    gap = r + sw / 2
    out = arrow(a, c, gap, gap + 3, swl, head) + arrow(b, c, gap, gap + 3, swl, head)
    for (x, y), col in ((a, BLUE), (b, BLUE), (c, AMBER)):
        out += f'  <circle cx="{x:.1f}" cy="{y:.1f}" r="{r:.1f}" fill="{fill}" stroke="{col}" stroke-width="{sw:.1f}"/>\n'
    return out


def icon():
    body = mark((56, 70), (56, 186), (198, 128), 33, 13.1, 11.6, 25)
    return ('<?xml version="1.0" encoding="UTF-8"?>\n'
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" width="256" height="256" '
            'role="img" aria-label="PLSsemLab">\n  <title>PLSsemLab</title>\n'
            '  <rect x="6" y="6" width="244" height="244" rx="52" fill="#F4F7FB" stroke="#D5DEEA" stroke-width="2"/>\n'
            f'{body}</svg>\n')


def ribbon():
    # jamovi's Analyses ribbon (menuIcons): viewBox 74 x 44, strokes of 2-4
    # units, jamovi's palette, a little padding, plain SVG with no metadata.
    # ES: Cinta de análisis de jamovi: 74 x 44, trazos de 2 a 4, sin metadatos.
    body = mark((15, 12), (15, 32), (58, 22), 6.5, 3.5, 3.0, 6.5)
    return ('<svg version="1.1" viewBox="0 0 74 44" xmlns="http://www.w3.org/2000/svg">\n'
            + body.replace("  <", "<") + '</svg>\n')


def logo(theme):
    t = THEMES[theme]
    body = mark((100, 86), (100, 160), (206, 123), 17, 12, 10.8, 18, fill=t["fill"])
    return ('<?xml version="1.0" encoding="UTF-8"?>\n'
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 780 230" width="780" height="230" '
            'role="img" aria-label="PLSsemLab">\n  <title>PLSsemLab</title>\n'
            '  <g transform="translate(-48 0)">\n'
            f'{body}'
            f'  <text x="250" y="128" font-family="{FONT}" font-size="94" font-weight="700" textLength="520" '
            f'lengthAdjust="spacing" fill="{t["text"]}" letter-spacing="-1"><tspan fill="{t["accent"]}">PLSsem</tspan>Lab</text>\n'
            f'  <text x="254" y="172" font-family="{FONT}" font-size="27" fill="{t["tagline"]}" textLength="514" '
            'lengthAdjust="spacing">Partial Least Squares SEM for jamovi</text>\n'
            '  </g>\n</svg>\n')


if __name__ == "__main__":
    (OUT / "plssemlab-icon.svg").write_text(icon(), encoding="utf-8")
    (OUT / "analysis-plssemlab.svg").write_text(ribbon(), encoding="utf-8")
    (OUT.parent.parent / "jamovi" / "icons").mkdir(exist_ok=True)
    (OUT.parent.parent / "jamovi" / "icons" / "plssemlab.svg").write_text(ribbon(), encoding="utf-8")
    (OUT / "plssemlab-logo.svg").write_text(logo("light"), encoding="utf-8")
    (OUT / "plssemlab-logo-dark.svg").write_text(logo("dark"), encoding="utf-8")
    if shutil.which("rsvg-convert"):
        subprocess.run(["rsvg-convert", "-w", "256", "-h", "256", str(OUT / "plssemlab-icon.svg"),
                        "-o", str(ASSETS / "plssemlab-icon.png")], check=True)
