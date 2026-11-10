"""
PLSsemLab logo: builds the icon and the light/dark logos as SVG, with the
suite's visual system (AssumptionsLab, EGAnetLab, MokkenLab, FiabilityLab):
256 px rounded square, Okabe-Ito colours, white rings on nodes, one grey
dashed element, wordmark with the method name in the module's accent colour.

The icon is a small PLS path model: a formative construct (indicators point
in), two reflective constructs (indicators point out), two significant
structural paths (solid) and one non-significant path (dashed grey).

ES: Logo de PLSsemLab: genera el icono y los logos claro/oscuro en SVG con el
sistema visual de la suite. El icono es un pequeño modelo de rutas PLS: un
constructo formativo (los indicadores apuntan hacia él), dos reflectivos (los
indicadores salen de ellos), dos rutas estructurales significativas (sólidas)
y una no significativa (gris discontinua).

Run: python3 docs/logo/build_logo.py   (writes the SVGs next to this file)
"""
import math
from pathlib import Path

OUT = Path(__file__).resolve().parent

BLUE, ORANGE, GREEN = "#0072B2", "#E69F00", "#009E73"
GREY = "#8A94A6"
THEMES = {
    "light": dict(ring="#FFFFFF", path="#1F4E79", text="#1F2A44",
                  accent="#9A6200", tagline="#5B6B85"),
    "dark":  dict(ring="#0D1117", path="#7FB2E5", text="#F2F5FA",
                  accent="#F2B94D", tagline="#AEB9CC"),
}
R = 21          # construct radius
S = 16          # indicator square side
A, B, C = (84, 96), (172, 96), (128, 178)


def arrow(x1, y1, x2, y2, color, width, head, dashed=False, opacity=None):
    """A line from (x1, y1) to (x2, y2) with a filled triangular head at the end.
    ES: Una línea con punta triangular rellena en el extremo final."""
    ang = math.atan2(y2 - y1, x2 - x1)
    bx, by = x2 - head * math.cos(ang), y2 - head * math.sin(ang)   # base of the head
    px, py = -math.sin(ang) * head * 0.55, math.cos(ang) * head * 0.55
    op = f' opacity="{opacity}"' if opacity else ""
    dash = ' stroke-dasharray="6 5"' if dashed else ""
    return (f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{bx:.1f}" y2="{by:.1f}" stroke="{color}" '
            f'stroke-width="{width}" stroke-linecap="round"{dash}{op}/>\n'
            f'  <polygon points="{x2:.1f},{y2:.1f} {bx + px:.1f},{by + py:.1f} {bx - px:.1f},{by - py:.1f}" '
            f'fill="{color}"{op}/>\n')


def edge_point(c, toward, r):
    """Point on a circle of radius r around c, in the direction of `toward`.
    ES: Punto sobre el círculo de radio r en c, en dirección a `toward`."""
    ang = math.atan2(toward[1] - c[1], toward[0] - c[0])
    return c[0] + r * math.cos(ang), c[1] + r * math.sin(ang)


def drawing(t):
    out = []
    # Structural paths first, so the constructs sit on top.
    # ES: Primero las rutas estructurales, para que los constructos queden encima.
    for (p, q, kind) in [(A, B, "solid"), (B, C, "solid"), (A, C, "dashed")]:
        x1, y1 = edge_point(p, q, R + 3)
        x2, y2 = edge_point(q, p, R + 4)
        if kind == "solid":
            out.append(arrow(x1, y1, x2, y2, t["path"], 5, 12, opacity=0.9))
        else:
            out.append(arrow(x1, y1, x2, y2, GREY, 3, 9, dashed=True, opacity=0.9))
    # Formative block (A): indicators on the left, arrows into the construct.
    # ES: Bloque formativo (A): indicadores a la izquierda, flechas hacia el constructo.
    for y in (64, 96, 128):
        x2, y2 = edge_point(A, (30, y), R + 4)
        out.append(arrow(30 + S / 2 + 1, y, x2, y2, BLUE, 3, 8, opacity=0.8))
    # Reflective blocks (B right, C bottom): arrows out to the indicators.
    # ES: Bloques reflectivos (B a la derecha, C abajo): flechas hacia los indicadores.
    for y in (64, 96, 128):
        x1, y1 = edge_point(B, (226, y), R + 3)
        out.append(arrow(x1, y1, 226 - S / 2 - 1, y, ORANGE, 3, 8, opacity=0.8))
    for x in (96, 128, 160):
        x1, y1 = edge_point(C, (x, 226), R + 3)
        out.append(arrow(x1, y1, x, 226 - S / 2 - 1, GREEN, 3, 8, opacity=0.8))
    # Indicators (squares) and constructs (circles), white-ringed as in the suite.
    # ES: Indicadores (cuadrados) y constructos (círculos), con anillo como en la suite.
    for (cx, cy, col) in [(30, y, BLUE) for y in (64, 96, 128)] + \
                         [(226, y, ORANGE) for y in (64, 96, 128)] + \
                         [(x, 226, GREEN) for x in (96, 128, 160)]:
        out.append(f'  <rect x="{cx - S / 2}" y="{cy - S / 2}" width="{S}" height="{S}" rx="4" '
                   f'fill="{col}" stroke="{t["ring"]}" stroke-width="3"/>\n')
    for (c, col) in [(A, BLUE), (B, ORANGE), (C, GREEN)]:
        out.append(f'  <circle cx="{c[0]}" cy="{c[1]}" r="{R}" fill="{col}" '
                   f'stroke="{t["ring"]}" stroke-width="3.5"/>\n')
    return "".join(out)


HEAD = '<?xml version="1.0" encoding="UTF-8"?>\n'

icon = (HEAD + '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" width="256" height="256" '
        'role="img" aria-label="PLSsemLab">\n  <title>PLSsemLab</title>\n'
        '  <rect x="6" y="6" width="244" height="244" rx="52" fill="#F4F7FB" stroke="#D5DEEA" stroke-width="2"/>\n'
        # Scaled to 88 % around the centre, so the drawing keeps the same
        # margin as the other icons of the suite.
        # ES: Escalado al 88 % alrededor del centro, con el mismo margen que
        # los demás iconos de la suite.
        + '  <g transform="translate(15.4 15.4) scale(0.88)">\n' + drawing(THEMES["light"])
        + '  </g>\n</svg>\n')
(OUT / "plssemlab-icon.svg").write_text(icon, encoding="utf-8")

for name, t in THEMES.items():
    suffix = "" if name == "light" else "-dark"
    logo = (HEAD + '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 860 230" width="860" height="230" '
            'role="img" aria-label="PLSsemLab">\n  <title>PLSsemLab</title>\n'
            '  <g transform="translate(0 -6) scale(0.95)">\n' + drawing(t) + '  </g>\n'
            f'  <text x="250" y="128" font-family="Segoe UI, Helvetica Neue, Arial, sans-serif" font-size="94" '
            f'font-weight="700" fill="{t["text"]}" letter-spacing="-1"><tspan fill="{t["accent"]}">PLSsem</tspan>Lab</text>\n'
            f'  <text x="254" y="172" font-family="Segoe UI, Helvetica Neue, Arial, sans-serif" font-size="25" '
            f'fill="{t["tagline"]}" letter-spacing="0.5">Partial least squares SEM for jamovi</text>\n</svg>\n')
    (OUT / f"plssemlab-logo{suffix}.svg").write_text(logo, encoding="utf-8")
print("written to", OUT)
