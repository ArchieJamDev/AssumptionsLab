"""
AssumptionsLab logo: builds the module icon and the light/dark README logos as
SVG, in the suite's visual system (MokkenLab, PLSsemLab, EGAnetLab): a 256 px
rounded tile (#F4F7FB), white shapes with thick strokes in blue (#6B9DE8) and
amber (#E6AC40), grey lines (#9F9F9F) with round caps, and the wordmark with
the method name in blue; the tagline is stretched to the width of the title,
as in the other logos.

The mark is a normal curve with three points on it: the mean (amber) and one
standard deviation on each side (blue), the reference shape that most
assumption checks compare the data against.

ES: Logo de AssumptionsLab: genera el icono del módulo y los logos claro y
oscuro del README con el sistema visual de la suite. El símbolo es una curva
normal con tres puntos: la media (ámbar) y una desviación típica a cada lado
(azul), la forma de referencia con la que se comparan los datos en la mayoría
de las comprobaciones de supuestos.

Run: python3 docs/logo/build_logo.py   (writes the SVGs next to this file and,
when rsvg-convert is available, jamovi/assets/assumptionslab-icon.png and the
PNG sizes in inst/assets/icons/)
"""
import math
import shutil
import subprocess
from pathlib import Path

OUT = Path(__file__).resolve().parent
ROOT = OUT.parent.parent
BLUE, AMBER, GREY, BASE = "#6B9DE8", "#E6AC40", "#9F9F9F", "#C9D2DE"
FONT = "Segoe UI, Helvetica Neue, Arial, sans-serif"
THEMES = {
    "light": dict(fill="#FFFFFF", base=BASE, text="#1F2A44", accent="#4A86E0", tagline="#5B6B85"),
    "dark": dict(fill="#1E2533", base="#4A5568", text="#E8EDF5", accent="#7FA9EC", tagline="#A9B4C7"),
}


def mark(x0, x1, base, height, sd, r, sw, swl, fill="#FFFFFF", base_col=BASE):
    """Normal curve from x0 to x1 over a baseline, with points at the mean and
    at +/- 1.33 sd. ES: Curva normal con puntos en la media y a ambos lados."""
    mid = (x0 + x1) / 2

    def y(x):
        z = (x - mid) / sd
        return base - height * math.exp(-z * z / 2)

    pts = " ".join(f"{x0 + (x1 - x0) * i / 60:.1f},{y(x0 + (x1 - x0) * i / 60):.1f}" for i in range(61))
    gap = sw * 1.1
    out = (f'  <polyline points="{pts}" fill="none" stroke="{GREY}" stroke-width="{swl:.1f}" '
           'stroke-linecap="round" stroke-linejoin="round"/>\n'
           f'  <line x1="{x0:.1f}" y1="{base + gap:.1f}" x2="{x1:.1f}" y2="{base + gap:.1f}" '
           f'stroke="{base_col}" stroke-width="{swl * 0.69:.1f}" stroke-linecap="round"/>\n')
    for dx, col in ((-sd * 1.33, BLUE), (0, AMBER), (sd * 1.33, BLUE)):
        x = mid + dx
        out += (f'  <circle cx="{x:.1f}" cy="{y(x):.1f}" r="{r:.1f}" fill="{fill}" '
                f'stroke="{col}" stroke-width="{sw:.1f}"/>\n')
    return out


def icon():
    body = mark(40, 216, 200, 118, 30, 17, 12, 11.6)
    return ('<?xml version="1.0" encoding="UTF-8"?>\n'
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" width="256" height="256" '
            'role="img" aria-label="AssumptionsLab">\n  <title>AssumptionsLab</title>\n'
            '  <rect x="6" y="6" width="244" height="244" rx="52" fill="#F4F7FB" stroke="#D5DEEA" stroke-width="2"/>\n'
            f'{body}</svg>\n')


def logo(theme):
    t = THEMES[theme]
    body = mark(52, 228, 172, 112, 30, 15, 11, 10.8, fill=t["fill"], base_col=t["base"])
    return ('<?xml version="1.0" encoding="UTF-8"?>\n'
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1080 230" width="1080" height="230" '
            'role="img" aria-label="AssumptionsLab">\n  <title>AssumptionsLab</title>\n'
            f'{body}'
            f'  <text x="250" y="128" font-family="{FONT}" font-size="94" font-weight="700" '
            f'textLength="760" lengthAdjust="spacing" fill="{t["text"]}" letter-spacing="-1"><tspan fill="{t["accent"]}">Assumptions</tspan>Lab</text>\n'
            f'  <text x="254" y="172" font-family="{FONT}" font-size="27" fill="{t["tagline"]}" '
            'textLength="756" lengthAdjust="spacing">Statistical assumptions assessment for jamovi</text>\n'
            '</svg>\n')


def png(svg, size, dest):
    subprocess.run(["rsvg-convert", "-w", str(size), "-h", str(size), str(svg), "-o", str(dest)], check=True)


if __name__ == "__main__":
    (OUT / "assumptionslab-icon.svg").write_text(icon(), encoding="utf-8")
    (OUT / "assumptionslab-logo.svg").write_text(logo("light"), encoding="utf-8")
    (OUT / "assumptionslab-logo-dark.svg").write_text(logo("dark"), encoding="utf-8")
    if shutil.which("rsvg-convert"):
        svg = OUT / "assumptionslab-icon.svg"
        png(svg, 256, ROOT / "jamovi" / "assets" / "assumptionslab-icon.png")
        icons = ROOT / "inst" / "assets" / "icons"
        for size in (64, 128, 256):
            png(svg, size, icons / f"assumptionslab-icon-{size}.png")
        png(svg, 1254, icons / "assumptionslab-icon.png")
