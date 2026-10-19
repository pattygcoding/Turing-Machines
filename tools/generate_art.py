#!/usr/bin/env python3
"""Generate the showcase's link-preview image.

It is committed under ``public/`` so the build copies it verbatim.  The favicon
(``public/logo.png``) is deliberately *not* drawn here: it is the shared Patrick
Goodwin logo, copied from the main site (``PG-Website/public/assets/images/logo.png``)
so every pattygcoding.com project carries the same icon.  Keeping it out of this
script means ``npm run art`` can never overwrite it.  Pillow is the only
dependency:

    python -m pip install pillow
    python tools/generate_art.py
"""

from __future__ import annotations

import os
import sys

from PIL import Image, ImageDraw, ImageFont

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PUBLIC = os.path.join(ROOT, "public")

NAVY = (5, 7, 12)
PANEL = (13, 19, 30)
PANEL_LIGHT = (17, 24, 35)
BORDER = (30, 44, 62)
MINT = (141, 235, 196)
TEXT = (233, 240, 238)
MUTED = (140, 156, 160)

FONTS = os.path.join(os.environ.get("WINDIR", "C:\\Windows"), "Fonts")


def font(name: str, size: int) -> ImageFont.FreeTypeFont:
    try:
        return ImageFont.truetype(os.path.join(FONTS, name), size)
    except OSError:
        return ImageFont.load_default(size)


def centred(draw: ImageDraw.ImageDraw, box, text: str, face, fill) -> None:
    left, top, right, bottom = box
    bounds = draw.textbbox((0, 0), text, font=face)
    width = bounds[2] - bounds[0]
    height = bounds[3] - bounds[1]
    draw.text(
        (left + (right - left - width) / 2, top + (bottom - top - height) / 2 - bounds[1]),
        text,
        font=face,
        fill=fill,
    )


def tape(draw: ImageDraw.ImageDraw, left: int, top: int, cell: int, gap: int, symbols: str) -> None:
    """Draw a horizontal tape and the read/write head beneath one cell."""
    width = len(symbols) * cell + (len(symbols) + 1) * gap
    height = cell + 2 * gap
    draw.rounded_rectangle(
        (left, top, left + width, top + height),
        radius=18,
        fill=PANEL,
        outline=BORDER,
        width=2,
    )
    face = font("consola.ttf", max(18, int(cell * 0.52)))
    for index, symbol in enumerate(symbols):
        x = left + gap + index * (cell + gap)
        y = top + gap
        draw.rounded_rectangle(
            (x, y, x + cell, y + cell),
            radius=10,
            fill=PANEL_LIGHT,
            outline=BORDER,
            width=1,
        )
        centred(draw, (x, y, x + cell, y + cell), symbol, face, MINT)
    # The head sits under the third cell and points at it.
    head_index = 2
    x = left + gap + head_index * (cell + gap) + cell / 2
    y = top + height + 6
    draw.polygon(
        [(x - 12, y + 16), (x + 12, y + 16), (x, y)],
        fill=MINT,
    )
    draw.rounded_rectangle(
        (x - cell / 2, y + 16, x + cell / 2, y + 26),
        radius=5,
        fill=MINT,
    )


def build_og_image() -> str:
    width, height = 1200, 630
    image = Image.new("RGB", (width, height), NAVY)
    draw = ImageDraw.Draw(image)
    draw.rectangle((0, 0, width, 8), fill=MINT)

    draw.text((70, 96), "ONE MACHINE  /  41 LANGUAGES", font=font("consola.ttf", 20), fill=MINT)
    heading = font("segoeuib.ttf", 62)
    draw.text((70, 150), "Turing", font=heading, fill=TEXT)
    draw.text((70, 228), "Machines", font=heading, fill=MINT)
    draw.text((70, 306), "Showcase", font=heading, fill=TEXT)
    body = font("segoeui.ttf", 27)
    draw.text((70, 430), "The same state machine, written by hand", font=body, fill=MUTED)
    draw.text((70, 468), "in every language.", font=body, fill=MUTED)
    draw.text((70, 552), "turing.pattygcoding.com", font=font("consola.ttf", 24), fill=MINT)

    tape(draw, 704, 214, 62, 8, "aabbcc")
    draw.text((704, 360), "q0 -> q1 -> q2 -> q3", font=font("consola.ttf", 22), fill=MUTED)
    draw.text((704, 396), "accept / reject", font=font("consola.ttf", 22), fill=MUTED)

    output = os.path.join(PUBLIC, "og-image.png")
    image.save(output, optimize=True)
    return output


def main() -> int:
    os.makedirs(PUBLIC, exist_ok=True)
    print("wrote " + build_og_image())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
