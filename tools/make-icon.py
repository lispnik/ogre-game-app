#!/usr/bin/env python3
"""make-icon.py -- write res/Behemoth.xcassets: a field of dark hexes, the
red Behemoth in the middle and three blue defenders below it.  1024x1024,
no alpha, 3x3 supersampled.  Slow (minutes): pure Python, no PIL.

    python3 tools/make-icon.py
"""
import json
import math
import os
import struct
import zlib

SIZE = 1024
SS = 3  # supersamples per axis
BG = (20, 27, 23)


def hexagon(cx, cy, r) -> list[tuple[float, float]]:
    return [(cx + r * math.cos(i * math.pi / 3), cy + r * math.sin(i * math.pi / 3)) for i in range(6)]


def inside(poly, x, y):
    hit = False
    j = len(poly) - 1
    for i in range(len(poly)):
        xi, yi = poly[i]
        xj, yj = poly[j]
        if (yi > y) != (yj > y) and x < (xj - xi) * (y - yi) / (yj - yi) + xi:
            hit = not hit
        j = i
    return hit


shapes = []  # (kind, geometry, colour), painted in order


def poly(points, colour):
    shapes.append(("poly", points, colour))


def disc(cx, cy, r, colour):
    shapes.append(("disc", (cx, cy, r), colour))


# The hex field.
R = 92
H = math.sqrt(3) * R
for col in range(-1, 9):
    for row in range(-1, 8):
        cx = col * 1.5 * R
        cy = row * H + (H / 2 if col % 2 else 0)
        poly(hexagon(cx, cy, R - 5), (36, 50, 40) if (col + row) % 3 else (44, 60, 47))

# The Behemoth, upper middle.
bx, by = 512, 420
poly(hexagon(bx, by, 250), (255, 120, 80))
poly(hexagon(bx, by, 236), (128, 26, 20))
poly(hexagon(bx, by, 150), (76, 15, 12))
for dx, dy in ((-150, -90), (150, -90), (-150, 90), (150, 90)):
    disc(bx + dx, by + dy, 34, (245, 128, 76))
poly([(bx - 24, by), (bx + 24, by), (bx + 24, by - 260), (bx - 24, by - 260)], (205, 64, 40))
disc(bx, by, 76, (166, 38, 26))

# Three defenders.
for dx in (-300, 0, 300):
    cx, cy = 512 + dx, 850 if dx else 880
    poly(hexagon(cx, cy, 112), (26, 51, 87))
    poly(hexagon(cx, cy, 100), (77, 133, 199))
    poly([(cx - 52, cy + 10), (cx + 52, cy + 10), (cx + 44, cy - 34), (cx - 44, cy - 34)], (209, 230, 255))
    poly([(cx - 4, cy - 30), (cx + 4, cy - 30), (cx + 4, cy - 100), (cx - 4, cy - 100)], (209, 230, 255))


def colour_at(x, y):
    c = BG
    for kind, g, colour in shapes:
        if kind == "poly":
            xs = [p[0] for p in g]
            ys = [p[1] for p in g]
            if min(xs) <= x <= max(xs) and min(ys) <= y <= max(ys) and inside(g, x, y):
                c = colour
        else:
            cx, cy, r = g
            if (x - cx) ** 2 + (y - cy) ** 2 <= r * r:
                c = colour
    return c


rows = []
for py in range(SIZE):
    row = bytearray(b"\x00")
    for px in range(SIZE):
        acc = [0, 0, 0]
        for sy in range(SS):
            for sx in range(SS):
                c = colour_at(px + (sx + 0.5) / SS, py + (sy + 0.5) / SS)
                acc[0] += c[0]; acc[1] += c[1]; acc[2] += c[2]
        n = SS * SS
        row += bytes((acc[0] // n, acc[1] // n, acc[2] // n))
    rows.append(bytes(row))


def chunk(kind, data):
    body = kind + data
    return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)


here = os.path.dirname(os.path.abspath(__file__))
catalogue = os.path.join(here, "..", "res", "Behemoth.xcassets")
iconset = os.path.join(catalogue, "AppIcon.appiconset")
os.makedirs(iconset, exist_ok=True)
with open(os.path.join(iconset, "icon.png"), "wb") as out:
    _ = out.write(b"\x89PNG\r\n\x1a\n")
    _ = out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", SIZE, SIZE, 8, 2, 0, 0, 0)))
    _ = out.write(chunk(b"IDAT", zlib.compress(b"".join(rows), 9)))
    _ = out.write(chunk(b"IEND", b""))
info = {"author": "behemoth", "version": 1}
with open(os.path.join(catalogue, "Contents.json"), "w") as f:
    json.dump({"info": info}, f, indent=2)
with open(os.path.join(iconset, "Contents.json"), "w") as f:
    json.dump({"images": [{"filename": "icon.png", "idiom": "universal", "platform": "ios",
                           "size": "1024x1024"}], "info": info}, f, indent=2)
print("wrote", os.path.join(iconset, "icon.png"))
