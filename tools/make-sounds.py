#!/usr/bin/env python3
"""make-sounds.py -- synthesise res/sounds/*.wav, short mono 22 kHz effects.

    python3 tools/make-sounds.py
"""
import math, os, random, struct, wave

RATE = 22050
OUT = os.path.join(os.path.dirname(__file__), "..", "res", "sounds")
random.seed(7)


def write(name, samples):
    os.makedirs(OUT, exist_ok=True)
    with wave.open(os.path.join(OUT, name + ".wav"), "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(RATE)
        peak = max(1e-9, max(abs(s) for s in samples))
        w.writeframes(b"".join(struct.pack("<h", int(32000 * 0.8 * s / peak)) for s in samples))


def noise_burst(seconds, decay, lowpass):
    out, y = [], 0.0
    for i in range(int(RATE * seconds)):
        t = i / RATE
        y += lowpass * (random.uniform(-1, 1) - y)
        out.append(y * math.exp(-t * decay))
    return out


def tone(freqs, seconds, decay=4.0):
    out = []
    for i in range(int(RATE * seconds)):
        t = i / RATE
        out.append(sum(math.sin(2 * math.pi * f * t) for f in freqs) * math.exp(-t * decay))
    return out


def sweep(f0, f1, seconds, decay):
    out, phase = [], 0.0
    n = int(RATE * seconds)
    for i in range(n):
        t = i / RATE
        f = f0 + (f1 - f0) * i / n
        phase += 2 * math.pi * f / RATE
        out.append(math.sin(phase) * math.exp(-t * decay))
    return out


def mix(*parts):
    n = max(len(p) for p in parts)
    return [sum(p[i] for p in parts if i < len(p)) for i in range(n)]


write("move", mix(noise_burst(0.12, 30, 0.15), [0.4 * s for s in sweep(220, 160, 0.12, 25)]))
write("shot", mix(noise_burst(0.18, 28, 0.6), [0.5 * s for s in sweep(900, 200, 0.12, 30)]))
write("hit", mix(noise_burst(0.30, 14, 0.35), [0.6 * s for s in sweep(300, 90, 0.25, 12)]))
write("boom", mix(noise_burst(0.9, 5, 0.08), [0.8 * s for s in sweep(110, 35, 0.8, 4)]))
write("rumble", mix(noise_burst(0.8, 2.5, 0.03), [0.5 * s for s in tone([42, 55], 0.8, 2.5)]))
write("won", [s for f in (523, 659, 784, 1046) for s in tone([f, f * 2.01], 0.16, 6)])
write("lost", [s for f in (392, 311, 247, 196) for s in tone([f, f * 1.5], 0.22, 5)])
print("wrote", sorted(os.listdir(OUT)))
