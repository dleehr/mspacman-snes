#!/usr/bin/env python3

# 0 1
# 2 3

rows = """
00000000
00002200
00002221
00023221
02232111
02221110
00221103
00111111

00000000
00000000
11100000
11111000
11111100
01112200
11000000
00000000

00111100
00111111
00111011
00011111
00011111
00001111
00000011
00000000

00000000
00000000
11000000
11112200
11111100
11111000
11100000
00000000
""".splitlines()

rows = [r for r in rows if r]

def chunk8(rows):
    x = 0
    while x < len(rows):
        yield rows[x:x+8]
        x = x + 8

chunks = chunk8(rows)

def fourbits(byte):
    return "{0:04b}".format(int(byte, 16))

def to_plane(rows, start):
    for row in rows:
        if not row:
            continue
        plane = [] # will have same as number of rows
        for byte in row:
            bits = fourbits(byte)
            plane.append(bits)
        for i in (0, -1):
            bitplane = ''.join([x[start + i] for x in plane])
            byte = int(bitplane, 2)
            yield byte.to_bytes(1, byteorder='little')

with open('Sprites.vra', 'wb') as f:
    for chunk in chunks:
        print(chunk)
        for byte in to_plane(chunk, 3):
            f.write(byte)
        for byte in to_plane(chunk, 1):
            f.write(byte)
