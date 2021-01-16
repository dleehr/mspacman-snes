#!/usr/bin/env python3

# first, colors for ms pac-man
# 00 000000 black
# 01 fffb01 yellow
# 02 ff2500 bow red
# 03 202bde eye blue
# 04..0f placeholders
# then, colors for background
# 10 000000 black (again - do we need this again?)
# 11 fb0007 red for border
# 12 fda985 pink inner border
# 13 d6d6d6 white dots

colors = """
000000
fffb01
ff2500
202bde
000000
000000
000000
000000
000000
000000
000000
000000
000000
000000
000000
000000
000000
fb0007
fda985
d6d6d6
""".splitlines()

def to_components(hex_color):
    return hex_color[0:2], hex_color[2:4], hex_color[4:6]

def fivebits(byte):
    return "{0:05b}".format(byte)

def five_msb(byte_str):
    byte = int(byte_str, 16)
    byte = int(byte / 8)
    return byte

with open('SpriteColors.pal', 'wb') as f:
    for color in colors:
        if not color:
            continue
        # split apart 123456 to [12,34,56]
        comps = to_components(color)
        # get the 5 msb of each component and recompose to a 15-bit
        joined = ''.join([fivebits(five_msb(f)) for f in comps[::-1]])
        # now parse back to int

        rgb555 = int(joined, 2)
        print(hex(rgb555))
        to_write = rgb555.to_bytes(2, byteorder='little')
        f.write(to_write)
# take the 5 most significant bits
# of each color component and reverse their order
