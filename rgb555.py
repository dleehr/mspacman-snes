#!/usr/bin/env python3

colors = """
000000
fffb01
ff2500
202bde
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
