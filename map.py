#!/usr/bin/env python3

from bitplanes import load
import re
import sys

pat = r'((\d)x)*(\d)([HV]*)'

def handle_tile(tile):
#     print(tile)
    vals = re.search(pat, tile)
    # Group 2 is the count
    # Group 3 is the tile
    # group 4 is h flip
    # group 5 is v lip
    piece = int(vals.group(3))
    repeat = int(vals.group(2) or 1)
    h_flip = 'H' in vals.group(4)
    v_flip = 'V' in vals.group(4)

#     components = tile.split('x')
#     piece = int(components[0].)
#     repeat = 0
#     h_flip = 'H' in tile.upper()
#     v_flip = 'V' in tile.upper()
#
    print(piece, repeat, h_flip, v_flip)

def write(rows, filename):
    for row in rows:
        for tile in row.split():
            handle_tile(tile)


def check_args():
    if not len(sys.argv) == 3:
        print('Usage: ', sys.argv[0], 'input.map output.tlm')
        sys.exit(1)

def main():
    check_args()
    rows = load(sys.argv[1])
    write(rows, sys.argv[2])

if __name__ == '__main__':
    main()
