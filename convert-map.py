#!/usr/bin/env python3

# Converts original map format (0 = blank, 1=side wall... 16=dot,)
# to a format where the wokkable tiles are <16 and the wall tiles start at 16

from bitplanes import load
import re
import sys

pat = r'((\d+)x)*(\d+)([HV]*)'

ROW_WIDTH = 32      # pad if less than this
EMPTY_TILE = '0'

def convert(piece):
    # translate 0,16,17 to 0, 1,2
    # translate 1-15 to 16-31
    if piece == 0:
        return piece;
    elif piece == 16:
        return 1
    elif piece == 17:
        return 2
    else:
        return piece + 15

def handle_tile(tile):
    vals = re.search(pat, tile)
    # Group 2 is the repeats
    # Group 3 is the tile
    # group 4 is h flip
    # group 5 is v lip
    piece = int(vals.group(3))
    repeat = int(vals.group(2) or 1)
    flips = vals.group(4)
    h_flip = 'H' in flips
    v_flip = 'V' in flips
    # convert the piece
    piece = convert(piece)
    if repeat > 1:
        return '{}x{}{}'.format(repeat, piece, flips)
    else:
        return '{}{}'.format(piece, flips)

def write(rows, filename):
    with open(filename, 'w') as f:
        for idx, row in enumerate(rows):
            tiles = [handle_tile(tile) for tile in row.split()]
            row = ' '.join(tiles)
            print(row, file=f)

def check_args():
    if not len(sys.argv) == 3:
        print('Usage: ', sys.argv[0], 'input.map output.map')
        sys.exit(1)

def main():
    check_args()
    rows = load(sys.argv[1])
    write(rows, sys.argv[2])

if __name__ == '__main__':
    main()
