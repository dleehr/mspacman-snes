#!/usr/bin/env python3

from bitplanes import load
import re
import sys

pat = r'((\d+)x)*(\d+)([HV]*)'

ROW_WIDTH = 32      # pad if less than this
EMPTY_TILE = '0'

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
#     print('tile', piece, 'repeat', repeat, 'H:', h_flip, 'V:', v_flip)
#    ; high vhopppcc
#    ; low cccccccc
    # low byte is just the piece number
    # If piece is > 10 bits we fail
    if piece > 1 << 10:
        raise Exception('Piece index too large: {}'.format(piece))
    low_byte = piece
    high_byte = piece >> 8 | v_flip << 7 | h_flip << 6
    for x in range(repeat):
        word = high_byte << 8 | low_byte
        print('{0:08b} {1:08b} ${2:04x} {3:03d} {4:1s}{5:1s} {6:02d}'.format(high_byte, low_byte, word, piece, 'H' if h_flip else '', 'V' if v_flip else '', x))
        # Check this ordering
        yield word.to_bytes(2, byteorder='little')

def write(rows, filename):
    print('vhopppcc cccccccc $WORD TIL HV RP')
    with open(filename, 'wb') as f:
        for idx, row in enumerate(rows):
            tiles_written = 0
            for tile in row.split():
                for word in handle_tile(tile):
                    f.write(word)
                    tiles_written = tiles_written + 1
            if tiles_written < ROW_WIDTH:
                # make some empty tiles
                padding_string = '{}x{}'.format(ROW_WIDTH - tiles_written, EMPTY_TILE)
                for word in handle_tile(padding_string):
                    f.write(word)
            print(' Row {:02d}: {} / {} ='.format(idx, tiles_written, ROW_WIDTH).rjust(33, '='))

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
