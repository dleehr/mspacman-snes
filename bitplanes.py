#!/usr/bin/env python3

import sys

def load(char_file):
    with open(char_file, 'r') as f:
        rows = f.readlines()
        rows = [r.strip() for r in rows if r]
        return rows

def chunk8(rows):
    x = 0
    while x < len(rows):
        yield rows[x:x+8]
        x = x + 8

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

def write(rows, filename):
    # strip out empty rows
    rows = [r for r in rows if r]
    chunks = chunk8(rows)
    with open(filename, 'wb') as f:
        for chunk in chunks:
            print(chunk)
            for byte in to_plane(chunk, 3):
                f.write(byte)
            for byte in to_plane(chunk, 1):
                f.write(byte)

def check_args():
    if not len(sys.argv) == 3:
        print('Usage: ', sys.argv[0], 'input.chr output.vra')
        sys.exit(1)

def main():
    check_args()
    rows = load(sys.argv[1])
    write(rows, sys.argv[2])

if __name__ == '__main__':
    main()
