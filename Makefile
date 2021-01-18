mspacman.smc: mspacman.o memmap.cfg
	ld65 -C memmap.cfg -o mspacman.smc mspacman.o

mspacman.o: mspacman.s Sprites.vra mspacman.pal level1.pal Walls.vra
	ca65 --cpu 65816 -s -o mspacman.o mspacman.s

Sprites.vra: bitplanes.py mspacman.chr
	python3 bitplanes.py mspacman.chr Sprites.vra

Walls.vra: bitplanes.py walls.chr
	python3 bitplanes.py walls.chr Walls.vra

mspacman.pal: rgb555.py mspacman.pxt
	python3 rgb555.py mspacman.pxt mspacman.pal

level1.pal: rgb555.py level1.pxt
	python3 rgb555.py level1.pxt level1.pal

.PHONY: clean

clean:
	rm -f mspacman.o mspacman.smc Sprites.vra mspacman.pal level1.pal Walls.vra
