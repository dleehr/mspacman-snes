sprite_bounce.smc: sprite_bounce.o memmap.cfg
	ld65 -C memmap.cfg -o sprite_bounce.smc sprite_bounce.o

sprite_bounce.o: sprite_bounce.s
	ca65 --cpu 65816 -s -o sprite_bounce.o sprite_bounce.s

.PHONY: clean

clean:
	rm -f sprite_bounce.o sprite_bounce.smc
