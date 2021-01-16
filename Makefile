mspacman.smc: mspacman.o memmap.cfg
	ld65 -C memmap.cfg -o mspacman.smc mspacman.o

mspacman.o: mspacman.s
	ca65 --cpu 65816 -s -o mspacman.o mspacman.s

.PHONY: clean

clean:
	rm -f mspacman.o mspacman.smc
