64th: 64th.o
	ld -g -o $@ $<

64th.o: 64th.asm
	nasm -w+error -g -f elf64 -o $@ $<

node_modules/urchin/urchin:
	npm install urchin

test: node_modules/urchin/urchin 64th .PHONY
	$< test

clean:
	rm *.o 64th

.PHONY:
