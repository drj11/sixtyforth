f: f.o
	ld -g -o $@ $<

f.o: f.asm
	nasm -g -f elf64 -o $@ $<

node_modules/urchin/urchin:
	npm install urchin

test: node_modules/urchin/urchin f .PHONY
	$< test

clean:
	rm *.o f

.PHONY:
