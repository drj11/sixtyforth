64th: 64th.o rc.o
	ld -g -o $@ $^

rc.o: rc.4
	objcopy --input binary --output elf64-x86-64 --binary-architecture i386:x86-64 $< $@

64th.o: 64th.asm
	nasm -w+error -g -f elf64 -o $@ -l ignore/listing $<

node_modules/urchin/urchin:
	npm install urchin

test: node_modules/urchin/urchin 64th .PHONY
	$< test

clean:
	rm *.o 64th

.PHONY:
