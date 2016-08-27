f: f.o
	ld -g -o $@ $<

f.o: f.asm
	nasm -g -f elf64 -o $@ $<

clean:
	rm *.o f
