f: f.o
	ld -s -o $@ $<

f.o: f.asm
	nasm -f elf64 -o $@ $<

clean:
	rm *.o f
