64th.exe: 64th.obj rc.obj windows.obj
	link $^ /ENTRY:_start /LARGEADDRESSAWARE:NO /SUBSYSTEM:console /map /section:stack,WR /section:rstack,WR /section:.data,WRE kernel32.lib  /OUT:$@

rc.obj: rc.4
	objcopy -I binary -O pe-x86-64 -B i386:x86-64  $^ $@

64th.obj: 64th.asm
	nasm -g -dPLATFORM_DEF=1 -f win64 -o $@ $< -l 64th.lst


windows.obj: windows.asm
	nasm -g -f win64 -o $@ $< -l ignore/windows.lst

node_modules/urchin/urchin:
	npm install urchin

test: node_modules/urchin/urchin 64th .PHONY
	$< test

clean:
	rm *.obj 64th.exe

.PHONY: