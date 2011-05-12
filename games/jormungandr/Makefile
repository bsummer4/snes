data_files  = JormungandrChar.dat JormungandrColor.dat JormungandrFloorMap.dat
data_files += JormungandrFrameMap.dat JormungandrTrailMap.dat
AC = ../assembler/65816asm

all: jormungandr.smc
$(AC):; cd ../assembler; make
%.smc: %.asm $(data_files) $(AC); $(AC) $<
clean:; rm -f jormungandr.smc
test: all; snes9x jormungandr.smc
