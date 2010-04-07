data_files =  Phoenix_Color.dat Phoenix_Char.dat  Phoenix_Frame_BG_Map.dat
data_files += Phoenix_Star_BG_Map.dat
AC = ../assembler/65816asm

all: phoenix.smc
$(AC):; cd ../assembler; make
phoenix.smc: $(data_files) phoenix.asm $(AC); $(AC) phoenix.asm
clean:; rm -f phoenix.smc
test: all; snes9x phoenix.smc
