CC = ./compiler.lisp
ASM = ../assembler/65816asm

all:;
clean:; rm -f *.asm *.smc
../assembler/65816asm:; cd ../assembler; make
test: $(ASM)
	$(CC) < asm-input > asm-input.asm
	$(CC) < c-input > c-input.asm
	$(ASM) c-input.asm
	$(ASM) asm-input.asm
