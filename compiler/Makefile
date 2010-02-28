CC = ./compiler.lisp
ASM = ../assembler/65816asm

all:;
clean:; rm -f *.asm *.smc *~
../assembler/65816asm:; cd ../assembler; make
test: $(ASM)
	$(CC) < inputs/c > c.asm
	$(ASM) c.asm
