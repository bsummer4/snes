CC = ./compiler.lisp
ASM = ../assembler/65816asm

all: compiler;
clean:; rm -f *.fasl *.asm *.smc *~ compiler
../assembler/65816asm:; cd ../assembler; make
test: $(ASM)
	$(CC) < inputs/c > c.asm
	$(ASM) c.asm

compiler: asm.lisp c.lisp compiler.lisp lib.lisp package.lisp Makefile
	sbcl --noinform \
             --eval "(defvar *script?* nil)" \
             --script "compiler.lisp"
