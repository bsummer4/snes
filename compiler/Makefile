CC = ./compiler.lisp
ASM = ../assembler/65816asm

all: compiler;
clean:; rm -f *.fasl *.asm *.smc *~ compiler

../assembler/65816asm:; cd ../assembler; make
compiler: asm.lisp c.lisp compiler.lisp lib.lisp package.lisp Makefile
	sbcl --noinform \
             --eval "(defvar *script?* nil)" \
             --script "compiler.lisp"

test: $(ASM)
	cp inputs/colors.asm .
	$(ASM) colors.asm
	$(CC) < inputs/c > c.asm
	$(ASM) c.asm

colors: test; snes9x colors.smc
c: test; snes9x c.smc
run: c colors
