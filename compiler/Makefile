CC = ./compiler.lisp
ASM = ../assembler/65816asm

all: compiler;
clean:; rm -f *.fasl *.asm *.smc *~ compiler

../assembler/65816asm:; cd ../assembler; make
compiler: asm.lisp c.lisp compiler.lisp lib.lisp package.lisp Makefile
	sbcl --noinform \
             --eval "(defvar *script?* nil)" \
             --script "compiler.lisp"

c.asm: inputs/c compiler; $(CC) < $< > $@
colors.asm: inputs/colors.asm compiler; cp $< $@
test: c.asm colors.asm $(ASM)
	$(ASM) colors.asm
	$(ASM) c.asm

colors: test; snes9x colors.smc
c: test; snes9x c.smc
run: c colors
