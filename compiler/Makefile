CC = ./compiler.lisp
ASM = ../assembler/65816asm

all: compiler;
clean:; rm -f *.fasl *.asm *.smc *~ compiler
install: compiler; ln -f compiler ~/bin/scc

../assembler/65816asm:; cd ../assembler; make
compiler: asm.lisp c.lisp c-impl.lisp compiler.lisp lib.lisp package.lisp Makefile
	sbcl --noinform \
             --eval "(defvar *script?* nil)" \
             --script "compiler.lisp"

c.asm: inputs/c $(CC); $(CC) < $< > $@
colors.asm: inputs/colors.asm; cp $< $@

%.smc: %.asm $(ASM); $(ASM) $<

colors: colors.smc; snes9x colors.smc
c: c.smc; snes9x c.smc
