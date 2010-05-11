CC = ./compiler.lisp
ASM = ../assembler/65816asm
DAT = pongChar.dat pongFrame.dat pongPallet.dat
LISP = asm.lisp back.lisp c-analize.lisp c-impl.lisp compiler.lisp
LISP += far-back.lisp front.lisp lib.lisp macroexpand-dammit.lisp package.lisp
LISP += state.lisp tag.lisp toplevel.lisp

all: compiler;
clean:; rm -f *.fasl *.asm *.smc *~ compiler reset.asm snes_registers.h *.dat
install: compiler; ln -f compiler ~/bin/scc

../assembler/65816asm:; cd ../assembler; make
compiler: $(LISP) Makefile
	sbcl --noinform \
             --eval "(defvar *script?* nil)" \
             --script "compiler.lisp"

reset.asm:; cp -f inputs/reset.asm .
snes_registers.h:; cp -f inputs/snes_registers.h .
c.asm: inputs/c $(CC) reset.asm snes_registers.h $(DAT)
	$(CC) < $< > $@
colors.asm: inputs/colors.asm; cp $< $@

%.smc: %.asm $(ASM); $(ASM) $<
%.dat: inputs/%.dat; cp $< $@

colors: colors.smc; snes9x colors.smc
c: c.smc
	cp inputs/c c.asm c.smc /home/ben/tmp/hydra-parker/www-home/hacking
	snes9x c.smc
