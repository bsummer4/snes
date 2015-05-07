AC = ../assembler/65816asm
SC = ../compiler/compiler

all: flashing.smc
clean:; rm -f flashing.smc flashing.asm
test: all; snes9x flashing.smc

$(AC):; cd ../assembler; make
$(SC):; cd ../compiler; make
%.smc: %.asm $(AC); $(AC) $<
%.asm: %.c.lisp $(SC); $(SC) < $< > $@
