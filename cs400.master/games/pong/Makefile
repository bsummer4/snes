data_files  =
AC = ../assembler/65816asm

all: pong.smc
test: all; snes9x pong.smc
clean:; rm -f pong.smc

%.asm: %.c.lisp $(CC); $(CC) < $< > $@
%.smc: %.asm $(data_files) $(AC); $(AC) $<
$(AC):; cd ../assembler; make
