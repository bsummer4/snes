CC = gcc
CFLAGS = -D__DEBUG__ -g3 -std=gnu99 -W -Wall -Werror -pedantic
HSFLAGS = -O3

all: 65816asm pre
clean:; rm -f *.hi *.o 65816asm pre readme.html
docs: readme.html

65816asm: main.o; $(CC) $(CFLAGS) $^ -o $@
pre: pre.hs; ghc $(HSFLAGS) $^ -o $@
main.o: main.c assembler.h data.h; $(CC) $(CFLAGS) -c $<
readme.html: README; markdown < $^ > $@
