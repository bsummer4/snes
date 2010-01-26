CC = gcc
CFLAGS = -D__DEBUG__ -g3
65616asm: main.o; $(CC) $^ -o $@
main.o: main.c assembler.h data.h; $(CC) -c $<
clean:; rm -f *.o 65816asm
