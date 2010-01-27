CC = gcc
CFLAGS = -D__DEBUG__ -g3 -std=gnu99 -W -Wall -Werror
65816asm: main.o; $(CC) $(CFLAGS) $^ -o $@
main.o: main.c assembler.h data.h; $(CC) $(CFLAGS) -c $<
clean:; rm -f *.o 65816asm
