CFLAGS = -Wall -Wextra -ggdb -pg --coverage
LIBS = -lm
.PHONY: clean all

all: $(patsubst %.c,%.exe,$(wildcard *.c))

clean:
	rm -f *.exe *.hi *.o

%.exe : %.c
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

%.exe : %.hs
	ghc -dynamic -o $@ $^
