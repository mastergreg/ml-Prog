LIBS=-lm
EXTRAS=-Wall -Wextra -O3
all:	magic
convert.o:	convert.c convert.h
	g++ ${LIBS} -c convert.c ${EXTRAS}
magic:	convert.o magic.c
	g++ ${LIBS} convert.o magic.c -o magic ${EXTRAS}
clean:
	rm magic convert.o
