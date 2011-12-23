CFLAGS = -g -Wall
LIBS = -lm

ROOT = /usr/local

all: liberic.so

liberic.so: eric.o
	$(CC) $(CFLAGS) --shared -fpic eric.o -o liberic.so -lm

eric.o: eric.c
	$(CC) $(CFLAGS) --shared -fpic eric.c -c -lm


install: liberic.so
	cp eric.h $(ROOT)/include/eric.h
	cp liberic.so $(ROOT)/lib/liberic.so
	ldconfig

clean:
	rm -f *.o *~
