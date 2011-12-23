CFLAGS = -g -Wall `sdl-config --cflags`
LIBS = -lm -leric -lglut -lGLU -lGL

ROOT = /usr/local

all: libericgl.so

libericgl.so: ericgl.o
	$(CC) $(CFLAGS) --shared -fpic ericgl.o -o libericgl.so $(LIBS)

ericgl.o: ericgl.c
	$(CC) $(CFLAGS) --shared -fpic ericgl.c -c $(LIBS)

install: libericgl.so
	cp ericgl.h $(ROOT)/include/ericgl.h
	cp libericgl.so $(ROOT)/lib/libericgl.so
	ldconfig
