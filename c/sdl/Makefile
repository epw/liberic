CFLAGS = -g -Wall `sdl-config --cflags` -L/usr/local/lib
LIBS = -lm -leric `sdl-config --libs` -lSDL_image -lSDL_gfx -lSDL_ttf -lSDL_mixer

ROOT = /usr/local

all: libericsdl.so

libericsdl.so: ericsdl.o
	$(CC) $(CFLAGS) --shared -fpic ericsdl.o -o libericsdl.so -lm

ericsdl.o: ericsdl.c
	$(CC) $(CFLAGS) --shared -fpic ericsdl.c -c -lm

install: libericsdl.so
	cp ericsdl.h $(ROOT)/include/ericsdl.h
	cp libericsdl.so $(ROOT)/lib/libericsdl.so
	ldconfig

clean:
	rm -f *~ *.o
