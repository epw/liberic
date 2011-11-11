CFLAGS = -g -Wall `sdl-config --cflags` -L/usr/local/lib
LIBS = -lm -leric `sdl-config --libs` -lSDL_image -lSDL_gfx -lSDL_ttf -lSDL_mixer

all: libericsdl.so

libericsdl.so: ericsdl.o
	$(CC) $(CFLAGS) --shared -fpic ericsdl.c -c -lm
	$(CC) $(CFLAGS) --shared -fpic ericsdl.o -o libericsdl.so -lm

install: libericsdl.so
	cp ericsdl.h /usr/local/include/ericsdl.h
	cp libericsdl.so /usr/local/lib/libericsdl.so
	ldconfig

clean:
	rm -f *~ *.o
