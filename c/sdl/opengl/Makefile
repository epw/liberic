CFLAGS = -g -Wall `sdl-config --cflags`
LIBS = -lm -leric -lericsdl `sdl-config --libs` \
	-lSDL_image -lSDL_gfx -lSDL_ttf -lSDL_mixer -lglut -lGLU -lGL

ROOT=/usr/local

all: libericsdlgl.so

libericsdlgl.so: ericsdlgl.o
	$(CC) $(CFLAGS) --shared -fpic ericsdlgl.o -o libericsdlgl.so -lm

ericsdlgl.o: ericsdlgl.c
	$(CC) $(CFLAGS) --shared -fpic ericsdlgl.c -c -lm

install: libericsdlgl.so
	cp ericsdlgl.h $(ROOT)/include/ericsdlgl.h
	cp libericsdlgl.so $(ROOT)/lib/libericsdlgl.so
	ldconfig
