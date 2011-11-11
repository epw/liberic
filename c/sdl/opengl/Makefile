CFLAGS = -g -Wall `sdl-config --cflags`
LIBS = -lm -leric -lericsdl `sdl-config --libs` \
	-lSDL_image -lSDL_gfx -lSDL_ttf -lSDL_mixer -lglut -lGLU -lGL

all: libericsdlgl.so

libericsdlgl.so: ericsdlgl.o
	$(CC) $(CFLAGS) --shared -fpic ericsdlgl.c -c -lm
	$(CC) $(CFLAGS) --shared -fpic ericsdlgl.o -o libericsdlgl.so -lm

install: libericsdlgl.so
	cp ericsdlgl.h /usr/local/include/ericsdlgl.h
	cp libericsdlgl.so /usr/local/lib/libericsdlgl.so
	ldconfig
