CFLAGS = -g -Wall `sdl-config --cflags`
LIBS = -lm -leric -lglut -lGLU -lGL

all: libericgl.so

libericgl.so: ericgl.o
	$(CC) $(CFLAGS) --shared -fpic ericgl.c -c $(LIBS)
	$(CC) $(CFLAGS) --shared -fpic ericgl.o -o libericgl.so $(LIBS)

install: libericgl.so
	cp ericgl.h /usr/local/include/ericgl.h
	cp libericgl.so /usr/local/lib/libericgl.so
	ldconfig
