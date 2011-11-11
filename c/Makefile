CFLAGS = -g -Wall
LIBS = -lm

all: liberic.so

liberic.so:
	$(CC) $(CFLAGS) --shared -fpic eric.c -c -lm
	$(CC) $(CFLAGS) --shared -fpic eric.o -o liberic.so -lm

install: liberic.so
	cp eric.h /usr/local/include/eric.h
	cp liberic.so /usr/local/lib/liberic.so
	ldconfig

clean:
	rm -f *.o *~
