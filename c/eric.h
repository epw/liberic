/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details */

#ifndef _ERIC_H_
#define _ERIC_H_

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <memory.h>
#include <time.h>
#include <sys/time.h>
#include <fcntl.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <stddef.h>
#include <termio.h>
#include <termios.h>
#include <errno.h>
#include <stdarg.h>
#include <error.h>
#include <assert.h>

/* Macros to convert numbers from Degrees TO Radians and from
 * Radians TO Degrees. M_PI constant is used, so number will be
 * promoted to double-precision. */
#define DTOR(x) ((x) * M_PI / 180)
#define RTOD(x) ((x) * 180 / M_PI)

double get_time (void);
int mkrand (int minval, int maxval);

int open_read (FILE *f, char *filename, int quit);
int open_write (FILE *f, char *filename, int quit);

double square (double n);

double lscale (double val, double from_left, double from_right,
	       double to_left, double to_right);
double lscale_clamp (double val, double from_left, double from_right,
		     double to_left, double to_right);

double cube (double n);

int *make_arrayi (int n, ...);
float *make_arrayf (int n, ...);
double *make_arrayd (int n, ...);

int signi (int n);
int signd (double n);

char *find_line (FILE *f, char *s);
char *find_string (FILE *f, char *s);

char **strsplit (char *string, char *delim);

void *xcalloc (size_t nmemb, size_t size);
void *xmalloc (size_t size);
void *xrealloc (void *ptr, size_t size);

#endif
