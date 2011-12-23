/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 *
 * This library contains a variety of functions which have been found
 * to be useful when writing in C. It also ensures that a large number
 * of header files are included by default.
 *
 * MACROS:
 * DTOR(x) - Convert degrees to radians
 * RTOD(x) - Convert radians to degrees
 *
 * FUNCTIONS:
 * get_time (): Return the double-precision number of seconds since
 *              the epoch.
 * mkrand (): Return a pseudorandom integer in the range
 *            [minval, maxval]. 
 * open_read (): Open file for reading. If quit != 0, then use quit as
 *               exit code if fopen() returns NULL.
 * open_write (): As open_read() but with mode "w".
 * open_append (): As open_read() but with mode "a".
 * square (): Return square of n.
 * cube (): Return cube of n.
 * lscale (): Scale value from the range [from_left, from_right] to
 *            the range [to_left, to_right]. Out-of-bounds values are
 *            unaltered.
 * lscale_clamp (): As lscale(), but out-of-bounds values are
 *                  normallized to closest of lower or higher bounds.
 * make_arrayi (): Allocate and return array of integers with n
 *                 elements, using following arguments as initial
 *                 values.
 * make_arrayf (): As make_arrayi(), but array is of floats.
 * make_arrayd (): As make_arrayi(), but array is of doubles.
 * signi (): Return sign of argument as -1 or 1 (0 returns 1).
 * signd (): As signi(), but takes and returns a double.
 * find_line (): Search given open file for a line equivalent to the
 *               given string. Returns line number or -1. May not
 *               work if lines are longer than _ERIC_H_MAX_LINE_LENGTH
 * find_string (): Search given open file for given string. Returns
 *                 character of string start or -1. May not work if
 *                 lines are longer than _ERIC_H_MAX_LINE_LENGTH
 * strsplit (): Split given string on deliminator, allocating new
 *              array of character arrays to store the pieces found.
 * xmalloc (): As malloc(), but immediately prints to stderr and
 *             aborts if malloc() returned NULL.
 * xcalloc (): As calloc(), but immediately prints to stderr and
 *             aborts if calloc() returned NULL.
 * xrealloc (): As realloc(), but immediately prints to stderr and
 *             aborts if realloc() returned NULL.
 */

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

#define DTOR(x) ((x) * M_PI / 180)
#define RTOD(x) ((x) * 180 / M_PI)

double get_time (void);
int mkrand (int minval, int maxval);

FILE *open_read (char *filename, int quit);
FILE *open_write (char *filename, int quit);
FILE *open_append (char *filename, int quit);

double square (double n);
double cube (double n);

double lscale (double val, double from_left, double from_right,
	       double to_left, double to_right);
double lscale_clamp (double val, double from_left, double from_right,
		     double to_left, double to_right);

int *make_arrayi (int n, ...);
float *make_arrayf (int n, ...);
double *make_arrayd (int n, ...);

int signi (int n);
double signd (double n);

#define _ERIC_H_MAX_LINE_LENGTH 1024
int find_line (FILE *f, char *s);
int find_string (FILE *f, char *s);

char **strsplit (char *string, char *delim);

void *xcalloc (size_t nmemb, size_t size);
void *xmalloc (size_t size);
void *xrealloc (void *ptr, size_t size);

#endif
