/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details */
#include "eric.h"

/* Return a double-precision representation of the current number of
 * seconds since the epoch. */
double
get_time (void)
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return (tv.tv_sec + tv.tv_usec / 1000000.0);
}

/* Return a pseudorandom integer from the interval [minval, maxval]. */
int
mkrand (int minval, int maxval)
{
	maxval++;
	return (floor ((double)random () / RAND_MAX
		       * (maxval - minval) + minval));
}

/* Convenience functions to open a file for reading or writing, given
 * a pointer to a file pointer, and the name.
 *
 * Returns 1 on success. If quit is 0, returns 0 on failure. Otherwise,
 * exits with return code equal to quit on failure. */
int
open_read (FILE **f, char *filename, int quit)
{
	if (((*f) = fopen (filename, "r")) == NULL) {
		printf ("Error. Can't open %s\n", filename);
		if (quit)
			exit (quit);
		else
			return 0;
	}
	return 1;
}
int
open_write (FILE **f, char *filename, int quit)
{
	if (((*f) = fopen (filename, "w")) == NULL) {
		printf ("Error. Can't open %s\n", filename);
		if (quit)
			exit (quit);
		else
			return 0;
	}
	return 1;
}

double
square (double n)
{
	return n * n;
}

/* Linearlly interpolate value from initial upper and lower bounds to
 * given upper and lower bounds. Returns to_right if
 * from_left == from_right. */
double
lscale (double val,
	double from_left, double from_right,
	double to_left, double to_right)
{
	if (from_left == from_right)
		return (to_right);

	return ((val - from_left)
		/ (from_right - from_left)
		* (to_right - to_left)
		+ to_left);
}

/* As lscale(), but returned value is guaranteed to be in the range
 * [to_left, to_right]. */
double
lscale_clamp (double val,
	double from_left, double from_right,
	double to_left, double to_right)
{
	double x;

	if (from_left == from_right)
		return (to_right);

	x = (val - from_left)
		/ (from_right - from_left)
		* (to_right - to_left)
		+ to_left;
	if (to_left < to_right) {
		if (x < to_left)
			x = to_left;
		if (x > to_right)
			x = to_right;
	} else {
		if (x < to_right)
			x = to_right;
		if (x > to_left)
			x = to_left;
	}
	return (x);

}

double
cube (double n)
{
	return n * n * n;
}

/* Use stdarg to dynamically allocate new array of n integer, floats,
 * or doubles. Arguments following n are used to initialize array.
 * Returns NULL on error. */
int *
make_arrayi (int n, ...)
{
	int i;
	int *array;
	va_list ap;

	array = calloc (n, sizeof *array);
	if (array == NULL) {
		return NULL;
	}
	va_start (ap, n);

	for (i = 0; i < n; i++) {
		array[i] = va_arg (ap, int);
	}
	va_end (ap);

	return array;
}
float *
make_arrayf (int n, ...)
{
	int i;
	float *array;
	va_list ap;

	array = calloc (n, sizeof *array);
	if (array == NULL) {
		return NULL;
	}
	va_start (ap, n);

	for (i = 0; i < n; i++) {
		array[i] = va_arg (ap, double);
	}
	va_end (ap);

	return array;
}
double *
make_arrayd (int n, ...)
{
	int i;
	double *array;
	va_list ap;

	array = calloc (n, sizeof *array);
	if (array == NULL) {
		return NULL;
	}

	va_start (ap, n);

	for (i = 0; i < n; i++) {
		array[i] = va_arg (ap, double);
	}
	va_end (ap);

	return array;
}

/* Return the sign of an integer or double, as 1 or -1. Treats 0
 * as positive. */
int
signi (int n)
{
	if (n < 0)
		return -1;
	return 1;
}
int
signd (double n)
{
	if (n < 0)
		return -1.0;
	return 1.0;
}

/* Read in file line by line, checking whether any line matches s
 * with strcmp(). Returns line number of first matched line that is
 * found, or -1 if no match exists. Will not work on files with
 * lines with more than 1000 characters between newlines. */
int
find_line (FILE *f, char *s)
{
	int line;
	char c;
	char buf[1000];

	line = -1;

	while ((c = getc (f)) != EOF) {
		ungetc (c, f);
		line++;
		fgets (buf, sizeof buf, f);
		if ((strcmp (buf, s)) == 0) {
			return line;
		}
	}
	return -1;
}

/* Reads in file to search for specific string as substring within a
 * line. Returns number of characters read before match was found, or
 * -1 if no match exists. Will not work on files with lines with more
 * than 1000 characters between newlines. */
int
find_string (FILE *f, char *s)
{
	int char_num;
	char c;
	char buf[1000];
	char *str;

	char_num = 0;
	while ((c = getc (f)) != EOF) {
		ungetc (c, f);
		fgets (buf, sizeof buf, f);
		str = strstr (buf, s);
		if (str != NULL) {
			/* Must add distance into line that substring
			 * begins. */
			return char_num + (str - buf);
		}
		char_num += strlen(buf);
	}
	return -1;
}

int
append_piece (char *p, char *start, char *delim, int num, char ***pieces)
{
	char *piece;

	piece = xcalloc (p - start, sizeof *piece);
	strncpy (piece, start, p - start);
	num++;
	(*pieces) = xrealloc (*pieces, num * (sizeof *pieces));
	(*pieces)[num - 1] = piece;

	return num;
}

/* Split a string on given DELIMinator, allocating new array of pointers
 * to character arrays to store each piece. */
char **
strsplit (char *string, char *delim)
{
	char *p, *start = string, **pieces = NULL;
	int num = 0;

	for (p = string; (*p) != '\0'; p++) {
		if ((strncmp (p, delim, strlen (delim))) == 0) {
			num = append_piece (p, start, delim, num, &pieces);

			p += strlen (delim);
			start = p;
		}
	}

	num = append_piece (p, start, delim, num, &pieces);

	num++;
	pieces = xrealloc (pieces, num * (sizeof *pieces));
	pieces[num - 1] = NULL;

	return pieces;
}

/* This should be used instead of calloc(). It ensures that the memory actually
 * was allocated, and prints an error and aborts immediately if it wasn't. */
void *
xcalloc (size_t nmemb, size_t size)
{
	void *mem;

	mem = calloc (nmemb, size);
	if (mem == NULL && nmemb != 0 && size != 0) {
		fprintf (stderr, "ERROR: calloc() returned NULL!\n");
		exit (-1);
	}

	return mem;
}

/* This should be used instead of malloc(). It ensures that the memory actually
 * was allocated, and prints an error and aborts immediately if it wasn't. */
void *
xmalloc (size_t size)
{
	void *mem;

	mem = malloc (size);
	if (mem == NULL && size != 0) {
		fprintf (stderr, "ERROR: malloc() returned NULL!\n");
		exit (-1);
	}

	return mem;
}

/* This should be used instead of realloc(). It ensures that the memory actually
 * was allocated, and prints an error and aborts immediately if it wasn't. */
void *
xrealloc (void *ptr, size_t size)
{
	void *mem;

	mem = realloc (ptr, size);
	if (mem == NULL && size != 0) {
		fprintf (stderr, "ERROR: realloc() returned NULL!\n");
		exit (-1);
	}

	return mem;
}
