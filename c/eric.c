/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details */
#include "eric.h"

double
get_time (void)
{
	struct timeval tv;
	gettimeofday (&tv, NULL);
	return (tv.tv_sec + tv.tv_usec / 1000000.0);
}

int
mkrand (int minval, int maxval)
{
	maxval++;
	return (floor ((double)random () / RAND_MAX
		       * (maxval - minval) + minval));
}

FILE *
open_read (char *filename, int quit)
{
	FILE *f;
	if ((f = fopen (filename, "r")) == NULL) {
		printf ("Error. Can't open %s\n", filename);
		if (quit)
			exit (quit);
		else
			return NULL;
	}
	return f;
}
FILE *
open_write (char *filename, int quit)
{
	FILE *f;
	if ((f = fopen (filename, "w")) == NULL) {
		printf ("Error. Can't open %s\n", filename);
		if (quit)
			exit (quit);
		else
			return NULL;
	}
	return f;
}
FILE *
open_append (char *filename, int quit)
{
	FILE *f;
	if ((f = fopen (filename, "a")) == NULL) {
		printf ("Error. Can't open %s\n", filename);
		if (quit)
			exit (quit);
		else
			return NULL;
	}
	return f;
}

double
square (double n)
{
	return n * n;
}
double
cube (double n)
{
	return n * n * n;
}

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

int
signi (int n)
{
	if (n < 0)
		return -1;
	return 1;
}
double
signd (double n)
{
	if (n < 0)
		return -1.0;
	return 1.0;
}

int
find_line (FILE *f, char *s)
{
	int line;
	char c;
	char buf[_ERIC_H_MAX_LINE_LENGTH];

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

int
find_string (FILE *f, char *s)
{
	int char_num;
	char c;
	char buf[_ERIC_H_MAX_LINE_LENGTH];
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
append_piece (char *p, char *start, int num, char ***pieces)
{
	char *piece;

	piece = xcalloc (p - start, sizeof *piece);
	strncpy (piece, start, p - start);
	num++;
	(*pieces) = xrealloc (*pieces, num * (sizeof *pieces));
	(*pieces)[num - 1] = piece;

	return num;
}

char **
strsplit (char *string, char *delim)
{
	char *p, *start = string, **pieces = NULL;
	int num = 0;

	for (p = string; (*p) != '\0'; p++) {
		if ((strncmp (p, delim, strlen (delim))) == 0) {
			num = append_piece (p, start, num, &pieces);

			p += strlen (delim);
			start = p;
		}
	}

	num = append_piece (p, start, num, &pieces);

	num++;
	pieces = xrealloc (pieces, num * (sizeof *pieces));
	pieces[num - 1] = NULL;

	return pieces;
}

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
