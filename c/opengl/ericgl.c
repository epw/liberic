/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 */
#include "ericgl.h"

void
init_gl (int *argc, char **argv)
{
	qobj[SOLID] = gluNewQuadric ();
	gluQuadricDrawStyle (qobj[SOLID], GLU_FILL);
	qobj[WIRE] = gluNewQuadric ();
	gluQuadricDrawStyle (qobj[WIRE], GLU_LINE);

	glShadeModel (GL_SMOOTH);

	glutInit (argc, argv);

	libview.dist = 10;
	libview.phi = M_PI / 4;
}

void
vadd (double in1[3], double in2[3], double out[3])
{
	double result[3];
	int i;

	for (i = 0; i < 3; i++) {
		result[i] = in1[i] + in2[i];
	}
	for (i = 0; i < 3; i++) {
		out[i] = result[i];
	}
}

void
vsub (double in1[3], double in2[3], double out[3])
{
	double result[3];
	int i;

	for (i = 0; i < 3; i++) {
		result[i] = in1[i] - in2[i];
	}
	for (i = 0; i < 3; i++) {
		out[i] = result[i];
	}
}

double
vdot (double in1[3], double in2[3])
{
	return (in1[0]*in2[0]+in1[1]*in2[1]+in1[2]*in2[2]);
}

void
vset (double x, double y, double z, double v[3])
{
	v[0] = x;
	v[1] = y;
	v[2] = z;
}

void
vmul (double a, double in[3], double out[3])
{
	int i;

	for (i = 0; i < 3; i++) {
		out[i] = a*in[i];
	}
}

int check_sphere_collision(double pos1[3], double vel1[3], double radius1,
			   double pos2[3], double vel2[3], double radius2)
{
	int touching;
	double dist;
	double between[3];
	double factor;
	double newvel1[3];
	double newvel2[3];

	vadd(pos1, vel1, pos1);

	vadd(pos2, vel2, pos2);

	vsub (pos1, pos2, between);

	dist = sqrt (vdot(between, between));

	if (dist <= radius1 + radius2) {
		touching = 1;
		factor = vdot(vel1, between) / vdot(between, between);
		vmul(factor, between, newvel1);

		factor = vdot(vel2, between) / vdot(between, between);
		vmul(factor, between, newvel2);

		vadd(vel2, newvel1, vel2);
		vsub (vel1, newvel1, vel1);

		vadd(vel1, newvel2, vel1);
		vsub (vel2, newvel2, vel2);		
	}
	if (dist > radius1 + radius2 + 0.1)
		touching = 0;

	return touching;
}

void grid(int lights, float color, int colors)
{
	float normal_color[3];

	if (lights)
		glDisable(GL_LIGHTING);

	normal_color[0] = color;
	normal_color[1] = color;
	normal_color[2] = color;

	glBegin(GL_LINES);
	if (colors)
		glColor3f(0.0, 0.0, 1.0);
	glVertex3f(-5.0, 0.0, -5.0);
	glVertex3f(5.0, 0.0, -5.0);
	glColor3fv(normal_color);
	glVertex3f(-5.0, 0.0, -4.0);
	glVertex3f(5.0, 0.0, -4.0);
	glVertex3f(-5.0, 0.0, -3.0);
	glVertex3f(5.0, 0.0, -3.0);
	glVertex3f(-5.0, 0.0, -2.0);
	glVertex3f(5.0, 0.0, -2.0);
	glVertex3f(-5.0, 0.0, -1.0);
	glVertex3f(5.0, 0.0, -1.0);
	if (colors)
		glColor3f(1.0, 0.0, 0.0);
	glVertex3f(-5.0, 0.0, 0.0);
	glVertex3f(5.0, 0.0, 0.0);
	glColor3fv(normal_color);
	glVertex3f(-5.0, 0.0, 1.0);
	glVertex3f(5.0, 0.0, 1.0);
	glVertex3f(-5.0, 0.0, 2.0);
	glVertex3f(5.0, 0.0, 2.0);
	glVertex3f(-5.0, 0.0, 3.0);
	glVertex3f(5.0, 0.0, 3.0);
	glVertex3f(-5.0, 0.0, 4.0);
	glVertex3f(5.0, 0.0, 4.0);
	if (colors)
		glColor3f(1.0, 1.0, 0.0);
	glVertex3f(-5.0, 0.0, 5.0);
	glVertex3f(5.0, 0.0, 5.0);
	glEnd();
	glBegin(GL_LINES);
	glColor3fv(normal_color);
	glVertex3f(-5.0, 0.0, -5.0);
	glVertex3f(-5.0, 0.0, 5.0);
	glVertex3f(-4.0, 0.0, -5.0);
	glVertex3f(-4.0, 0.0, 5.0);
	glVertex3f(-3.0, 0.0, -5.0);
	glVertex3f(-3.0, 0.0, 5.0);
	glVertex3f(-2.0, 0.0, -5.0);
	glVertex3f(-2.0, 0.0, 5.0);
	glVertex3f(-1.0, 0.0, -5.0);
	glVertex3f(-1.0, 0.0, 5.0);
	if (colors)
		glColor3f(0.0, 1.0, 0.0);
	glVertex3f(0.0, 0.0, -5.0);
	glVertex3f(0.0, 0.0, 5.0);
	glColor3fv(normal_color);
	glVertex3f(1.0, 0.0, -5.0);
	glVertex3f(1.0, 0.0, 5.0);
	glVertex3f(2.0, 0.0, -5.0);
	glVertex3f(2.0, 0.0, 5.0);
	glVertex3f(3.0, 0.0, -5.0);
	glVertex3f(3.0, 0.0, 5.0);
	glVertex3f(4.0, 0.0, -5.0);
	glVertex3f(4.0, 0.0, 5.0);
	glVertex3f(5.0, 0.0, -5.0);
	glVertex3f(5.0, 0.0, 5.0);
	glEnd();
	if (lights)
		glEnable(GL_LIGHTING);
}

void colorless_grid(int lights)
{
	if (lights)
		glDisable(GL_LIGHTING);

	glBegin(GL_LINES);
	glVertex3f(-5.0, 0.0, -5.0);
	glVertex3f(5.0, 0.0, -5.0);
	glVertex3f(-5.0, 0.0, -4.0);
	glVertex3f(5.0, 0.0, -4.0);
	glVertex3f(-5.0, 0.0, -3.0);
	glVertex3f(5.0, 0.0, -3.0);
	glVertex3f(-5.0, 0.0, -2.0);
	glVertex3f(5.0, 0.0, -2.0);
	glVertex3f(-5.0, 0.0, -1.0);
	glVertex3f(5.0, 0.0, -1.0);
	glVertex3f(-5.0, 0.0, 0.0);
	glVertex3f(5.0, 0.0, 0.0);
	glVertex3f(-5.0, 0.0, 1.0);
	glVertex3f(5.0, 0.0, 1.0);
	glVertex3f(-5.0, 0.0, 2.0);
	glVertex3f(5.0, 0.0, 2.0);
	glVertex3f(-5.0, 0.0, 3.0);
	glVertex3f(5.0, 0.0, 3.0);
	glVertex3f(-5.0, 0.0, 4.0);
	glVertex3f(5.0, 0.0, 4.0);
	glVertex3f(-5.0, 0.0, 5.0);
	glVertex3f(5.0, 0.0, 5.0);
	glEnd();
	glBegin(GL_LINES);
	glVertex3f(-5.0, 0.0, -5.0);
	glVertex3f(-5.0, 0.0, 5.0);
	glVertex3f(-4.0, 0.0, -5.0);
	glVertex3f(-4.0, 0.0, 5.0);
	glVertex3f(-3.0, 0.0, -5.0);
	glVertex3f(-3.0, 0.0, 5.0);
	glVertex3f(-2.0, 0.0, -5.0);
	glVertex3f(-2.0, 0.0, 5.0);
	glVertex3f(-1.0, 0.0, -5.0);
	glVertex3f(-1.0, 0.0, 5.0);
	glVertex3f(0.0, 0.0, -5.0);
	glVertex3f(0.0, 0.0, 5.0);
	glVertex3f(1.0, 0.0, -5.0);
	glVertex3f(1.0, 0.0, 5.0);
	glVertex3f(2.0, 0.0, -5.0);
	glVertex3f(2.0, 0.0, 5.0);
	glVertex3f(3.0, 0.0, -5.0);
	glVertex3f(3.0, 0.0, 5.0);
	glVertex3f(4.0, 0.0, -5.0);
	glVertex3f(4.0, 0.0, 5.0);
	glVertex3f(5.0, 0.0, -5.0);
	glVertex3f(5.0, 0.0, 5.0);
	glEnd();
	if (lights)
		glEnable(GL_LIGHTING);
}

void small_grid(int lights, int color)
{
	float normal_color[3];

	if (lights)
		glDisable(GL_LIGHTING);

	if (color == 0) {
		normal_color[0] = 0.0;
		normal_color[1] = 0.0;
		normal_color[2] = 0.0;
	} else {
		normal_color[0] = 1.0;
		normal_color[1] = 1.0;
		normal_color[2] = 1.0;
	}

	glBegin(GL_LINES);
	glColor3f(0.0, 0.0, 1.0);
	glVertex3f(-2.0, 0.0, -2.0);
	glVertex3f(2.0, 0.0, -2.0);
	glColor3fv(normal_color);
	glVertex3f(-2.0, 0.0, -1.0);
	glVertex3f(2.0, 0.0, -1.0);
	glColor3f(1.0, 0.0, 0.0);
	glVertex3f(-2.0, 0.0, 0.0);
	glVertex3f(2.0, 0.0, 0.0);
	glVertex3f(-2.0, 0.0, 1.0);
	glVertex3f(2.0, 0.0, 1.0);
	glColor3f(1.0, 1.0, 0.0);
	glVertex3f(-2.0, 0.0, 2.0);
	glVertex3f(2.0, 0.0, 2.0);
	glEnd();

	glBegin(GL_LINES);
	glColor3fv(normal_color);
	glVertex3f(-2.0, 0.0, -2.0);
	glVertex3f(-2.0, 0.0, 2.0);
	glVertex3f(-1.0, 0.0, -2.0);
	glVertex3f(-1.0, 0.0, 2.0);
	glColor3f(0.0, 1.0, 0.0);
	glVertex3f(0.0, 0.0, -2.0);
	glVertex3f(0.0, 0.0, 2.0);
	glColor3fv(normal_color);
	glVertex3f(1.0, 0.0, -2.0);
	glVertex3f(1.0, 0.0, 2.0);
	glVertex3f(2.0, 0.0, -2.0);
	glVertex3f(2.0, 0.0, 2.0);
	glEnd();

	if (lights)
		glEnable(GL_LIGHTING);
}

float red[4] = { 1.0, 0.0, 0.0, 1.0 };
float orange[4] = { 1.0, 0.5, 0.0, 1.0 };
float yellow[4] = { 1.0, 1.0, 0.0, 1.0 };
float green[4] = { 0.0, 1.0, 0.0, 1.0 };
float blue [4] = { 0.0, 0.0, 1.0, 1.0 };
float magenta [4] = { 1.0, 0.0, 1.0, 1.0 };
float black[4] = { 0.0, 0.0, 0.0, 1.0 };
float darkgray[4] = { 0.2, 0.2, 0.2, 1.0 };
float gray[4] = { 0.5, 0.5, 0.5, 1.0 };
float lightgray[4] = { 0.8, 0.8, 0.8, 1.0 };
float brown[4] = { 0.5, 0.25, 0.0, 1.0 };
float none[4] = { 0.0, 0.0, 0.0, 1.0 };
float white[4] = { 1.0, 1.0, 1.0, 1.0 };
float shiny[4] = { .3, .3, .3, 1 };
float dull[4] = { 0, 0, 0, 1 };
float diffuse_default[4] = { .8, .8, .8, 1 };
float ambient_default[4] = { .2, .2, .2, 1 };

void
set_color (float *ambient, float *diffuse, float *specular, float shininess)
{
	glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT, ambient);
	glMaterialfv (GL_FRONT_AND_BACK, GL_DIFFUSE, diffuse);
	glMaterialfv (GL_FRONT_AND_BACK, GL_SPECULAR, specular);
	glMaterialf (GL_FRONT_AND_BACK, GL_SHININESS, shininess);
}

void
output(GLfloat x, GLfloat y, void *font, char *string)
{
	int len, i;

	glRasterPos2f(x, y);
	len = (int) strlen(string);
	for (i = 0; i < len; i++) {
		glutBitmapCharacter(font, string[i]);
	}
}

void
draw_letter (double xsize, double ysize, double zsize, int c)
{
	double y;

	c = toupper (c);

	glDisable (GL_LIGHTING);
	glPushMatrix();
	glScalef(xsize, ysize, zsize);
	if (c == 'A') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, .5);
		glVertex2f(0, 1.5);
		glVertex2f(1, .5);
		glVertex2f(1, -1);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(-1, .5);
		glVertex2f(1, .5);
		glEnd ();
	}
	if (c == 'B') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(.5, .75);
		glVertex2f(-1, .25);
		glVertex2f(.5, -.5);
		glVertex2f(-1, -1);
		glEnd ();
	}
	if (c == 'C') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'D') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(.5, .25);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(.5, .25);
		glEnd ();
	}
	if (c == 'E') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == 'F') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == 'G') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, 1.5);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(1, 0);
		glVertex2f(0, 0);
		glEnd ();
	}
	if (c == 'H') {
		glBegin (GL_LINES);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, -1);
		glVertex2f(1, 1.5);
		glVertex2f(1, -1);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == 'I') {
		glBegin (GL_LINES);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(0, -1);
		glVertex2f(0, 1.5);
		glEnd ();
	}
	if (c == 'J') {
		glBegin (GL_LINES);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glEnd ();
		glBegin (GL_LINE_STRIP);
		glVertex2f(0, 1.5);
		glVertex2f(0, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 0);
		glEnd ();
	}
	if (c == 'K') {
		glBegin (GL_LINES);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, 0);
		glVertex2f(.5, 1.5);
		glVertex2f(-1, 0);
		glVertex2f(.5, -1);
		glEnd ();
	}
	if (c == 'L') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, -1);
		glVertex2f(.5, -1);
		glEnd ();
	}
	if (c == 'M') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-.5, 1.5);
		glVertex2f(0, -1);
		glVertex2f(.5, 1.5);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == 'N') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, -1);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'O') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == 'P') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, .5);
		glVertex2f(-1, .5);
		glEnd ();
	}
	if (c == 'Q') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, -1);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(0, 0);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == 'R') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, .5);
		glVertex2f(-1, .5);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == 'S') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, 1.5);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glEnd ();
	}
	if (c == 'T') {
		glBegin (GL_LINES);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(0, 1.5);
		glVertex2f(0, -1);
		glEnd ();
	}
	if (c == 'U') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'V') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(0, -1);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'W') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(-.3, -1);
		glVertex2f(0, 1.5);
		glVertex2f(.3, -1);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'X') {
		glBegin (GL_LINES);
		glVertex2f(-1, 1.5);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(1, 1.5);
		glEnd ();
	}
	if (c == 'Y') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(0, .25);
		glVertex2f(0, -1);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(1, 1.5);
		glVertex2f(0, .25);
		glEnd ();
	}
	if (c == 'Z') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == '0') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, -1);
		glEnd ();
	}
	if (c == '1') {
		glBegin (GL_LINES);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(-.5, 1);
		glVertex2f(0, 1.5);
		glVertex2f(0, -1);
		glVertex2f(0, 1.5);
		glEnd ();
	}
	if (c == '2') {
		glBegin (GL_LINE_STRIP);
		glVertex2f (-1, 1.5);
		glVertex2f (1, 1.5);
		glVertex2f (1, .25);
		glVertex2f (-1, .25);
		glVertex2f (-1, -1);
		glVertex2f (1, -1);
		glEnd ();
	}
	if (c == '3') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(1, 1.5);
		glVertex2f(-1, 1.5);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == '4') {
		glBegin (GL_LINE_STRIP);
		glVertex2f (1, -1);
		glVertex2f (1, 1.5);
		glVertex2f (-1, 0);
		glVertex2f (1, 0);
		glEnd ();
	}
	if (c == '5') {
		glBegin (GL_LINE_STRIP);
		glVertex2f (1, 1.5);
		glVertex2f (-1, 1.5);
		glVertex2f (-1, .25);
		glVertex2f (1, .25);
		glVertex2f (1, -1);
		glVertex2f (-1, -1);
		glEnd ();
	}
	if (c == '6') {
		glBegin (GL_LINE_STRIP);
		glVertex2f (1, 1.5);
		glVertex2f (-1, 1.5);
		glVertex2f (-1, -1);
		glVertex2f (1, -1);
		glVertex2f (1, .25);
		glVertex2f (-1, .25);
		glEnd ();
	}
	if (c == '7') {
		glBegin (GL_LINE_STRIP);
		glVertex2f (-1, 1.5);
		glVertex2f (1, 1.5);
		glVertex2f (1, -1);
		glEnd ();
	}
	if (c == '8') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(1, -1);
		glVertex2f(-1, -1);
		glVertex2f(-1, 1.5);
		glVertex2f(1, 1.5);
		glVertex2f(1, -1);
		glEnd ();
		glBegin (GL_LINES);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == '9') {
		glBegin (GL_LINE_STRIP);
		glVertex2f(-1, -1);
		glVertex2f(1, -1);
		glVertex2f(1, 1.5);
		glVertex2f(-1, 1.5);
		glVertex2f(-1, .25);
		glVertex2f(1, .25);
		glEnd ();
	}
	if (c == '.') {
		glBegin (GL_POINTS);
		glVertex2f (0, -1);
		glEnd ();
	}
	if (c == '!') {
		glBegin (GL_LINES);
		glVertex2f(0, -.5);
		glVertex2f(0, 1.5);
		glEnd ();
		glBegin (GL_POINTS);
		glVertex2f (0, -1);
		glEnd ();
	}
	if (c == '-') {
		glBegin (GL_LINES);
		glVertex2f (-1, .25);
		glVertex2f (1, .25);
		glEnd ();
	}
	if (c == '(') {
		glBegin (GL_LINE_STRIP);
		for (y = -1; y < 1.5; y += .1)
			glVertex2f (1 - sqrt(1.25 - fabs(y - .25)), y);
		glEnd ();
	}
	if (c == ')') {
		glBegin (GL_LINE_STRIP);
		for (y = -1; y < 1.5; y += .1)
			glVertex2f (sqrt(1.25 - fabs(y - .25)) - 1, y);
		glEnd ();
	}

	glPopMatrix();
	glEnable (GL_LIGHTING);
}

void
draw_string (double xscale, double yscale, double zscale, float w, char *s)
{
	int i;

	glLineWidth (w);
	glPushMatrix();
	for (i = 0; s[i] != '\0'; i++) {
		glTranslatef (2.5 * xscale, 0, 0);
		draw_letter (xscale, yscale, zscale, s[i]);
	}
	glPopMatrix();
}

double
string_length (double xscale, char *s)
{
	int i;
	double length = 0;

	for (i = 0; s[i] != '\0'; i++) {
		length += 2.5 * xscale;
	}
	return (length);
}

void
turn (float rot_x, float rot_y, float rot_z)
{
	glRotatef (rot_x, 1, 0, 0);
	glRotatef (rot_y, 0, 1, 0);
	glRotatef (rot_z, 0, 0, 1);
}

void
rotating_LookAt (const float center_x, const float center_y,
		 const float center_z, const float dist, float local_theta)
{
	float x, z;

	x = dist * sin(DTOR(local_theta));
	z = dist * cos(DTOR(local_theta));

	gluLookAt (x+center_x, center_y, z+center_z,
		   center_x, center_y, center_z,
		   0.0, 1.0, 0.0);
}

void
start_alpha ()
{
	glDepthMask (GL_FALSE);
	glEnable (GL_BLEND);
}
void
stop_alpha ()
{
	glDepthMask (GL_TRUE);
	glDisable (GL_BLEND);
}

void
getpointerpos(int mx, int my, double *pos)
{
	GLint viewport[4];
	GLdouble modelview[16];
	GLdouble projection[16];
	GLfloat winX, winY, winZ;
	GLdouble posX, posY, posZ;
	double x0, y0, z0, x1, y1, z1;
	double t;

	glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
	glGetDoublev( GL_PROJECTION_MATRIX, projection );
	glGetIntegerv( GL_VIEWPORT, viewport );

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 0;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x0 = posX;
	y0 = posY;
	z0 = posZ;

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 1;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x1 = posX;
	y1 = posY;
	z1 = posZ;

	t = (-y0 + pos[1]) / ((double) (-y0 + y1));

	pos[0] = x0 * (1 - t) + x1 * t;
	pos[2] = z0 * (1 - t) + z1 * t;
}
void
getpointerpos_Zplane(int mx, int my, double *pos)
{
	GLint viewport[4];
	GLdouble modelview[16];
	GLdouble projection[16];
	GLfloat winX, winY, winZ;
	GLdouble posX, posY, posZ;
	double x0, y0, z0, x1, y1, z1;
	double t;

	glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
	glGetDoublev( GL_PROJECTION_MATRIX, projection );
	glGetIntegerv( GL_VIEWPORT, viewport );

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 0;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x0 = posX;
	y0 = posY;
	z0 = posZ;

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 1;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x1 = posX;
	y1 = posY;
	z1 = posZ;

	t = (-z0 + pos[2]) / ((double) (-z0 + z1));

	pos[0] = x0 * (1 - t) + x1 * t;
	pos[1] = y0 * (1 - t) + y1 * t;
}
void
getpointerpos_model_proj_view(int mx, int my, GLdouble *modelview,
			      GLdouble *projection, GLint *viewport,
			      double *pos)
{
	GLfloat winX, winY, winZ;
	GLdouble posX, posY, posZ;
	double x0, y0, z0, x1, y1, z1;
	double t;

	glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
	glGetDoublev( GL_PROJECTION_MATRIX, projection );
	glGetIntegerv( GL_VIEWPORT, viewport );

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 0;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x0 = posX;
	y0 = posY;
	z0 = posZ;

	winX = (float)mx;
	winY = (float)viewport[3] - (float)my;
	winZ = 1;

	gluUnProject(winX, winY, winZ, modelview, projection, viewport,
		     &posX, &posY, &posZ);

	x1 = posX;
	y1 = posY;
	z1 = posZ;

	t = (-y0 + pos[1]) / ((double) (-y0 + y1));

	pos[0] = x0 * (1 - t) + x1 * t;
	pos[2] = z0 * (1 - t) + z1 * t;
}

void
makeTexture (GLuint texName, int ImageSize, GLubyte ***Image)
{
	glBindTexture (GL_TEXTURE_2D, texName);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA, ImageSize, ImageSize, 0,
		      GL_RGBA, GL_UNSIGNED_BYTE, Image);
	glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
}

void
drawTextureRectangle (GLuint texName, double *posA, double *posB,
			double *posC, double *posD)
{
	glEnable (GL_TEXTURE_2D);
	glBindTexture (GL_TEXTURE_2D, texName);
	glBegin (GL_QUADS);
	glTexCoord2f (0, 0);
	glVertex3dv (posA);
	glTexCoord2f (0, 1);
	glVertex3dv (posB);
	glTexCoord2f (1, 1);
	glVertex3dv (posC);
	glTexCoord2f (1, 0);
	glVertex3dv (posD);
	glEnd ();
	glDisable (GL_TEXTURE_2D);
}

void
draw_sphere_fraction (int solid, double r, double slices, double stacks,
		      double thetafraction, double phifraction)
{
	double x, y, z;
	double phi, theta;

	if (solid == SOLID)
		glBegin (GL_QUADS);
	else
		glBegin (GL_LINES);
	for (theta = 0; theta < (DTOR(360))*thetafraction;
	     theta += (DTOR(360))/(double)slices) {
		for(phi = DTOR(-90); phi < DTOR(90) * phifraction;
		    phi += (DTOR(180)) / (double) stacks) {
			x = r * sin (theta) * cos (phi);
			y = r * sin (phi);
			z = r * cos (theta) * cos (phi);
			glVertex3f (x, y, z);
			theta += (M_PI*2)/slices;
			x = r * sin (theta) * cos (phi);
			y = r * sin (phi);
			z = r * cos (theta) * cos (phi);
			glVertex3f (x, y, z);
			if (solid == WIRE)
				glVertex3f (x, y, z);
			phi += (M_PI*2)/slices;
			x = r * sin (theta) * cos (phi);
			y = r * sin (phi);
			z = r * cos (theta) * cos (phi);
			glVertex3f (x, y, z);
			theta -= (M_PI*2)/slices;
			x = r * sin (theta) * cos (phi);
			y = r * sin (phi);
			z = r * cos (theta) * cos (phi);
			glVertex3f (x, y, z);
			if (solid == WIRE)
				glVertex3f (x, y, z);
			phi -= (M_PI*2)/slices;
		}
	}
	glEnd ();
}

void
stroke_string (void *font, char *s)
{
	char buf[1000];
	unsigned char *ubuf;
	int i;

	for (i = 0; i < strlen (s); i++) {
		glPushMatrix();
		strcpy (buf, s);
		buf[i] = 0;
		ubuf = (unsigned char *) buf;
		glTranslatef (glutStrokeLength(font, ubuf), 0, 0);
		glutStrokeCharacter (font, s[i]);
		glPopMatrix();
	}
}

void
writePPM (char *filename, int WIDTH, int HEIGHT)
{
	FILE *f;
	int i;
	unsigned char *pixels;
	char s[1000];

	glRasterPos2i (0, 0);
	pixels = (unsigned char *) malloc (sizeof (unsigned char)
					   * WIDTH * HEIGHT * 3);
	glReadPixels (0, 0, WIDTH, HEIGHT, GL_RGB,
		      GL_UNSIGNED_BYTE, (GLvoid *)pixels);
	f = fopen ("/tmp/image.ppm", "w");
	if (f == NULL) {
		fprintf (stderr, "Error (processInput): Can't write "
			 "to /tmp/image.ppm");
		return;
	}
	fprintf (f, "P6\n%d %d\n255\n", WIDTH, HEIGHT);
	for (i = 0; i < HEIGHT * WIDTH * 3; i++) {
		fprintf (f, "%c", pixels[i]);
	}
	fclose (f);
	sprintf (s, "flipppm /tmp/image.ppm >> %s", filename);
	system (s);
}
		
void
distanceLookAt (double x, double y, double z, double dist, double theta,
		double phi)
{
	double pos[3];
	int yaxis = 1;
	int intphi;
	double floatphi;

	theta = RTOD(theta);
	phi = RTOD(phi);

	intphi = (int) phi;
	floatphi = phi - intphi;

	intphi = intphi % 360;

	phi = intphi + floatphi;

	if (abs(phi) > 90) {
		yaxis = -1;
		if (abs(phi) > 270) {
			yaxis = 1;
		}
	}
			
	theta = DTOR(theta);
	phi = DTOR(phi);

	if (dist < 0)
		dist = 0;

	pos[0] = dist * sin (theta) * cos (phi) + x;
	pos[1] = dist * sin (phi) + y;
	pos[2] = dist * cos (theta) * cos (phi) + z;


	gluLookAt (pos[0], pos[1], pos[2],
		   x, y, z,
		   0, yaxis, 0);
}

void
angleLookAt (double x, double y, double z, double theta, double phi)
{
	gluLookAt (x, y, z,
		   x + sin (theta) * cos (phi), y + sin (phi),
		   z + cos (theta) * cos (phi),
		   0, 1, 0);
}

double
hypot3d (double x, double y, double z)
{
	return sqrt ( (x * x) + (y * y) + (z * z));
}

void
view_look_at (struct view localview)
{
	distanceLookAt (localview.pos[X], localview.pos[Y], localview.pos[Z],
			localview.dist, localview.theta, localview.phi);
}
 
 
