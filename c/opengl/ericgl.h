/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 *
 * This library contains a variety of functions which have been found
 * to be useful when writing in C. It also ensures that a large number
 * of header files are included by default.
 *
 * CONSTANTS
 * X, Y, Z: Enumerated to allow three-element arrays to be indexed
 *          with coordinates.
 * SOLID: Used to note when quadric object should represent filled
 *        polygons.
 * WIRE: Used to note when quadric object should represent
 *       wireframes.
 *
 * VARIABLES
 * red, orange yellow green, blue, magenta, black, darkgray, gray,
 *     lightgray, brown, none, white, shiny, dull, diffuse_default,
 *     ambient_default: Material values
 * libview: The view structure (see below) accessed by default in the
 *          library.
 *
 * STRUCTURES
 * view: Representation of a camera position for a camera looking at a
 *       given point.
 *
 * FUNCTIONS
 * init_gl (): Initialize OpenGL and GLUT.
 * vadd (): Add two vectors and put the result in out.
 * vsub (): Subtract two vectors and put the result in out.
 * vdot (): Return dot product of two vectors.
 * vset (): Set three elements of vector to values given.
 * vmul (): Multiply vector by constant and put the result in out.
 * check_sphere_collision (): Return whether two spheres with given
 *                            positions and radii will collide given
 *                            their velocity vectors.
 * grid (): Render a simple grid, with or without lighting enabled,
 *          with a given shade of gray, and with or without colored
 *          edge- and center-lines.
 * colorless_grid (): Render a simple grid with no color information
 *                    included.
 * small_grid (): Render a 1x1 unit grid.
 * set_color (): Use glMaterialf() and glMaterialfv() to set
 *               GL_AMBIENT, GL_DIFFUSE, GL_SPECULAR, and
 *               GL_SHININESS.
 * output (): Draw string to window directly.
 * draw_letter (): Draw single letter in scene, according to simple
 *                 library font.
 * draw_string (): Draw string in scene, using draw_letter()
 *                 repeatedly.
 * string_length (): Length in units of given string if rendered with
 *                   draw_string ().
 * turn (): Perform rotations around X, Y, and then Z axes.
 * rotating_look_at (): Move camera to look at given position, placed
 *                     dist units away, parallel to the X-Z plane.
 * start_alpha (): Begin alpha blending of elements in scene.
 * stop_alpha (): Stop alpha blending of elements in scene.
 * getpointerpos (): Take screen coordinates (mx, my) and return X and
 *                   Z world coordinates for the point on the X-Z
 *                   plane with Y-coordinate pos[Y]. Sets pos[X] and
 *                   pos[Z].
 * getpointerpos_Z0plane (): As getpointerpos(), but projects onto Z=0
 *                          plane.
 * getpointerpos_model_proj_view (): As getpointerpos(), and also
 *                                   stores modelview and projection
 *                                   matricies, and viewport.
 * make_texture (): Create new 2D texture using given name, with
 *                 particular size as width and height, and RGBA data
 *                 stored in Image.
 * draw_texture_rectangle (): Render simple rectangle with texture
 *                          represented by texName.
 * draw_sphere_fraction (): Render piece of sphere.
 * stroke_string (): Render string using given font and
 *                   glutStrokeCharacter().
 * write_PPM (): Save current frame as PPM file.
 * distance_look_at (): Position camera using point of focus, distance,
 *                    theta angle, and phi angle.
 * angle_look_at (): Position camera using camera center, theta angle,
 *                 and phi angle.
 * hypot3d (): Third-dimension generalization of the Pythagorean
 *             Theorem.
 * view_look_at (): Apply distanceLookAt to a view structure.
 */

#ifndef _ERICGL_H_
#define _ERICGL_H_

#include <eric.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#include <GL/freeglut.h>

enum { X, Y, Z };

enum {
	SOLID,
	WIRE,
};
GLUquadricObj *qobj[2];

void init_gl (int *argc, char **argv);

void vadd (double in1[3], double in2[3], double out[3]);
void vsub (double in1[3], double in2[3], double out[3]);
double vdot (double in1[3], double in2[3]);
void vset (double x, double y, double z, double v[3]);
void vmul (double a, double in[3], double out[3]);
int check_sphere_collision (double pos1[3], double vel1[3], double radius1,
			    double pos2[3], double vel2[3],
			    double radius2);
void grid (int lights, float color, int colors);
void colorless_grid (int lights);
void small_grid (int lights, int color);

float red[4];
float orange[4];
float yellow[4];
float green[4];
float blue[4];
float magenta[4];
float black[4];
float darkgray[4];
float gray[4];
float lightgray[4];
float brown[4];
float none[4];
float white[4];
float shiny[4];
float dull[4];
float diffuse_default[4];
float ambient_default[4];

void set_color (float *ambient, float *diffuse, float *specular,
		float shininess);
void output(GLfloat x, GLfloat y, void *font, char *string);
void draw_letter (double xsize, double ysize, double zsize, int c);
void draw_string (double xscale, double yscale, double zscale,float w,char *s);
double string_length (double xscale, char *s);
void turn (float rot_x, float rot_y, float rot_z);
void rotating_look_at (const float center_x, const float center_y,
		      const float center_z, const float dist,
		      float local_theta);
void start_alpha (void);
void stop_alpha (void);
void getpointerpos(int mx, int my, double *pos);
void getpointerpos_Z0plane(int mx, int my, double *pos);
void getpointerpos_model_proj_view(int mx, int my, GLdouble *modelview,
				   GLdouble *projection, GLint *viewport,
				   double *pos);
void make_texture (GLuint texName, int ImageSize, GLubyte ***Image);
void draw_texture_rectangle (GLuint texName, double *posA, double *posB,
			   double *posC, double *posD);
void draw_sphere_fraction (int solid, double r, double slices, double stacks,
			   double thetafraction, double phifraction);
void stroke_string (void *font, char *s);
void write_PPM (char *filename, int WIDTH, int HEIGHT);
void distance_look_at (double x, double y, double z, double dist, double theta,
		     double phi);
void angle_look_at (double x, double y, double z, double theta, double phi);
double hypot3d (double x, double y, double z);

struct view {
	double pos[3];
	double dist;
	double theta, phi;
} libview;

void view_look_at (struct view view);

#endif
