#include <eric.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/freeglut.h>

#ifndef ERICGL_H

#define ERICGL_H

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
void rotating_LookAt (const float center_x, const float center_y,
		      const float center_z, const float dist,
		      float local_theta);
void start_alpha (void);
void stop_alpha (void);
void getpointerpos(int mx, int my, double *pos);
void getpointerpos_Zplane(int mx, int my, double *pos);
void getpointerpos_model_proj_view(int mx, int my, GLdouble *modelview,
				   GLdouble *projection, GLint *viewport,
				   double *pos);
void makeTexture (GLuint texName, int ImageSize, GLubyte ***Image);
void drawTextureRectangle (GLuint texName, double *posA, double *posB,
			   double *posC, double *posD);
void draw_sphere_fraction (int solid, double r, double slices, double stacks,
			   double thetafraction, double phifraction);
void stroke_string (void *font, char *s);
void writePPM (char *filename, int WIDTH, int HEIGHT);
void distanceLookAt (double x, double y, double z, double dist, double theta,
		     double phi);
void angleLookAt (double x, double y, double z, double theta, double phi);
double hypot3d (double x, double y, double z);

struct view {
	double pos[3];
	double dist;
	double theta, phi;
} libview;

void view_look_at (struct view view);

#endif
