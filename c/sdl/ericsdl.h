/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details */

#ifndef _ERICSDL_H_
#define _ERICSDL_H_

#include <eric.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_gfxPrimitives.h>

/* Allow two-dimensional arrays to easily be indexed to reference X
 * and Y coordinates. */
enum {
	X = 0,
	Y = 1,
};

/* Type to represent a rectangle. Fields should be treated as read-only,
 * and only altered through given functions. Left, right, top, and bottom
 * give coordinates in pixels of the named sides. */
struct rectangle {
	int x, y;
	int x2, y2;
	int left, right, top, bottom;
	int center[2];
};

struct rectangle make_rect_x_y (int x, int y, int x2, int y2);
struct rectangle make_rect_w_h (int x, int y, int w, int h);
struct rectangle sdl_to_rect (SDL_Rect sdlrect);

void move_rect (struct rectangle *rect, int dx, int dy);
int rect_w (struct rectangle rect);
int rect_h (struct rectangle rect);
SDL_Rect rect_to_sdl (struct rectangle rect);

/* Library variable to allow all functions to rely on the single
 * window surface. */
SDL_Surface *screen;

/* Set by start_sdl */
int library_width, library_height;

/* Contain common color values */
Uint32 red;
Uint32 orange;
Uint32 yellow;
Uint32 green;
Uint32 blue;
Uint32 purple;
Uint32 black;
Uint32 white;

void start_sdl (int width, int height, Uint32 flags);
void draw_blit(SDL_Surface *image, int x, int y);
void draw_rect (struct rectangle rect, Uint32 color);
void draw_box (struct rectangle rect, Uint32 color);
void draw_text (char *string, TTF_Font *font, int x, int y, Uint32 color);

#endif
