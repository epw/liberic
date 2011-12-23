/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 *
 * This library contains a variety of tools which have been found
 * to be useful when writing with SDL in C. It also ensures that the
 * most useful SDL header files are included by default.
 *
 * CONSTANTS
 * X, Y: 0 and 1, to allow two-element arrays for storing positions to
 *       be indexed with [X] and [Y].
 *
 * VARIABLES
 * screen: The surface representing the main window allocated by the
 *         system.
 * library_width: The width of the window.
 * library_height: The height of the window.
 * red, orange, yellow, green, blue, purple, black, white: Color
 *         constants.
 *
 * STRUCTURES
 * rectangle: A flexible structure representing a rectangle. Fields
 *            store pixel values for coordinates, and should be
 *            treated as read-only in client programs. They can be
 *            altered through library functions.
 *
 * FUNCTIONS
 * make_rect_x_y (): Allocate new rectangle structure for the
 *                   rectangle with corners (x, y) and (x2, y2).
 * make_rect_w_h (): Allocate new rectangle structure for the
 *                   rectangle with corners (x, y) and (x+w, y+h).
 * sdl_to_rect (): Convert SDL_Rect to rectangle structure.
 * move_rect (): Update rectangle to be moved dx pixels right and dy
 *               pixels down.
 * rect_w (): Return width of the rectangle.
 * rect_h (): Return height of the rectangle.
 * rect_to_sdl (): Convert rectangle structure to SDL_Rect.
 * start_sdl (): Set up SDL library. Create window with given size and
 *               flags, and sets library variables.
 * draw_blit (): Blit surface to (x, y) on screen.
 * draw_rect (): Draw rectangle (outline only)
 * draw_box (): Draw rectangle (filled)
 * draw_text (): Draw string with given font with upper left corner at
 *               (x, y) using color.
 */

#ifndef _ERICSDL_H_
#define _ERICSDL_H_

#include <eric.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_gfxPrimitives.h>

enum {
	X = 0,
	Y = 1,
};

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

SDL_Surface *screen;

int library_width, library_height;

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
