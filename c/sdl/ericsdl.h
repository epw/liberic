#include <eric.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_gfxPrimitives.h>

enum {
	X,
	Y,
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

//Uint16 CreateHicolorPixel(SDL_PixelFormat * fmt, Uint8 red, Uint8 green,
//			  Uint8 blue);

//void setpixel (int x, int y, Uint16 pixel_color);
//void setpixelRGB (int x, int y, Uint8 red, Uint8 green, Uint8 blue);
//void rectangle (int x, int y, int w, int h, Uint16 color);
//void rectanglelines (int x1, int y1, int x2, int y2, Uint16 color);
void start_sdl (int width, int height, Uint32 flags);
//void draw_line (int x0, int y0, int x1, int y1, Uint16 color);
void draw_blit(SDL_Surface *image, int x, int y);
//void draw_circle (int center_x, int center_y, int radius, Uint16 color);

void draw_rect (struct rectangle rect, Uint32 color);
void draw_box (struct rectangle rect, Uint32 color);
void draw_text (char *string, TTF_Font *font, int x, int y, Uint32 color);
