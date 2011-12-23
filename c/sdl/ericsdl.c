#include "ericsdl.h"

int library_width, library_height;

/* Ensure all fields of a rectangle structure are consistent. Should
 * be called any time fields in a rectangle are directly altered. */
void
update_rect (struct rectangle *rect)
{
	int tmp;

	if (rect->x > rect->x2) {
		tmp = rect->x;
		rect->x = rect->x2;
		rect->x2 = tmp;
	}
	if (rect->y > rect->y2) {
		tmp = rect->y;
		rect->y = rect->y2;
		rect->y2 = tmp;
	}
	rect->left = rect->x;
	rect->top = rect->y;
	rect->right = rect->x2;
	rect->bottom = rect->y2;
	rect->center[X] = rect->x + rect_w(*rect)/2;
	rect->center[Y] = rect->y + rect_h(*rect)/2;
}

/* Make new rectangle with corners (x, y) and (x2, y2) */
struct rectangle
make_rect_x_y (int x, int y, int x2, int y2)
{
	struct rectangle rect;

	rect.x = x;
	rect.y = y;
	rect.x2 = x2;
	rect.y2 = y2;

	update_rect (&rect);

	return rect;
}
/* Make new rectangle with upper-left corner (x, y), width w,
 * and height h. */
struct rectangle
make_rect_w_h (int x, int y, int w, int h)
{
	return make_rect_x_y (x, y, x + w, y + h);
}
/* Make new rectangle with same size and position as sdlrect. */
struct rectangle
sdl_to_rect (SDL_Rect sdlrect)
{
	return make_rect_w_h (sdlrect.x, sdlrect.y, sdlrect.w, sdlrect.h);
}

/* Move rectangle by dx pixels right and dy pixels down. */
void
move_rect (struct rectangle *rect, int dx, int dy)
{
	rect->x += dx;
	rect->y += dy;
	rect->x2 += dx;
	rect->y2 += dy;

	update_rect (rect);
}

/* Return width of rectangle. */
int
rect_w (struct rectangle rect)
{
	return rect.x2 - rect.x;
}
/* Return height of rectangle. */
int
rect_h (struct rectangle rect)
{
	return rect.y2 - rect.y;
}

/* Convert rectangle to instance of SDL_Rect */
SDL_Rect
rect_to_sdl (struct rectangle rect)
{
	SDL_Rect sdlrect;

	sdlrect.x = rect.x;
	sdlrect.y = rect.y;
	sdlrect.w = rect_w(rect);
	sdlrect.h = rect_h(rect);

	return sdlrect;
}

Uint32 red;
Uint32 orange;
Uint32 yellow;
Uint32 green;
Uint32 blue;
Uint32 purple;
Uint32 black;
Uint32 white;

void
start_sdl (int width, int height, Uint32 flags)
{
	srandom (time (NULL));

	if(SDL_Init(SDL_INIT_VIDEO) != 0) {
		printf("can't initialize SDL: %s\n", SDL_GetError());
		exit (1);
	}
	atexit(SDL_Quit);
	screen = SDL_SetVideoMode(width, height, 32, flags);

	if(screen == NULL) {
		printf("can't set video mode: %s\n", SDL_GetError());
		exit (1);
	}
	
	library_width = width;
	library_height = height;

	red = 0xff0000ff;
	orange = 0xff9900ff;
	yellow = 0xffff00ff;
	green = 0x00ff00ff;
	blue = 0x0000ffff;
	purple = 0x7700ffff;
	black = 0x000000ff;
	white = 0xffffffff;
}

void
draw_blit(SDL_Surface *image, int x, int y)
{
	SDL_Rect dest;
	dest.x = x;
	dest.y = y;
	SDL_BlitSurface(image, NULL, screen, &dest);
}

void
draw_rect (struct rectangle rect, Uint32 color)
{
	int x1, y1, x2, y2;

	if (rect.x2 < rect.x) {
		x1 = rect.x2;
		x2 = rect.x;
	} else {
		x1 = rect.x;
		x2 = rect.x2;
	}

	if (rect.y2 < rect.y) {
		y1 = rect.y2;
		y2 = rect.y;
	} else {
		y1 = rect.y;
		y2 = rect.y2;
	}
	
	rectangleColor (screen, x1, y1, x2, y2, color);
}
void
draw_box (struct rectangle rect, Uint32 color)
{
	int x1, y1, x2, y2;

	if (rect.x2 < rect.x) {
		x1 = rect.x2;
		x2 = rect.x;
	} else {
		x1 = rect.x;
		x2 = rect.x2;
	}

	if (rect.y2 < rect.y) {
		y1 = rect.y2;
		y2 = rect.y;
	} else {
		y1 = rect.y;
		y2 = rect.y2;
	}
	
	boxColor (screen, x1, y1, x2, y2, color);
}

void
draw_text (char *string, TTF_Font *font, int x, int y, Uint32 color)
{
	SDL_Color sdlcolor;
	SDL_Rect dest;
	SDL_Surface *text;

	SDL_GetRGB (color, screen->format, &(sdlcolor.r), &(sdlcolor.g),
		    &(sdlcolor.b));
	sdlcolor.unused = 0;
	dest.x = x;
	dest.w = 0;
	dest.y = y;
	dest.h = 0;
	text = TTF_RenderText_Solid (font, string, sdlcolor);
	SDL_BlitSurface (text, NULL, screen, &dest);
	SDL_FreeSurface (text);
}
