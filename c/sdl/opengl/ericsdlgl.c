/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 */
#include "ericsdlgl.h"

void
init_sdl_gl (int width, int height)
{
	if (SDL_Init(SDL_INIT_JOYSTICK | SDL_INIT_VIDEO) != 0) {
		printf ("Error: %s\n", SDL_GetError());
		exit(1);
	}

	atexit(SDL_Quit);

	srandom (time (NULL));

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 1);

	SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
	SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 6);
	SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);

	if (SDL_SetVideoMode(width, height, 16, SDL_OPENGL) == NULL) {
		printf ("Error: %s\n", SDL_GetError());
		exit(1);
	}
}

void
init_sdl_gl_flags (int width, int height, Uint32 flags)
{
	if (SDL_Init(SDL_INIT_JOYSTICK | SDL_INIT_VIDEO) != 0) {
		printf ("Error: %s\n", SDL_GetError());
		exit(1);
	}

	atexit(SDL_Quit);

	srandom (time (NULL));

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 1);

	SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
	SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 6);
	SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);

	if (SDL_SetVideoMode(width, height, 16, SDL_OPENGL | flags) == NULL) {
		printf ("Error: %s\n", SDL_GetError());
		exit(1);
	}
}
