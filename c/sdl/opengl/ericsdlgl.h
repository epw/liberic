/* Copyright (C) Eric Willisson 2006-2011
 * This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
 * for details
 *
 * This library aids in initializing OpenGL in SDL.
 *
 * FUNCTIONS:
 * init_sdl_gl (): Initialize an SDL window which allows OpenGL
 *                 rendering with default flags.
 * init_sdl_gl_flags (): Initialize an SDL window which allows OpenGL
 *                 rendering with given flags.
 */
#include <eric.h>
#include <SDL/SDL.h>

void init_sdl_gl (int width, int height);
void init_sdl_gl_flags (int width, int height, Uint32 flags);
