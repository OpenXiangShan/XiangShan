#include "common.h"

#include <SDL2/SDL.h>

#define SCREEN_PORT 0x100 // Note that this is not the standard
#define SCREEN_MMIO 0x4100
#define SCREEN_H 320
#define SCREEN_W 400


static SDL_Window *window;
static SDL_Renderer *renderer;
static SDL_Texture *texture;

void update_screen(void *vmem) {
  SDL_UpdateTexture(texture, NULL, vmem, SCREEN_W * sizeof(uint32_t));
  SDL_RenderClear(renderer);
  SDL_RenderCopy(renderer, texture, NULL, NULL);
  SDL_RenderPresent(renderer);
}

uint32_t screen_size(void) {
  return ((SCREEN_W) << 16) | (SCREEN_H);
}

void init_sdl() {
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(SCREEN_W * 2, SCREEN_H * 2, 0, &window, &renderer);
  SDL_SetWindowTitle(window, "NOOP");
  texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888,
      SDL_TEXTUREACCESS_STATIC, SCREEN_W, SCREEN_H);
}
