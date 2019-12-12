#include "common.h"
#include <sys/time.h>
#include <SDL2/SDL.h>

void send_key(uint8_t, bool);
void init_sdl(void);
void set_abort(void);

void init_uart(void);
extern "C" void init_sd(void);

static struct timeval boot = {};

void init_device(void) {
  init_sdl();
  init_uart();
  init_sd();
  gettimeofday(&boot, NULL);
}

void poll_event() {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_QUIT: set_abort();

                     // If a key was pressed
      case SDL_KEYDOWN:
      case SDL_KEYUP: {
                        uint8_t k = event.key.keysym.scancode;
                        bool is_keydown = (event.key.type == SDL_KEYDOWN);
                        send_key(k, is_keydown);
                        break;
                      }
      default: break;
    }
  }
}

uint32_t uptime(void) {
  struct timeval t;
  gettimeofday(&t, NULL);

  int s = t.tv_sec - boot.tv_sec;
  int us = t.tv_usec - boot.tv_usec;
  if (us < 0) {
    s --;
    us += 1000000;
  }

  return s * 1000 + (us + 500) / 1000;
}
