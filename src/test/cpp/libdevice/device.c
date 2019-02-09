#include "common.h"
#include <SDL2/SDL.h>

extern void send_key(uint8_t, bool);

int poll_event() {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_QUIT: return 1;

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
  return 0;
}
