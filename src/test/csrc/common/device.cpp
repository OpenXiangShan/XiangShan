/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#include <sys/time.h>
#include <SDL2/SDL.h>

#include "device.h"

void send_key(uint8_t, bool);
void init_sdl(void);

void init_uart(void);
extern "C" void init_sd(void);
extern "C" void init_flash(void);

static struct timeval boot = {};

void init_device(void) {
  init_sdl();
  init_uart();
  init_sd();
  // init_flash();
  gettimeofday(&boot, NULL);
}

void poll_event() {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_QUIT: break; //set_abort();

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
