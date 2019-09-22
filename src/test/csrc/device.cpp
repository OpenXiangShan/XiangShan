#include "common.h"
#include <sys/time.h>
#include <SDL2/SDL.h>

void send_key(uint8_t, bool);
uint32_t read_key(void);
void init_sdl(void);
void update_screen(void *vmem);
uint32_t screen_size(void);
void set_abort(void);

int uart_getc(void);
void uart_putc(char c);
void init_uart(void);

static struct timeval boot = {};
static uint64_t vmem[0x400000 / sizeof(uint64_t)];

void init_device(void) {
  init_sdl();
  init_uart();
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

extern "C" void device_helper(
    uint8_t req_wen, uint64_t req_addr, uint64_t req_wdata, uint8_t req_wmask, uint64_t *resp_rdata) {
  switch (req_addr) {
      // read uartlite stat register
    case 0x40600008: *resp_rdata = 0x01; break; // set UARTLITE_RX_VALID
      // read uartlite ctrl register
    case 0x4060000c: *resp_rdata = 0; break;
      // write uartlite tx fifo
    case 0x40600004: if (req_wen) uart_putc((char)req_wdata); break;
      // read uartlite rx fifo
    case 0x40600000: *resp_rdata = uart_getc(); break;
      // read RTC
    case 0x40700000: *resp_rdata = uptime(); break;
      // read key
    case 0x40900000: *resp_rdata = read_key(); break;
      // read screen size
    case 0x40800000: *resp_rdata = screen_size(); break;
      // write vga sync
    case 0x40800004: update_screen(vmem); break;
    default:
      if (req_addr >= 0x40000000 && req_addr < 0x40400000 && req_wen) {
        // write to vmem
        vmem[(req_addr - 0x40000000) / sizeof(uint64_t)] = req_wdata;
      }
      else {
        eprintf("bad address = 0x%08x, wen = %d\n", (uint32_t)req_addr, req_wen);
        assert(0);
      }
  }
}
