#include "common.h"
#include <sys/time.h>

static struct timeval boot = {};

void device_init(void) {
  gettimeofday(&boot, NULL);
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
    uint8_t req_wen, uint32_t req_addr, uint32_t req_wdata, uint32_t *resp_rdata) {
  switch (req_addr) {
      // read uartlite stat register
    case 0x40600008:
      // read uartlite ctrl register
    case 0x4060000c: *resp_rdata = 0; break;
      // write uartlite data register
    case 0x40600004: if (req_wen) eprintf("%c", (uint8_t)req_wdata); break;
      // read RTC
    case 0x40700000: *resp_rdata = uptime(); break;
      // read key
    case 0x40900000: *resp_rdata = 0; break;
      // read screen size
    case 0x40800000: *resp_rdata = 0; break;
      // write vga sync
    case 0x40800004: *resp_rdata = 0; break;
    default:
      if (req_addr >= 0x40000000 && req_addr < 0x40400000 && req_wen) {
        // write to vmem
      }
      else {
        assert(0);
      }
  }
}
