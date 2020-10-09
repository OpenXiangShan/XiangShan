#include "common.h"

extern "C" {

static FILE *fp = NULL;

void sd_setaddr(uint32_t addr) {
  fseek(fp, addr, SEEK_SET);
  //printf("set addr to 0x%08x\n", addr);
  //assert(0);
}

void sd_read(uint32_t *data) {
  fread(data, 4, 1, fp);
  //printf("read data = 0x%08x\n", *data);
  //assert(0);
}

void init_sd(void) {
  fp = fopen("/home/yzh/projectn/debian.img", "r");
  if(!fp)
  {
    eprintf(ANSI_COLOR_MAGENTA "[warning] sdcard img not found\n");
  }
}

}
