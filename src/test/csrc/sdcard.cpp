#include "common.h"
#include "sdcard.h"

FILE *fp = NULL;

extern "C" {

void sd_setaddr(uint32_t addr) {
  fseek(fp, addr, SEEK_SET);
  //printf("set addr to 0x%08x\n", addr);
  //assert(0);
}

uint32_t sd_read(int ren) {
  if (ren) {
    uint32_t data;
    fread(&data, 4, 1, fp);
    //printf("read data = 0x%08x\n", *data);
    return data;
  }
  return 0xdeadbeaf;
}

void init_sd(void) {
  fp = fopen("/home/xyn/debian/debian.img", "r");
  if(!fp)
  {
    eprintf(ANSI_COLOR_MAGENTA "[warning] sdcard img not found\n");
  }
}

}
