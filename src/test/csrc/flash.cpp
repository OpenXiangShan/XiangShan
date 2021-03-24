#include "common.h"
#include "flash.h"

#define USE_BIN

FILE *flash_fp = NULL;

extern "C" {

void flash_read(uint32_t addr, uint64_t *data) {
  fseek(flash_fp, addr, SEEK_SET);
  fread(data, 8, 1, flash_fp);
  //printf("read data = 0x%08x\n", *data);
  //assert(0);
}

void init_flash(void) {
  flash_fp = fopen("/home/jy/Project/nexus-am/tests/cputest/build/dummy-riscv64-noop.bin", "r");
  if(!flash_fp)
  {
    eprintf(ANSI_COLOR_MAGENTA "[warning] flash img not found\n");
  }
}

}
