#include "common.h"
#include "flash.h"

FILE *flash_fp = NULL;

extern "C" {

void flash_read(uint32_t addr, uint64_t *data) {
#ifdef USE_BIN
  fseek(flash_fp, addr, SEEK_SET);
  fread(data, 8, 1, flash_fp);
#else
  uint32_t index = addr & 0x00000fff;
  switch(index>>3){
    case 0 :
      *data = 0x01f292930010029b;
      break;
    case 1 :
      *data = 0x00028067;
      break;
    default :
      *data = 0;
  }
#endif
}

void init_flash(void) {
#ifdef USE_BIN

  flash_fp = fopen("/home/jy/Project/nexus-am/tests/cputest/build/dummy-riscv64-noop.bin", "r");
  if(!flash_fp)
  {
    eprintf(ANSI_COLOR_MAGENTA "[warning] flash img not found\n");
  }
  printf("use bin as a flash!\n"); 
#else 
  printf("use fixed 3 instructions!\n");
#endif
}

}
