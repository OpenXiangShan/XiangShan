#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define EMU_RAM_SIZE (256 * 1024 * 1024UL)

static uint64_t *ram;
static long img_size = 0;


void init_ram() {
  const char *img = "./ram.bin";
  //assert(img != NULL);

  printf("The image is %s\n", img);

  // initialize memory using Linux mmap
  printf("Using simulated %luMB RAM\n", EMU_RAM_SIZE / (1024 * 1024));
  ram = (uint64_t *)mmap(NULL, EMU_RAM_SIZE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (ram == (uint64_t *)MAP_FAILED) {
    printf("Cound not mmap 0x%lx bytes\n", EMU_RAM_SIZE);
    //assert(0);
  }

  FILE *fp = fopen(img, "rb");
  if (fp == NULL) {
    printf("Can not open '%s'\n", img);
    //assert(0);
  }

  fseek(fp, 0, SEEK_END);
  img_size = ftell(fp);
  if (img_size > EMU_RAM_SIZE) {
    img_size = EMU_RAM_SIZE;
  }

  fseek(fp, 0, SEEK_SET);
  fread(ram, img_size, 1, fp);

  fclose(fp);

}

void ram_finish() {
  munmap(ram, EMU_RAM_SIZE);
}


uint64_t ram_read_helper(uint8_t en, uint64_t rIdx) {
  if (en && rIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
    rIdx %= EMU_RAM_SIZE / sizeof(uint64_t);
  }
  uint64_t rdata = (en) ? ram[rIdx] : 0;
  return rdata;
}

void ram_write_helper(uint64_t wIdx, uint64_t wdata, uint64_t wmask, uint8_t wen) {
  if (wen) {
    if (wIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
      printf("ERROR: ram wIdx = 0x%lx out of bound!\n", wIdx);
      return;
      //assert(wIdx < EMU_RAM_SIZE / sizeof(uint64_t));
    }
    ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask);
  }
}
