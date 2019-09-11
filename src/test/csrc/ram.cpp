#include "common.h"

#define RAMSIZE (128 * 1024 * 1024)

static uint64_t ram[RAMSIZE / sizeof(uint64_t)];
static long img_size = 0;
void* get_img_start() { return &ram[0x100000 / sizeof(uint64_t)]; }
long get_img_size() { return img_size; }

void init_ram(const char *img, const char *mainargs) {
  assert(img != NULL);
  FILE *fp = fopen(img, "rb");
  if (fp == NULL) {
    printf("Can not open '%s'\n", img);
    assert(0);
  }

  printf("The image is %s\n", img);

  fseek(fp, 0, SEEK_END);
  img_size = ftell(fp);

  fseek(fp, 0, SEEK_SET);
  int ret = fread(get_img_start(), img_size, 1, fp);
  assert(ret == 1);
  fclose(fp);

  strcpy((char *)ram, mainargs);
}

extern "C" void ram_helper(
    uint64_t rIdx, uint64_t *rdata, uint64_t wIdx, uint64_t wdata, uint64_t wmask, uint8_t wen) {
  *rdata = ram[rIdx];
  if (wen) { ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask); }
}
