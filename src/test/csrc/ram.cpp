#include "common.h"

#define RAMSIZE (128 * 1024 * 1024)

static uint32_t ram[RAMSIZE / sizeof(uint32_t)];
static long img_size = 0;
void* get_img_start() { return &ram[0x100000 / sizeof(uint32_t)]; }
long get_img_size() { return img_size; }

inline uint32_t tran(int ptenum, int pteidx) {
  return (((0x80000000+4*1024*1024*ptenum+4*1024*pteidx)&0xfffff000)>>2) | 0x1b;
}

void addpage() {
#define PAGESIZE (4 * 1024)  // 4KB = 2^12B
#define PTEVOLUME (PAGESIZE * 1024) // 4MB = 2^22B
#define PTENUM (RAMSIZE / PTEVOLUME) // 128MB / 4MB = 32
#define PDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 1))) //0x88000000-0x1000*33 = 0x87fdf000
#define PTEADDR(i) (PDEADDR + 0x1000*(i+1))
  uint32_t pde[1024];
  uint32_t pte[PTENUM][1024];
  
  for(int addr = 0; addr < PTENUM ;addr++) {
    pde[addr + (0x80000000/PTEVOLUME)] = ((PTEADDR(addr)&0xfffff000)>>2) | 0x1b ;
  }

  for(int outidx = 0; outidx < PTENUM; outidx++ ) {
    for(int inidx = 0; inidx < 1024; inidx++ ) {
      pte[outidx][inidx] = tran(outidx, inidx);
    }
  }
  
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+1)), pde, PAGESIZE);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*PTENUM), pte, PAGESIZE*PTENUM);
}

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

  //new add
  addpage();
  //new end
}

extern "C" void ram_helper(
    uint32_t rIdx, uint32_t *rdata, uint32_t wIdx, uint32_t wdata, uint32_t wmask, uint8_t wen) {
  *rdata = ram[rIdx];
  if (wen) { ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask); }
}
