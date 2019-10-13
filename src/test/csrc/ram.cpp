#include "common.h"

#define RAMSIZE (128 * 1024 * 1024)

static uint64_t ram[RAMSIZE / sizeof(uint64_t)];
static long img_size = 0;
void* get_img_start() { return &ram[0x100000 / sizeof(uint64_t)]; }
long get_img_size() { return img_size; }

inline uint32_t tran(int ptenum, int pteidx) {
  return (((0x80000000+4*1024*1024*ptenum+4*1024*pteidx)&0xfffff000)>>2) | 0x1b;
}
/*
void addpageSv32() {
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
*/

void addpageSv39() {
//three layers
//addr range: 0x0000000080000000 - 0x0000000088000000 for 128MB from 2GB - 2GB128MB
//the first layer: one entry for 1GB. (512GB in total by 512 entries). need the 2th entries
//the second layer: one entry for 2MB. (1GB in total by 512 entries). need the 0th-63rd entries
//the third layer: one entry for 4KB (2MB in total by 512 entries). need 64 with each one all  

#define PAGESIZE (4 * 1024)  // 4KB = 2^12B
#define ENTRYNUM (PAGESIZE / 8) //512 2^9
#define PTEVOLUME (PAGESIZE * ENTRYNUM) // 2MB
#define PTENUM (RAMSIZE / PTEVOLUME) // 128MB / 2MB = 64
#define PDDENUM 1
#define PDENUM 1
#define PDDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 2))) //0x88000000 - 0x1000*66
#define PDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 1))) //0x88000000 - 0x1000*65
#define PTEADDR(i) (0x88000000 - (PAGESIZE * PTENUM) + (PAGESIZE * i)) //0x88000000 - 0x100*64

  uint64_t pdde[ENTRYNUM];
  uint64_t pde[ENTRYNUM];
  uint64_t pte[PTENUM][ENTRYNUM];
  
  //special addr for mmio 0x40600000+
  uint64_t pdemmio[ENTRYNUM];
  uint64_t ptemmio[ENTRYNUM];
  pdde[2] = ((PDEADDR & 0xfffff000) >> 2) | 0x1b;
  pdemmio[3] = (((PDDEADDR-PAGESIZE*2) & 0xfffff000) >> 2) | 0x1b;
  ptemmio[0] = (((0x40600000 & 0xfffff000) >> 2) | 0x1b);

  //special addr for mmio map clint // but no add is also right
  uint64_t pteclint[ENTRYNUM];
  pde[0x110] = (((PDDEADDR-PAGESIZE*3) & 0xfffff000) >> 2) | 0x1b;
  for(int i = 0; i < 16; i++) {
    pteclint[i] = (((0xa2000000 + PAGESIZE*i) * 0xfffff000) >> 2) | 0x1b;
  }

  pdde[1] = (((PDDEADDR-PAGESIZE) & 0xfffff000) >> 2) | 0x1b;

  for(int i = 0; i < PTENUM ;i++) {
    pde[i] = ((PTEADDR(i)&0xfffff000)>>2) | 0x1b ;
  }

  for(int outidx = 0; outidx < PTENUM; outidx++ ) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++ ) {
      pte[outidx][inidx] = (((0x80000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000)>>2) | 0x1b;
    }
  }
  
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+3)),pteclint,PAGESIZE);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+2)),ptemmio,PAGESIZE);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+1)),pdemmio,PAGESIZE);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM)), pdde, PAGESIZE*PDDENUM);
  memcpy((char *)ram+(RAMSIZE-PAGESIZE*(PTENUM+PDENUM)), pde, PAGESIZE*PDENUM);
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
  addpageSv39();
  //new end
}

extern "C" void ram_helper(
    uint64_t rIdx, uint64_t *rdata, uint64_t wIdx, uint64_t wdata, uint64_t wmask, uint8_t wen) {
  *rdata = ram[rIdx];
  if (wen) { ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask); }
}
