#include <sys/mman.h>

#include "common.h"
#include "ram.h"
#include "compress.h"

// #define TLB_UNITTEST

#ifdef WITH_DRAMSIM3
#include "cosimulation.h"
CoDRAMsim3 *dram = NULL;
#endif

static uint64_t *ram;
static long img_size = 0;
static pthread_mutex_t ram_mutex;

void* get_img_start() { return &ram[0]; }
long get_img_size() { return img_size; }
void* get_ram_start() { return &ram[0]; }
long get_ram_size() { return EMU_RAM_SIZE; }

#ifdef TLB_UNITTEST
void addpageSv39() {
//three layers
//addr range: 0x0000000080000000 - 0x0000000088000000 for 128MB from 2GB - 2GB128MB
//the first layer: one entry for 1GB. (512GB in total by 512 entries). need the 2th entries
//the second layer: one entry for 2MB. (1GB in total by 512 entries). need the 0th-63rd entries
//the third layer: one entry for 4KB (2MB in total by 512 entries). need 64 with each one all
#define TOPSIZE (128 * 1024 * 1024)
#define PAGESIZE (4 * 1024)  // 4KB = 2^12B
#define ENTRYNUM (PAGESIZE / 8) //512 2^9
#define PTEVOLUME (PAGESIZE * ENTRYNUM) // 2MB
#define PTENUM (TOPSIZE / PTEVOLUME) // 128MB / 2MB = 64
#define PDDENUM 1
#define PDENUM 1
#define PDDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 2))) //0x88000000 - 0x1000*66
#define PDEADDR (0x88000000 - (PAGESIZE * (PTENUM + 1))) //0x88000000 - 0x1000*65
#define PTEADDR(i) (0x88000000 - (PAGESIZE * PTENUM) + (PAGESIZE * i)) //0x88000000 - 0x100*64
#define PTEMMIONUM 128
#define PDEMMIONUM 1
#define PTEDEVNUM 128
#define PDEDEVNUM 1

  uint64_t pdde[ENTRYNUM];
  uint64_t pde[ENTRYNUM];
  uint64_t pte[PTENUM][ENTRYNUM];

  // special addr for mmio 0x40000000 - 0x4fffffff
  uint64_t pdemmio[ENTRYNUM];
  uint64_t ptemmio[PTEMMIONUM][ENTRYNUM];

  // special addr for internal devices 0x30000000-0x3fffffff
  uint64_t pdedev[ENTRYNUM];
  uint64_t ptedev[PTEDEVNUM][ENTRYNUM];

  // dev: 0x30000000-0x3fffffff
  pdde[0] = (((PDDEADDR-PAGESIZE*(PDEMMIONUM+PTEMMIONUM+PDEDEVNUM)) & 0xfffff000) >> 2) | 0x1;

  for (int i = 0; i < PTEDEVNUM; i++) {
    pdedev[ENTRYNUM-PTEDEVNUM+i] = (((PDDEADDR-PAGESIZE*(PDEMMIONUM+PTEMMIONUM+PDEDEVNUM+PTEDEVNUM-i)) & 0xfffff000) >> 2) | 0x1;
  }

  for(int outidx = 0; outidx < PTEDEVNUM; outidx++) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++) {
      ptedev[outidx][inidx] = (((0x30000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000) >> 2) | 0xf;
    }
  }

  // mmio: 0x40000000 - 0x4fffffff
  pdde[1] = (((PDDEADDR-PAGESIZE*PDEMMIONUM) & 0xfffff000) >> 2) | 0x1;

  for(int i = 0; i < PTEMMIONUM; i++) {
    pdemmio[i] = (((PDDEADDR-PAGESIZE*(PTEMMIONUM+PDEMMIONUM-i)) & 0xfffff000) >> 2) | 0x1;
  }

  for(int outidx = 0; outidx < PTEMMIONUM; outidx++) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++) {
      ptemmio[outidx][inidx] = (((0x40000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000) >> 2) | 0xf;
    }
  }

  //0x800000000 - 0x87ffffff
  pdde[2] = ((PDEADDR & 0xfffff000) >> 2) | 0x1;
  //pdde[2] = ((0x80000000&0xc0000000) >> 2) | 0xf;

  for(int i = 0; i < PTENUM ;i++) {
    // pde[i] = ((PTEADDR(i)&0xfffff000)>>2) | 0x1;
    pde[i] = (((0x80000000+i*2*1024*1024)&0xffe00000)>>2) | 0xf;
  }

  for(int outidx = 0; outidx < PTENUM; outidx++ ) {
    for(int inidx = 0; inidx < ENTRYNUM; inidx++ ) {
      pte[outidx][inidx] = (((0x80000000 + outidx*PTEVOLUME + inidx*PAGESIZE) & 0xfffff000)>>2) | 0xf;
    }
  }

  printf("try to add identical tlb page to ram\n");
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM+PTEMMIONUM+PDEDEVNUM+PTEDEVNUM)),ptedev,PAGESIZE*PTEDEVNUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM+PTEMMIONUM+PDEDEVNUM)),pdedev,PAGESIZE*PDEDEVNUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM+PTEMMIONUM)),ptemmio, PAGESIZE*PTEMMIONUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM+PDEMMIONUM)), pdemmio, PAGESIZE*PDEMMIONUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDDENUM+PDENUM)), pdde, PAGESIZE*PDDENUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*(PTENUM+PDENUM)), pde, PAGESIZE*PDENUM);
  memcpy((char *)ram+(TOPSIZE-PAGESIZE*PTENUM), pte, PAGESIZE*PTENUM);
}
#endif

void init_ram(const char *img) {
  assert(img != NULL);

  printf("The image is %s\n", img);

  // initialize memory using Linux mmap
  printf("Using simulated %luMB RAM\n", EMU_RAM_SIZE / (1024 * 1024));
  ram = (uint64_t *)mmap(NULL, EMU_RAM_SIZE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (ram == (uint64_t *)MAP_FAILED) {
    printf("Cound not mmap 0x%lx bytes\n", EMU_RAM_SIZE);
    assert(0);
  }

#ifdef TLB_UNITTEST
  //new add
  addpageSv39();
  //new end
#endif

  int ret;
  if (isGzFile(img)) {
    printf("Gzip file detected and loading image from extracted gz file\n");
    img_size = readFromGz(ram, img, EMU_RAM_SIZE, LOAD_RAM);
    assert(img_size >= 0);
  }
  else {
    FILE *fp = fopen(img, "rb");
    if (fp == NULL) {
      printf("Can not open '%s'\n", img);
      assert(0);
    }

    fseek(fp, 0, SEEK_END);
    img_size = ftell(fp);
    if (img_size > EMU_RAM_SIZE) {
      img_size = EMU_RAM_SIZE;
    }

    fseek(fp, 0, SEEK_SET);
    ret = fread(ram, img_size, 1, fp);

    assert(ret == 1);
    fclose(fp);
  }

#ifdef WITH_DRAMSIM3
  #if !defined(DRAMSIM3_CONFIG) || !defined(DRAMSIM3_OUTDIR)
  #error DRAMSIM3_CONFIG or DRAMSIM3_OUTDIR is not defined
  #endif
  assert(dram == NULL);
  dram = new ComplexCoDRAMsim3(DRAMSIM3_CONFIG, DRAMSIM3_OUTDIR);
  // dram = new SimpleCoDRAMsim3(90);
#endif

  pthread_mutex_init(&ram_mutex, 0);

}

void ram_finish() {
  munmap(ram, EMU_RAM_SIZE);
#ifdef WITH_DRAMSIM3
  dramsim3_finish();
#endif
  pthread_mutex_destroy(&ram_mutex);
}


extern "C" uint64_t ram_read_helper(uint8_t en, uint64_t rIdx) {
  if (en && rIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
    rIdx %= EMU_RAM_SIZE / sizeof(uint64_t);
  }
  pthread_mutex_lock(&ram_mutex);
  uint64_t rdata = (en) ? ram[rIdx] : 0;
  pthread_mutex_unlock(&ram_mutex);
  return rdata;
}

extern "C" void ram_write_helper(uint64_t wIdx, uint64_t wdata, uint64_t wmask, uint8_t wen) {
  if (wen) {
    if (wIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
      printf("ERROR: ram wIdx = 0x%lx out of bound!\n", wIdx);
      assert(wIdx < EMU_RAM_SIZE / sizeof(uint64_t));
    }
    pthread_mutex_lock(&ram_mutex);
    ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask);
    pthread_mutex_unlock(&ram_mutex);
  }
}

#ifdef WITH_DRAMSIM3
#include <iostream>

void dramsim3_finish() {
  delete dram;
}

#define MAX_AXI_DATA_LEN 8

// currently does not support masked read or write
struct dramsim3_meta {
  uint8_t  len;
  uint8_t  size;
  uint8_t  offset;
  uint8_t  id;
  uint64_t data[MAX_AXI_DATA_LEN];
};

void axi_read_data(const axi_ar_channel &ar, dramsim3_meta *meta) {
  uint64_t address = ar.addr % EMU_RAM_SIZE;
  uint64_t beatsize = 1 << ar.size;
  uint8_t  beatlen  = ar.len + 1;
  uint64_t transaction_size = beatsize * beatlen;
  assert((transaction_size % sizeof(uint64_t)) == 0);
  // axi burst FIXED
  if (ar.burst == 0x0) {
    std::cout << "axi burst FIXED not supported!" << std::endl;
    assert(0);
  }
  // axi burst INCR
  else if (ar.burst == 1) {
    assert(transaction_size / sizeof(uint64_t) <= MAX_AXI_DATA_LEN);
    for (int i = 0; i < transaction_size / sizeof(uint64_t); i++) {
      meta->data[i] = ram[address / sizeof(uint64_t)];
      address += sizeof(uint64_t);
    }
  }
  // axi burst WRAP
  else if (ar.burst == 2) {
    uint64_t low = (address / transaction_size) * transaction_size;
    uint64_t high = low + transaction_size;
    assert(transaction_size / sizeof(uint64_t) <= MAX_AXI_DATA_LEN);
    for (int i = 0; i < transaction_size / sizeof(uint64_t); i++) {
      if (address == high) {
        address = low;
      }
      meta->data[i] = ram[address / sizeof(uint64_t)];
      address += sizeof(uint64_t);
    }
  }
  else {
    std::cout << "reserved arburst!" << std::endl;
    assert(0);
  }
  meta->len = beatlen;
  meta->size = beatsize;
  meta->offset = 0;
  meta->id = ar.id;
}

CoDRAMRequest *dramsim3_request(const axi_channel &axi, bool is_write) {
  uint64_t address = (is_write) ? axi.aw.addr : axi.ar.addr;
  dramsim3_meta *meta = new dramsim3_meta;
  // WRITE
  if (is_write) {
    meta->len = axi.aw.len + 1;
    meta->size = 1 << axi.aw.size;
    meta->offset = 0;
    meta->id = axi.aw.id;
  }
  else {
    axi_read_data(axi.ar, meta);
  }
  CoDRAMRequest *req = new CoDRAMRequest();
  req->address = address;
  req->is_write = is_write;
  req->meta = meta;
  return req;
}

static CoDRAMResponse *wait_resp_r = NULL;
static CoDRAMResponse *wait_resp_b = NULL;
static CoDRAMRequest *wait_req_w = NULL;
// currently only accept one in-flight read + one in-flight write
static uint64_t raddr, roffset = 0, rlen;
static uint64_t waddr, woffset = 0, wlen;

void dramsim3_helper_rising(const axi_channel &axi) {
  // ticks DRAMsim3 according to CPU_FREQ:DRAM_FREQ
  dram->tick();

  // read data fire: check the last read request
  if (axi_check_rdata_fire(axi)) {
    if (wait_resp_r == NULL) {
      printf("ERROR: There's no in-flight read request.\n");
      assert(wait_resp_r != NULL);
    }
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_resp_r->req->meta);
    meta->offset++;
    // check whether the last rdata response has finished
    if (meta->offset == meta->len) {
      delete meta;
      delete wait_resp_r->req;
      delete wait_resp_r;
      wait_resp_r = NULL;
    }
  }

  // read address fire: accept a new request
  if (axi_check_raddr_fire(axi)) {
    dram->add_request(dramsim3_request(axi, false));
  }

  // the last write transaction is acknowledged
  if (axi_check_wack_fire(axi)) {
    if (wait_resp_b == NULL) {
      printf("ERROR: write response fire for nothing in-flight.\n");
      assert(wait_resp_b != NULL);
    }
    // flush data to memory
    uint64_t waddr = wait_resp_b->req->address % EMU_RAM_SIZE;
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_resp_b->req->meta);
    void *start_addr = ram + (waddr / sizeof(uint64_t));
    memcpy(start_addr, meta->data, meta->len * meta->size);
    for (int i = 0; i < meta->len; i++) {
    //   uint64_t address = wait_resp_b->req->address % EMU_RAM_SIZE;
    //   ram[address / sizeof(uint64_t) + i] = meta->data[i];
      // printf("flush write to memory[0x%ld] = 0x%lx\n", address)
    }
    delete meta;
    delete wait_resp_b->req;
    delete wait_resp_b;
    wait_resp_b = NULL;
  }

  // write address fire: accept a new write request
  if (axi_check_waddr_fire(axi)) {
    if (wait_req_w != NULL) {
      printf("ERROR: The last write request has not finished.\n");
      assert(wait_req_w == NULL);
    }
    wait_req_w = dramsim3_request(axi, true);
    // printf("accept a new write request to addr = 0x%lx, len = %d\n", axi.aw.addr, axi.aw.len);
  }

  // write data fire: for the last write transaction
  if (axi_check_wdata_fire(axi)) {
    if (wait_req_w == NULL) {
      printf("ERROR: wdata fire for nothing in-flight.\n");
      assert(wait_req_w != NULL);
    }
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_req_w->meta);
    void *data_start = meta->data + meta->offset * meta->size / sizeof(uint64_t);
    axi_get_wdata(axi, data_start, meta->size);
    meta->offset++;
    // printf("accept a new write data\n");
  }
  if (wait_req_w) {
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_req_w->meta);
    // if this is the last beat
    if (meta->offset == meta->len && dram->will_accept(wait_req_w->address, true)) {
      dram->add_request(wait_req_w);
      wait_req_w = NULL;
    }
  }
}

void dramsim3_helper_falling(axi_channel &axi) {
  // default branch to avoid wrong handshake
  axi.aw.ready = 0;
  axi.w.ready  = 0;
  axi.b.valid  = 0;
  axi.ar.ready = 0;
  axi.r.valid  = 0;

  // RDATA: if finished, we try the next rdata response
  if (!wait_resp_r)
    wait_resp_r = dram->check_read_response();
  // if there's some data response, put it onto axi bus
  if (wait_resp_r) {
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_resp_r->req->meta);
    // printf("meta->size %d offset %d\n", meta->size, meta->offset*meta->size/sizeof(uint64_t));
    void *data_start = meta->data + meta->offset*meta->size / sizeof(uint64_t);
    axi_put_rdata(axi, data_start, meta->size, meta->offset == meta->len - 1, meta->id);
  }

  // RADDR: check whether the read request can be accepted
  axi_addr_t raddr;
  if (axi_get_raddr(axi, raddr) && dram->will_accept(raddr, false)) {
    axi_accept_raddr(axi);
    // printf("try to accept read request to 0x%lx\n", raddr);
  }

  // WREQ: check whether the write request can be accepted
  // Note: block the next write here to simplify logic
  axi_addr_t waddr;
  if (wait_req_w == NULL && axi_get_waddr(axi, waddr) && dram->will_accept(waddr, true)) {
    axi_accept_waddr(axi);
    axi_accept_wdata(axi);
    // printf("try to accept write request to 0x%lx\n", waddr);
  }

  // WDATA: check whether the write data can be accepted
  if (wait_req_w != NULL && dram->will_accept(wait_req_w->address, true)) {
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_req_w->meta);
    // we have to check whether the last finished write request has been accepted by dram
    if (meta->offset != meta->len) {
      axi_accept_wdata(axi);
    }
  }

  // WRESP: if finished, we try the next write response
  if (!wait_resp_b)
    wait_resp_b = dram->check_write_response();
  // if there's some write response, put it onto axi bus
  if (wait_resp_b) {
    dramsim3_meta *meta = static_cast<dramsim3_meta *>(wait_resp_b->req->meta);
    axi_put_wack(axi, meta->id);
  }
}

#endif
