#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <elf.h>
#include <inttypes.h>

#include <assert.h>
#include <sys/time.h>
#include <time.h>

enum { BOARD_ultraZ, BOARD_zedboard, BOARD_zcu102, BOARD_sidewinder };
static const struct BoardConfig {
  char *name;
  uintptr_t ddr_size;
  uintptr_t ddr_base;
  uintptr_t gpio_reset_base;
} board_config [] = {
  [BOARD_ultraZ] = {"ultraZ", 0x40000000, 0x40000000, 0x80001000},
  [BOARD_axu3cg] = {"axu3cg", 0x40000000, 0x40000000, 0x80001000},
  [BOARD_zedboard] = {"zedboard", 0x10000000, 0x10000000, 0x41200000},
  [BOARD_zcu102] = {"zcu102", 0x80000000, 0x800000000, 0x80010000},
  [BOARD_sidewinder] = {"sidewinder", 0x80000000, 0x800000000, 0x80010000}
};

#define NR_BOARD (sizeof(board_config) / sizeof(board_config[0]))

const struct BoardConfig *bc;

#define GPIO_RESET_TOTAL_SIZE	0x1000

void *ddr_base;
volatile uint32_t *gpio_reset_base;
int	fd;

static inline void my_fread(char *filename, uint64_t *addr) {
  FILE *fp = fopen(filename, "rb");
  assert(fp);

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  printf("sizeof(%s) = %ld\n", filename, size);

  fseek(fp, 0, SEEK_SET);
  fread(addr, size, 1, fp);

  fclose(fp);
}

void loader(char *imgfile, uintptr_t offset) {
  my_fread(imgfile, ddr_base + offset);
//  my_fread(dtbfile, ddr_base + offset + 0x8);
//  strcpy(ddr_base, "t");
  strcpy(ddr_base + 0x1000, "ref");
}

void* create_map(size_t size, int fd, off_t offset) {
  void *base = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, offset);

  if (base == MAP_FAILED) {
    perror("init_mem mmap failed:");
    close(fd);
    exit(1);
  }

  printf("mapping paddr 0x%lx to vaddr 0x%" PRIxPTR "\n", offset, (uintptr_t)base);

  return base;
}

void init_map() {
  fd = open("/dev/mem", O_RDWR|O_SYNC);
  if (fd == -1)  {
    perror("init_map open failed:");
    exit(1);
  }

  printf("board = %s, ddr_size = 0x%" PRIxPTR ", ddr_base = 0x%" PRIxPTR "\n",
		  bc->name, bc->ddr_size, bc->ddr_base);
  ddr_base = create_map(bc->ddr_size, fd, bc->ddr_base);
  gpio_reset_base = create_map(GPIO_RESET_TOTAL_SIZE, fd, bc->gpio_reset_base);
}

void finish_map() {
  munmap((void *)gpio_reset_base, GPIO_RESET_TOTAL_SIZE);
  munmap((void *)ddr_base, bc->ddr_size);
  close(fd);
}

void help() {
  printf("Usage: axi-loader [board] reset\n");
  printf("       axi-loader [board] fetch [ddr_offset]\n");
  printf("       axi-loader [board] [bin] [ddr_offset]\n");
  printf("Supported boards:\n");
  int i;
  for (i = 0; i < NR_BOARD; i ++) {
    printf("%s ", board_config[i].name);
  }
}

void resetn(int val) {
  gpio_reset_base[0] = val;
}

int main(int argc, char *argv[]) {
  if (argc > 1 && strcmp(argv[1], "-h") == 0) {
    help();
    return 0;
  }

  uintptr_t offset = 0;
  if (argc > 3) {
    char *p;
    offset = strtoll(argv[3], &p, 0);
    if (!(argv[3][0] != '\0' && *p == '\0')) {
      printf("invalid offset = %s, set offset = 0\n", argv[3]);
      offset = 0;
    }
  }

  int j;
  for (j = 0; j < NR_BOARD; j ++) {
    if (strcmp(argv[1], board_config[j].name) == 0) {
      bc = &board_config[j];
      break;
    }
  }
  if (j == NR_BOARD) {
    printf("invalid board = %s\n", argv[1]);
    help();
    exit(1);
  }

  init_map();

  if (strcmp(argv[2], "reset") == 0) {
    resetn(0);
    finish_map();
    return 0;
  }
  else if (strcmp(argv[2], "fetch") == 0) {
    printf("fetch word at offset = 0x%lx, result = 0x%08x\n",
		    offset, *(uint32_t *)(ddr_base + offset));
    finish_map();
    return 0;
  }

  printf("loading %s to offset = 0x%" PRIxPTR "\n", argv[2], offset);
  resetn(0);
  usleep(1000);
  loader(argv[2], offset);
  resetn(1);

  finish_map();

  return 0;
}
