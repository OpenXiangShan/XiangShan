#include "compress.h"

double calcTime(timeval s, timeval e) {
  double sec, usec;
  sec = e.tv_sec - s.tv_sec;
  usec = e.tv_usec - s.tv_usec;
  return 1000*sec + usec/1000.0;
}

// Return whether the file is a gz file
int isGzFile(const char *filename) {
  assert(filename != NULL && strlen(filename) >= 4);
  return !strcmp(filename + (strlen(filename) - 3), ".gz");
}

long snapshot_compressToFile(uint8_t *ptr, const char *filename, long buf_size) {
  gzFile compressed_mem = gzopen(filename, "wb");

  if(compressed_mem == NULL) {
    printf("Can't open compressed binary file '%s'", filename);
    return -1;
  }

  long curr_size = 0;
  const uint32_t chunk_size = 16384;
  long *temp_page = new long[chunk_size];
  long *pmem_current = (long*)ptr;

  while (curr_size < buf_size) {
    memset(temp_page, 0, chunk_size * sizeof(long));
    for (uint32_t x = 0; x < chunk_size / sizeof(long); x++) {
      pmem_current = (long*)((uint8_t*)ptr + curr_size + x * sizeof(long));
      if (*pmem_current != 0) {
        *(temp_page + x) = *pmem_current;
      }
    }
    uint32_t bytes_write = gzwrite(compressed_mem, temp_page, chunk_size);
    if (bytes_write <= 0) { printf("Compress failed\n"); break; }
    curr_size += bytes_write;
    // assert(bytes_write % sizeof(long) == 0);

  }
  printf("Write %lu bytes from gz stream in total\n", curr_size);

  delete [] temp_page;

  if(gzclose(compressed_mem)) {
    printf("Error closing '%s'\n", filename);
    return -1;
  }
  return curr_size;
}

long readFromGz(void* ptr, const char *file_name, long buf_size, uint8_t load_type) {
  assert(buf_size > 0);
  gzFile compressed_mem = gzopen(file_name, "rb");

  if(compressed_mem == NULL) {
    printf("Can't open compressed binary file '%s'", file_name);
    return -1;
  }

  uint64_t curr_size = 0;
  const uint32_t chunk_size = 16384;

  // Only load from RAM need check
  if (load_type == LOAD_RAM && (buf_size % chunk_size) != 0) {
    printf("RAMSIZE must be divisible by chunk_size\n");
    assert(0);
  }
  
  long *temp_page = new long[chunk_size];
  long *pmem_current = (long*)ptr;

  while (curr_size < buf_size) {
    uint32_t bytes_read = gzread(compressed_mem, temp_page, chunk_size);
    if (bytes_read == 0) { break; }
    assert(load_type != LOAD_RAM || bytes_read % sizeof(long) == 0);
    for (uint32_t x = 0; x < bytes_read / sizeof(long) + 1; x++) {
      if (*(temp_page + x) != 0) {
        pmem_current = (long*)((uint8_t*)ptr + curr_size + x * sizeof(long));
        *pmem_current = *(temp_page + x);
      }
    }
    curr_size += bytes_read;
  }
  printf("Read %lu bytes from gz stream in total\n", curr_size);

  delete [] temp_page;

  if(gzclose(compressed_mem)) {
    printf("Error closing '%s'\n", file_name);
    return -1;
  }
  return curr_size;
}