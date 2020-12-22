#ifndef COMPRESS_H
#define COMPRESS_H

#include "common.h"

#include <zlib.h>
#include <sys/time.h>

#define LOAD_SNAPSHOT 0
#define LOAD_RAM 1

double calcTime(timeval s, timeval e);

int isGzFile(const char *filename);
long snapshot_compressToFile(uint8_t *ptr, const char *filename, long buf_size);
long readFromGz(void* ptr, const char *file_name, long buf_size, uint8_t load_type);

void nonzero_large_memcpy(const void* __restrict dest, const void* __restrict src, size_t n);

#endif
