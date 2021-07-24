/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

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
