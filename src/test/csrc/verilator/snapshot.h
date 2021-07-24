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

#ifndef SNAPSHOT_H
#define SNAPSHOT_H

#ifdef VM_SAVABLE
#include "VSimTop.h"
#include <verilated_save.h>
#include <sys/mman.h>
#include "compress.h"
#include "ram.h"

#define SNAPSHOT_SIZE (3UL * EMU_RAM_SIZE)

class VerilatedSaveMem : public VerilatedSerialize {
  const static long buf_size = SNAPSHOT_SIZE;
  uint8_t *buf = NULL;
  long size;

public:
  VerilatedSaveMem() {
    buf = NULL;
    size = 0;
  }
  ~VerilatedSaveMem() { }

  void init(const char *filename) {
    if (buf != NULL) {
      munmap(buf, SNAPSHOT_SIZE);
      buf = NULL;
    }
    buf = (uint8_t*)mmap(NULL, buf_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (buf == (uint8_t *)MAP_FAILED) {
      printf("Cound not mmap 0x%lx bytes\n", SNAPSHOT_SIZE);
      assert(0);
    }
    size = 0;
    m_filename = filename;
    header();
  }

  void unbuf_write(const void* __restrict datap, size_t size) VL_MT_UNSAFE_ONE {
    nonzero_large_memcpy(buf + this->size, datap, size);
    this->size += size;
  }

  void close() { }
  void flush();
  void save();
};

class VerilatedRestoreMem : public VerilatedDeserialize {
  const static long buf_size = SNAPSHOT_SIZE;
  uint8_t *buf;
  long size, buf_ptr;
  // gzFile compressed_mem;

public:
  VerilatedRestoreMem() {
    buf = (uint8_t*)mmap(NULL, buf_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (buf == (uint8_t *)MAP_FAILED) {
      printf("Cound not mmap 0x%lx bytes\n", SNAPSHOT_SIZE);
      assert(0);
    }
    size = 0;
    buf_ptr = 0;
  }
  ~VerilatedRestoreMem() { close(); }

  void open(const char* filenamep) VL_MT_UNSAFE_ONE;
  void open(const std::string& filename) VL_MT_UNSAFE_ONE { open(filename.c_str()); }

  long unbuf_read(uint8_t* dest, long rsize);

  void close() override VL_MT_UNSAFE_ONE;
  void flush() override VL_MT_UNSAFE_ONE {}
  void fill() override VL_MT_UNSAFE_ONE;
};
#endif

#endif
