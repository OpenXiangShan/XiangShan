#ifndef SNAPSHOT_H
#define SNAPSHOT_H

#ifdef VM_SAVABLE
#include "VXSSimSoC.h"
#include <verilated_save.h>

class VerilatedSaveMem : public VerilatedSerialize {
  const static long buf_size = 1024 * 1024 * 1024;
  uint8_t *buf;
  long size;

public:
  VerilatedSaveMem() {
    buf = new uint8_t[buf_size];
    size = 0;
  }
  ~VerilatedSaveMem() { delete buf; }

  void init(const char *filename) {
    size = 0;
    m_filename = filename;
    header();
  }

  void unbuf_write(const void* __restrict datap, size_t size) VL_MT_UNSAFE_ONE {
    memcpy(buf + this->size, datap, size);
    this->size += size;
  }

  void close() { }
  void flush();
  void save();
};

class VerilatedRestoreMem : public VerilatedDeserialize {
  const static long buf_size = (1024 * 1024 * 1024UL);
  uint8_t *buf;
  long size, buf_ptr;
  // gzFile compressed_mem;

public:
  VerilatedRestoreMem() {
    buf = new uint8_t[buf_size];
    size = 0;
  }
  ~VerilatedRestoreMem() { close(); delete buf; }

  void open(const char* filenamep) VL_MT_UNSAFE_ONE;
  void open(const std::string& filename) VL_MT_UNSAFE_ONE { open(filename.c_str()); }

  int unbuf_read(uint8_t* dest, long rsize);

  void close();
  void flush() {}
  void fill();
};
#endif

#endif
