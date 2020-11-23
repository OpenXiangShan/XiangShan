#ifndef SNAPSHOT_H
#define SNAPSHOT_H

#ifdef VM_SAVABLE
#include "VXSSimSoC.h"
#include <verilated_save.h>

class VerilatedSaveMem : public VerilatedSave {
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
#endif

#endif
