#ifndef _ROCKET_VERILATOR_H
#define _ROCKET_VERILATOR_H

#include "verilated_vcd_c.h"
#include <stdlib.h>
#include <stdio.h>

extern bool verbose;
extern bool done_reset;

class VerilatedVcdFILE : public VerilatedVcdFile {
 public:
  VerilatedVcdFILE(FILE* file) : file(file) {}
  ~VerilatedVcdFILE() {}
  bool open(const std::string& name) override {
    // file should already be open
    return file != NULL;
  }
  void close() override {
    // file should be closed elsewhere
  }
  ssize_t write(const char* bufp, ssize_t len) override {
    return fwrite(bufp, 1, len, file);
  }
 private:
  FILE* file;
};

#endif
