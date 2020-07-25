#include "emu.h"
#include <verilated_save.h>

class VerilatedSaveMem : public VerilatedSave {
  const static long buf_size = 1024 * 1024 * 1024;
  uint8_t *buf;
  long size;

public:
  VerilatedSaveMem(const char *filename) {
    buf = new uint8_t[buf_size];
    size = 0;
    m_filename = filename;
    header();
  }

  ~VerilatedSaveMem() {
    delete buf;
  }

  void mywrite(const void* __restrict datap, size_t size) VL_MT_UNSAFE_ONE {
    memcpy(buf + this->size, datap, size);
    this->size += size;
  }

  void close() { }
  void flush() {
    long flush_size = m_cp - m_bufp;
    assert(buf_size - size > flush_size);
    memcpy(buf + size, m_bufp, flush_size);
    size += flush_size;
    m_cp = m_bufp;
  }

  void clear() { size = 0; }

  void save() {
    if (size == 0) return;
    trailer();
    flush();
    FILE *fp = fopen(m_filename.c_str(), "w");
    assert(fp != NULL);
    fwrite(buf, size, 1, fp);
    fclose(fp);
    size = 0;
  }
};

void* get_ram_start();
long get_ram_size();

char* Emulator::my_strftime(time_t time) {
  static char buf[64];
  strftime(buf, sizeof(buf), "%F@%T", localtime(&time));
  return buf;
}

char* Emulator::snapshot_filename(const char *name) {
  static char buf[1024];
  char *noop_home = getenv("NOOP_HOME");
  assert(noop_home != NULL);
  snprintf(buf, 1024, "%s/build/%s.snapshot", noop_home, name);
  return buf;
}

void Emulator::snapshot_save(const char *filename) {
  VerilatedSaveMem stream(filename);
  stream << *dut_ptr;
  stream.flush();

  long size = get_ram_size();
  stream.mywrite(&size, sizeof(size));
  stream.mywrite(get_ram_start(), size);

  stream.save();
}

void Emulator::snapshot_load(const char *filename) {
  VerilatedRestore stream;
  stream.open(filename);
  stream >> *dut_ptr;

  long size;
  stream.read(&size, sizeof(size));
  assert(size == get_ram_size());
  stream.read(get_ram_start(), size);
}
