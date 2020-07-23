#include "emu.h"

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
  VerilatedSave stream;
  stream.open(filename);
  stream << *dut_ptr;

  long size = get_ram_size();
  stream.write(&size, sizeof(size));
  stream.write(get_ram_start(), size);
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
