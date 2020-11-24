#include "snapshot.h"

#ifdef VM_SAVABLE
void VerilatedSaveMem::flush() {
  long flush_size = m_cp - m_bufp;
  assert(buf_size - size > flush_size);
  memcpy(buf + size, m_bufp, flush_size);
  size += flush_size;
  m_cp = m_bufp;
}

void VerilatedSaveMem::save() {
  if (size == 0) return;
  trailer();
  flush();
  FILE *fp = fopen(m_filename.c_str(), "w");
  assert(fp != NULL);
  fwrite(buf, size, 1, fp);
  fclose(fp);
  size = 0;
  printf("save snapshot to %s...\n", m_filename.c_str());
}
#endif
