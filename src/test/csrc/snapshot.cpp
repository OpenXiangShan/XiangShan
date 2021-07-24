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

#include "snapshot.h"
#include "compress.h"

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
  auto saved_filename = m_filename;
  if (size <= (512 * 1024 * 1024UL)) {
    FILE *fp = fopen(saved_filename.c_str(), "w");
    assert(fp != NULL);
    fwrite(buf, size, 1, fp);
    fclose(fp);
  }
  else {
    saved_filename = saved_filename + ".gz";
    // timeval s, e;
    // gettimeofday(&s, NULL);
    snapshot_compressToFile(buf, saved_filename.c_str(), size);
    // gettimeofday(&e, NULL);
    // printf("Compress cost time (msec.usec): %lf\n", calcTime(s, e));
  }
  size = 0;
  printf("save snapshot to %s...\n", saved_filename.c_str());
}

void VerilatedRestoreMem::fill() {
  m_assertOne.check();
  if (VL_UNLIKELY(!isOpen())) return;
  // Move remaining characters down to start of buffer.  (No memcpy, overlaps allowed)
  vluint8_t* rp = m_bufp;
  for (vluint8_t* sp = m_cp; sp < m_endp; *rp++ = *sp++) {}  // Overlaps
  m_endp = m_bufp + (m_endp - m_cp);
  m_cp = m_bufp;  // Reset buffer
  // Read into buffer starting at m_endp
  while (true) {
      ssize_t remaining = (m_bufp + bufferSize() - m_endp);
      if (remaining == 0) break;
      errno = 0;
      ssize_t got = unbuf_read(m_endp, remaining);
      if (got > 0) {
          m_endp += got;
      } else if (VL_UNCOVERABLE(got < 0)) {
          if (VL_UNCOVERABLE(errno != EAGAIN && errno != EINTR)) {
              // LCOV_EXCL_START
              // write failed, presume error (perhaps out of disk space)
              std::string msg = std::string(__FUNCTION__) + ": " + strerror(errno);
              VL_FATAL_MT("", 0, "", msg.c_str());
              close();
              break;
              // LCOV_EXCL_STOP
          }
      } else {  // got==0, EOF
          // Fill buffer from here to end with NULLs so reader's don't
          // need to check eof each character.
          while (m_endp < m_bufp + bufferSize()) *m_endp++ = '\0';
          break;
      }
  }
}

void VerilatedRestoreMem::open(const char* filename) {
  m_assertOne.check();
  if (isOpen()) return;
  VL_DEBUG_IF(VL_DBG_MSGF("- restore: opening restore file %s\n", filenamep););

  if (VL_UNCOVERABLE(filename[0] == '|')) {
      assert(0);  // LCOV_EXCL_LINE // Not supported yet.
  } else {
    if(isGzFile(filename)) {
      timeval s, e;
      gettimeofday(&s, NULL);
      size = readFromGz(buf, filename, buf_size, LOAD_SNAPSHOT);
      gettimeofday(&e, NULL);
      // printf("Uncompress cost time (msec.usec): %lf\n", calcTime(s, e));
      assert(size > 0);
    } else {
      FILE *fp = fopen(filename, "r");
      assert(fp != NULL);

      fseek(fp, 0, SEEK_END);
      size = ftell(fp);
      rewind(fp);
      assert(fread(buf, size, 1, fp) > 0);
      fclose(fp);
    }
  }
  m_isOpen = true;
  m_filename = filename;
  m_cp = m_bufp;
  m_endp = m_bufp;
  header();
}

void VerilatedRestoreMem::close() {
  if (!isOpen()) return;
    trailer();
    flush();
    m_isOpen = false;
}

long VerilatedRestoreMem::unbuf_read(uint8_t* dest, long rsize) {
  assert(rsize > 0);
  assert(buf_size > 0);
  assert(buf_ptr >= 0);
  if(buf_ptr + rsize > size) {
    rsize = size - buf_ptr;
  }
  for(long i = 0; i < rsize; i++) {
    dest[i] = buf[buf_ptr + i];
  }
  buf_ptr += rsize;

  return rsize;
}
#endif
