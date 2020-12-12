#include "snapshot.h"
#include <zlib.h>
#include<sys/time.h>

#ifdef VM_SAVABLE

double calcTime(timeval s, timeval e) {
  double sec, usec;
  sec = e.tv_sec - s.tv_sec;
  usec = e.tv_usec - s.tv_usec;
  return 1000*sec + usec/1000.0;
}

// Return whether the file is a gz file
int VerilatedRestoreMem::isGzFile(const char *filename) {
  assert(filename != NULL && strlen(filename) >= 4);
  return !strcmp(filename + (strlen(filename) - 3), ".gz");
}

long VerilatedSaveMem::compressToFile(uint8_t *ptr, const char *filename, long buf_size) {
  gzFile compressed_mem = gzopen(filename, "wb");

  if(compressed_mem == NULL) {
    printf("Can't open compressed binary file '%s'", filename);
    return -1;
  }

  long curr_size = 0;
  const uint32_t chunk_size = 16384;
  long *temp_page = new long[chunk_size];
  long *pmem_current = (long*)ptr;

  while (curr_size < buf_size) {
    memset(temp_page, 0, chunk_size * sizeof(long));
    for (uint32_t x = 0; x < chunk_size / sizeof(long); x++) {
      pmem_current = (long*)((uint8_t*)ptr + curr_size + x * sizeof(long));
      if (*pmem_current != 0) {
        *(temp_page + x) = *pmem_current;
      }
    }
    uint32_t bytes_write = gzwrite(compressed_mem, temp_page, chunk_size);
    if (bytes_write <= 0) { printf("Compress failed\n"); break; }
    curr_size += bytes_write;
    // assert(bytes_write % sizeof(long) == 0);

  }
  printf("Write %lu bytes from gz stream in total\n", curr_size);

  delete [] temp_page;

  if(gzclose(compressed_mem)) {
    printf("Error closing '%s'\n", filename);
    return -1;
  }
  return curr_size;
}

long VerilatedRestoreMem::readFromGz(void* ptr, const char *file_name, long buf_size) {
  assert(buf_size > 0);
  gzFile compressed_mem = gzopen(file_name, "rb");

  if(compressed_mem == NULL) {
    printf("Can't open compressed binary file '%s'", file_name);
    return -1;
  }

  uint64_t curr_size = 0;
  const uint32_t chunk_size = 16384;
  long *temp_page = new long[chunk_size];
  long *pmem_current = (long*)ptr;

  while (curr_size < buf_size) {
    uint32_t bytes_read = gzread(compressed_mem, temp_page, chunk_size);
    if (bytes_read == 0) { break; }
    // assert(bytes_read % sizeof(long) == 0);
    for (uint32_t x = 0; x < bytes_read / sizeof(long) + 1; x++) {
      if (*(temp_page + x) != 0) {
        pmem_current = (long*)((uint8_t*)ptr + curr_size + x * sizeof(long));
        *pmem_current = *(temp_page + x);
      }
    }
    curr_size += bytes_read;
  }
  printf("Read %lu bytes from gz stream in total\n", curr_size);

  delete [] temp_page;

  if(gzclose(compressed_mem)) {
    printf("Error closing '%s'\n", file_name);
    return -1;
  }
  return curr_size;
}

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
  if(size <= (512 * 1024 * 1024UL)){
    FILE *fp = fopen(m_filename.c_str(), "w");
    assert(fp != NULL);
    fwrite(buf, size, 1, fp);
    fclose(fp);
  } else {
    timeval s, e;
    gettimeofday(&s, NULL);
    compressToFile(buf, (m_filename + ".gz").c_str(), size);
    gettimeofday(&e, NULL);
    printf("Compress cost time (msec.usec): %lf\n", calcTime(s, e));
  }
  size = 0;
  printf("save snapshot to %s...\n", m_filename.c_str());
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
      size = readFromGz(buf, filename, buf_size);
      gettimeofday(&e, NULL);
      printf("Uncompress cost time (msec.usec): %lf\n", calcTime(s, e));
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
