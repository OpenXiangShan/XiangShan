#include "common.h"
#include "emu.h"

int assert_count = 0;
static pthread_mutex_t assert_mutex;

void assert_init() {
  pthread_mutex_init(&assert_mutex, 0);
}

void assert_finish() {
  pthread_mutex_destroy(&assert_mutex);
}

extern "C" void xs_assert(long long line) {
  pthread_mutex_lock(&assert_mutex);
  printf("Assertion failed at line %lld.\n", line);
  assert_count++;
  pthread_mutex_unlock(&assert_mutex);
}
