#include <csignal>
#include "common.h"

int assert_count = -1;
static pthread_mutex_t assert_mutex;
int signal_num = 0;

void assert_init() {
  pthread_mutex_init(&assert_mutex, 0);
}

void assert_finish() {
  pthread_mutex_destroy(&assert_mutex);
}

extern "C" void xs_assert(long long line) {
  pthread_mutex_lock(&assert_mutex);
  if (assert_count >= 0) {
    printf("Assertion failed at line %lld.\n", line);
    assert_count++;
  }
  pthread_mutex_unlock(&assert_mutex);
}

void sig_handler(int signo) {
  if (signal_num != 0) 
    exit(0);
  signal_num = signo;
}
