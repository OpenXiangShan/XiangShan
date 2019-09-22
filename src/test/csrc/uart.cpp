#include "common.h"
#include "stdlib.h"

#define QUEUE_SIZE 1024
static char queue[QUEUE_SIZE] = {};
static int f = 0, r = 0;

static void uart_enqueue(char ch) {
  int next = (r + 1) % QUEUE_SIZE;
  if (next != f) {
    // not full
    queue[r] = ch;
    r = next;
  }
}

// generate a random key every 1s for pal
static int rand_key(void) {
  uint32_t uptime(void);
  static uint32_t lasttime = 0;
  uint32_t now = uptime();

  if (now - lasttime > 1000) {
    lasttime = now;
    return "uiojkl"[rand()% 6];
  } else {
    return 0;
  }
}

int uart_getc(void) {
  int k = 0;
  if (f != r) {
    k = queue[f];
    f = (f + 1) % QUEUE_SIZE;
  } else {
    k = rand_key();
  }
  return k;
}

void uart_putc(char c) {
  eprintf("%c", c);
}

static void preset_input() {
  char rtthread_cmd[128] = "memtrace\n";
  char init_cmd[128] = "2";     // choose PAL
  char *buf = init_cmd;
  int i;
  for (i = 0; i < strlen(buf); i ++) {
    uart_enqueue(buf[i]);
  }
}

void init_uart(void) {
  preset_input();
}
