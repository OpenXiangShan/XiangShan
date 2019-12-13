#include "common.h"
#include <inttypes.h>

enum {
  STATE_GOODTRAP = 0,
  STATE_BADTRAP,
  STATE_ABORT,
  STATE_RUNNING = -1
};

static int g_trapCode = STATE_RUNNING;
static uint64_t g_trapPC = 0;
static uint64_t g_cycleCnt = 0, g_instrCnt = 0;

bool is_finish() { return g_trapCode != STATE_RUNNING; }

extern "C" void monitor(int trapCode, uint64_t trapPC, uint64_t cycleCnt, uint64_t instrCnt) {
  g_trapCode = trapCode;
  g_trapPC = trapPC;
  g_cycleCnt = cycleCnt;
  g_instrCnt = instrCnt;
}

void set_abort(void) {
  g_trapCode = STATE_ABORT;
}

int display_trapinfo(uint64_t max_cycles) {
  switch (g_trapCode) {
    case STATE_GOODTRAP:
      eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_BADTRAP:
      eprintf(ANSI_COLOR_RED "HIT BAD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_ABORT:
      eprintf(ANSI_COLOR_RED "ABORT at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_RUNNING:
      eprintf(ANSI_COLOR_RED "Timeout after %" PRIx64 " cycles\n" ANSI_COLOR_RESET, max_cycles);
      break;
  }

  double ipc = (double)g_instrCnt / g_cycleCnt;
  eprintf(ANSI_COLOR_MAGENTA "total guest instructions = %" PRIu64 "\n" ANSI_COLOR_RESET, g_instrCnt);
  eprintf(ANSI_COLOR_MAGENTA "instrCnt = %" PRIu64 ", cycleCnt = %" PRIu64 ", IPC = %lf\n" ANSI_COLOR_RESET,
      g_instrCnt, g_cycleCnt, ipc);
  return g_trapCode != STATE_GOODTRAP;
}
