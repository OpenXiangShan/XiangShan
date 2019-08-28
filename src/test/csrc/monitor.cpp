#include "common.h"

enum {
  STATE_GOODTRAP = 0,
  STATE_BADTRAP,
  STATE_ABORT,
  STATE_RUNNING = -1
};

static int g_trapCode = STATE_RUNNING;
static int g_trapPC = 0;
static int g_cycleCnt = 0, g_instrCnt = 0;

bool is_finish() { return g_trapCode != STATE_RUNNING; }

extern "C" void monitor(int trapCode, int trapPC, int cycleCnt, int instrCnt) {
  g_trapCode = trapCode;
  g_trapPC = trapPC;
  g_cycleCnt = cycleCnt;
  g_instrCnt = instrCnt;
}

void set_abort(void) {
  g_trapCode = STATE_ABORT;
}

void display_trapinfo(void) {
  switch (g_trapCode) {
    case STATE_GOODTRAP:
      eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP at pc = 0x%08x\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_BADTRAP:
      eprintf(ANSI_COLOR_RED "HIT BAD TRAP at pc = 0x%08x\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_ABORT:
      eprintf(ANSI_COLOR_RED "ABORT at pc = 0x%08x\n" ANSI_COLOR_RESET, g_trapPC);
      break;
  }

  double ipc = (double)g_instrCnt / g_cycleCnt;
  eprintf(ANSI_COLOR_MAGENTA "instrCnt = %d, cycleCnt = %d, IPC = %lf\n" ANSI_COLOR_RESET,
      g_instrCnt, g_cycleCnt, ipc);
}
