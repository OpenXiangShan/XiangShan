#include "common.h"

enum {
  STATE_GOODTRAP = 0,
  STATE_BADTRAP,
  STATE_RUNNING = -1
};

bool is_finish = false;
static int g_trapCode = 0;
static int g_trapPC = 0;
static int g_cycleCnt = 0, g_instrCnt = 0;

extern "C" void monitor(int trapCode, int trapPC, int cycleCnt, int instrCnt) {
  g_trapCode = trapCode;
  g_trapPC = trapPC;
  g_cycleCnt = cycleCnt;
  g_instrCnt = instrCnt;
  is_finish = true;
}

void display_trapinfo(void) {
  switch (g_trapCode) {
    case STATE_GOODTRAP:
      eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP at pc = 0x%08x\n" ANSI_COLOR_RESET, g_trapPC);
      break;
    case STATE_BADTRAP:
      eprintf(ANSI_COLOR_RED "HIT BAD TRAP at pc = 0x%08x\n" ANSI_COLOR_RESET, g_trapPC);
      break;
  }

  double ipc = (double)g_instrCnt / g_cycleCnt;
  eprintf(ANSI_COLOR_MAGENTA "instrCnt = %d, cycleCnt = %d, IPC = %lf\n" ANSI_COLOR_RESET,
      g_instrCnt, g_cycleCnt, ipc);
}
