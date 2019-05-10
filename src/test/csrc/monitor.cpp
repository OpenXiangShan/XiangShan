#include "common.h"

int monitor_state = STATE_RUNNING;

extern "C" void monitor(int code) {
  monitor_state = code;
}
