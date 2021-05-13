#include <common.h>
#include <locale.h>
#include "difftest.h"
#include "device.h"
#include "goldenmem.h"
#include "ram.h"

static bool has_reset = false;

extern "C" void simv_init() {
  printf("simv compiled at %s, %s\n", __DATE__, __TIME__);
  setlocale(LC_NUMERIC, "");

  init_goldenmem();
  difftest_init();
  init_device();

  assert_init();
  init_ram("ram.bin");

}

extern "C" int simv_step() {
  if (assert_count > 0) {
    return 1;
  }
  if (difftest_state() != -1) {
    int trapCode = difftest_state();
    switch (trapCode) {
      case 0:
        eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP\n" ANSI_COLOR_RESET);
        break;
      default:
        eprintf(ANSI_COLOR_RED "Unknown trap code: %d\n", trapCode);
    }
    return trapCode + 1;
  }
  return difftest_step();
}
