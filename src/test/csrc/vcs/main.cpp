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

extern "C" void simv_step() {
  if (difftest_step()) {
    printf("Difftest error\n");
  }
}
