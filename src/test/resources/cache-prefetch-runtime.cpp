// Runtime regression for cache.CachePrefetchRuntimeMain.
// Link the emitted constantin.cpp and difftest's real constantin_static.cpp.
#include "VCachePrefetchRuntimeHarness.h"
#include "verilated.h"
#include <cstdio>
#include <cstdlib>

extern void constantinLoad(const char *path);

static void check(bool condition, const char *message) {
  if (!condition) {
    std::fprintf(stderr, "FAIL: %s\n", message);
    std::exit(1);
  }
}

int main(int argc, char **argv) {
  check(argc == 4, "expected cst-file, expected-hart0, expected-hart1");
  Verilated::commandArgs(argc, argv);
  constantinLoad(argv[1]);
  const bool enabled = std::atoi(argv[2]);
  const bool otherEnabled = std::atoi(argv[3]);
  VCachePrefetchRuntimeHarness dut;
  auto tick = [&]() {
    dut.clock = 0; dut.eval();
    dut.clock = 1; dut.eval();
    dut.clock = 0; dut.eval();
  };
  dut.reset = 1;
  dut.io_csrEnable = 1;
  dut.io_coreRequest_valid = 0;
  dut.io_coreRequest_bits = 0x1234;
  dut.io_coreOutput_ready = 0;
  dut.io_receive = 0;
  dut.io_train = 0;
  dut.io_address = 0x80000100;
  dut.io_l2Ready = 1;
  for (int i = 0; i < 8; ++i) tick();
  dut.reset = 0;
  tick();
  check(dut.io_enabled == enabled, "hart 0 Constantin override");
  check(dut.io_otherHartEnabled == otherEnabled, "independent hart 1 Constantin setting");

  dut.io_coreRequest_valid = 1;
  for (int i = 0; i < 8; ++i) {
    tick();
    check(dut.io_coreOutput_valid == enabled, "core prefetch blocked/preserved");
    check(dut.io_coreRequest_ready == !enabled, "disabled requests drain; enabled requests stall");
    check(dut.io_coreOutput_bits == 0x1234, "request payload preserved");
  }
  dut.io_coreOutput_ready = 1;
  tick();
  check(dut.io_coreRequest_ready, "enabled request proceeds after downstream becomes ready");

  int requests = 0;
  dut.io_receive = 1;
  for (int i = 0; i < 48; ++i) {
    tick();
    if (dut.io_l2Valid) {
      ++requests;
      check(dut.io_l2Address == 0x80000100, "real L2 receiver request address");
    }
  }
  check(enabled ? requests > 0 : requests == 0, "master switch controls real L2 requests");

  dut.io_csrEnable = 0;
  for (int i = 0; i < 32; ++i) tick();
  for (int i = 0; i < 16; ++i) {
    tick();
    check(!dut.io_l2Valid, "master enable does not override disabled CSR");
  }
  dut.io_csrEnable = 1;
  int resumed = 0;
  for (int i = 0; i < 48; ++i) {
    tick();
    resumed += dut.io_l2Valid;
  }
  check(enabled ? resumed > 0 : resumed == 0, "CSR re-enable still respects master switch");
  // Disabled prefetchers must not translate training addresses or stall training.
  if (!enabled) {
    dut.io_train = 1;
    for (int i = 0; i < 256; ++i) {
      dut.io_address = 0x80000000 + (i % 64) * 64;
      tick();
      check(dut.io_trainReady, "disabled L2 training never backpressures its source");
      check(!dut.io_tlbValid, "disabled prefetch training never issues TLB requests");
      check(!dut.io_l2Valid, "disabled prefetch training never emits cache requests");
    }
  }
  dut.final();
  std::printf("PASS hart0=%d hart1=%d L2_requests=%d resumed=%d\n",
              enabled, otherEnabled, requests, resumed);
  return 0;
}
