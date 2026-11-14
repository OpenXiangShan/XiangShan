#include "VStreamHistoryTestTop.h"
#include "verilated.h"
#include <array>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

static VStreamHistoryTestTop dut;
static uint64_t cycle = 0;
static void edge() {
  dut.clock = 1; dut.eval();
  dut.clock = 0; dut.eval();
  ++cycle;
}
static void check(uint64_t got, uint64_t want, const char *field) {
  if (got != want) throw std::runtime_error("cycle " + std::to_string(cycle) + " " + field +
    " got " + std::to_string(got) + " expected " + std::to_string(want));
}
static void reset() {
  dut.io_train_valid = 0; dut.io_nativeTrain = 0; dut.io_flush = 0;
  dut.io_candidates_0_valid = dut.io_candidates_1_valid = dut.io_candidates_2_valid = 0;
  dut.reset = 1; for (int i = 0; i < 4; ++i) edge(); dut.reset = 0; dut.eval();
}
int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);
  if (argc != 2) return 2;
  try {
    reset(); cycle = 0;
    std::ifstream input(argv[1]);
    if (!input) throw std::runtime_error("cannot open vectors");
    std::array<uint64_t, 27> v{};
    while (input >> v[0]) {
      for (size_t i = 1; i < v.size(); ++i) if (!(input >> v[i])) throw std::runtime_error("short vector");
      dut.io_train_valid = v[0]; dut.io_train_bits_eligible = v[1];
      dut.io_train_bits_pc = v[2]; dut.io_train_bits_vaddr = v[3] << 6;
      dut.io_flush = v[4];
#define CAND(L, OFF) dut.io_candidates_##L##_valid = 1; dut.io_candidates_##L##_region = v[OFF] >> 4; dut.io_candidates_##L##_bitmap = v[OFF+1];
      CAND(0, 5); CAND(1, 8); CAND(2, 11);
      dut.eval();
      check(dut.io_allow_0, v[7], "allow L1");
      check(dut.io_allow_1, v[10], "allow L2");
      check(dut.io_allow_2, v[13], "allow L3");
#define STATE(NAME, I) check(dut.io_inspect_##NAME, v[I], #NAME)
      STATE(valid, 14);
      if (v[14]) {
      STATE(frontier, 15); STATE(span, 16); STATE(offset, 17);
      STATE(streamDirection, 18); STATE(censored, 19); STATE(overflow, 20); STATE(epoch, 21); STATE(length, 22);
#if HISTORY_POLICY != 1
      STATE(second, 23);
#endif
      STATE(historyDecr, 24); STATE(confidence, 25); STATE(tag, 26);
      }
      edge();
    }
    std::cout << "Policy " << HISTORY_POLICY << ": " << cycle << " differential cycles PASS\n";
    reset();
    std::array<uint64_t, 3> original{}, filtered{}, blocked{};
    auto observe = [&]() {
#define OBS(L) if (dut.io_original_##L##_valid) ++original[L]; \
      if (dut.io_filtered_##L##_valid) { \
        check(dut.io_original_##L##_valid, 1, "no extra native candidates"); \
        check(dut.io_filtered_##L##_bits_region, dut.io_original_##L##_bits_region, "native target preserved"); \
        check(dut.io_filtered_##L##_bits_bit_vec, dut.io_original_##L##_bits_bit_vec, "native bitmap preserved"); \
        ++filtered[L]; \
      } else if (dut.io_original_##L##_valid) ++blocked[L];
      OBS(0); OBS(1); OBS(2);
    };
    auto tick = [&]() { dut.eval(); observe(); edge(); };
    for (int phase = 0; phase < 3; ++phase) {
      for (int segment = 0; segment < 12; ++segment) {
        int length = phase == 2 ? 96 : 8;
        int sign = phase == 1 ? -1 : 1;
        uint64_t base = 0x80000000ULL + phase * 0x10000000ULL + segment * 0x100000ULL;
        for (int b = 0; b < length * 16; ++b) {
          dut.io_nativeTrain = 1; dut.io_nativePC = 0x80001000ULL + phase * 4;
          dut.io_nativeVA = base + sign * int64_t(b * 64);
          dut.eval();
          int guard = 0;
          while (!dut.io_nativeReady) { tick(); if (++guard > 20) throw std::runtime_error("native deadlock"); }
          tick();
          dut.io_nativeTrain = 0; tick(); tick();
        }
      }
    }
    dut.io_nativeTrain = 0; for (int i = 0; i < 10; ++i) tick();
    for (int l = 0; l < 3; ++l) {
      if (!original[l] || !filtered[l] || !blocked[l]) throw std::runtime_error("unexercised native level " + std::to_string(l+1));
      std::cout << "L" << l+1 << " native=" << original[l] << " allowed=" << filtered[l] << " blocked=" << blocked[l] << '\n';
    }
    std::cout << "Stream integration PASS\n";
  } catch (const std::exception &e) { std::cerr << e.what() << '\n'; return 1; }
  dut.final();
}
