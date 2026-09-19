#!/usr/bin/env bash

set -uo pipefail

export NOOP_HOME=/home/liuquanchen/projects/xs-env/XiangShan
WORKLOAD_DIR="$NOOP_HOME/ready-to-run"
LOG_DIR="$NOOP_HOME/TestLog/BaseBenchmark"
run_failed=0
GREEN=$'\033[1;32m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
mkdir -p "$LOG_DIR"

echo "========== 开始执行 coremark =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/coremark-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/coremark-riscv64-xs.log"; then
  printf '%sSUCCESS%s: coremark-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: coremark 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 dhrystone =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/dhrystone-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/dhrystone-riscv64-xs.log"; then
  printf '%sSUCCESS%s: dhrystone-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: dhrystone 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 hello =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/hello-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/hello-riscv64-xs.log"; then
  printf '%sSUCCESS%s: hello-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: hello 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 hpmdriver =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/hpmdriver-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/hpmdriver-riscv64-xs.log"; then
  printf '%sSUCCESS%s: hpmdriver-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: hpmdriver 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 litenes_opt =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/litenes-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/litenes-riscv64-xs.log"; then
  printf '%sSUCCESS%s: litenes-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: litenes_opt 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 maprobe =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/maprobe-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/maprobe-riscv64-xs.log"; then
  printf '%sSUCCESS%s: maprobe-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: maprobe 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 mem_test_bw =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/mem_test_bw-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/mem_test_bw-riscv64-xs.log"; then
  printf '%sSUCCESS%s: mem_test_bw-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: mem_test_bw 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 mem_test_latency =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/build-mem_test_latency--riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/build-mem_test_latency--riscv64-xs.log"; then
  printf '%sSUCCESS%s: build-mem_test_latency--riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: mem_test_latency 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 microbench =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/microbench-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/microbench-riscv64-xs.log"; then
  printf '%sSUCCESS%s: microbench-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: microbench 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 rva-trigger =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/rva-trigger-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/rva-trigger-riscv64-xs.log"; then
  printf '%sSUCCESS%s: rva-trigger-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: rva-trigger 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 rvv-trigger =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/rvv-trigger-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/rvv-trigger-riscv64-xs.log"; then
  printf '%sSUCCESS%s: rvv-trigger-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: rvv-trigger 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 slider =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/slider-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/slider-riscv64-xs.log"; then
  printf '%sSUCCESS%s: slider-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: slider 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 stream =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/stream-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/stream-riscv64-xs.log"; then
  printf '%sSUCCESS%s: stream-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: stream 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 typing =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/typing-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/typing-riscv64-xs.log"; then
  printf '%sSUCCESS%s: typing-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: typing 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 开始执行 zcb-test =========="
if cd "$NOOP_HOME" &&
  ./build/emu -i "$WORKLOAD_DIR/zcb-test-riscv64-xs.bin" \
    2>&1 | tee "$LOG_DIR/zcb-test-riscv64-xs.log"; then
  printf '%sSUCCESS%s: zcb-test-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: zcb-test 执行失败，继续执行后续 workload\n' "$RED" "$RESET" >&2
  run_failed=1
fi

echo "========== 全部 workload 执行完成 =========="
echo "日志保存目录：$LOG_DIR"

if ((run_failed == 0)); then
  printf '%s' "$GREEN"
  echo "██████╗  █████╗ ███████╗███████╗"
  echo "██╔══██╗██╔══██╗██╔════╝██╔════╝"
  echo "██████╔╝███████║███████╗███████╗"
  echo "██╔═══╝ ██╔══██║╚════██║╚════██║"
  echo "██║     ██║  ██║███████║███████║"
  echo "╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝"
  printf '%s' "$RESET"
else
  printf '%sERROR%s: 存在 workload 执行失败\n' "$RED" "$RESET" >&2
  exit 1
fi
