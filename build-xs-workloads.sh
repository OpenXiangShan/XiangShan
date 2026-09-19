#!/usr/bin/env bash

set -uo pipefail

export AM_HOME=/home/liuquanchen/projects/xs-env/nexus-am
export LINUX_GNU_TOOLCHAIN=1
OUTPUT_DIR=/home/liuquanchen/projects/xs-env/XiangShan/ready-to-run
build_failed=0
GREEN=$'\033[1;32m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
mkdir -p "$OUTPUT_DIR"

echo "========== 开始构建 coremark =========="
if cd "$AM_HOME/apps/coremark" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: coremark-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: coremark 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 dhrystone =========="
if cd "$AM_HOME/apps/dhrystone" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: dhrystone-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: dhrystone 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 hello =========="
if cd "$AM_HOME/apps/hello" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: hello-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: hello 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 hpmdriver =========="
if cd "$AM_HOME/apps/hpmdriver" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: hpmdriver-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: hpmdriver 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 litenes_opt =========="
if cd "$AM_HOME/apps/litenes_opt" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: litenes-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: litenes_opt 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 maprobe =========="
if cd "$AM_HOME/apps/maprobe" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: maprobe-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: maprobe 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 mem_test_bw =========="
if cd "$AM_HOME/apps/mem_test/mem_test_bw" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: mem_test_bw-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: mem_test_bw 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 mem_test_latency =========="
if cd "$AM_HOME/apps/mem_test/mem_test_latency" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: build-mem_test_latency--riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: mem_test_latency 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 microbench =========="
if cd "$AM_HOME/apps/microbench" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: microbench-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: microbench 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 rva-trigger =========="
if cd "$AM_HOME/apps/rva-trigger" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: rva-trigger-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: rva-trigger 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 rvv-trigger =========="
if cd "$AM_HOME/apps/rvv-trigger" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: rvv-trigger-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: rvv-trigger 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 slider =========="
if cd "$AM_HOME/apps/slider" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: slider-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: slider 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 stream =========="
if cd "$AM_HOME/apps/stream" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: stream-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: stream 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 typing =========="
if cd "$AM_HOME/apps/typing" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: typing-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: typing 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 开始构建 zcb-test =========="
if cd "$AM_HOME/apps/zcb-test" &&
  make ARCH=riscv64-xs -j8 &&
  cp ./build/*.bin "$OUTPUT_DIR/"; then
  printf '%sSUCCESS%s: zcb-test-riscv64-xs.bin\n' "$GREEN" "$RESET"
else
  printf '%sERROR%s: zcb-test 构建或复制失败，继续构建后续 workload\n' "$RED" "$RESET" >&2
  build_failed=1
fi

echo "========== 全部 workload 处理完成 =========="
echo ".bin 文件保存目录：$OUTPUT_DIR"

if ((build_failed == 0)); then
  printf '%s' "$GREEN"
  echo "██████╗  █████╗ ███████╗███████╗"
  echo "██╔══██╗██╔══██╗██╔════╝██╔════╝"
  echo "██████╔╝███████║███████╗███████╗"
  echo "██╔═══╝ ██╔══██║╚════██║╚════██║"
  echo "██║     ██║  ██║███████║███████║"
  echo "╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝"
  printf '%s' "$RESET"
else
  printf '%sERROR%s: 存在 workload 构建或复制失败\n' "$RED" "$RESET" >&2
  exit 1
fi
