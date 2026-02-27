#!/usr/bin/env bash

# This script is used to generate needed information for a bug report.

set -euo pipefail

XS_ROOT=$(realpath $(dirname "$(realpath "${BASH_SOURCE[0]:-$0}")")/..)
REPORT_ROOT=${XS_ROOT}/bug-report
REPORT_FILE=${REPORT_ROOT}.tar.gz

function report_value() {
  local file=$1
  local name=$2
  local value=$3
  echo "=== $name ===" >> "${REPORT_ROOT}/$file"
  echo "$value" >> "${REPORT_ROOT}/$file"
  echo >> "${REPORT_ROOT}/$file"
}

function report() {
  local file=$1
  local name=$2
  local cmd=$3
  echo "  -> $cmd"
  report_value "$file" "$name" "$(bash -c "$cmd")"
}

function init() {
  echo "Generating bug report..."

  rm -rf "${REPORT_ROOT}"
  rm -f "${REPORT_FILE}"
  mkdir -p "${REPORT_ROOT}"

  report "date" "date" "date"
}

function basic() {
  echo
  echo "Checking hw..."
  report "hardware" "cpu" "lscpu | grep 'Model name'"
  report "hardware" "memory" "free -h"
  report "hardware" "disk" "df -h ${XS_ROOT}"

  echo
  echo "Checking sw..."
  report "sw" "os" "cat /etc/os-release || cat /usr/lib/os-release || echo 'Unknown OS'"
  report "sw" "uname" "uname -a"
  report "sw" "python" "python3 --version 2>&1"
  report "sw" "gcc" "gcc --version 2>&1 | head -n 1"
  report "sw" "clang" "clang --version 2>&1 | head -n 1"
  report "sw" "glibc" "ldd --version 2>&1 | head -n 1"
  report "sw" "java" "java -version 2>&1 | head -n 1"
  report "sw" "mill" "mill -i --version 2>&1 | head -n 1"

  echo
  echo "Checking repo..."
  report "repo" "commit" "git -C ${XS_ROOT} log -1 --oneline --no-abbrev-commit"
  report "repo" "status" "git -C ${XS_ROOT} status --short"
  report "repo" "submodule" "git -C ${XS_ROOT} submodule status"
  report "repo" "diff" "git -C ${XS_ROOT} diff"
}

function runtime() {
  echo
  echo "Checking build/run info..."
  if [ "$LANG" = "zh_CN.UTF-8" ]; then
    echo "请提供您使用的编译命令（如 'make CONFIG=MinimalConfig -j8'）："
    read -r BUILD_CMD
    report_value "build" "command" "$BUILD_CMD"
    echo "请提供您使用的运行命令（如 './build/emu -i test.bin --diff ref.so'）："
    echo "如果您遇到的问题是编译失败或其它没有运行的情况，请直接按回车键跳过。"
    read -r RUN_CMD
    report_value "run" "command" "$RUN_CMD"
    echo "如果您有编译、运行日志、测试用例（二进制、源代码），"
    echo "请将它们复制到以下目录："
    echo "    ${REPORT_ROOT}"
    echo "随后按回车键继续生成报告。"
  else
    echo "Please provide the build command you used (e.g., 'make CONFIG=MinimalConfig -j8'):"
    read -r BUILD_CMD
    report_value "build" "command" "$BUILD_CMD"
    echo "Please provide the run command you used (e.g., './build/emu -i test.bin --diff ref.so'):"
    echo "If your issue is about build failure or other non-running cases, just press Enter to skip."
    read -r RUN_CMD
    report_value "run" "command" "$RUN_CMD"
    echo "If you have build or runtime logs, or test cases (binaries or source code),"
    echo "please copy them to the following directory:"
    echo "    ${REPORT_ROOT}"
    echo "Then press Enter to continue generating the report."
  fi
  read -r
}

function finalize() {
  echo
  echo "Compressing report..."
  tar -czf "${REPORT_FILE}" -C "${REPORT_ROOT}" .

  echo
  if [ "$LANG" = "zh_CN.UTF-8" ]; then
    echo "Bug 报告已生成在："
    echo "    ${REPORT_FILE}"
    echo "请在提交 Bug 报告时附上此文件。"
    echo "提交 Bug 报告请访问："
    echo "    https://github.com/OpenXiangShan/XiangShan/issues/new"
  else
    echo "Bug report generated at:"
    echo "    ${REPORT_FILE}"
    echo "Please attach this file when submitting your bug report."
    echo "Submit bug report:"
    echo "    https://github.com/OpenXiangShan/XiangShan/issues/new"
  fi
}

ONLY_BASIC=false
for args in "$@"; do
  case $args in
    --basic)
      ONLY_BASIC=true
      shift
      ;;
    *)
      echo "Unknown argument: $args"
      echo "Usage: $0 [--basic]"
      echo "  --basic: only include basic info (hardware, environment, repo)"
      exit 1
      ;;
  esac
done

init
basic
if [ "$ONLY_BASIC" = false ]; then
  runtime
fi
finalize
