#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
policy=$(sed -n 's/.*StreamHistoryParams(policy: Int = \([123]\)).*/\1/p' src/main/scala/xiangshan/mem/prefetch/StreamHistory.scala)
mkdir -p build/history-scratch build/history-test
export TMPDIR="$PWD/build/history-scratch" TMP="$PWD/build/history-scratch" TEMP="$PWD/build/history-scratch"
export JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:-} -Djava.io.tmpdir=$TMPDIR"
export NOOP_HOME="$PWD"
python3 scripts/stream-history/generate-vectors.py --policy "$policy" --output build/history-test/vectors.txt > build/history-test/reference-events.json
mill -i -j 8 xiangshan.test.runMain xiangshan.mem.prefetch.StreamHistoryTestMain \
  --config DefaultConfig --target-dir build/history-test --target systemverilog \
  --firtool-opt '--default-layer-specialization=enable' > build/history-test/generate.log 2>&1
verilator --cc --exe --build -j 8 --assert -Wno-fatal \
  --top-module StreamHistoryTestTop --Mdir build/history-test/obj \
  -CFLAGS "-DHISTORY_POLICY=$policy" build/history-test/StreamHistoryTestTop.sv \
  "$PWD/scripts/stream-history/history-test.cpp" > build/history-test/verilator.log 2>&1
build/history-test/obj/VStreamHistoryTestTop build/history-test/vectors.txt | tee build/history-test/result.log
