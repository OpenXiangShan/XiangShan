#!/usr/bin/env bash
# Run a workload under difftest and print the numbers that matter for the second block work.
#
# usage: scripts/run-block2-eval.sh <emu> <workload.bin> <logfile>
set -u

EMU=${1:?emu binary}
BIN=${2:?workload}
LOG=${3:?log file}

export NOOP_HOME=${NOOP_HOME:-$(pwd)}
export NEMU_HOME=${NEMU_HOME:-$(pwd)/ready-to-run}

"$EMU" -i "$BIN" --diff "$NOOP_HOME/ready-to-run/riscv64-nemu-interpreter-so" > "$LOG" 2>&1
status=$?

echo "=== $(basename "$BIN") via $(basename "$EMU") (exit $status) ==="
grep -E "HIT GOOD TRAP|ABORT at pc|different at pc" "$LOG" | head -5
grep -E "instrCnt = .*IPC" "$LOG"

echo "--- second block ---"
grep -oE "bpu: (s3SecondBlock[A-Za-z0-9]*|s2Override|s3Override), [0-9]+" "$LOG" | sed 's/^bpu: //' | sort -u
echo "--- block2 predictor lookups ---"
grep -oE "block2: lookup[A-Za-z0-9]*, [0-9]+" "$LOG" | sed 's/^block2: //' | sort -u
echo "--- pTAGE ---"
grep -oE "ptage: (predHit|predMiss|predTwoBlocks|trainEvent|trainPaired|trainP2Kept|trainP2Dropped), [0-9]+" "$LOG" | sed 's/^ptage: //' | sort -u
echo "--- train refusal / frontend stalls ---"
grep -oE "bpu: trainRefused[A-Za-z0-9_]*, [0-9]+" "$LOG" | sed 's/^bpu: //' | sort -u
grep -oE "perfAnalyzer: fetch_bubble_ftq_not_valid, [0-9]+" "$LOG" | sort -u

exit $status
