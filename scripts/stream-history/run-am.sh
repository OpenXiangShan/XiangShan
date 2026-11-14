#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
: "${AM_HOME:?Set AM_HOME to a nexus-am checkout}"
cores=${1:?Pass four available physical CPU IDs, e.g. 32,33,34,35}
mkdir -p build/history-scratch build/history-am
export TMPDIR="$PWD/build/history-scratch" TMP="$PWD/build/history-scratch" TEMP="$PWD/build/history-scratch"
export NOOP_HOME="$PWD"
free -h > build/history-am/resources.log
nproc >> build/history-am/resources.log
lscpu -e=CPU,NODE,SOCKET,CORE,ONLINE >> build/history-am/resources.log
uptime >> build/history-am/resources.log
python3 - "$cores" <<'PY'
import os, subprocess, sys
ids = [int(x) for x in sys.argv[1].split(',')]
assert len(ids) == 4 and len(set(ids)) == 4, 'pass exactly four distinct CPU IDs'
assert set(ids) <= os.sched_getaffinity(0), 'CPU outside allocated affinity'
rows = [x.split(',') for x in subprocess.check_output(['lscpu', '-p=CPU,CORE,SOCKET'], text=True).splitlines() if not x.startswith('#')]
physical = [(int(core), int(socket)) for cpu,core,socket in rows if int(cpu) in ids]
assert len(set(physical)) == 4, 'four physical cores required, not SMT siblings'
PY
make -C tests/stream-history-am ARCH=riscv64-xs -j4 > build/history-am/compile.log 2>&1
sha256sum build/emu tests/stream-history-am/build/stream-history-smoke-riscv64-xs.bin > build/history-am/inputs.sha256
numactl --physcpubind="$cores" --localalloc ./build/emu \
  -i tests/stream-history-am/build/stream-history-smoke-riscv64-xs.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so --ram-size=256MB \
  --max-cycles=20000000 --max-instr=5000000 \
  > build/history-am/simulator-out.txt 2> build/history-am/simulator-err.txt
python3 scripts/stream-history/summarize.py build/history-am/simulator-out.txt \
  --require-smoke --json build/history-am/summary.json > build/history-am/summary.txt
cat build/history-am/summary.txt
