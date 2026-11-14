#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p build/history-scratch
export TMPDIR="$PWD/build/history-scratch" TMP="$PWD/build/history-scratch" TEMP="$PWD/build/history-scratch"
export JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:-} -Djava.io.tmpdir=$TMPDIR"
export NOOP_HOME="$PWD"
free -h
nproc
lscpu -e=CPU,NODE,SOCKET,CORE,ONLINE
uptime
# Four host emulation threads; one simulated hart, as in the profiling builds.
make emu CONFIG=DefaultConfig EMU_THREADS=4 JVM_XMX=48G -j12
