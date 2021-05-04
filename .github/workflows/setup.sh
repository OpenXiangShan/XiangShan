export NEMU_HOME=/home/ci-runner/xsenv/NEMU
export RVTEST_HOME=/home/ci-runner/xsenv/riscv-tests
export AM_HOME=/home/ci-runner/xsenv/nexus-am
export DRAMSIM3_HOME=/home/ci-runner/xsenv/DRAMsim3

if [[ -z "$PERF_HOME" ]]; then
  echo "PERF_HOME is not set"
  exit 1
fi

