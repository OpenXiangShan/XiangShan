#!/bin/bash
make clean
python3 ./scripts/xiangshan.py --build \
  --config "MinimalConfig" \
  --dramsim3 /nfs/home/share/ci-workloads/DRAMsim3 --with-dramsim3 \
  --threads 8 \
  --trace \
  --emulator verilator \
  > >(tee .build.log) 2> >(tee .build.err)
