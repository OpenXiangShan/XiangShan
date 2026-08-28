#!/bin/bash
make clean
python3 ./scripts/xiangshan.py --build \
  --config "DefaultConfig" \
  --dramsim3 /nfs/home/share/ci-workloads/DRAMsim3 --with-dramsim3 \
  --threads 1 \
  --emulator gsim \
  --pgo /nfs/home/chenzhuo/plrutest/XiangShan/ready-to-run/coremark-2-iteration.bin \
  --llvm-profdata llvm-profdata
  > >(tee .build.log) 2> >(tee .build.err)
