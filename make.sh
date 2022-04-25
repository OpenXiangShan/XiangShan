make verilog CONFIG=LabeledConfig NUM_CORES=2
make emu CONFIG=LabeledConfig NUM_CORES=2 EMU_TRACE=1 EMU_THREADS=16 -j128
#./build/emu -i "./ready-to-run/coremark-2-iteration.bin ./ready-to-run/coremark-2-iteration.bin"  --diff ./ready-to-run/riscv64-nemu-interpreter-dual-so --no-diff --dump-wave -b 0 -e 6000
#./build/emu -i "./ready-to-run/linux_bbl0.bin ./ready-to-run/linux_bbl1.bin"  --diff ./ready-to-run/riscv64-nemu-interpreter-dual-so --no-diff --dump-tl -b 0 -e 0 > dual_linux_32_log.txt
./build/emu -i "./ready-to-run/redis_linux_core0.bin ./ready-to-run/stream_linux_core1.bin"  --diff ./ready-to-run/riscv64-nemu-interpreter-dual-so --no-diff --dump-tl -b 0 -e 0