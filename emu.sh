# This script will do emulation for Xiangshan and run test by linux.bin

cd .. && source env.sh && cd XiangShan

# cd ../NEMU && make clean && make riscv64-xs-ref_defconfig && make -j && cd ../XiangShan

# make clean
# make init
make emu CONFIG=MinimalConfig -j128 EMU_OPTIMIZE= EMU_THREADS=16 EMU_TRACE=fst 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S).log
# make emu CONFIG=MinimalConfig -j128 EMU_OPTIMIZE= EMU_THREADS=16 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S).log
# make emu -j128 EMU_OPTIMIZE= EMU_THREADS=16 2>&1 EMU_TRACE=fst | tee > $(date +%Y-%m-%d_%H-%M-%S).log

# nohup ./build/emu -i $NOOP_HOME/ready-to-run/linux.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-linux.log &

# nohup ./build/emu -i $NOOP_HOME/ready-to-run/crypto-riscv64-xs.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-crypto.log &
# nohup ./build/emu -i $NOOP_HOME/ready-to-run/bitmanip-riscv64-xs.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-bitmanip.log &
# nohup ./build/emu -i $NOOP_HOME/ready-to-run/bitmanip-riscv64-xs-2.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-bitmanip.log &
# nohup ./build/emu -i $NOOP_HOME/ready-to-run/rv32uzbc-p-clmul.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-rv32uzbc.log &

# nohup ./build/emu -i $NOOP_HOME/ready-to-run/coremark-2-iteration.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-coremark.log &
# nohup ./build/emu -i $NOOP_HOME/ready-to-run/rv64mi-p-csr.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-csr.log &

# nohup ./build/emu --enable-fork -W 20000000 -I 40000000 -i /nfs/share/zyy/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0/tonto/23672/_23672_0.114033_.zstd 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-tonto.log &

# nohup ./build/emu -i /nfs/home/jijunxiong/xs-env/XiangShan/ready-to-run/crypto-riscv64-xs.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-crypto.log &

# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvkned-p-zvkned.bin --dump-wave -b 0 -e 10000 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned.log &
nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed.bin --dump-wave -b 0 -e 10000 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvksed.log &
# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvknha-p-zvknha.bin --dump-wave -b 0 -e 10000 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvknha.log &

# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvkned-p-zvkned.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned.log &
# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvksed.log &
# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvknha-p-zvknha.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvknha.log &

# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_realworld_vec.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec.log &
# nohup ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_realworld_normal.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal.log &

# ./build/riscv64-nemu-interpreter -b $NOOP_HOME/ready-to-run/linux.bin
# ./build/riscv64-nemu-interpreter -b /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvkned-p-vaesef_vs.bin

# bash tmp/cr-run.sh

# TEST: A CSR READ BUG?
nohup ./build/emu -i /nfs-nvme/home/share/lixin/csrtest-riscv64-xs.bin --dump-wave -b 0 -e 10000 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-csrtest.log &

# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_vec_1000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec_1000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_vec_2000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec_2000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_vec_3000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec_3000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_vec_4000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec_4000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_vec_5000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_vec_5000.log

# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_normal_1000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal_1000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_normal_2000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal_2000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_normal_3000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal_3000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_normal_4000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal_4000.log
# ./build/emu -i /nfs/home/jijunxiong/riscv-tests/isa/build/rv64zvksed-p-zvksed_real_normal_5000.bin 2>&1 | tee > $(date +%Y-%m-%d_%H-%M-%S)-zvkned_realworld_normal_5000.log
