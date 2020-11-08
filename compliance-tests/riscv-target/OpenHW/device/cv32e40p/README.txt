#
# Example flow
#

# setup environment
isetup
svsetup -xcelium

mkdir TOP
cd TOP
export TOP=$(pwd)

#
# core-v-verif
#
git clone https://github.com/<fork>/core-v-verif.git 
pushd core-v-verif
    # until merged onto Master
    git checkout rm_update
popd 

pushd core-v-verif/cv32/sim/uvmt_cv32
    MAKEVARS="SIMULATOR=xrun TARGET=XCELIUM"
    make ${MAKEVARS} clean_all
    make ${MAKEVARS} sanity
popd

#
# compliance
#
git clone https://github.com/<fork>/riscv-compliance

pushd riscv-compliance
    export CORE_V_VERIF=${TOP}/core-v-verif
    # setup gcc compiler and PREFIX
    # PATH=${PATH}:<PATH_TO_BIN>
    export RISCV_PREFIX=riscv-none-embed-

    make RISCV_TARGET=OpenHW RISCV_DEVICE=cv32e40p NOTRAPS=1 RISCV_ISA=rv32i
    make RISCV_TARGET=OpenHW RISCV_DEVICE=cv32e40p NOTRAPS=1 RISCV_ISA=rv32im
    make RISCV_TARGET=OpenHW RISCV_DEVICE=cv32e40p NOTRAPS=1 RISCV_ISA=rv32imc
    make RISCV_TARGET=OpenHW RISCV_DEVICE=cv32e40p NOTRAPS=1 RISCV_ISA=rv32Zicsr
    make RISCV_TARGET=OpenHW RISCV_DEVICE=cv32e40p NOTRAPS=1 RISCV_ISA=rv32Zifencei
popd
