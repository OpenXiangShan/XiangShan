#!/bin/bash


# NEED TO CHECK THAT TARGET IS riscvOVPsim or exit

# mapping
# RVI         rv32i
# RVM         rv32im
# RVIC,RV32IC rv32imc
# RVZicsr     rv32Zicsr
# RVZifencei  rv32Zifencei

declare -A map
map[rv32i]=RVI
map[rv32im]=RVM
map[rv32imc]=RVIC,RV32IC
map[rv32Zicsr]=RVZicsr
map[rv32Zifencei]=RVZifencei
map[rv64i]=RV64I
map[rv64im]=RV64M
map[rv64imc]=RV64IC

declare -A varMap
varMap[rv32i]=RV32I
varMap[rv32im]=RV32IM
varMap[rv32imc]=RV32IMC
varMap[rv32Zicsr]=RV32I
varMap[rv32Zifencei]=RV32I
varMap[rv64i]=RV64I
varMap[rv64im]=RV64IM
varMap[rv64imc]=RV64IMC

if [[ ${COVERTYPE} == "" ]]; then
    COVERTYPE=basic
fi

ALL_ISA=$(ls -1 work)
for ISA in ${ALL_ISA}; do
    echo "Running $ISA"
    RISCV_ISA=${ISA}
    RISCV_CVG=${map[${RISCV_ISA}]}
    RISCV_VARIANT=${varMap[${RISCV_ISA}]}

    if [ -z "${RISCV_CVG}" ]; then
        echo "Skipping $ISA"
        continue
    fi
    ${ROOTDIR}/riscv-ovpsim/bin/Linux64/riscvOVPsim.exe \
        --cover ${COVERTYPE} \
        --variant ${RISCV_VARIANT} \
        --extensions ${RISCV_CVG} \
        --inputfiles work/${RISCV_ISA} \
        --outputfile work/${RISCV_ISA}/${COVERTYPE}.coverage.yaml \
        --reportfile work/${RISCV_ISA}/${COVERTYPE}.coverage.txt \
        --countthreshold 1 \
        --showuncovered \
        --nosimulation --logfile work/${RISCV_ISA}/${COVERTYPE}.coverage.run.log
    
done


   
