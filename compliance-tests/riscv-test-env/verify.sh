#!/bin/bash


printf "\n\nCompare to reference files ... \n\n";
FAIL=0
RUN=0
#RISCV_ISA="rv32i"
#ROOTDIR='pwd'


for ref in ${ROOTDIR}/work/${RISCV_ISA}/*.reference_output_n;
do 
    base=$(basename ${ref})
    stub=${base//".reference_output_n"/}
    sig=${ROOTDIR}/work/${RISCV_ISA}/bin_log/${stub}.signature_output

    RUN=$((${RUN} + 1))
    
    #
    # Ensure both files exist
    #
    if [ -f ${ref} ] && [ -f ${sig} ]; then 
        echo -n "Check $(printf %24s ${stub})"
    else
        echo    "Check $(printf %24s ${stub}) ... IGNORE"
        continue
    fi
    diff --ignore-case --strip-trailing-cr ${ref} ${sig} #&> /dev/null
    if [ $? == 0 ]
    then
        echo " ... OK"
    else
        echo " ... FAIL"
        FAIL=$((${FAIL} + 1))
    fi
done

# warn on missing reverse reference
for sig in ${ROOTDIR}/work/${RISCV_ISA}/bin_log/*.signature_output; 
do
    base=$(basename ${sig})
    stub=${base//".signature_output"/}
    ref=${ROOTDIR}/work/${RISCV_ISA}/${stub}.reference_output_n

    if [ -f $sig ] && [ ! -f ${ref} ]; then
        echo "Error: sig ${sig} no corresponding ${ref}"
        FAIL=$((${FAIL} + 1))
    fi
done

declare -i status=0
if [ ${FAIL} == 0 ]; then
    echo "--------------------------------"
    echo -n "OK: ${RUN}/${RUN} "
    status=0
else
    echo "--------------------------------"
    echo -n "FAIL: ${FAIL}/${RUN} "
    status=1
fi
#echo "RISCV_TARGET=${RISCV_TARGET} RISCV_DEVICE=${RISCV_DEVICE} RISCV_ISA=${RISCV_ISA}"
echo
exit ${status}
