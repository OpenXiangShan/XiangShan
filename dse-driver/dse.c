#include <stdint.h>
#include "dse.h"


// // static const uint64_t robsize_array[] = {256, 128, 64, 32, 16, 8, 4, 2};
// static const uint64_t robsize_array[] = {32};
// // static const uint64_t lqsize_array[] = {256, 128, 64, 32, 16, 12, 8, 4, 2};
// static const uint64_t lqsize_array[] = {16};
// // static const uint64_t sqsize_array[] = {256, 128, 64, 32, 16, 12, 8, 4, 2};
// static const uint64_t sqsize_array[] = {32};
// // static const uint64_t ftqsize_array[] = {128, 64, 32, 16, 12, 8, 4, 2};
// static const uint64_t ftqsize_array[] = {16};
// // static const uint64_t ibsize_array[] = {256, 128, 64, 32, 16, 12, 8, 4, 2};
// static const uint64_t ibsize_array[] = {12};
// // static const uint64_t intdqsize_array[] = {12, 8, 4, 2};
// static const uint64_t intdqsize_array[] = {12};
// // static const uint64_t fpdqsize_array[] = {12, 8, 4, 2};
// static const uint64_t fpdqsize_array[] = {12};
// // static const uint64_t lsdqsize_array[] = {12, 8, 4, 2};
// static const uint64_t lsdqsize_array[] = {12};
// // static const uint64_t l2mshrs_array[] = {14, 6, 1};
// static const uint64_t l2mshrs_array[] = {14};
// // static const uint64_t l3mshrs_array[] = {14, 6, 1};
// static const uint64_t l3mshrs_array[] = {14};
// // static const uint64_t l2sets_array[] = {128, 64};
// static const uint64_t l2sets_array[] = {64};
// // static const uint64_t l3sets_array[] = {1024, 512};
// static const uint64_t l3sets_array[] = {512};

__attribute__((aligned(4)))
void main() {
    // 写入 MAX_INSTR_REG
    *(volatile uint64_t *)MAX_INSTR_REG = MAX_INSTR_CNT;

    // 写入 MAX_EPOCH_REG
    *(volatile uint64_t *)MAX_EPOCH_REG = *(volatile uint64_t *)MAX_EPOCH_ADDR;

    // 读取 ping-pong 寄存器并反转
    *(volatile uint8_t *)PINGPONG_REG ^= 1;

    // 读取 EPOCH_REG 寄存器
    uint64_t epoch = *(volatile uint64_t *)EPOCH_REG;


    // 根据 ctrlsel 写入参数寄存器
    if (*(volatile uint8_t *)PINGPONG_REG == 0) {
        *(volatile uint64_t *)ROBSIZE0_REG = *(volatile uint64_t *)ROBSIZE_ADDR;
        *(volatile uint64_t *)LQSIZE0_REG = *(volatile uint64_t *)LQSIZE_ADDR;
        *(volatile uint64_t *)SQSIZE0_REG = *(volatile uint64_t *)SQSIZE_ADDR;
        *(volatile uint64_t *)FTQ0_REG = *(volatile uint64_t *)FTQSIZE_ADDR;
        *(volatile uint64_t *)IBUFSIZE0_REG = *(volatile uint64_t *)IBUFSIZE_ADDR;
        // *(volatile uint64_t *)INTDQSIZE0_REG = *(volatile uint64_t *)INTDQSIZE_ADDR;
        // *(volatile uint64_t *)FPDQSIZE0_REG = *(volatile uint64_t *)FPDQSIZE_ADDR;
        // *(volatile uint64_t *)LSDQSIZE0_REG = *(volatile uint64_t *)LSDQSIZE_ADDR;
        // *(volatile uint64_t *)L2MSHRS0_REG = *(volatile uint64_t *)L2MSHRS_ADDR;
        // *(volatile uint64_t *)L3MSHRS0_REG = *(volatile uint64_t *)L3MSHRS_ADDR;
        // *(volatile uint64_t *)L2SETS0_REG = *(volatile uint64_t *)L2SETS_ADDR;
        // *(volatile uint64_t *)L3SETS0_REG = *(volatile uint64_t *)L3SETS_ADDR;
        // *(volatile uint64_t *)INTPHYREGS0_REG = *(volatile uint64_t *)INTPHYREGS_ADDR;
        // *(volatile uint64_t *)FPPHYREGS0_REG = *(volatile uint64_t *)FPPHYREGS_ADDR;
        // *(volatile uint64_t *)RASSIZE0_REG = *(volatile uint64_t *)RASSIZE_ADDR;
        // *(volatile uint64_t *)DCACHEWAYS0_REG = *(volatile uint64_t *)DCACHEWAYS_ADDR;
        // *(volatile uint64_t *)DCACHEMSHRS0_REG = *(volatile uint64_t *)DCACHEMSHRS_ADDR;
    } else {
        *(volatile uint64_t *)ROBSIZE1_REG = *(volatile uint64_t *)ROBSIZE_ADDR;
        *(volatile uint64_t *)LQSIZE1_REG = *(volatile uint64_t *)LQSIZE_ADDR;
        *(volatile uint64_t *)SQSIZE1_REG = *(volatile uint64_t *)SQSIZE_ADDR;
        *(volatile uint64_t *)FTQ1_REG = *(volatile uint64_t *)FTQSIZE_ADDR;
        *(volatile uint64_t *)IBUFSIZE1_REG = *(volatile uint64_t *)IBUFSIZE_ADDR;
        // *(volatile uint64_t *)INTDQSIZE1_REG = *(volatile uint64_t *)INTDQSIZE_ADDR;
        // *(volatile uint64_t *)FPDQSIZE1_REG = *(volatile uint64_t *)FPDQSIZE_ADDR;
        // *(volatile uint64_t *)LSDQSIZE1_REG = *(volatile uint64_t *)LSDQSIZE_ADDR;
        // *(volatile uint64_t *)L2MSHRS1_REG = *(volatile uint64_t *)L2MSHRS_ADDR;
        // *(volatile uint64_t *)L3MSHRS1_REG = *(volatile uint64_t *)L3MSHRS_ADDR;
        // *(volatile uint64_t *)L2SETS1_REG = *(volatile uint64_t *)L2SETS_ADDR;
        // *(volatile uint64_t *)L3SETS1_REG = *(volatile uint64_t *)L3SETS_ADDR;
        // *(volatile uint64_t *)INTPHYREGS1_REG = *(volatile uint64_t *)INTPHYREGS_ADDR;
        // *(volatile uint64_t *)FPPHYREGS1_REG = *(volatile uint64_t *)FPPHYREGS_ADDR;
        // *(volatile uint64_t *)RASSIZE1_REG = *(volatile uint64_t *)RASSIZE_ADDR;
        // *(volatile uint64_t *)DCACHEWAYS1_REG = *(volatile uint64_t *)DCACHEWAYS_ADDR;
        // *(volatile uint64_t *)DCACHEMSHRS1_REG = *(volatile uint64_t *)DCACHEMSHRS_ADDR;
    }

    // test
    // if (*(volatile uint8_t *)PINGPONG_REG == 0) {
    //     *(volatile uint64_t *)ROBSIZE0_REG = robsize_array[epoch % (sizeof(robsize_array) / sizeof(robsize_array[0]))];
    //     *(volatile uint64_t *)LQSIZE0_REG = lqsize_array[epoch % (sizeof(lqsize_array) / sizeof(lqsize_array[0]))];
    //     *(volatile uint64_t *)SQSIZE0_REG = sqsize_array[epoch % (sizeof(sqsize_array) / sizeof(sqsize_array[0]))];
    //     *(volatile uint64_t *)FTQ0_REG = ftqsize_array[epoch % (sizeof(ftqsize_array) / sizeof(ftqsize_array[0]))];
    //     *(volatile uint64_t *)IBUFSIZE0_REG = ibsize_array[epoch % (sizeof(ibsize_array) / sizeof(ibsize_array[0]))];
    //     *(volatile uint64_t *)INTDQSIZE0_REG = intdqsize_array[epoch % (sizeof(intdqsize_array) / sizeof(intdqsize_array[0]))];
    //     *(volatile uint64_t *)FPDQSIZE0_REG = fpdqsize_array[epoch % (sizeof(fpdqsize_array) / sizeof(fpdqsize_array[0]))];
    //     *(volatile uint64_t *)LSDQSIZE0_REG = lsdqsize_array[epoch % (sizeof(lsdqsize_array) / sizeof(lsdqsize_array[0]))];
    //     *(volatile uint64_t *)L2MSHRS0_REG = l2mshrs_array[epoch % (sizeof(l2mshrs_array) / sizeof(l2mshrs_array[0]))];
    //     *(volatile uint64_t *)L3MSHRS0_REG = l3mshrs_array[epoch % (sizeof(l3mshrs_array) / sizeof(l3mshrs_array[0]))];
    //     *(volatile uint64_t *)L2SETS0_REG = l2sets_array[epoch % (sizeof(l2sets_array) / sizeof(l2sets_array[0]))];
    //     *(volatile uint64_t *)L3SETS0_REG = l3sets_array[epoch % (sizeof(l3sets_array) / sizeof(l3sets_array[0]))];
    // } else {
    //     *(volatile uint64_t *)ROBSIZE1_REG = robsize_array[epoch % (sizeof(robsize_array) / sizeof(robsize_array[0]))];
    //     *(volatile uint64_t *)LQSIZE1_REG = lqsize_array[epoch % (sizeof(lqsize_array) / sizeof(lqsize_array[0]))];
    //     *(volatile uint64_t *)SQSIZE1_REG = sqsize_array[epoch % (sizeof(sqsize_array) / sizeof(sqsize_array[0]))];
    //     *(volatile uint64_t *)FTQ1_REG = ftqsize_array[epoch % (sizeof(ftqsize_array) / sizeof(ftqsize_array[0]))];
    //     *(volatile uint64_t *)IBUFSIZE1_REG = ibsize_array[epoch % (sizeof(ibsize_array) / sizeof(ibsize_array[0]))];
    //     *(volatile uint64_t *)INTDQSIZE1_REG = intdqsize_array[epoch % (sizeof(intdqsize_array) / sizeof(intdqsize_array[0]))];
    //     *(volatile uint64_t *)FPDQSIZE1_REG = fpdqsize_array[epoch % (sizeof(fpdqsize_array) / sizeof(fpdqsize_array[0]))];
    //     *(volatile uint64_t *)LSDQSIZE1_REG = lsdqsize_array[epoch % (sizeof(lsdqsize_array) / sizeof(lsdqsize_array[0]))];
    //     *(volatile uint64_t *)L2MSHRS1_REG = l2mshrs_array[epoch % (sizeof(l2mshrs_array) / sizeof(l2mshrs_array[0]))];
    //     *(volatile uint64_t *)L3MSHRS1_REG = l3mshrs_array[epoch % (sizeof(l3mshrs_array) / sizeof(l3mshrs_array[0]))];
    //     *(volatile uint64_t *)L2SETS1_REG = l2sets_array[epoch % (sizeof(l2sets_array) / sizeof(l2sets_array[0]))];
    //     *(volatile uint64_t *)L3SETS1_REG = l3sets_array[epoch % (sizeof(l3sets_array) / sizeof(l3sets_array[0]))];
    // }


    // 增加 epoch
    *(volatile uint64_t *)EPOCH_REG = epoch + 1;

    // 写入 ctrlsel 寄存器
    *(volatile uint8_t *)CTRLSEL_REG = *(volatile uint8_t *)PINGPONG_REG;
}
