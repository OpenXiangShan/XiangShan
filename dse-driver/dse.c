#include <stdint.h>

#define PINGPONG_REG   0x39020000
#define CTRLSEL_REG    0x39020004
#define MAX_INSTR_REG  0x39020008
#define EPOCH_REG      0x39020010
#define ROBSIZE0_REG   0x39020100
#define ROBSIZE1_REG   0x39020108

static const uint64_t robsize_array[] = {0, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2};

void main() {

    // 写入 MAX_INSTR_REG
    *(volatile uint64_t *)MAX_INSTR_REG = 1000000;

    // 读取 ping-pong 寄存器并反转
    *(volatile uint8_t *)PINGPONG_REG ^= 1;

    // 读取 EPOCH_REG 寄存器
    uint64_t epoch = *(volatile uint64_t *)EPOCH_REG;

    // 根据 ctrlsel 写入 robsize 寄存器
    if (*(volatile uint8_t *)PINGPONG_REG == 0) {
        *(volatile uint64_t *)ROBSIZE0_REG = robsize_array[epoch % (sizeof(robsize_array) / sizeof(robsize_array[0]))];
    } else {
        *(volatile uint64_t *)ROBSIZE1_REG = robsize_array[epoch % (sizeof(robsize_array) / sizeof(robsize_array[0]))];
    }

    // 增加 epoch
    *(volatile uint64_t *)EPOCH_REG = epoch + 1;

    // 写入 ctrlsel 寄存器
    *(volatile uint8_t *)CTRLSEL_REG = *(volatile uint8_t *)PINGPONG_REG;
}