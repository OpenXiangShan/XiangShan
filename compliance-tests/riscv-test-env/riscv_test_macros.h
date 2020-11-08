// RISC-V Compliance IO Test Header File

/*
 * Copyright (c) 2005-2018 Imperas Software Ltd., www.imperas.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied.
 *
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


//
// In general the following registers are reserved
// ra, a0, t0, t1
// Additionally on an assertion violation, t1, t2 are overwritten
// x1, x10, x5, x6, x7 respectively
// Floating registers reserved
// f5
//

#define MASK_XLEN(x) ((x) & ((1 << (__riscv_xlen - 1) << 1) - 1))

#define SEXT_IMM(x) ((x) | (-(((x) >> 11) & 1) << 11))

// Base function for integer operations
#define TEST_CASE(testreg, destreg, correctval, swreg, offset, code... ) \
    code; \
    sw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_GPR_EQ(testreg, destreg, correctval) \

// Base functions for single precision floating point operations
#define TEST_CASE_FP(test_num, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    flw reg1, 0(a0); \
    flw reg2, 4(a0); \
    lw t1, 8(a0); \
    code; \
    fsw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_SFPR_EQ(destreg, t1, correctval) \
    .pushsection .data; \
    .align 3; \
    test_ ## test_num ## _data: \
      .float val1; \
      .float val2; \
      .word correctval; \
    .popsection

#define TEST_CASE_FP_I(test_num, destreg, reg, correctval, val, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    lw t1, 0(a0); \
    code; \
    fsw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_SFPR_EQ(destreg, t1, correctval) \
    .pushsection .data; \
    .align 1; \
    test_ ## test_num ## _data: \
      .word correctval; \
    .popsection

#define TEST_CASE_FP_I2(test_num, destreg, reg, correctval, val, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    flw reg, 0(a0); \
    lw t1, 4(a0); \
    code; \
    sw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_GPR_EQ(x31, destreg, correctval) \
    .pushsection .data; \
    .align 2; \
    test_ ## test_num ## _data: \
      .float val; \
      .word correctval; \
    .popsection

#define TEST_CASE_FP_4REG(test_num, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    flw reg1, 0(a0); \
    flw reg2, 4(a0); \
    flw reg3, 8(a0); \
    lw t1, 12(a0); \
    code; \
    fsw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_SFPR_EQ(destreg, t1, correctval) \
    .pushsection .data; \
    .align 4; \
    test_ ## test_num ## _data: \
      .float val1; \
      .float val2; \
      .float val3; \
      .word correctval; \
    .popsection

#define TEST_CASE_FP_FMVXS(test_num, destreg, reg, correctval, val, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    flw reg, 0(a0); \
    code; \
    sw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_GPR_EQ(x31, destreg, correctval) \
    .pushsection .data; \
    .align 1; \
    test_ ## test_num ## _data: \
      .float val; \
    .popsection

#define TEST_CASE_FP_FMVSX(test_num, destreg, reg, correctval, val, swreg, offset, code...) \
    la  a0, test_ ## test_num ## _data; \
    li reg, val; \
    code; \
    fsw destreg, offset(swreg); \
    lw a1, 0(a0); \
    RVTEST_IO_ASSERT_SFPR_EQ(destreg, a1, correctval) \
    .pushsection .data; \
    .align 1; \
    test_ ## test_num ## _data: \
      .word correctval; \
    .popsection

// Base functions for double precision floating point operations - rv32d
#define TEST_CASE_FPD(test_num, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    fld reg1, 0(a0); \
    fld reg2, 8(a0); \
    code; \
    fsd destreg, offset(swreg); \
    lw t1, 16(a0); \
    lw t2, 20(a0); \
    la a0, store_ ## test_num ## _data; \
    fsd destreg, 0(a0); \
    lw a1, 0(a0); \
    lw a2, 4(a0); \
    RVTEST_IO_ASSERT_DFPR_EQ(destreg, t2, t1, a2, a1, correctval) \
    .pushsection .data; \
    .align 3; \
    test_ ## test_num ## _data: \
      .double val1; \
      .double val2; \
      .dword correctval; \
    .popsection; \
    .pushsection .data; \
    store_ ## test_num ## _data: \
      .fill 1, 8, -1; \
    .popsection

#define TEST_CASE_FPD_I(test_num, destreg, reg, correctval, val, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    code; \
    fsd destreg, offset(swreg); \
    lw t1, 0(a0); \
    lw t2, 4(a0); \
    la a0, store_ ## test_num ## _data; \
    fsd destreg, 0(a0); \
    lw a1, 0(a0); \
    lw a2, 4(a0); \
    RVTEST_IO_ASSERT_DFPR_EQ(destreg, t2, t1, a2, a1, correctval) \
    .pushsection .data; \
    .align 1; \
    test_ ## test_num ## _data: \
      .dword correctval; \
    .popsection; \
    store_ ## test_num ## _data: \
      .fill 1, 8, -1; \
    .popsection

#define TEST_CASE_FPD_I2(test_num, destreg, reg, correctval, val, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    fld reg, 0(a0); \
    lw t1, 8(a0); \
    code; \
    sw destreg, offset(swreg); \
    RVTEST_IO_ASSERT_GPR_EQ(x31, destreg, correctval) \
    .pushsection .data; \
    .align 2; \
    test_ ## test_num ## _data: \
      .double val; \
      .word correctval; \
    .popsection

#define TEST_CASE_FPD_4REG(test_num, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset, code... ) \
    la  a0, test_ ## test_num ## _data; \
    fld reg1, 0(a0); \
    fld reg2, 8(a0); \
    fld reg3, 16(a0); \
    code; \
    fsd destreg, offset(swreg); \
    lw t1, 24(a0); \
    lw t2, 28(a0); \
    la a0, store_ ## test_num ## _data; \
    fsd destreg, 0(a0); \
    lw a1, 0(a0); \
    lw a2, 4(a0); \
    RVTEST_IO_ASSERT_DFPR_EQ(destreg, t2, t1, a2, a1, correctval) \
    .pushsection .data; \
    .align 4; \
    test_ ## test_num ## _data: \
      .double val1; \
      .double val2; \
      .double val3; \
      .dword correctval; \
    .popsection; \
    .pushsection .data; \
    store_ ## test_num ## _data: \
      .fill 1, 8, -1; \
    .popsection

//Tests for a instructions with register-register operand
#define TEST_RR_OP(inst, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li  reg1, MASK_XLEN(val1); \
      li  reg2, MASK_XLEN(val2); \
      inst destreg, reg1, reg2; \
    )

#define TEST_RR_SRC1( inst, destreg, reg, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li destreg, MASK_XLEN(val1); \
      li reg, MASK_XLEN(val2); \
      inst destreg, destreg, reg; \
    )

#define TEST_RR_SRC2( inst, destreg, reg, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li reg, MASK_XLEN(val1); \
      li destreg, MASK_XLEN(val2); \
      inst destreg, reg, destreg; \
    )

#define TEST_RR_SRC12( inst, destreg, correctval, val, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li destreg, MASK_XLEN(val1); \
      inst destreg, destreg, destreg; \
    )

#define TEST_RR_ZERO1( inst, destreg, reg, correctval, val, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li reg, MASK_XLEN(val); \
      inst destreg, x0, reg; \
    )

#define TEST_RR_ZERO2( inst, destreg, reg, correctval, val, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li reg, MASK_XLEN(val); \
      inst destreg, reg, x0; \
    )

#define TEST_RR_ZERO12( inst, destreg, correctval, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      inst destreg, x0, x0; \
    )

#define TEST_RR_ZERODEST( inst, reg1, reg2, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, x0, 0, swreg, offset, \
      li reg1, MASK_XLEN(val1); \
      li reg2, MASK_XLEN(val2); \
      inst x0, reg1, reg2; \
    )

//Tests for a instructions with register-immediate operand
#define TEST_IMM_OP( inst, destreg, reg, correctval, val, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li reg, MASK_XLEN(val); \
      inst destreg, reg, SEXT_IMM(imm); \
    )

#define TEST_IMM_SRC( inst, destreg, correctval, val, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li destreg, MASK_XLEN(val); \
      inst destreg, destreg, SEXT_IMM(imm); \
    )

#define TEST_IMM_ZEROSRC( inst, destreg, correctval, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      inst destreg, x0, SEXT_IMM(imm); \
    )

#define TEST_IMM_ZERODEST( inst, reg, val, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, x0, 0, swreg, offset, \
      li reg, MASK_XLEN(val); \
      inst x0, reg, SEXT_IMM(imm); \
    )

#define TEST_IMM_ONEREG( inst, destreg, correctval, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      inst destreg, SEXT_IMM(imm); \
      )

#define TEST_AUIPC(inst, destreg, correctval, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      1: \
      inst destreg, imm; \
      la swreg, 1b; \
      sub destreg, destreg, swreg; \
      )

//Tests for a compressed instruction
#define TEST_CR_OP( inst, destreg, reg, correctval, val1, val2, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li reg, MASK_XLEN(val1); \
      li destreg, MASK_XLEN(val2); \
      inst destreg, reg; \
      )

#define TEST_CI_OP( inst, destreg, correctval, val, imm, swreg, offset, testreg) \
    TEST_CASE(testreg, destreg, correctval, swreg, offset, \
      li destreg, MASK_XLEN(val); \
      inst destreg, imm; \
      )

#define TEST_CI_OP_NOREG(inst, correctval, imm, swreg, offset, testreg) \
    TEST_CASE(testreg,x0, correctval, swreg, offset, \
      inst imm; \
      )

//Tests for floating point instructions - single precision
#define TEST_FP_OP(test_num, inst, destreg, reg1, reg2, correctval, val1, val2, swreg, offset) \
      TEST_CASE_FP(test_num, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, \
        inst destreg, reg1, reg2; \
        )

#define TEST_FP_ONEREG(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FP(test_num, destreg, reg, reg, correctval, val, val, swreg, offset, \
        inst destreg, reg; \
        )

#define TEST_FP_4REG(test_num, inst, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset) \
      TEST_CASE_FP_4REG(test_num, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset, \
        inst destreg, reg1, reg2, reg3; \
        )

#define TEST_FP_I(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FP_I(test_num, destreg, reg, correctval, val, swreg, offset, \
        li reg, MASK_XLEN(val); \
        inst destreg, reg; \
        )

#define TEST_FP_I2(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FP_I2(test_num, destreg, reg, correctval, val, swreg, offset, \
        inst destreg, reg; \
        )

//Tests for floating point instructions - double precision
#define TEST_FPD_OP(test_num, inst, destreg, reg1, reg2, correctval, val1, val2, swreg, offset) \
      TEST_CASE_FPD(test_num, destreg, reg1, reg2, correctval, val1, val2, swreg, offset, \
        inst destreg, reg1, reg2; \
        )

#define TEST_FPD_ONEREG(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FPD(test_num, destreg, reg, reg, correctval, val, val, swreg, offset, \
        inst destreg, reg; \
        )

#define TEST_FPD_4REG(test_num, inst, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset) \
      TEST_CASE_FPD_4REG(test_num, destreg, reg1, reg2, reg3, correctval, val1, val2, val3, swreg, offset, \
        inst destreg, reg1, reg2, reg3; \
        )

#define TEST_FPD_I(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FPD_I(test_num, destreg, reg, correctval, val, swreg, offset, \
        li reg, MASK_XLEN(val); \
        inst destreg, reg; \
        )

#define TEST_FPD_I2(test_num, inst, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FPD_I2(test_num, destreg, reg, correctval, val, swreg, offset, \
        inst destreg, reg; \
        )

#define TEST_CADDI16SP(correctval, imm, swreg, offset, testreg) \
      TEST_CASE(testreg,x2, correctval, swreg, offset, \
      c.addi16sp x2, imm; \
      )

#define TEST_CADDI4SPN(destreg, correctval, imm, swreg, offset, testreg) \
      TEST_CASE(testreg,destreg, correctval, swreg, offset, \
        c.addi4spn destreg, x2, SEXT_IMM(imm); \
        )

#define TEST_CJL(inst, reg, val, swreg, offset) \
      li x10, val; \
      la reg, 1f; \
      inst reg; \
      li x10, 0x123ab; \
1: \
      sw x10, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, x10, val); \

#define ABS(x) ((x >> 11) ^ x) - (x >> 11)

#define TEST_CJ(inst, reg, val, swreg, offset) \
      li reg, val; \
      inst 1f; \
      li reg, 0x123ab; \
1: \
      sw reg, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, reg, val); \

#define TEST_CL(inst, reg, imm, swreg, offset) \
      la reg, test_data; \
      inst reg, imm(reg); \
      sw reg, offset(swreg); \

// lw reg, imm(x2)
// c.lwsp reg, imm(x2)
#define TEST_CLWSP(reg, imm, swreg, offset) \
      la x2, test_data; \
      c.lwsp reg, imm(x2); \
      sw reg, offset(swreg); \

#define TEST_CSW(test_data, inst, reg1, reg2, val, imm, swreg, offset) \
      li reg1, val; \
      la reg2, test_data; \
      inst reg1, imm(reg2); \
      lw reg1, imm(reg2); \
      sw reg1, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, reg1, val); \

#define TEST_CSWSP(test_data, reg, val, imm, swreg, offset) \
      la x2, test_data; \
      li reg, val; \
      c.swsp reg, imm(x2); \
      lw reg, imm(x2); \
      sw reg, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, reg, val); \

#define TEST_CBEQZ(reg, val, swreg, offset) \
      li reg, val; \
      c.sub reg, reg; \
      c.beqz reg, 3f; \
      li reg, 0x123ab; \
3: \
      sw reg, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, reg, 0x0); \

#define TEST_CBNEZ(reg, val, swreg, offset) \
      li reg, val; \
      c.bnez reg, 4f; \
      li reg, 0x0; \
4: \
      sw reg, offset(swreg); \
      RVTEST_IO_ASSERT_GPR_EQ(x31, reg, val); \

#define TEST_FMVXS(test_num, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FP_FMVXS(test_num, destreg, reg, correctval, val, swreg, offset, \
        fmv.x.s destreg, reg; \
        )

#define TEST_FMVSX(test_num, destreg, reg, correctval, val, swreg, offset) \
      TEST_CASE_FP_FMVSX(test_num, destreg, reg, correctval, val, swreg, offset, \
        fmv.s.x destreg, reg; \
        )

#define SWSIG(a,b)

