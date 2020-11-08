// RISC-V Compliance Test Header File
// Copyright (c) 2017, Codasip Ltd. All Rights Reserved.
// See LICENSE for license details.
//
// Description: Common header file for RV32I tests

#ifndef _COMPLIANCE_TEST_H
#define _COMPLIANCE_TEST_H

#include "riscv_test.h"

//-----------------------------------------------------------------------
// RV Compliance Macros
//-----------------------------------------------------------------------

#define RV_COMPLIANCE_HALT                                                    \
  			li TESTNUM, 1;\
  			SWSIG (0, TESTNUM);\
  			la t0, begin_signature; \
				la t1, read_signature; \
  			la t2, end_signature; \
				sub t2, t2, t0;\
				srli t2, t2, 2;\
  			addi t2, t2, -1; \
  			lw t4, 0(t0);\
  			sw t4, 0(t1);\
  			1: \
  			addi t0, t0, 4;\
  			addi t1, t1, 4;\
  			lw t4, 0(t0);\
  			sw t4, 0(t1);\
  			addi t2, t2, -1; \
  			bgtz t2, 1b; \
        beq x0, TESTNUM, fail; \
				pass:																														\
				1:																																	\
				j 1b;																															\
        fail:\
        fence \

#define RV_COMPLIANCE_RV32M                                                   \
        RVTEST_RV32M                                                          \

#define RV_COMPLIANCE_CODE_BEGIN                                              \
        RVTEST_CODE_BEGIN                                                     \

#define RV_COMPLIANCE_CODE_END                                                \
        RVTEST_CODE_END                                                       \

#define RV_COMPLIANCE_DATA_BEGIN                                              \
        RVTEST_DATA_BEGIN                                                     \

#define RV_COMPLIANCE_DATA_END                                                \
        RVTEST_DATA_END                                                       \

#endif
