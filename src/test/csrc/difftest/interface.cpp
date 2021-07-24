/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#include "interface.h"

extern "C" int v_difftest_init() {
  return difftest_init();
}

extern "C" int v_difftest_step() {
  return difftest_step();
}

#define RETURN_NO_NULL \
  if (difftest == NULL) return;

INTERFACE_TRAP_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_trap_event();
  packet->valid    = valid;
  packet->code     = code;
  packet->pc       = pc;
  packet->cycleCnt = cycleCnt;
  packet->instrCnt = instrCnt;
}

INTERFACE_ARCH_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_arch_event();
  packet->interrupt = intrNo;
  packet->exception = cause;
  packet->exceptionPC = exceptionPC;
}

INTERFACE_INSTR_COMMIT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_instr_commit(index);
  packet->valid    = valid;
  if (packet->valid) {
    packet->pc       = pc;
    packet->inst     = instr;
    packet->skip     = skip;
    packet->isRVC    = isRVC;
    packet->scFailed = scFailed;
    packet->wen      = wen;
    packet->wdest    = wdest;
    packet->wdata    = wdata;
  }
}

INTERFACE_CSR_STATE {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_csr_state();
  packet->priviledgeMode = priviledgeMode;
  packet->mstatus = mstatus;
  packet->sstatus = sstatus;
  packet->mepc = mepc;
  packet->sepc = sepc;
  packet->mtval = mtval;
  packet->stval = stval;
  packet->mtvec = mtvec;
  packet->stvec = stvec;
  packet->mcause = mcause;
  packet->scause = scause;
  packet->satp = satp;
  packet->mip = mip;
  packet->mie = mie;
  packet->mscratch = mscratch;
  packet->sscratch = sscratch;
  packet->mideleg = mideleg;
  packet->medeleg = medeleg;
}

INTERFACE_INT_REG_STATE {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_arch_reg_state();
  packet->gpr[ 0] = gpr_0;
  packet->gpr[ 1] = gpr_1;
  packet->gpr[ 2] = gpr_2;
  packet->gpr[ 3] = gpr_3;
  packet->gpr[ 4] = gpr_4;
  packet->gpr[ 5] = gpr_5;
  packet->gpr[ 6] = gpr_6;
  packet->gpr[ 7] = gpr_7;
  packet->gpr[ 8] = gpr_8;
  packet->gpr[ 9] = gpr_9;
  packet->gpr[10] = gpr_10;
  packet->gpr[11] = gpr_11;
  packet->gpr[12] = gpr_12;
  packet->gpr[13] = gpr_13;
  packet->gpr[14] = gpr_14;
  packet->gpr[15] = gpr_15;
  packet->gpr[16] = gpr_16;
  packet->gpr[17] = gpr_17;
  packet->gpr[18] = gpr_18;
  packet->gpr[19] = gpr_19;
  packet->gpr[20] = gpr_20;
  packet->gpr[21] = gpr_21;
  packet->gpr[22] = gpr_22;
  packet->gpr[23] = gpr_23;
  packet->gpr[24] = gpr_24;
  packet->gpr[25] = gpr_25;
  packet->gpr[26] = gpr_26;
  packet->gpr[27] = gpr_27;
  packet->gpr[28] = gpr_28;
  packet->gpr[29] = gpr_29;
  packet->gpr[30] = gpr_30;
  packet->gpr[31] = gpr_31;
}

INTERFACE_FP_REG_STATE {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_arch_reg_state();
  packet->fpr[ 0] = fpr_0;
  packet->fpr[ 1] = fpr_1;
  packet->fpr[ 2] = fpr_2;
  packet->fpr[ 3] = fpr_3;
  packet->fpr[ 4] = fpr_4;
  packet->fpr[ 5] = fpr_5;
  packet->fpr[ 6] = fpr_6;
  packet->fpr[ 7] = fpr_7;
  packet->fpr[ 8] = fpr_8;
  packet->fpr[ 9] = fpr_9;
  packet->fpr[10] = fpr_10;
  packet->fpr[11] = fpr_11;
  packet->fpr[12] = fpr_12;
  packet->fpr[13] = fpr_13;
  packet->fpr[14] = fpr_14;
  packet->fpr[15] = fpr_15;
  packet->fpr[16] = fpr_16;
  packet->fpr[17] = fpr_17;
  packet->fpr[18] = fpr_18;
  packet->fpr[19] = fpr_19;
  packet->fpr[20] = fpr_20;
  packet->fpr[21] = fpr_21;
  packet->fpr[22] = fpr_22;
  packet->fpr[23] = fpr_23;
  packet->fpr[24] = fpr_24;
  packet->fpr[25] = fpr_25;
  packet->fpr[26] = fpr_26;
  packet->fpr[27] = fpr_27;
  packet->fpr[28] = fpr_28;
  packet->fpr[29] = fpr_29;
  packet->fpr[30] = fpr_30;
  packet->fpr[31] = fpr_31;
}

INTERFACE_SBUFFER_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_sbuffer_state();
  packet->resp = sbufferResp;
  if (packet->resp) {
    packet->addr = sbufferAddr;
    packet->data[0] = sbufferData_0;
    packet->data[1] = sbufferData_1;
    packet->data[2] = sbufferData_2;
    packet->data[3] = sbufferData_3;
    packet->data[4] = sbufferData_4;
    packet->data[5] = sbufferData_5;
    packet->data[6] = sbufferData_6;
    packet->data[7] = sbufferData_7;
    packet->data[8] = sbufferData_8;
    packet->data[9] = sbufferData_9;
    packet->data[10] = sbufferData_10;
    packet->data[11] = sbufferData_11;
    packet->data[12] = sbufferData_12;
    packet->data[13] = sbufferData_13;
    packet->data[14] = sbufferData_14;
    packet->data[15] = sbufferData_15;
    packet->data[16] = sbufferData_16;
    packet->data[17] = sbufferData_17;
    packet->data[18] = sbufferData_18;
    packet->data[19] = sbufferData_19;
    packet->data[20] = sbufferData_20;
    packet->data[21] = sbufferData_21;
    packet->data[22] = sbufferData_22;
    packet->data[23] = sbufferData_23;
    packet->data[24] = sbufferData_24;
    packet->data[25] = sbufferData_25;
    packet->data[26] = sbufferData_26;
    packet->data[27] = sbufferData_27;
    packet->data[28] = sbufferData_28;
    packet->data[29] = sbufferData_29;
    packet->data[30] = sbufferData_30;
    packet->data[31] = sbufferData_31;
    packet->data[32] = sbufferData_32;
    packet->data[33] = sbufferData_33;
    packet->data[34] = sbufferData_34;
    packet->data[35] = sbufferData_35;
    packet->data[36] = sbufferData_36;
    packet->data[37] = sbufferData_37;
    packet->data[38] = sbufferData_38;
    packet->data[39] = sbufferData_39;
    packet->data[40] = sbufferData_40;
    packet->data[41] = sbufferData_41;
    packet->data[42] = sbufferData_42;
    packet->data[43] = sbufferData_43;
    packet->data[44] = sbufferData_44;
    packet->data[45] = sbufferData_45;
    packet->data[46] = sbufferData_46;
    packet->data[47] = sbufferData_47;
    packet->data[48] = sbufferData_48;
    packet->data[49] = sbufferData_49;
    packet->data[50] = sbufferData_50;
    packet->data[51] = sbufferData_51;
    packet->data[52] = sbufferData_52;
    packet->data[53] = sbufferData_53;
    packet->data[54] = sbufferData_54;
    packet->data[55] = sbufferData_55;
    packet->data[56] = sbufferData_56;
    packet->data[57] = sbufferData_57;
    packet->data[58] = sbufferData_58;
    packet->data[59] = sbufferData_59;
    packet->data[60] = sbufferData_60;
    packet->data[61] = sbufferData_61;
    packet->data[62] = sbufferData_62;
    packet->data[63] = sbufferData_63;
    packet->mask = sbufferMask;
  }
}

INTERFACE_STORE_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_store_event(index);
  packet->valid = valid;
  if (packet->valid) {
    packet->addr = storeAddr;
    packet->data = storeData;
    packet->mask = storeMask;
  }
}

INTERFACE_LOAD_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_load_event(index);
  packet->valid = valid;
  if (packet->valid) {
    packet->paddr = paddr;
    packet->opType = opType;
    packet->fuType = fuType;
  }
}

INTERFACE_ATOMIC_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_atomic_event();
  packet->resp = resp;
  if (packet->resp) {
    packet->addr = addr;
    packet->data = data;
    packet->mask = mask;
    packet->fuop = fuop;
    packet->out  = out;
  }
}

INTERFACE_PTW_EVENT {
  RETURN_NO_NULL
  auto packet = difftest[coreid]->get_ptw_event();
  packet->resp = resp;
  if (packet->resp) {
    packet->addr = addr;
    packet->data[0] = data_0;
    packet->data[1] = data_1;
    packet->data[2] = data_2;
    packet->data[3] = data_3;
  }
}

