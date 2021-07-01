/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

#ifndef __NEMU_PROXY_H
#define __NEMU_PROXY_H

#include <unistd.h>
#include <dlfcn.h>

#include "common.h"

class NemuProxy {
public:
  // public callable functions
  void (*memcpy_from_dut)(paddr_t dest, void *src, size_t n);
  void (*memcpy_from_ref)(void *dest, paddr_t src, size_t n);
  void (*get_regs)(void *c);
  void (*set_regs)(const void *c);
  void (*get_mastatus)(void *s);
  void (*set_mastatus)(const void *s);
  void (*get_csr)(void *c);
  void (*set_csr)(const void *c);
  vaddr_t (*disambiguate_exec)(void *disambiguate_para);
  int (*store_commit)(uint64_t *saddr, uint64_t *sdata, uint8_t *smask);
  void (*exec)(uint64_t n);
  void (*raise_intr)(uint64_t no);
  void (*isa_reg_display)();

  NemuProxy(int coreid);
private:
};

struct SyncState {
  uint64_t lrscValid;
  uint64_t lrscAddr;
};

struct DisambiguationState {
  uint64_t exceptionNo;
  uint64_t mtval;
  uint64_t stval;
};

void ref_misc_put_gmaddr(uint8_t* ptr);

#endif