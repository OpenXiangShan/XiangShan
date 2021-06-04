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

#include <emu.h>
#include <difftest.h>
#include <assert.h>
#include <unistd.h>
#include <dlfcn.h>

void ref_difftest_memcpy_from_dut(paddr_t dest, void *src, size_t n, int coreid);
void ref_difftest_memcpy_from_ref(void *dest, paddr_t src, size_t n, int coreid);
void ref_difftest_getregs(void *c, int coreid);
void ref_difftest_setregs(const void *c, int coreid);
void ref_difftest_get_mastatus(void *s, int coreid);
void ref_difftest_set_mastatus(const void *s, int coreid);
void ref_difftest_get_csr(void *c, int coreid);
void ref_difftest_set_csr(const void *c, int coreid);
vaddr_t ref_disambiguate_exec(void *disambiguate_para, int coreid);
int ref_difftest_store_commit(uint64_t *saddr, uint64_t *sdata, uint8_t *smask, int coreid);
void ref_difftest_exec(uint64_t n, int coreid);
void ref_difftest_raise_intr(uint64_t NO, int coreid);
void ref_isa_reg_display(int coreid);
void ref_difftest_init(int coreid);
void ref_misc_put_gmaddr(uint8_t* ptr);

#endif