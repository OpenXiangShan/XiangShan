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

#include <cassert>
#include <cstdio>
#include <cstring>
#include "axi4.h"


// ar channel: (1) read raddr; (2) try to accept the address; (3) check raddr fire
bool axi_get_raddr(const axi_channel &axi, axi_addr_t &addr) {
  if (axi.ar.valid) {
    addr = axi.ar.addr;
    return true;
  }
  return false;
}

void axi_accept_raddr(axi_channel &axi) {
  axi.ar.ready = 1;
}

bool axi_check_raddr_fire(const axi_channel &axi) {
  if (axi.ar.valid && axi.ar.ready) {
#ifdef DEBUG_LOG_AXI4
    printf("axi ar channel fired addr = 0x%lx, id = %d\n", axi.ar.addr, axi.ar.id);
#endif
    return true;
  }
  return false;
}


// r channel: (1) put rdata; (2) check rdata fire
void axi_put_rdata(axi_channel &axi, void *src, size_t n, bool last, uint8_t id) {
  memcpy(axi.r.data, src, n);
  axi.r.valid = 1;
  axi.r.last = (last) ? 1 : 0;
  axi.r.id = id;
}

bool axi_check_rdata_fire(const axi_channel &axi) {
  if (axi.r.ready && axi.r.valid) {
#ifdef DEBUG_LOG_AXI4
    printf("axi r channel fired data = %lx, id = %d\n", axi.r.data[0], axi.r.id);
#endif
    return true;
  }
  return false;
}


// aw channel: (1) read waddr; (2) try to accept the address; (3) check waddr fire
bool axi_get_waddr(const axi_channel &axi, axi_addr_t &addr) {
  if (axi.aw.valid) {
    addr = axi.aw.addr;
    return true;
  }
  return false;
}

void axi_accept_waddr(axi_channel &axi) {
  axi.aw.ready = 1;
}

bool axi_check_waddr_fire(const axi_channel &axi) {
  if (axi.aw.valid && axi.aw.ready) {
    assert(axi.aw.burst == 1 || (axi.aw.burst == 2 && ((axi.aw.addr & 0x3f) == 0)));
#ifdef DEBUG_LOG_AXI4
    printf("axi aw channel fired\n");
#endif
    return true;
  }
  return false;
}


// w channel: (1) accept wdata; (2) get wdata; (3) check wdata fire
void axi_accept_wdata(axi_channel &axi) {
  axi.w.ready = 1;
}

bool axi_check_wdata_fire(const axi_channel &axi) {
  if (axi.w.valid && axi.w.ready) {
#ifdef DEBUG_LOG_AXI4
    printf("axi w channel fired\n");
#endif
    return true;
  }
  return false;
}

void axi_get_wdata(const axi_channel &axi, void *dest, size_t n) {
  memcpy(dest, axi.w.data, n);
}


// b channel: (1) put response; (2) check response fire
void axi_put_wack(axi_channel &axi, uint8_t id) {
  axi.b.valid = 1;
  axi.b.id = id;
}

bool axi_check_wack_fire(const axi_channel &axi) {
  if (axi.b.valid && axi.b.ready) {
#ifdef DEBUG_LOG_AXI4
    printf("axi b channel fired\n");
#endif
    return true;
  }
  return false;
}
