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

#ifndef __AXI4_H
#define __AXI4_h

#include <stdint.h>

// #define DEBUG_LOG_AXI4

// 4*64 bits
#define AXI_DATA_WIDTH_64 4

typedef uint64_t axi_addr_t;
typedef uint64_t axi_data_t[AXI_DATA_WIDTH_64];
#define axi_copy_data(dest, src) \
  memcpy(dest, src, sizeof(uint64_t)*AXI_DATA_WIDTH_64);

struct axi_aw_channel {
  uint8_t       ready;
  uint8_t       valid;
  axi_addr_t    addr;
  uint8_t       prot;
  uint8_t       id;
  uint8_t       user;
  uint8_t       len;
  uint8_t       size;
  uint8_t       burst;
  uint8_t       lock;
  uint8_t       cache;
  uint8_t       qos;
};

struct axi_w_channel {
  uint8_t       ready;
  uint8_t       valid;
  axi_data_t    data;
  uint8_t       strb;
  uint8_t       last;
};

struct axi_b_channel {
  uint8_t       ready;
  uint8_t       valid;
  uint8_t       resp;
  uint8_t       id;
  uint8_t       user;
};

struct axi_ar_channel {
  uint8_t       ready;
  uint8_t       valid;
  axi_addr_t    addr;
  uint8_t       prot;
  uint8_t       id;
  uint8_t       user;
  uint8_t       len;
  uint8_t       size;
  uint8_t       burst;
  uint8_t       lock;
  uint8_t       cache;
  uint8_t       qos;
};

struct axi_r_channel {
  uint8_t       ready;
  uint8_t       valid;
  uint8_t       resp;
  axi_data_t    data;
  uint8_t       last;
  uint8_t       id;
  uint8_t       user;
};

struct axi_channel {
  struct axi_aw_channel aw;
  struct axi_w_channel  w;
  struct axi_b_channel  b;
  struct axi_ar_channel ar;
  struct axi_r_channel  r;
};

// dut helper for AXI

// NOTE: change this when migrating between different hardware designs
#define DUT_AXI(name) io_memAXI_0_##name

#define axi_aw_copy_from_dut_ptr(dut_ptr, aw)             \
  do {                                                    \
    aw.ready = dut_ptr->DUT_AXI(aw_ready);                \
    aw.valid = dut_ptr->DUT_AXI(aw_valid);                \
    aw.addr = dut_ptr->DUT_AXI(aw_bits_addr);             \
    aw.prot = dut_ptr->DUT_AXI(aw_bits_prot);             \
    aw.id = dut_ptr->DUT_AXI(aw_bits_id);                 \
    aw.len = dut_ptr->DUT_AXI(aw_bits_len);               \
    aw.size = dut_ptr->DUT_AXI(aw_bits_size);             \
    aw.burst = dut_ptr->DUT_AXI(aw_bits_burst);           \
    aw.lock = dut_ptr->DUT_AXI(aw_bits_lock);             \
    aw.cache = dut_ptr->DUT_AXI(aw_bits_cache);           \
    aw.qos = dut_ptr->DUT_AXI(aw_bits_qos);               \
  } while (0);

#define axi_aw_set_dut_ptr(dut_ptr, aw)                   \
  do {                                                    \
    dut_ptr->DUT_AXI(aw_ready) = aw.ready;                \
  } while (0);

#define axi_w_copy_from_dut_ptr(dut_ptr, w)               \
  do {                                                    \
    w.ready = dut_ptr->DUT_AXI(w_ready);                  \
    w.valid = dut_ptr->DUT_AXI(w_valid);                  \
    axi_copy_data(w.data, dut_ptr->DUT_AXI(w_bits_data))  \
    w.strb = dut_ptr->DUT_AXI(w_bits_strb);               \
    w.last = dut_ptr->DUT_AXI(w_bits_last);               \
  } while (0);

#define axi_w_set_dut_ptr(dut_ptr, w)                     \
  do {                                                    \
    dut_ptr->DUT_AXI(w_ready) = w.ready;                  \
  } while (0);

#define axi_b_copy_from_dut_ptr(dut_ptr, b)               \
  do {                                                    \
    b.ready = dut_ptr->DUT_AXI(b_ready);                  \
    b.valid = dut_ptr->DUT_AXI(b_valid);                  \
    b.resp = dut_ptr->DUT_AXI(b_bits_resp);               \
    b.id = dut_ptr->DUT_AXI(b_bits_id);                   \
  } while (0);

#define axi_b_set_dut_ptr(dut_ptr, b)                     \
  do {                                                    \
    dut_ptr->DUT_AXI(b_valid) = b.valid;                  \
    dut_ptr->DUT_AXI(b_bits_resp) = b.resp;               \
    dut_ptr->DUT_AXI(b_bits_id) = b.id;                   \
  } while (0);

#define axi_ar_copy_from_dut_ptr(dut_ptr, ar)             \
  do {                                                    \
    ar.ready = dut_ptr->DUT_AXI(ar_ready);                \
    ar.valid = dut_ptr->DUT_AXI(ar_valid);                \
    ar.addr = dut_ptr->DUT_AXI(ar_bits_addr);             \
    ar.prot = dut_ptr->DUT_AXI(ar_bits_prot);             \
    ar.id = dut_ptr->DUT_AXI(ar_bits_id);                 \
    ar.len = dut_ptr->DUT_AXI(ar_bits_len);               \
    ar.size = dut_ptr->DUT_AXI(ar_bits_size);             \
    ar.burst = dut_ptr->DUT_AXI(ar_bits_burst);           \
    ar.lock = dut_ptr->DUT_AXI(ar_bits_lock);             \
    ar.cache = dut_ptr->DUT_AXI(ar_bits_cache);           \
    ar.qos = dut_ptr->DUT_AXI(ar_bits_qos);               \
  } while (0);

#define axi_ar_set_dut_ptr(dut_ptr, ar)                   \
  do {                                                    \
    dut_ptr->DUT_AXI(ar_ready) = ar.ready;                \
  } while (0);

#define axi_r_copy_from_dut_ptr(dut_ptr, r)               \
  do {                                                    \
    r.ready = dut_ptr->DUT_AXI(r_ready);                  \
    r.valid = dut_ptr->DUT_AXI(r_valid);                  \
    r.resp = dut_ptr->DUT_AXI(r_bits_resp);               \
    axi_copy_data(r.data, dut_ptr->DUT_AXI(r_bits_data))  \
    r.last = dut_ptr->DUT_AXI(r_bits_last);               \
    r.id = dut_ptr->DUT_AXI(r_bits_id);                   \
  } while (0);

#define axi_r_set_dut_ptr(dut_ptr, r)                     \
  do {                                                    \
    dut_ptr->DUT_AXI(r_valid) = r.valid;                  \
    dut_ptr->DUT_AXI(r_bits_resp) = r.resp;               \
    axi_copy_data(dut_ptr->DUT_AXI(r_bits_data), r.data)  \
    dut_ptr->DUT_AXI(r_bits_last) = r.last;               \
    dut_ptr->DUT_AXI(r_bits_id) = r.id;                   \
  } while (0);

#define axi_copy_from_dut_ptr(dut_ptr, axi)               \
  do {                                                    \
    axi_aw_copy_from_dut_ptr(dut_ptr, axi.aw)             \
    axi_w_copy_from_dut_ptr(dut_ptr, axi.w)               \
    axi_b_copy_from_dut_ptr(dut_ptr, axi.b)               \
    axi_ar_copy_from_dut_ptr(dut_ptr, axi.ar)             \
    axi_r_copy_from_dut_ptr(dut_ptr, axi.r)               \
  } while (0);

#define axi_set_dut_ptr(dut_ptr, axi)                     \
  do {                                                    \
    axi_aw_set_dut_ptr(dut_ptr, axi.aw)                   \
    axi_w_set_dut_ptr(dut_ptr, axi.w)                     \
    axi_b_set_dut_ptr(dut_ptr, axi.b)                     \
    axi_ar_set_dut_ptr(dut_ptr, axi.ar)                   \
    axi_r_set_dut_ptr(dut_ptr, axi.r)                     \
  } while (0);

// ar channel: (1) read raddr; (2) try to accept the address; (3) check raddr fire
bool axi_get_raddr(const axi_channel &axi, axi_addr_t &addr);
void axi_accept_raddr(axi_channel &axi);
bool axi_check_raddr_fire(const axi_channel &axi);

// r channel: (1) put rdata; (2) check rdata fire
void axi_put_rdata(axi_channel &axi, void *src, size_t n, bool last, uint8_t id);
bool axi_check_rdata_fire(const axi_channel &axi);

// aw channel: (1) read waddr; (2) try to accept the address; (3) check waddr fire
bool axi_get_waddr(const axi_channel &axi, axi_addr_t &addr);
void axi_accept_waddr(axi_channel &axi);
bool axi_check_waddr_fire(const axi_channel &axi);

// w channel: (1) accept wdata; (2) get wdata; (3) check wdata fire
void axi_accept_wdata(axi_channel &axi);
void axi_get_wdata(const axi_channel &axi, void *dest, size_t n);
bool axi_check_wdata_fire(const axi_channel &axi);

// b channel: (1) put response; (2) check response fire
void axi_put_wack(axi_channel &axi, uint8_t id);
bool axi_check_wack_fire(const axi_channel &axi);

#endif
