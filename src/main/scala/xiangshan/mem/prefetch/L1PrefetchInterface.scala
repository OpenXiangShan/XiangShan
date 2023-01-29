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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}

class L1PrefetchReq (implicit p: Parameters) extends XSBundle with HasDCacheParameters{
  val paddr = UInt(PAddrBits.W)
  val alias = UInt(2.W)
  val confidence = UInt(1.W)
  val is_store = Bool()

  // only index bit is used, do not use tag
  def getVaddr(): UInt = {
    Cat(alias, paddr(DCacheSameVPAddrLength-1, 0))
  }

  // when l1 cache prefetch req arrives at load unit:
  // if (confidence == 1) 
  //   override load unit 2 load req
  // else if (load unit 1/2 is available)
  //   send prefetch req
  // else
  //   report prefetch !ready
}

class L1PrefetchHint (implicit p: Parameters) extends XSBundle with HasDCacheParameters{
  val loadbusy = Bool()
  val missqbusy = Bool()
}

class L1PrefetchFuzzer(implicit p: Parameters) extends DCacheModule{
  val io = IO(new Bundle() {
    // prefetch req interface
    val req = Decoupled(new L1PrefetchReq())
    // for fuzzer address gen
    val vaddr = Input(UInt(VAddrBits.W))
    val paddr = Input(UInt(PAddrBits.W))
  })

  // prefetch req queue is not provided, prefetcher must maintain its
  // own prefetch req queue.
  val rand_offset = LFSR64(seed=None)(3,0) << 6
  val rand_addr_select = LFSR64(seed=None)(3,0) === 0.U

  // use valid vaddr and paddr
  val rand_vaddr = DelayN(io.vaddr, 2)
  val rand_paddr = DelayN(io.paddr, 2)

  io.req.bits.paddr := 0x80000000L.U + rand_offset
  io.req.bits.alias := io.req.bits.paddr(13,12)
  io.req.bits.confidence := LFSR64(seed=None)(4,0) === 0.U
  io.req.bits.is_store := LFSR64(seed=None)(4,0) === 0.U
  io.req.valid := LFSR64(seed=None)(3,0) === 0.U
}