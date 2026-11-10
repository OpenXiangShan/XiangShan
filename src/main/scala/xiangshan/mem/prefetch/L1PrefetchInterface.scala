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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.cache._

trait HasL1PrefetchSourceParameter {
  // l1 prefetch source related
  def L1PfSourceBits = 3
  // Stream 0, Stride 1
  def L1PrefetcherNum = 2
  def L1_HW_PREFETCH_NULL = 0.U
  def L1_HW_PREFETCH_CLEAR = 1.U // used to be a prefetch, clear by demand request
  def L1_HW_PREFETCH_STRIDE = 2.U
  def L1_HW_PREFETCH_STREAM = 3.U
  def L1_HW_PREFETCH_STORE  = 4.U
  def L1_HW_PREFETCH_BERTI = 5.U
  
  // ------------------------------------------------------------------------------------------------------------------------
  // timeline: L1_HW_PREFETCH_NULL  --(pf by stream)--> L1_HW_PREFETCH_STREAM --(pf hit by load)--> L1_HW_PREFETCH_CLEAR
  // ------------------------------------------------------------------------------------------------------------------------

  def isDemand(value: UInt) = value <= L1_HW_PREFETCH_CLEAR
  def isPrefetchRelated(value: UInt) = value >= L1_HW_PREFETCH_CLEAR
  def isFromL1Prefetch(value: UInt)  = value > L1_HW_PREFETCH_CLEAR
  def isPrefetchClear(value: UInt)   = value === L1_HW_PREFETCH_CLEAR
  def isFromStride(value: UInt)      = value === L1_HW_PREFETCH_STRIDE
  def isFromStream(value: UInt)      = value === L1_HW_PREFETCH_STREAM
  def isFromBerti(value: UInt) = value === L1_HW_PREFETCH_BERTI

  private val source2string = scala.collection.mutable.Map[String, BigInt]()
}

class L1PrefetchSource(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val value = UInt(L1PfSourceBits.W)
}

class L1PrefetchReq(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val confidence = UInt(1.W)
  val is_store = Bool()
  val pf_source = new L1PrefetchSource

  // when l1 cache prefetch req arrives at load unit:
  // if (confidence == 1)
  //   override load unit 2 load req
  // else if (load unit 1/2 is available)
  //   send prefetch req
  // else
  //   report prefetch !ready
}

class L1PrefetchHint(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
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
  val rand_offset = LFSR64(seed=Some(123L))(5,0) << 6
  val rand_addr_select = LFSR64(seed=Some(567L))(3,0) === 0.U

  // use valid vaddr and paddr
  val rand_vaddr = DelayN(io.vaddr, 2)
  val rand_paddr = DelayN(io.paddr, 2)

  io.req.bits.paddr := PmemRanges.map(_.lower).min.U + rand_offset
  io.req.bits.vaddr := 0.U
  io.req.bits.confidence := LFSR64(seed=Some(789L))(4,0) === 0.U
  io.req.bits.is_store := LFSR64(seed=Some(890L))(4,0) === 0.U
  io.req.valid := LFSR64(seed=Some(901L))(3,0) === 0.U
}
