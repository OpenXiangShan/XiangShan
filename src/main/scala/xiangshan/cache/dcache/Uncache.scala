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

package xiangshan.cache

import chisel3._
import chisel3.util._
import utils._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters, TLMessages}
import xiangshan.{MicroOp, Redirect, HasXSParameter}
import xiangshan.backend.fu.fpu.FMAMidResultIO
import xiangshan.cache.mmu.MMUIOBaseBundle

import scala.math.max

class UncachePtr(implicit p: Parameters) extends CircularQueuePtr[UncachePtr](
  p => p(XSCoreParamsKey).UncacheBufferSize
){

}

object UncachePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): UncachePtr = {
    val ptr = Wire(new UncachePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

class MMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    //  Control IO
    val hartId = Input(UInt())
    val enableOutstanding = Input(Bool())

    //  Client requests
    val req = Flipped(DecoupledIO(new DCacheWordReq))
    val resp = DecoupledIO(new DCacheWordRespWithError)

    //  TileLink
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    //  This entry is selected.
    val select = Input(Bool())
  })

  //  ================================================
  //  FSM state description:
  //  s_invalid     : Entry is invalid.
  //  s_refill_req  : Send Acquire request.
  //  s_refill_resp : Wait for Grant response.
  //  s_send_resp   : Send Uncache response.
  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req = Reg(new DCacheWordReq)
  def storeReq = req.cmd === MemoryOpConstants.M_XWR

  //  Assign default values to output signals.
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits := DontCare

  io.mem_acquire.valid := false.B
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := false.B

  //  Receive request
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire) {
      req := io.req.bits
      req.addr := io.req.bits.addr
      state := s_refill_req
    }
  }

  //  Refill
  //  TODO: determine 'lgSize' in memend
  //  sizeMap
  //  1.U -> 0.U
  //  2.U -> 1.U
  //  4.U -> 2.U
  //  .....
  val sizeMap = Seq(
    1.U   -> 0.U,
    2.U   -> 1.U,
    4.U   -> 2.U,
    8.U   -> 3.U
  )

  val size = PopCount(req.mask.asUInt)
  val (lgSize, legal) = PriorityMuxWithFlag(sizeMap.map(m => (size===m._1) -> m._2))
  assert(!(io.mem_acquire.valid && !legal))

  val load = edge.Get(
    fromSource      = io.hartId,
    toAddress       = req.addr,
    lgSize          = lgSize
  )._2

  val store = edge.Put(
    fromSource      = io.hartId,
    toAddress       = req.addr,
    lgSize          = lgSize,
    data            = req.data,
    mask            = req.mask
  )._2

  XSDebug("entry: %d state: %d\n", io.hartId, state)

  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits := Mux(storeReq, store, load)

    when (io.mem_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire()) {
      req.data := io.mem_grant.bits.data
      assert(refill_done, "Uncache response should be one beat only!")
      state := Mux(storeReq && io.enableOutstanding, s_invalid, s_send_resp)
    }
  }
  
  //  Response
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.data   := req.data
    // meta data should go with the response
    io.resp.bits.id     := req.id
    io.resp.bits.miss   := false.B
    io.resp.bits.replay := false.B
    io.resp.bits.tag_error := false.B
    io.resp.bits.error := false.B

    when (io.resp.fire()) {
      state := s_invalid
    }
  }
  //  End
}

class UncacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt())
  val enableOutstanding = Input(Bool())
  val flush = Flipped(new UncacheFlushBundle)
  val lsq = Flipped(new UncacheWordIO)
}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL
class Uncache()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  def lgSize: Int = log2Up(UncacheBufferSize)
  def idRange: Int = lgSize max 8 // Hart Id

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

class UncacheImp(outer: Uncache)
  extends LazyModuleImp(outer)
    with HasTLDump
    with HasXSParameter
    with HasPerfEvents
{
  val io = IO(new UncacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val req  = io.lsq.req
  val resp = io.lsq.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d

  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val enqPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val deqPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val cmtPtr = RegInit(0.U.asTypeOf(new UncachePtr))

  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare

  val entries = Seq.fill(UncacheBufferSize) { Module(new MMIOEntry(edge)) }
  for ((entry, i) <- entries.zipWithIndex) {
    entry.io.hartId := io.hartId
    entry.io.enableOutstanding := io.enableOutstanding

   //  Enqueue
    entry.io.req.valid := (i.U === enqPtr.value) && req.valid
    entry.io.req.bits := req.bits

    when (i.U === enqPtr.value) {
      req_ready := entry.io.req.ready
    }

    //  Acquire
    entry.io.select := (edge.hasData(entry.io.mem_acquire.bits) || cmtPtr === deqPtr /* Load */) && (i.U === deqPtr.value)

    //  Grant
    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits := DontCare
    when (i.U === cmtPtr.value) {
      entry.io.mem_grant <> mem_grant
    }

    entry.io.resp.ready := false.B
    when (i.U === cmtPtr.value) {
      io.lsq.resp <> entry.io.resp
    }

  }

  io.lsq.req.ready := req_ready
  when (io.enableOutstanding) {
    //  Enqueue
    when (req.fire) {
      enqPtr := enqPtr + 1.U
    }

    //  Dequeue
    when (mem_acquire.fire) {
      deqPtr := Mux(edge.hasData(mem_acquire.bits), deqPtr + 1.U /* Store */, deqPtr /* Load */)
    } .elsewhen (mem_grant.fire && edge.hasData(mem_grant.bits) /* Load */) {
      deqPtr := deqPtr + 1.U
    }

    //  Commit
    when (mem_grant.fire) {
      cmtPtr := Mux(edge.hasData(mem_grant.bits), cmtPtr /* Load */, cmtPtr + 1.U /* Store */)
    } .elsewhen (io.lsq.resp.fire /* Load */) {
      cmtPtr := cmtPtr + 1.U
    }
  } .otherwise {
    when (io.lsq.resp.fire) {
      enqPtr := enqPtr + 1.U
      deqPtr := deqPtr + 1.U
      cmtPtr := cmtPtr + 1.U
    }
  }

  TLArbiter.lowestFromSeq(edge, mem_acquire, entries.map(_.io.mem_acquire))
  io.flush.empty := RegNext(cmtPtr === enqPtr)

  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")

  // print all input/output requests for debug purpose
  // print req/resp
  XSDebug(req.fire(), "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire(), "data: %x\n", req.bits.data)

  // print tilelink messages
  when(mem_acquire.valid){
    XSDebug("mem_acquire valid, ready=%d ", mem_acquire.ready)
    mem_acquire.bits.dump
  }
  when (mem_grant.fire()) {
    XSDebug("mem_grant fire ")
    mem_grant.bits.dump
  }

  //  Performance Counters
  def isStore: Bool = io.lsq.req.bits.cmd === MemoryOpConstants.M_XWR
  XSPerfAccumulate("mmio_store", io.lsq.req.fire && isStore)
  XSPerfAccumulate("mmio_load", io.lsq.req.fire && !isStore)
  XSPerfAccumulate("mmio_outstanding", mem_acquire.fire && (cmtPtr =/= deqPtr))
  val perfEvents = Seq(
    ("mmio_store", io.lsq.req.fire && isStore),
    ("mmio_load", io.lsq.req.fire && !isStore),
    ("mmio_outstanding", mem_acquire.fire && (cmtPtr =/= deqPtr))
  )

  generatePerfEvent()
  //  End
}
