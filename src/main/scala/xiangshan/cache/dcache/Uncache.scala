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
import chipsalliance.rocketchip.config.Parameters
import utils._
import utility._
import xiangshan._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}

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

// One miss entry deals with one mmio request
class MMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    //  MSHR ID
    val hartId = Input(UInt())
    //  Control IO
    val enableOutstanding = Input(Bool())

    //  Client requests
    val req = Flipped(DecoupledIO(new UncacheWordReq))
    val resp = DecoupledIO(new DCacheWordRespWithError)

    //  TileLink
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    //  This entry is selected.
    val select = Input(Bool())
    val atomic = Output(Bool())
  })
  //  ================================================
  //  FSM state description:
  //  s_invalid     : Entry is invalid.
  //  s_refill_req  : Send Acquire request.
  //  s_refill_resp : Wait for Grant response.
  //  s_send_resp   : Send Uncache response.
  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req = Reg(new UncacheWordReq)
  val resp_data = Reg(UInt(DataBits.W))
  def storeReq = req.cmd === MemoryOpConstants.M_XWR

  //  Assign default values to output signals.
  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits := DontCare

  io.mem_acquire.valid := false.B
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := false.B

  io.atomic := req.atomic
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
  val size = PopCount(req.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
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
    io.mem_acquire.valid := true.B && io.select
    io.mem_acquire.bits := Mux(storeReq, store, load)

    when (io.mem_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire) {
      resp_data := io.mem_grant.bits.data
      assert(refill_done, "Uncache response should be one beat only!")
      state := Mux(storeReq && io.enableOutstanding, s_invalid, s_send_resp)
    }
  }

  //  Response
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.data   := resp_data
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
  override def shouldBeInlined: Boolean = false
  def idRange: Int = UncacheBufferSize

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

class UncacheImp(outer: Uncache)extends LazyModuleImp(outer)
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
  val need_fence = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val enqPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val issPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val deqPtr = RegInit(0.U.asTypeOf(new UncachePtr))
  val fence = RegInit(Bool(), false.B)

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
    entry.io.select := (i.U === issPtr.value) && Mux(entry.io.atomic, issPtr.value === deqPtr.value, !fence)

    when (i.U === issPtr.value) {
      need_fence := entry.io.atomic
    }

    //  Grant
    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits := DontCare
    when (i.U === deqPtr.value) {
      entry.io.mem_grant <> mem_grant
    }

    entry.io.resp.ready := false.B
    when (i.U === deqPtr.value) {
      io.lsq.resp <> entry.io.resp
    }

  }

  io.lsq.req.ready := req_ready
  when (io.enableOutstanding) {
    //  Uncache Buffer is a circular queue, which contains UncacheBufferSize entries.
    //  Description:
    //    enqPtr: Point to an invalid (means that the entry is free) entry. 
    //    issPtr: Point to a ready entry, the entry is ready to issue. 
    //    deqPtr: Point to the oldest entry, which was issued but has not accepted response (used to keep order with the program order).
    //
    //  When outstanding disabled, only one read/write request can be accepted at a time.
    //
    //  Example (Enable outstanding): 
    //    1. enqPtr: 
    //       1) Before enqueue
    //          enqPtr --
    //                  |
    //                  |
    //                  V
    //          +--+--+--+--+
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          +--+--+--+--+
    //
    //      2) After
    //          enqPtr+1 ---
    //                     |
    //                     |
    //                     V
    //          +--+--+--+--+
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          +--+--+--+--+
    //
    //    2. issPtr: 
    //      1) Before issue
    //          issPtr --
    //                  |
    //                  |
    //                  V
    //          +--+--+--+--+
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          +--+--+--+--+
    //
    //      2) After issue
    //          issPtr+1 --
    //                    |
    //                    |
    //                    V
    //          +--+--+--+--+
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          +--+--+--+--+
    //
    //   3. deqPtr: 
    //      1) Before dequeue
    //          deqPtr --
    //                  |
    //                  |
    //                  V
    //          +--+--+--+--+
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          |  |  |  |  |
    //          +--+--+--+--+
    //
    //      2) After dequeue
    //          deqPtr --                    deqPtr+1 --
    //                  |                              |
    //                  |                              |
    //                  V                              V
    //          +--+--+--+--+       or      +--+--+--+--+
    //          |  |  |  |  |               |  |  |  |  |
    //          |  |  |  |  |               |  |  |  |  |
    //          |  |  |  |  |               |  |  |  |  |
    //          +--+--+--+--+               +--+--+--+--+
    //              (load)                     (store)
    //
    //      3) After response
    //          deqPtr+1 ---                   deqPtr--
    //                     |                          |
    //                     |                          |
    //                     V                          V
    //          +--+--+--+--+       or      +--+--+--+--+
    //          |  |  |  |  |               |  |  |  |  |
    //          |  |  |  |  |               |  |  |  |  |
    //          |  |  |  |  |               |  |  |  |  |
    //          +--+--+--+--+               +--+--+--+--+
    //              (load)                     (store)
    //
    
    //  Enqueue
    when (req.fire) {
      enqPtr := enqPtr + 1.U
    }

    //  Issue
    when (mem_acquire.fire) {
      issPtr := issPtr + 1.U
    }

    when (mem_acquire.fire) {
      fence := need_fence
    }

    //  Dequeue
    when (mem_grant.fire) {
      deqPtr := Mux(edge.hasData(mem_grant.bits), deqPtr /* Load */, deqPtr + 1.U /* Store */)
    } .elsewhen (io.lsq.resp.fire /* Load */) {
      deqPtr := deqPtr + 1.U
    }

    when (mem_grant.fire && fence) {
      fence := false.B
    }
  } .otherwise {
    when (io.lsq.resp.fire) {
      enqPtr := enqPtr + 1.U
      issPtr := issPtr + 1.U
      deqPtr := deqPtr + 1.U
    }
  }

  TLArbiter.lowestFromSeq(edge, mem_acquire, entries.map(_.io.mem_acquire))
  io.flush.empty := deqPtr === enqPtr

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
  XSPerfAccumulate("mmio_outstanding", mem_acquire.fire && (deqPtr =/= issPtr))
  val perfEvents = Seq(
    ("mmio_store", io.lsq.req.fire && isStore),
    ("mmio_load", io.lsq.req.fire && !isStore),
    ("mmio_outstanding", mem_acquire.fire && (deqPtr =/= issPtr))
  )

  generatePerfEvent()
  //  End
}
