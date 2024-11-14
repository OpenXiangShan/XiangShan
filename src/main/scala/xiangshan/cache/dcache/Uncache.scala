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
import org.chipsalliance.cde.config.Parameters
import utils._
import utility._
import xiangshan._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}

class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

class UncacheEntry(implicit p: Parameters) extends DCacheBundle {
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val data = UInt(XLEN.W)
  val mask = UInt((XLEN/8).W)
  val id = UInt(uncacheIdxBits.W)
  val nc = Bool()
  val atomic = Bool()
  
  // FIXME lyq: data and resp_data can be merged?
  val resp_data = UInt(XLEN.W)
  val resp_nderr = Bool()

  def set(x: UncacheWordReq): Unit = {
    cmd := x.cmd
    addr := x.addr
    data := x.data
    mask := x.mask
    id := x.id
    nc := x.nc
    atomic := x.atomic
    resp_nderr := false.B
    resp_data := 0.U
  }

  def update(x: TLBundleD): Unit = {
    resp_data := x.data
    resp_nderr := x.denied
  }

  def toUncacheWordResp(): UncacheWordResp = {
    val r = Wire(new UncacheWordResp)
    r := DontCare
    r.data := resp_data
    r.id := id
    r.nderr := resp_nderr
    r.nc := nc
    r.is2lq := cmd === MemoryOpConstants.M_XRD
    r.miss := false.B
    r.replay := false.B
    r.tag_error := false.B
    r.error := false.B
    r
  }
}

class UncacheEntryState(implicit p: Parameters) extends DCacheBundle {
  // FIXME lyq: state is multi bools or UInt()?
  // valid (-> waitSame) -> inflight -> waitReturn
  val valid = Bool()
  val inflight = Bool() // uncache -> L2
  val waitSame = Bool()
  val waitReturn = Bool() // uncache -> LSQ
  
  def init: Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }

  def isValid(): Bool = valid
  def isInflight(): Bool = inflight
  def isWaitReturn(): Bool = waitReturn
  def isWaitSame(): Bool = waitSame
  def can2Uncache(): Bool = valid && !inflight && !waitSame && !waitReturn
  def can2Lsq(): Bool = valid && waitReturn
  
  def setValid(x: Bool): Unit = { valid := x}
  def setInflight(x: Bool): Unit = { inflight := x}
  def setWaitReturn(x: Bool): Unit = { waitReturn := x }
  def setWaitSame(x: Bool): Unit = { waitSame := x}
  
  def updateUncacheResp(): Unit = {
    assert(inflight, "The request was not sent and a response was received")
    inflight := false.B
    waitReturn := true.B
  }
  def updateReturn(): Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }
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

/* Uncache Buffer */
class UncacheImp(outer: Uncache)extends LazyModuleImp(outer)
  with HasTLDump
  with HasXSParameter
  with HasPerfEvents
{
  private val INDEX_WIDTH = log2Up(UncacheBufferSize)
  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")
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
  io.lsq.req.ready := req_ready
  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare


  /******************************************************************
   * Data Structure
   ******************************************************************/

  val entries = Reg(Vec(UncacheBufferSize, new UncacheEntry))
  val states = RegInit(VecInit(Seq.fill(UncacheBufferSize)(0.U.asTypeOf(new UncacheEntryState))))
  val fence = RegInit(Bool(), false.B)
  val s_idle :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val uState = RegInit(s_idle)
  
  def sizeMap[T <: Data](f: Int => T) = VecInit((0 until UncacheBufferSize).map(f))

  val q0_entry = Wire(new UncacheEntry)
  val q0_canSentIdx = Wire(UInt(INDEX_WIDTH.W))
  val q0_canSent = Wire(Bool())
  /******************************************************************
   * uState for non-outstanding
   ******************************************************************/

  switch(uState){
    is(s_idle){
      when(req.fire){
        uState := s_refill_req
      }
    }
    is(s_refill_req){
      when(mem_acquire.fire){
        uState := s_refill_resp
      }
    }
    is(s_refill_resp){
      when(mem_grant.fire){
        uState := s_send_resp
      }
    }
    is(s_send_resp){
      when(resp.fire){
        uState := s_idle
      }
    }
  }


  /******************************************************************
   * Enter Buffer
   *  Version 0 (better timing)
   *    e0 judge: alloc/merge write vec
   *    e1 alloc
   * 
   *  Version 1 (better performance)
   *    solved in one cycle for achieving the original performance.
   ******************************************************************/

  /**
    TODO lyq: how to merge
    1. same addr
    2. same cmd
    3. valid
    FIXME lyq: not merge now due to the following issues
    1. load cann't be merged
    2. how to merge store and response precisely
  */

  val e0_invalids = sizeMap(i => !states(i).isValid())
  val e0_invalid_oh = VecInit(PriorityEncoderOH(e0_invalids)).asUInt
  val e0_fire = req.fire
  val e0_req = req.bits

  req_ready := e0_invalid_oh.orR
  
  for (i <- 0 until UncacheBufferSize) {
    val alloc = e0_fire && e0_invalid_oh(i)
    when(alloc){
      entries(i).set(e0_req)
      states(i).setValid(true.B)
      
      // judge whether wait same block: e0 & q0
      val waitSameVec = sizeMap(j => 
        e0_req.addr === entries(j).addr && states(j).isValid() && states(j).isInflight()
      )
      val waitQ0 = e0_req.addr === q0_entry.addr && q0_canSent
      when (waitSameVec.reduce(_ || _) || waitQ0) {
        states(i).setWaitSame(true.B)
      }
    }
  }


  /******************************************************************
   * Uncache Req
   *  Version 0 (better timing)
   *    q0: choose which one is sent
   *    q0: sent
   * 
   *  Version 1 (better performance)
   *    solved in one cycle for achieving the original performance.
   *    NOTE: "Enter Buffer" & "Uncache Req" not a continuous pipeline,
   *          because there is no guarantee that mem_aquire will be always ready.
   ******************************************************************/

  val q0_canSentVec = sizeMap(i => 
    // (io.enableOutstanding || uState === s_refill_req) && // FIXME lyq: comment for debug
    states(i).can2Uncache()
  )
  val q0_res = PriorityEncoderWithFlag(q0_canSentVec)
  q0_canSentIdx := q0_res._1
  q0_canSent := q0_res._2
  q0_entry := entries(q0_canSentIdx)

  val size = PopCount(q0_entry.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(q0_canSent && !legal))

  val q0_load = edge.Get(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize
  )._2

  val q0_store = edge.Put(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize,
    data            = q0_entry.data,
    mask            = q0_entry.mask
  )._2

  val q0_isStore = q0_entry.cmd === MemoryOpConstants.M_XWR

  mem_acquire.valid := q0_canSent
  mem_acquire.bits := Mux(q0_isStore, q0_store, q0_load)
  when(mem_acquire.fire){
    states(q0_canSentIdx).setInflight(true.B)

    // q0 should judge whether wait same block
    (0 until UncacheBufferSize).map(j => 
      when(q0_entry.addr === entries(j).addr && states(j).isValid() && !states(j).isWaitReturn()){
        states(j).setWaitSame(true.B)
      }
    )
  }


  /******************************************************************
   * Uncache Resp
   ******************************************************************/

  val (_, _, refill_done, _) = edge.addr_inc(mem_grant)
  
  mem_grant.ready := true.B
  when (mem_grant.fire) {
    val id = mem_grant.bits.source
    entries(id).update(mem_grant.bits)
    states(id).updateUncacheResp()
    assert(refill_done, "Uncache response should be one beat only!")

    // remove state of wait same block
    (0 until UncacheBufferSize).map(j => 
      when(entries(id).addr === entries(j).addr && states(j).isValid() && states(j).isWaitSame()){
        states(j).setWaitSame(false.B)
      }
    )
  }


  /******************************************************************
   * Return to LSQ
   ******************************************************************/

  val r0_canSentVec = sizeMap(i => states(i).can2Lsq())
  val (r0_canSentIdx, r0_canSent) = PriorityEncoderWithFlag(r0_canSentVec)
  resp.valid := r0_canSent
  resp.bits := entries(r0_canSentIdx).toUncacheWordResp()
  when(resp.fire){
    states(r0_canSentIdx).updateReturn()
  }


  /******************************************************************
   * Buffer Flush
   * // FIXME lyq: how to deal
   * 1. when io.flush.valid is true
   * 2. when io.lsq.req.bits.atomic is true
   ******************************************************************/

  val invalid_entries = PopCount(states.map(!_.isValid()))
  io.flush.empty := invalid_entries === UncacheBufferSize.U


  /******************************************************************
   * Debug / Performance
   ******************************************************************/

  /* Debug Counters */
  // print all input/output requests for debug purpose
  // print req/resp
  XSDebug(req.fire, "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire, "data: %x\n", req.bits.data)
  // print tilelink messages
  when(mem_acquire.valid){
    XSDebug("mem_acquire valid, ready=%d ", mem_acquire.ready)
    mem_acquire.bits.dump
  }
  when (mem_grant.fire) {
    XSDebug("mem_grant fire ")
    mem_grant.bits.dump
  }

  /* Performance Counters */
  def isStore: Bool = io.lsq.req.bits.cmd === MemoryOpConstants.M_XWR
  XSPerfAccumulate("uncache_mmio_store", io.lsq.req.fire && isStore && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_mmio_load", io.lsq.req.fire && !isStore && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_store", io.lsq.req.fire && isStore && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_load", io.lsq.req.fire && !isStore && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_outstanding", uState =/= s_refill_req && mem_acquire.fire)
  
  val perfEvents = Seq(
    ("uncache_mmio_store", io.lsq.req.fire && isStore && !io.lsq.req.bits.nc),
    ("uncache_mmio_load", io.lsq.req.fire && !isStore && !io.lsq.req.bits.nc),
    ("uncache_nc_store", io.lsq.req.fire && isStore && io.lsq.req.bits.nc),
    ("uncache_nc_load", io.lsq.req.fire && !isStore && io.lsq.req.bits.nc),
    ("uncache_outstanding", uState =/= s_refill_req && mem_acquire.fire)
  )

  generatePerfEvent()
  //  End
}
