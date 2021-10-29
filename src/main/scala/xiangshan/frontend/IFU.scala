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

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._
import utils._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth /8
  def mmioBeats = FetchWidth * 4 * 8 / mmioBusWidth
  def mmioMask  = VecInit(List.fill(PredictWidth)(true.B)).asUInt
}

trait HasIFUConst extends HasXSParameter {
  def addrAlign(addr: UInt, bytes: Int, highest: Int): UInt = Cat(addr(highest-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  def fetchQueueSize = 2
}

class IfuPtr(implicit p: Parameters) extends CircularQueuePtr[IfuPtr](entries = 2){
  override def cloneType = (new IfuPtr).asInstanceOf[this.type]
}

object IfuPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): IfuPtr = {
    val ptr = Wire(new IfuPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: IfuPtr)(implicit p: Parameters): IfuPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class IfuToFtqIO(implicit p:Parameters) extends XSBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(new FtqToIfuIO)
  val toFtq   = new IfuToFtqIO
}

class NewIFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface
  val icacheInter     = Vec(2, new ICacheMainPipeBundle)
  val toIbuffer       = Decoupled(new FetchToIBuffer)
}

// record the situation in which fallThruAddr falls into
// the middle of an RVI inst
class LastHalfInfo(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val middlePC = UInt(VAddrBits.W)
  def matchThisBlock(startAddr: UInt) = valid && middlePC === startAddr
}

class IfuToPreDecode(implicit p: Parameters) extends XSBundle {
  val data          = if(HasCExtension) Vec(PredictWidth + 1, UInt(16.W)) else Vec(PredictWidth, UInt(32.W))
  val startAddr     = UInt(VAddrBits.W)
  val fallThruAddr  = UInt(VAddrBits.W)
  val fallThruError = Bool()
  val isDoubleLine  = Bool()
  val ftqOffset     = Valid(UInt(log2Ceil(PredictWidth).W))
  val target        = UInt(VAddrBits.W)
  val pageFault     = Vec(2, Bool())
  val accessFault   = Vec(2, Bool())
  val instValid     = Bool()
  val lastHalfMatch = Bool()
  val oversize      = Bool()
}

class NewIFU(implicit p: Parameters) extends XSModule with HasICacheParameters with HasIFUConst
{
  println(s"icache ways: ${nWays} sets:${nSets}")
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toICache, fromICache) = (VecInit(io.icacheInter.map(_.req)), VecInit(io.icacheInter.map(_.resp)))

  def isCrossLineReq(start: UInt, end: UInt): Bool = start(blockOffBits) ^ end(blockOffBits)

  def isLastInCacheline(fallThruAddr: UInt): Bool = fallThruAddr(blockOffBits - 1, 1) === 0.U

  class TlbExept(implicit p: Parameters) extends XSBundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }


  //---------------------------------------------
  //  Fetch Stage 1 :
  //  * Send req to ICache Meta/Data
  //  * Check whether need 2 line fetch
  //---------------------------------------------

  val f0_valid                             = fromFtq.req.valid
  val f0_ftq_req                           = fromFtq.req.bits
  val f0_situation                         = VecInit(Seq(isCrossLineReq(f0_ftq_req.startAddr, f0_ftq_req.fallThruAddr), isLastInCacheline(f0_ftq_req.fallThruAddr)))
  val f0_doubleLine                        = f0_situation(0) || f0_situation(1)
  val f0_vSetIdx                           = VecInit(get_idx((f0_ftq_req.startAddr)), get_idx(f0_ftq_req.fallThruAddr))
  val f0_fire                              = fromFtq.req.fire()

  val f0_flush, f1_flush, f1_flush, f2_flush = WireInit(false.B)
  val from_bpu_f0_flush, from_bpu_f1_flush, from_bpu_f1_flush, from_bpu_f2_flush = WireInit(false.B)

  from_bpu_f0_flush := fromFtq.flushFromBpu.shouldFlushByStage2(f0_ftq_req.ftqIdx) ||
                       fromFtq.flushFromBpu.shouldFlushByStage3(f0_ftq_req.ftqIdx)

  val f2_redirect = WireInit(false.B)
  f2_flush := fromFtq.redirect.valid
  f1_flush := f2_flush || f2_redirect
  f1_flush := f1_flush || from_bpu_f1_flush
  f0_flush := f1_flush || from_bpu_f0_flush

  val f1_ready, f1_ready, f2_ready         = WireInit(false.B)

  fromFtq.req.ready := toICache(0).ready && toICache(1).ready && f1_ready && GTimer() > 500.U

  toICache(0).valid       := fromFtq.req.valid
  toICache(0).bits.vaddr  := fromFtq.req.bits.startAddr
  toICache(1).valid       := fromFtq.req.valid && doubleLine
  toICache(1).bits.vaddr  := fromFtq.req.bits.startAddr

  XSPerfAccumulate("ifu_bubble_ftq_not_valid",   !f0_valid )
  XSPerfAccumulate("ifu_bubble_pipe_stall",    f0_valid && fetch_req(0).ready && fetch_req(1).ready && !f1_ready )
  XSPerfAccumulate("ifu_bubble_sram_0_busy",   f0_valid && !fetch_req(0).ready  )
  XSPerfAccumulate("ifu_bubble_sram_1_busy",   f0_valid && !fetch_req(1).ready  )

  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * Send req to ITLB and TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------

  //TODO: handle fetch exceptions

  val icacheRespAllValid = WireInit(false.B)

  val f1_valid      = RegInit(false.B)
  val f1_ftq_req    = RegEnable(next = f0_ftq_req,    enable=f0_fire)
  val f1_situation  = RegEnable(next = f0_situation,  enable=f0_fire)
  val f1_doubleLine = RegEnable(next = f0_doubleLine, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,    enable=f0_fire)
  val f1_fire       = f1_valid && icacheRespAllValid && f1_ready

  f1_ready := f1_ready && icacheRespAllValid || !f1_valid

  from_bpu_f1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx)

  val preDecoder      = Module(new PreDecode)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)

  //flush generate and to Ftq
  val predecodeOutValid = WireInit(false.B)

  when(f1_flush)                  {f1_valid  := false.B}
  .elsewhen(f0_fire && !f0_flush) {f1_valid  := true.B}
  .elsewhen(f1_fire)              {f1_valid  := false.B}

  val f1_datas        = VecInit((0 unitl PortNumber).map(i =>fromICache(i).bits.readData))
  val f1_except_pf    = VecInit((0 unitl PortNumber).map(i =>fromICache(i).bits.pageFault))
  val f1_except_af    = VecInit((0 unitl PortNumber).map(i =>fromICache(i).bits.accessFault))

  def cut(cacheline: UInt, start: UInt) : Vec[UInt] ={
    if(HasCExtension){
      val result   = Wire(Vec(PredictWidth + 1, UInt(16.W)))
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 2, UInt(16.W)))
      val startPtr = Cat(0.U(1.W), start(blockOffBits-1, 1))
      (0 until PredictWidth + 1).foreach( i =>
        result(i) := dataVec(startPtr + i.U)
      )
      result
    } else {
      val result   = Wire(Vec(PredictWidth, UInt(32.W)) )
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 4, UInt(32.W)))
      val startPtr = Cat(0.U(1.W), start(blockOffBits-1, 2))
      (0 until PredictWidth).foreach( i =>
        result(i) := dataVec(startPtr + i.U)
      )
      result
    }
  }

  val preDecoder      = Module(new PreDecode)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)

  val f1_cut_data = cut( Cat(f1_datas.map(cacheline => cacheline.asUInt ).reverse).asUInt, f1_ftq_req.startAddr )

  val f1_jump_valids          = Fill(PredictWidth, !preDecoderOut.cfiOffset.valid)   | Fill(PredictWidth, 1.U(1.W)) >> (~preDecoderOut.cfiOffset.bits)
  val f1_predecode_valids     = VecInit(preDecoderOut.pd.map(instr => instr.valid)).asUInt & f1_jump_valids

  //---------------------------------------------
  //  Fetch Stage 3 :
  //  * get data from last stage (hit from f1_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val f2_valid          = RegInit(false.B)
  val f2_ftq_req        = RegEnable(next = f1_ftq_req,    enable=f1_fire)
  val f2_situation      = RegEnable(next = f1_situation,  enable=f1_fire)
  val f2_doubleLine     = RegEnable(next = f1_doubleLine, enable=f1_fire)
  val f2_fire           = io.toIbuffer.fire()

  when(f2_flush)                  {f2_valid := false.B}
  .elsewhen(f1_fire && !f1_flush) {f2_valid := true.B }
  .elsewhen(io.toIbuffer.fire())  {f2_valid := false.B}

  f2_ready := io.toIbuffer.ready || !f1_valid

  val f2_cut_data       = RegEnable(next = f1_cut_data, enable=f1_fire)
  val f2_except_pf      = RegEnable(next = f1_except_pf, enable = f1_fire)
  val f2_except_af      = RegEnable(next = f1_except_af, enable = f1_fire)
  val f2_hit            = RegEnable(next = f1_hit   , enable = f1_fire)

  val f2_lastHalf       = RegInit(0.U.asTypeOf(new LastHalfInfo))
  val f2_lastHalfMatch  = f2_lastHalf.matchThisBlock(f2_ftq_req.startAddr)
  val f2_except         = VecInit((0 until 2).map{i => f2_except_pf(i) || f2_except_af(i)})
  val f2_has_except     = f2_valid && (f2_except_af.reduce(_||_) || f2_except_pf.reduce(_||_))


  val f2_bank_hit = RegEnable(next = f1_bank_hit, enable = f1_fire)
  val f2_req_0 = io.toIbuffer.fire()
  val f2_req_1 = io.toIbuffer.fire() && f2_doubleLine
  val f2_hit_0 = io.toIbuffer.fire() & f2_bank_hit(0)
  val f2_hit_1 = io.toIbuffer.fire() && f2_doubleLine & f2_bank_hit(1)

  preDecoderIn.instValid     :=  f2_valid && !f2_has_except
  preDecoderIn.data          :=  f2_cut_data
  preDecoderIn.startAddr     :=  f2_ftq_req.startAddr
  preDecoderIn.fallThruAddr  :=  f2_ftq_req.fallThruAddr
  preDecoderIn.fallThruError :=  f2_ftq_req.fallThruError
  preDecoderIn.isDoubleLine  :=  f2_doubleLine
  preDecoderIn.ftqOffset     :=  f2_ftq_req.ftqOffset
  preDecoderIn.target        :=  f2_ftq_req.target
  preDecoderIn.oversize      :=  f2_ftq_req.oversize
  preDecoderIn.lastHalfMatch :=  f2_lastHalfMatch
  preDecoderIn.pageFault     :=  f2_except_pf
  preDecoderIn.accessFault   :=  f2_except_af


  // TODO: What if next packet does not match?
  when (f2_flush) {
    f2_lastHalf.valid := false.B
  }.elsewhen (io.toIbuffer.fire()) {
    f2_lastHalf.valid := preDecoderOut.hasLastHalf
    f2_lastHalf.middlePC := preDecoderOut.realEndPC
  }

  val f2_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt

  io.toIbuffer.valid          := f2_valid
  io.toIbuffer.bits.instrs    := preDecoderOut.instrs
  io.toIbuffer.bits.valid     := f2_predecode_range & preDecoderOut.instrRange.asUInt
  io.toIbuffer.bits.pd        := preDecoderOut.pd
  io.toIbuffer.bits.ftqPtr    := f2_ftq_req.ftqIdx
  io.toIbuffer.bits.pc        := preDecoderOut.pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := preDecoderOut.takens(i)}
  io.toIbuffer.bits.foldpc    := preDecoderOut.pc.map(i => XORFold(i(VAddrBits-1,1), MemPredPCWidth))
  io.toIbuffer.bits.ipf       := preDecoderOut.pageFault
  io.toIbuffer.bits.acf       := preDecoderOut.accessFault
  io.toIbuffer.bits.crossPageIPFFix := preDecoderOut.crossPageIPF

  //Write back to Ftq
  val finishFetchMaskReg = RegNext(f2_valid && !(f1_fire && !f1_flush))

  toFtq.pdWb.valid           := !finishFetchMaskReg && f2_valid
  toFtq.pdWb.bits.pc         := preDecoderOut.pc
  toFtq.pdWb.bits.pd         := preDecoderOut.pd
  toFtq.pdWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid :=  f2_predecode_range(i)}
  toFtq.pdWb.bits.ftqIdx     := f2_ftq_req.ftqIdx
  toFtq.pdWb.bits.ftqOffset  := f2_ftq_req.ftqOffset.bits
  toFtq.pdWb.bits.misOffset  := preDecoderOut.misOffset
  toFtq.pdWb.bits.cfiOffset  := preDecoderOut.cfiOffset
  toFtq.pdWb.bits.target     := preDecoderOut.target
  toFtq.pdWb.bits.jalTarget  := preDecoderOut.jalTarget
  toFtq.pdWb.bits.instrRange := preDecoderOut.instrRange

  val predecodeFlush     = preDecoderOut.misOffset.valid && f2_valid
  val predecodeFlushReg  = RegNext(predecodeFlush && !(f1_fire && !f1_flush))


  f2_redirect := !predecodeFlushReg && predecodeFlush

  XSPerfAccumulate("ifu_req",   io.toIbuffer.fire() )
  XSPerfAccumulate("ifu_miss",  io.toIbuffer.fire() && !f2_hit )
  XSPerfAccumulate("ifu_req_cacheline_0", f2_req_0  )
  XSPerfAccumulate("ifu_req_cacheline_1", f2_req_1  )
  XSPerfAccumulate("ifu_req_cacheline_0_hit",   f2_hit_0 )
  XSPerfAccumulate("ifu_req_cacheline_1_hit",   f2_hit_1 )
  XSPerfAccumulate("frontendFlush",  f2_redirect )
  XSPerfAccumulate("only_0_hit",      f2_only_0_hit   && io.toIbuffer.fire()  )
  XSPerfAccumulate("only_0_miss",     f2_only_0_miss  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_hit_1",     f2_hit_0_hit_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_miss_1",    f2_hit_0_miss_1 && io.toIbuffer.fire()  )
  XSPerfAccumulate("miss_0_hit_1",    f2_miss_0_hit_1  && io.toIbuffer.fire() )
  XSPerfAccumulate("miss_0_miss_1",   f2_miss_0_miss_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("cross_line_block", io.toIbuffer.fire() && f2_situation(0) )
  XSPerfAccumulate("fall_through_is_cacheline_end", io.toIbuffer.fire() && f2_situation(1) )
}
