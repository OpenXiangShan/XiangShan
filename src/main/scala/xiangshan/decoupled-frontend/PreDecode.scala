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
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import chisel3.{util, _}
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.cache._

trait HasPdConst extends HasXSParameter with HasICacheParameters with HasIFUConst{
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val brType::Nil = ListLookup(instr, List(BrType.notCFI), PreDecodeInst.brTable)
    val rd = Mux(isRVC(instr), instr(12), instr(11,7))
    val rs = Mux(isRVC(instr), Mux(brType === BrType.jal, 0.U, instr(11, 7)), instr(19, 15))
    val isCall = (brType === BrType.jal && !isRVC(instr) || brType === BrType.jalr) && isLink(rd) // Only for RV64
    val isRet = brType === BrType.jalr && isLink(rs) && !isCall
    List(brType, isCall, isRet)
  }
  def jal_offset(inst: UInt, rvc: Bool): UInt = {
    val rvc_offset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rvi_offset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val max_width = rvi_offset.getWidth
    SignExt(Mux(rvc, SignExt(rvc_offset, max_width), SignExt(rvi_offset, max_width)), XLEN)
  }
  def br_offset(inst: UInt, rvc: Bool): UInt = {
    val rvc_offset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rvi_offset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val max_width = rvi_offset.getWidth
    SignExt(Mux(rvc, SignExt(rvc_offset, max_width), SignExt(rvi_offset, max_width)), XLEN)
  }
  def getBasicBlockIdx( pc: UInt, start:  UInt ): UInt = {
    val byteOffset = pc - start
    (byteOffset - instBytes.U)(log2Ceil(PredictWidth),instOffsetBits)
  }

  def NOP = "h4501".U(16.W)
}

object BrType {
  def notCFI   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object ExcType {  //TODO:add exctype
  def notExc = "b000".U
  def apply() = UInt(3.W)
}

class PreDecodeInfo extends Bundle {  // 8 bit
  val valid   = Bool()
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  //val excType = UInt(3.W)
  def isBr    = brType === BrType.branch
  def isJal   = brType === BrType.jal
  def isJalr  = brType === BrType.jalr
  def notCFI  = brType === BrType.notCFI
}

class PreDecodeResp(implicit p: Parameters) extends XSBundle with HasPdConst {
  val pc          = Vec(PredictWidth, UInt(VAddrBits.W))
  val instrs      = Vec(PredictWidth, UInt(32.W))
  val pd          = Vec(PredictWidth, (new PreDecodeInfo))
  val takens      = Vec(PredictWidth, Bool())
  val misOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val cfiOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target       = UInt(VAddrBits.W)
  val jalTarget    = UInt(VAddrBits.W)
  val hasLastHalf  = Bool()
  val realEndPC    = UInt(VAddrBits.W)
  val instrRange   = Vec(PredictWidth, Bool())
  val pageFault    = Vec(PredictWidth, Bool())
  val accessFault  = Vec(PredictWidth, Bool())
  val crossPageIPF = Vec(PredictWidth, Bool())
}

class PreDecode(implicit p: Parameters) extends XSModule with HasPdConst{
  val io = IO(new Bundle() {
    val in = Input(new IfuToPreDecode)
    val out = Output(new PreDecodeResp)
  })

  val instValid     = io.in.instValid 
  val data          = io.in.data
  val pcStart       = io.in.startAddr
  val pcEnd         = io.in.fallThruAddr
  val pcEndError    = io.in.fallThruError
  val isDoubleLine  = io.in.isDoubleLine
  val bbOffset      = io.in.ftqOffset.bits
  val bbTaken       = io.in.ftqOffset.valid
  val bbTarget      = io.in.target
  val oversize      = io.in.oversize
  val pageFault     = io.in.pageFault
  val accessFault   = io.in.accessFault


  val validStart        = Wire(Vec(PredictWidth, Bool()))
  dontTouch(validStart)
  val validEnd          = Wire(Vec(PredictWidth, Bool()))
  val targets           = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))
  val misPred           = Wire(Vec(PredictWidth, Bool()))
  val takens            = Wire(Vec(PredictWidth, Bool()))
  val falseHit          = Wire(Vec(PredictWidth, Bool()))
  val instRange         = Wire(Vec(PredictWidth, Bool()))
  //"real" means signals that are genrated by repaired end pc of this basic block using predecode information
  val realEndPC         = Wire(UInt(VAddrBits.W))
  val realHasLastHalf   = Wire(Vec(PredictWidth, Bool()))
  val realMissPred      = Wire(Vec(PredictWidth, Bool()))
  val realTakens        = Wire(Vec(PredictWidth, Bool()))

  val rawInsts = if (HasCExtension) VecInit((0 until PredictWidth).map(i => Cat(data(i+1), data(i))))  
                       else         VecInit((0 until PredictWidth).map(i => data(i)))
  
  val nextLinePC =  align(pcStart, 64) + 64.U

  for (i <- 0 until PredictWidth) {
    //TODO: Terrible timing for pc comparing
    val isNextLine      = (io.out.pc(i) > nextLinePC)
    val nullInstruction = isNextLine && !isDoubleLine

    val hasPageFault   = validStart(i) && ((io.out.pc(i) < nextLinePC && pageFault(0))   || (io.out.pc(i) > nextLinePC && pageFault(1)))
    val hasAccessFault = validStart(i) && ((io.out.pc(i) < nextLinePC && accessFault(0)) || (io.out.pc(i) > nextLinePC && accessFault(1)))
    val exception      = hasPageFault || hasAccessFault
    val inst           = Mux(exception || nullInstruction , NOP, WireInit(rawInsts(i)))
    val expander       = Module(new RVCExpander)

    val isFirstInBlock = i.U === 0.U
    val isLastInBlock  = (i == PredictWidth - 1).B
    val currentPC      = pcStart + (i << 1).U((log2Ceil(PredictWidth)+1).W)
    val currentIsRVC   = isRVC(inst) && HasCExtension.B

    val lastIsValidEnd =  if (i == 0) { !io.in.lastHalfMatch } else { validEnd(i-1) || isFirstInBlock || !HasCExtension.B }
    
    validStart(i)   := (lastIsValidEnd || !HasCExtension.B)
    validEnd(i)     := validStart(i) && currentIsRVC || !validStart(i) || !HasCExtension.B

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC)
    val brOffset  = br_offset(inst, currentIsRVC)

    io.out.pd(i).valid         := (lastIsValidEnd || !HasCExtension.B)
    io.out.pd(i).isRVC         := currentIsRVC
    io.out.pd(i).brType        := brType
    io.out.pd(i).isCall        := isCall 
    io.out.pd(i).isRet         := isRet
    io.out.pc(i)               := currentPC
    io.out.pageFault(i)        := hasPageFault
    io.out.accessFault(i)      := hasAccessFault
    io.out.crossPageIPF(i)     := (io.out.pc(i) === align(realEndPC, 64) - 2.U) && !pageFault(0) && pageFault(1) && !currentIsRVC

    expander.io.in             := inst
    io.out.instrs(i)           := expander.io.out.bits

    takens(i)    := (validStart(i)  && (bbTaken && bbOffset === i.U && !io.out.pd(i).notCFI || io.out.pd(i).isJal || io.out.pd(i).isRet))

    val jumpTarget      = io.out.pc(i) + Mux(io.out.pd(i).isBr, brOffset, jalOffset)
    targets(i) := Mux(takens(i), jumpTarget, pcEnd)
                       //Banch and jal have wrong targets
    val targetFault    = (validStart(i)  && i.U === bbOffset && bbTaken && (io.out.pd(i).isBr || io.out.pd(i).isJal) && bbTarget =/= targets(i))  
                       //An not-CFI instruction is predicted taken
    val notCFIFault    = (validStart(i)  && i.U === bbOffset && io.out.pd(i).notCFI && bbTaken) 
                       //A jal instruction is predicted not taken
    val jalFault       = (validStart(i)  && !bbTaken && io.out.pd(i).isJal)
                       //A ret instruction is predicted not taken
    val retFault       = (validStart(i)  && !bbTaken && io.out.pd(i).isRet) 
                       //An invalid instruction is predicted taken
    val invalidInsFault  = (!validStart(i)  && i.U === bbOffset && bbTaken)

    misPred(i)   := targetFault  || notCFIFault || jalFault || retFault || invalidInsFault || pcEndError
    falseHit(i)  := invalidInsFault || notCFIFault

    realMissPred(i)     := misPred(i) && instRange(i)
    realHasLastHalf(i)  := instValid && currentPC === (realEndPC - 2.U) && validStart(i) && instRange(i) && !currentIsRVC
    realTakens(i)       := takens(i) && instRange(i)  
  }

  val jumpOH                  =  VecInit(io.out.pd.zipWithIndex.map{ case(inst, i) => inst.isJal  && validStart(i) }) //TODO: need jalr?
  val jumpOffset              =  PriorityEncoder(jumpOH)
  val rvcOH                   =  VecInit(io.out.pd.map(inst => inst.isRVC))
  val jumpPC                  =  io.out.pc(jumpOffset)
  val jumpIsRVC               =  rvcOH(jumpOffset)
  val jumpNextPC              =  jumpPC + Mux(jumpIsRVC, 2.U, 4.U)
  val (hasFalseHit, hasJump)  =  (ParallelOR(falseHit), ParallelOR(jumpOH))
  val endRange                =  ((Fill(PredictWidth, 1.U(1.W)) >> (~getBasicBlockIdx(realEndPC, pcStart))) | (Fill(PredictWidth, oversize)))
  val takeRange               =  Fill(PredictWidth, !ParallelOR(takens))   | Fill(PredictWidth, 1.U(1.W)) >> (~PriorityEncoder(takens))
  val fixCross                =  ((pcStart + (FetchWidth * 4).U) > nextLinePC) && !isDoubleLine
  val boundPC                 =  Mux(fixCross, nextLinePC - 2.U  ,pcStart + (FetchWidth * 4).U)

  instRange               :=  VecInit((0 until PredictWidth).map(i => endRange(i) &&  takeRange(i)))
  realEndPC               :=  Mux(hasFalseHit, Mux(hasJump && ((jumpNextPC < boundPC) || (jumpNextPC === boundPC) ), jumpNextPC, boundPC), pcEnd)

  val validLastOffset     = Mux(io.out.pd((PredictWidth - 1).U).valid, (PredictWidth - 1).U, (PredictWidth - 2).U)
  io.out.misOffset.valid  := ParallelOR(realMissPred)
  io.out.misOffset.bits   := Mux(pcEndError,validLastOffset,PriorityEncoder(realMissPred))
  io.out.instrRange.zipWithIndex.map{case (bit,i) => bit := instRange(i).asBool()}

  io.out.cfiOffset.valid  := ParallelOR(realTakens)
  io.out.cfiOffset.bits   := PriorityEncoder(realTakens)

  io.out.target           := Mux(io.out.cfiOffset.valid, targets(io.out.cfiOffset.bits), realEndPC)
  io.out.takens           := realTakens

  io.out.jalTarget        :=  targets(jumpOffset)

  io.out.hasLastHalf      := realHasLastHalf.reduce(_||_)
  io.out.realEndPC        := realEndPC

  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    for (i <- 0 until PredictWidth) {
      XSDebug(true.B,
        p"instr ${Hexadecimal(io.out.instrs(i))}, " +
        p"validStart ${Binary(validStart(i))}, " +
        p"validEnd ${Binary(validEnd(i))}, " +
        p"pc ${Hexadecimal(io.out.pc(i))}, " +
        p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
        p"brType ${Binary(io.out.pd(i).brType)}, " +
        p"isRet ${Binary(io.out.pd(i).isRet)}, " +
        p"isCall ${Binary(io.out.pd(i).isCall)}\n"
      )
    }
  }
}

class RVCExpander(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(new ExpandedInstruction)
  })

  if (HasCExtension) {
    io.out := new RVCDecoder(io.in, XLEN).decode
  } else {
    io.out := new RVCDecoder(io.in, XLEN).passthrough
  }
}
