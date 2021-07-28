package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import chisel3.{util, _}
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.cache._

trait HasPdconst extends HasXSParameter {
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
    byteOffset(4,1) - 1.U
  } 
  def MAXINSNUM = 16
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

class PreDecodeResp(implicit p: Parameters) extends XSBundle with HasPdconst {
  val pc          = Vec(MAXINSNUM, UInt(VAddrBits.W))
  val instrs      = Vec(MAXINSNUM, UInt(32.W))
  val pd          = Vec(MAXINSNUM, (new PreDecodeInfo))
  val takens      = Vec(MAXINSNUM, Bool())
  val misOffset    = ValidUndirectioned(UInt(4.W))
  val cfiOffset    = ValidUndirectioned(UInt(4.W))
  val target       = UInt(VAddrBits.W)
  val jalTarget    = UInt(VAddrBits.W)
  val hasLastHalf   = Bool()
}

class PreDecode(implicit p: Parameters) extends XSModule with HasPdconst with HasIFUConst {
  val io = IO(new Bundle() {
    val in = Input(new IfuToPreDecode)
    val out = Output(new PreDecodeResp)
  })

  val instValid = io.in.instValid 
  val startRange = io.in.startRange
  val data      = io.in.data
  val pcStart   = io.in.startAddr
  val pcEnd     = io.in.fallThruAddr
  val bbOffset  = io.in.ftqOffset.bits
  val bbTaken   = io.in.ftqOffset.valid
  val bbTarget  = io.in.target
  val oversize  = io.in.oversize

  val validStart   = Wire(Vec(MAXINSNUM, Bool()))
  val validEnd     = Wire(Vec(MAXINSNUM, Bool()))
  val targets      = Wire(Vec(MAXINSNUM, UInt(VAddrBits.W)))
  val misPred      = Wire(Vec(MAXINSNUM, Bool()))
  val takens       = Wire(Vec(MAXINSNUM, Bool()))
  val falseHit     = Wire(Vec(MAXINSNUM, Bool()))
  val instRange    = Wire(Vec(MAXINSNUM, Bool()))
  //"real" means signals that are genrated by repaired end pc of this basic block using predecode information
  val realEndPC    = Wire(UInt(VAddrBits.W))
  val realHasLastHalf  = Wire(Vec(MAXINSNUM, Bool()))
  val realMissPred      = Wire(Vec(MAXINSNUM, Bool()))
  val realTakens        = Wire(Vec(MAXINSNUM, Bool()))

  val rawInsts = if (HasCExtension) VecInit((0 until MAXINSNUM).map(i => Cat(data(i+1), data(i))))  
                       else         VecInit((0 until MAXINSNUM/2).map(i => Cat(data(i*2+1) ,data(i*2))))

  for (i <- 0 until MAXINSNUM) {
    val inst        = WireInit(rawInsts(i))
    val expander    = Module(new RVCExpander)
    
    val isFirstInBlock = i.U === 0.U
    val isLastInBlock  = (i == MAXINSNUM - 1).B
    val currentPC      = pcStart + (i << 1).U((log2Ceil(MAXINSNUM)+1).W)
    val currentIsRVC   = isRVC(inst) && HasCExtension.B
    

    val lastIsValidEnd =  if (i == 0) { !io.in.lastHalfMatch } else { validEnd(i-1) || isFirstInBlock || !HasCExtension.B }
    
    validStart(i)   := (lastIsValidEnd || !HasCExtension.B) && startRange(i)
    validEnd(i)     := validStart(i) && currentIsRVC || !validStart(i) || !HasCExtension.B

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC)
    val brOffset  = br_offset(inst, currentIsRVC)

    io.out.pd(i).isRVC  := currentIsRVC
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall 
    io.out.pd(i).isRet  := isRet
    io.out.pc(i)        := currentPC
    //io.out.pd(i).valid := validStart(i)
    //io.out.pd(i).excType := ExcType.notExc
    expander.io.in := inst
    io.out.instrs(i) := expander.io.out.bits

    takens(i)    := (validStart(i) && (bbTaken && bbOffset === i.U && !io.out.pd(i).notCFI || io.out.pd(i).isJal))

    val jumpTarget      = io.out.pc(i) + Mux(io.out.pd(i).isBr, brOffset, jalOffset)
    targets(i) := Mux(takens(i), jumpTarget, pcEnd)
                       //Banch and jal have wrong targets
    val targetFault    = (validStart(i)  && i.U === bbOffset && bbTaken && (io.out.pd(i).isBr || io.out.pd(i).isJal) && bbTarget =/= targets(i))  
                       //An not-CFI instruction is predicted taken
    val notCFIFault    = (validStart(i)  && i.U === bbOffset && io.out.pd(i).notCFI && bbTaken) 
                       //A jal instruction is predicted not taken
    val jalFault       = (validStart(i)  && !bbTaken && io.out.pd(i).isJal) 
                       //An invalid instruction is predicted taken
    val invalidInsFault  = (!validStart(i) && i.U === bbOffset && bbTaken)

    misPred(i)   := targetFault  || notCFIFault || jalFault || invalidInsFault
    falseHit(i)  := invalidInsFault || notCFIFault

    realMissPred(i)     := misPred(i) && instRange(i)
    realHasLastHalf(i)  := instValid && currentPC === (realEndPC - 2.U) && validStart(i) && instRange(i) && !currentIsRVC
    realTakens(i)       := takens(i) && instRange(i)  
  }

  //val jalOH                   =  VecInit(io.out.pd.zipWithIndex.map{ case(inst, i) => inst.isJal  && validStart(i) })
  val jumpOH                  =  VecInit(io.out.pd.zipWithIndex.map{ case(inst, i) => inst.isJal  && validStart(i) }) //TODO: need jalr?
  val jumpOffset              =  PriorityEncoder(jumpOH)
  val rvcOH                   =  VecInit(io.out.pd.map(inst => inst.isRVC))
  //val jumpPC                  =  Mux1H(jumpOH, io.out.pc)
  //val jumpIsRVC               =  Mux1H(jumpOH, Mux1H(jumpOH, VecInit(io.out.pd.map(inst => inst.isRVC))))
  val jumpPC                  =  io.out.pc(jumpOffset)
  val jumpIsRVC               =  rvcOH(jumpOffset)
  val jumpNextPC              =  jumpPC + Mux(jumpIsRVC, 2.U, 4.U)
  val (hasFalseHit, hasJump)  =  (ParallelOR(falseHit), ParallelOR(jumpOH))
  val endRange                =  ((Fill(16, 1.U(1.W)) >> (~getBasicBlockIdx(realEndPC, pcStart))) | (Fill(16, oversize))) 
  val takeRange               =  Fill(16, !ParallelOR(takens))   | Fill(16, 1.U(1.W)) >> (~PriorityEncoder(takens))

  instRange               :=  VecInit((0 until MAXINSNUM).map(i => endRange(i) & startRange(i) &&  takeRange(i)))
  realEndPC               :=  Mux(hasFalseHit, Mux(hasJump, jumpNextPC, pcStart + 32.U), pcEnd)


  io.out.pd.zipWithIndex.map{case (inst,i) => inst.valid := instRange(i) && validStart(i)}
  io.out.misOffset.valid  := ParallelOR(realMissPred)
  io.out.misOffset.bits   := PriorityEncoder(realMissPred)

  io.out.cfiOffset.valid  := ParallelOR(realTakens)
  io.out.cfiOffset.bits   := PriorityEncoder(realTakens)

  io.out.target           := targets(io.out.cfiOffset.bits)
  io.out.takens           := realTakens

  io.out.jalTarget        :=  targets(jumpOffset)

  io.out.hasLastHalf      := realHasLastHalf.reduce(_||_)

  for (i <- 0 until MAXINSNUM) {
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
