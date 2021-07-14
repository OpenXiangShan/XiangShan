package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import xiangshan._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.cache._

trait HasPdconst{ this: XSModule =>
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val brType::Nil = ListLookup(instr, List(BrType.notBr), PreDecodeInst.brTable)
    val rd = Mux(isRVC(instr), instr(12), instr(11,7))
    val rs = Mux(isRVC(instr), Mux(brType === BrType.jal, 0.U, instr(11, 7)), instr(19, 15))
    val isCall = (brType === BrType.jal && !isRVC(instr) || brType === BrType.jalr) && isLink(rd) // Only for RV64
    val isRet = brType === BrType.jalr && isLink(rs) && !isCall
    List(brType, isCall, isRet)
  }
  def jal_offset(inst: UInt, rvc: Bool): SInt = {
    Mux(rvc,
      Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W)).asSInt(),
      Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)).asSInt()
    )
  }
  def br_offset(inst: UInt, rvc: Bool): SInt = {
    Mux(rvc,
      Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W)).asSInt,
      Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)).asSInt()
    )
  }
  def MAXINSNUM = 16
}

object BrType {
  def notBr   = "b00".U
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
  def isBr = brType === BrType.branch
  def isJal = brType === BrType.jal
  def isJalr = brType === BrType.jalr
  def notCFI = brType === BrType.notBr
}

class PreDecodeResp(implicit p: Parameters) extends XSBundle with HasPdconst {
  val pc          = Vec(MAXINSNUM, UInt(VAddrBits.W))
  val instrs      = Vec(MAXINSNUM, UInt(32.W))
  val valid       = UInt(MAXINSNUM.W)
  val pd          = Vec(MAXINSNUM, (new PreDecodeInfo))
  val brTarget    = UInt(VAddrBits.W)
  val jalTarget   = UInt(VAddrBits.W)
  val jumpOffset  = ValidUndirectioned(UInt(log2Ceil(MAXINSNUM).W))
  val brOffset    = UInt(log2Ceil(MAXINSNUM).W)
}

class PreDecode(implicit p: Parameters) extends XSModule with HasPdconst with HasIFUConst {
  val io = IO(new Bundle() {
    val in = Input(new IfuToPreDecode)
    val out = Output(new PreDecodeResp)
  })

  val data      = io.in.data
  val pcStart   = io.in.startAddr 

  val validStart   = Wire(Vec(MAXINSNUM, Bool()))
  val validEnd     = Wire(Vec(MAXINSNUM, Bool()))
  val targets      = Wire(Vec(MAXINSNUM, UInt(VAddrBits.W)))

  val rawInsts = if (HasCExtension) VecInit((0 until MAXINSNUM).map(i => Cat(data(i+1), data(i))))  
                       else         VecInit((0 until MAXINSNUM/2).map(i => Cat(data(i*2+1) ,data(i*2))))

  for (i <- 0 until MAXINSNUM) {
    val inst        = WireInit(rawInsts(i))
    
    val isFirstInBlock = i.U === 0.U
    val isLastInBlock  = (i == MAXINSNUM - 1).B
    val currentIsRVC     = isRVC(inst) && HasCExtension.B

    val lastIsValidEnd =  validEnd(i-1)  || isFirstInBlock || !HasCExtension.B
    
    validStart(i) := lastIsValidEnd || !HasCExtension.B
    validEnd(i)   := validStart(i) && currentIsRVC || !validStart(i) || !HasCExtension.B

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC)
    val brOffset  = br_offset(inst, currentIsRVC)

    io.out.pd(i).isRVC := currentIsRVC
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall
    io.out.pd(i).isRet := isRet
    //io.out.pd(i).excType := ExcType.notExc
    io.out.instrs(i) := inst
    io.out.pc(i) := pcStart + (i << 1).U(log2Ceil(MAXINSNUM).W)

    
  }
  val isJumpOH = VecInit((0 until MAXINSNUM).map(i => (io.out.pd(i).isJal || io.out.pd(i).isJalr) && validStart(i)).reverse).asUInt()
  val isBrOH   = VecInit((0 until MAXINSNUM).map(i => io.out.pd(i).isBr && validStart(i)).reverse).asUInt()

  val jalOffset = PriorityEncoder(isJumpOH)
  val brOffset  = PriorityEncoder(isBrOH)

  io.out.valid := validStart
  io.out.jumpOffset.valid := isJumpOH((MAXINSNUM - 1).U)
  io.out.jumpOffset.bits  := jalOffset
  io.out.brOffset         := brOffset
  io.out.brTarget         := targets(brOffset)   //TODO: support more branch instructions basic-block
  io.out.jalTarget        := targets(jalOffset)


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
