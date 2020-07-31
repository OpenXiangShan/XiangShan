package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils.XSDebug
import xiangshan._
import xiangshan.backend.decode.isa.predecode.PreDecodeInst

trait HasPdconst{ this: XSModule =>
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val rd = instr(11,7)
    val rs = instr(19,15)
    val brType::Nil = ListLookup(instr, List(BrType.notBr), PreDecodeInst.brTable)
    val isCall = (brType === BrType.jal || brType === BrType.jalr) && isLink(rd) && !isRVC(instr)
    val isRet = brType === BrType.jalr && isLink(rs) && !isLink(rd) && !isRVC(instr)
    List(brType, isCall, isRet)
  }
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

class PreDecodeInfo extends XSBundle {  // 8 bit
  val isRVC   = Bool()
  val brType  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  val excType = UInt(3.W)
  def isBr = brType === BrType.branch
  def isJal = brType === BrType.jal
  def isJalr = brType === BrType.jalr
}

class PreDecodeResp extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val mask = UInt(PredictWidth.W)
  val pd = Vec(PredictWidth, (new PreDecodeInfo))
}

class PreDecode extends XSModule with HasPdconst{
  val io = IO(new Bundle() {
    val in = Input(new FakeIcacheResp)
    val prev = Flipped(ValidIO(UInt(16.W)))
    val out = Output(new PreDecodeResp)
  })

  val data = io.in.data
  val mask = io.in.mask

  val insts = Wire(Vec(PredictWidth, UInt(32.W)))
  val instsMask = Wire(Vec(PredictWidth, Bool()))
  val instsRVC = Wire(Vec(PredictWidth,Bool()))
  val instsPC = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))
  val nextHalf = Wire(UInt(16.W))

  for (i <- 0 until PredictWidth) {
    val inst = Wire(UInt(32.W))
    val valid = Wire(Bool())
    val pc = io.in.pc + (i << 1).U - Mux(io.prev.valid && (i.U === 0.U), 2.U, 0.U)

    if (i==0) {
      inst := Mux(io.prev.valid, Cat(data(15,0), io.prev.bits), data(31,0))
      valid := true.B
    } else if (i==1) {
      inst := data(47,16)
      valid := io.prev.valid || !(instsMask(0) && !isRVC(insts(0)))
    } else if (i==PredictWidth-1) {
      inst := Cat(0.U(16.W), data(i*16+15, i*16))
      valid := !(instsMask(i-1) && !isRVC(insts(i-1)) || !isRVC(inst))
    } else {
      inst := data(i*16+31, i*16)
      valid := !(instsMask(i-1) && !isRVC(insts(i-1)))
    }

    insts(i) := inst
    instsRVC(i) := isRVC(inst)
    instsMask(i) := mask(i) && valid 
    instsPC(i) := pc

    val brType::isCall::isRet::Nil = brInfo(inst)
    io.out.pd(i).isRVC := instsRVC(i)
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall
    io.out.pd(i).isRet := isRet
    io.out.pd(i).excType := ExcType.notExc
    io.out.instrs(i) := insts(i)
    io.out.pc(i) := instsPC(i)
    
  }
  io.out.mask := instsMask.asUInt

  for (i <- 0 until PredictWidth) {
    XSDebug(true.B,
      p"instr ${Hexdecimal(io.out.instrs(i))}, " +
      p"mask ${Binary(instsMask(i))}, " +
      p"pc ${Hexdecimal(io.out.pc(i))}, " +
      p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
      p"brType ${Binary(io.out.pd(i).brType)}, " +
      p"isRet ${Binary(io.out.pd(i).isRet)}, " +
      p"isCall ${Binary(io.out.pd(i).isCall)}\n"
    )
  }
}
