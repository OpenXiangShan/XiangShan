package xiangshan.frontend

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
  def notCFI = brType === BrType.notBr
}

class PreDecodeResp extends XSBundle with HasIFUConst {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val mask = UInt(PredictWidth.W)
  // one for the first bank
  val lastHalf = Bool()
  val pd = Vec(PredictWidth, (new PreDecodeInfo))
}

class PreDecode extends XSModule with HasPdconst with HasIFUConst {
  val io = IO(new Bundle() {
    val in = Input(new ICacheResp)
    val prev = Flipped(ValidIO(UInt(16.W)))
    val prev_pc = Input(UInt(VAddrBits.W))
    val out = Output(new PreDecodeResp)
  })

  val data = io.in.data
  val mask = io.in.mask
  
  val packetAlignedPC = packetAligned(io.in.pc)
  val packetOffset = offsetInPacket(io.in.pc)

  val firstValidIdx = packetOffset // io.prev.valid should only occur with firstValidIdx = 0
  XSError(firstValidIdx =/= 0.U && io.prev.valid && HasCExtension.B, p"pc:${io.in.pc}, mask:${io.in.mask}, prevhalfInst valid occurs on unaligned fetch packet\n")

  val instsMask = Wire(Vec(PredictWidth, Bool()))
  val instsEndMask = Wire(Vec(PredictWidth, Bool()))

  val rawInsts = if (HasCExtension) {
                   VecInit((0 until PredictWidth).map(i => if (i == PredictWidth-1) Cat(0.U(16.W), data(i*16+15, i*16))
                                                         else data(i*16+31, i*16)))
                 } else {
                   VecInit((0 until PredictWidth).map(i => data(i*32+31, i*32)))
                 }

  for (i <- 0 until PredictWidth) {
    val inst = WireInit(rawInsts(i))
    val validStart = Wire(Bool()) // is the beginning of a valid inst
    val validEnd = Wire(Bool())  // is the end of a valid inst

    val isFirstInPacket = i.U === firstValidIdx
    val isLastInPacket = (i == PredictWidth-1).B
    val currentRVC = isRVC(inst) && HasCExtension.B

    val lastIsValidEnd = (if (i == 0) { !io.prev.valid } else { instsEndMask(i-1) || isFirstInPacket }) || !HasCExtension.B
    
    inst := (if (HasCExtension)
               Mux(io.prev.valid && i.U === 0.U,
                 Cat(rawInsts(i)(15,0), io.prev.bits),
                 rawInsts(i))
             else
               rawInsts(i))

    // when disable rvc, every 4 bytes should be an inst
    validStart := lastIsValidEnd && !(isLastInPacket && !currentRVC) || !HasCExtension.B
    validEnd := validStart && currentRVC || !validStart && !(isLastInPacket && !currentRVC) || !HasCExtension.B

    val currentLastHalf = lastIsValidEnd && (isLastInPacket && !currentRVC) && HasCExtension.B

    instsMask(i) := (if (i == 0) Mux(io.prev.valid, validEnd, validStart) else validStart)
    instsEndMask(i) := validEnd

    val brType::isCall::isRet::Nil = brInfo(inst)
    io.out.pd(i).isRVC := currentRVC
    io.out.pd(i).brType := brType
    io.out.pd(i).isCall := isCall
    io.out.pd(i).isRet := isRet
    io.out.pd(i).excType := ExcType.notExc
    io.out.instrs(i) := inst
    io.out.pc(i) := Mux(io.prev.valid && HasCExtension.B && (i==0).B, io.prev_pc, Cat(packetIdx(io.in.pc), (i << instOffsetBits).U(log2Ceil(packetBytes).W)))

    if (i == PredictWidth-1) { io.out.lastHalf := currentLastHalf }
  }
  io.out.mask := instsMask.asUInt & mask

  for (i <- 0 until PredictWidth) {
    XSDebug(true.B,
      p"instr ${Hexadecimal(io.out.instrs(i))}, " +
      p"mask ${Binary(instsMask(i))}, " +
      p"endMask ${Binary(instsEndMask(i))}, " +
      p"pc ${Hexadecimal(io.out.pc(i))}, " +
      p"isRVC ${Binary(io.out.pd(i).isRVC)}, " +
      p"brType ${Binary(io.out.pd(i).brType)}, " +
      p"isRet ${Binary(io.out.pd(i).isRet)}, " +
      p"isCall ${Binary(io.out.pd(i).isCall)}\n"
    )
  }
}

class RVCExpander extends XSModule {
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
