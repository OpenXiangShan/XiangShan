package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRegFileParameter {
  val NRReg = 32
}

class RegFile(width:Int, hasZero:Boolean = true) extends HasRegFileParameter with HasNOOPParameter {
  val rf = Mem(NRReg, UInt(width.W))
  def read(addr: UInt) : UInt = if(hasZero) Mux(addr === 0.U, 0.U, rf(addr)) else rf(addr)
  def write(addr: UInt, data: UInt) = { rf(addr) := data }
} 

class ScoreBoard(hasZero:Boolean = true) extends HasRegFileParameter {
  val busy = RegInit(0.U(NRReg.W))
  def isBusy(idx: UInt): Bool = busy(idx)
  def mask(idx: UInt) = (1.U(NRReg.W) << idx)(NRReg-1, 0)
  def update(setMask: UInt, clearMask: UInt) = {
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit when reg(i) is written
    // and issued at the same cycle.
    // Note that rf(0) is always free when hasZero==true.
    if(hasZero) busy := Cat(((busy & ~clearMask) | setMask)(NRReg-1, 1), 0.U(1.W))
    else busy := ((busy & ~clearMask) | setMask)
  }
}

class ISU(implicit val p: NOOPConfig) extends NOOPModule with HasRegFileParameter {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new DecodeIO)
    val wb = Flipped(new WriteBackIO)
    val flush = Input(Bool())
    val forward = Flipped(new ForwardIO)
  })

  io.out.bits := DontCare
  val rfSrc1 = io.in.bits.ctrl.rfSrc1
  val rfSrc2 = io.in.bits.ctrl.rfSrc2
  val rfDest = io.in.bits.ctrl.rfDest

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen

  val forwardRfWen = io.forward.wb.rfWen && io.forward.valid
  val dontForward = (io.forward.fuType =/= FuType.alu) && (io.forward.fuType =/= FuType.lsu)
  val src1DependEX = isDepend(rfSrc1, io.forward.wb.rfDest, forwardRfWen)
  val src2DependEX = isDepend(rfSrc2, io.forward.wb.rfDest, forwardRfWen)
  val src1DependWB = isDepend(rfSrc1, io.wb.rfDest, io.wb.rfWen)
  val src2DependWB = isDepend(rfSrc2, io.wb.rfDest, io.wb.rfWen)

  val src1ForwardNextCycle = src1DependEX && !dontForward
  val src2ForwardNextCycle = src2DependEX && !dontForward
  val src1Forward = src1DependWB && Mux(dontForward, !src1DependEX, true.B)
  val src2Forward = src2DependWB && Mux(dontForward, !src2DependEX, true.B)

  val sb = new ScoreBoard
  val src1Ready = !sb.isBusy(rfSrc1) || src1ForwardNextCycle || src1Forward
  val src2Ready = !sb.isBusy(rfSrc2) || src2ForwardNextCycle || src2Forward

  val (fprSrcReady,fprSrcData):(Bool,Array[UInt]) = if(HasFPU){
    val fpr = new RegFile(width = XLEN, hasZero = false)
    val fsb = new ScoreBoard(hasZero = false)
    val forwardFpWen = io.forward.wb.fpWen && io.forward.valid

    when (io.wb.fpWen) {
      fpr.write(io.wb.rfDest, io.wb.rfData)
      Debug(){
        printf(p"[isu] write fpr:${io.wb.rfDest} value=${Hexadecimal(io.wb.rfData)} " +
          p"at pc=${Hexadecimal(io.in.bits.cf.pc)}\n")
      }
    }

    val fsbClearMask = Mux(io.wb.fpWen && !isDepend(io.wb.rfDest, io.forward.wb.rfDest, forwardFpWen),
      fsb.mask(io.wb.rfDest), 0.U(NRReg.W))
    val fsbSetMask = Mux(io.out.fire() && io.in.bits.ctrl.fpWen, fsb.mask(rfDest), 0.U)
    when (io.flush) { fsb.update(0.U, Fill(NRReg, 1.U(1.W))) }
      .otherwise { fsb.update(fsbSetMask, fsbClearMask) }

    val instr = io.in.bits.cf.instr
    val (fpSrc1,fpSrc2,fpSrc3) = (instr(19, 15), instr(24, 20), instr(31, 27))
    val srcs = Seq(fpSrc1, fpSrc2, fpSrc3).zip(Seq(
      io.in.bits.ctrl.src1Type,
      io.in.bits.ctrl.src2Type,
      io.in.bits.ctrl.src3Type
    ))
    val dataVec = Array.fill(3)(Wire(UInt(XLEN.W)))
    // result
    (srcs.zipWithIndex.map({
      case ((src, t),i) =>
        val dependEX = isDepend(src, io.forward.wb.rfDest, forwardFpWen)
        val dependWB = isDepend(src, io.wb.rfDest, io.wb.fpWen)
        val forwardEX = dependEX && !dontForward
        val forwardWB = dependWB && Mux(dontForward, !dependEX, true.B)
        dataVec(i) := MuxCase(fpr.read(src), Seq(
          forwardEX -> io.forward.wb.rfData,
          forwardWB -> io.wb.rfData
        ))
        (!fsb.busy(src) || forwardEX || forwardWB) || (t =/= SrcType.fp)
    }).reduceLeft(_ && _), dataVec)
  } else (true.B, Array.fill(3)(0.U))

  io.out.valid := io.in.valid && src1Ready && src2Ready && fprSrcReady

  val rf = new RegFile(XLEN)
//  io.out.bits.data.src1 := Mux1H(List(
//    (io.in.bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in.bits.cf.pc, AddrBits),
//    src1ForwardNextCycle -> io.forward  .wb.rfData,
//    (src1Forward && !src1ForwardNextCycle) -> io.wb.rfData,
//    ((io.in.bits.ctrl.src1Type =/= SrcType.pc) && !src1ForwardNextCycle && !src1Forward) -> rf.read(rfSrc1)
//  ))
//  io.out.bits.data.src2 := Mux1H(List(
//    (io.in.bits.ctrl.src2Type =/= SrcType.reg) -> io.in.bits.data.imm,
//    src2ForwardNextCycle -> io.forward.wb.rfData,
//    (src2Forward && !src2ForwardNextCycle) -> io.wb.rfData,
//    ((io.in.bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle && !src2Forward) -> rf.read(rfSrc2)
//  ))

  io.out.bits.data.src1 := MuxCase(rf.read(rfSrc1), Seq(
    (io.in.bits.ctrl.src1Type === SrcType.fp) -> fprSrcData(0),
    (io.in.bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in.bits.cf.pc, AddrBits),
    src1ForwardNextCycle -> io.forward.wb.rfData,
    src1Forward -> io.wb.rfData
  ))
  io.out.bits.data.src2 := MuxCase(rf.read(rfSrc2), Seq(
    (io.in.bits.ctrl.src2Type === SrcType.fp) -> fprSrcData(1),
    (io.in.bits.ctrl.src2Type =/= SrcType.reg) -> io.in.bits.data.imm,
    src2ForwardNextCycle -> io.forward.wb.rfData,
    src2Forward -> io.wb.rfData
  ))

  io.out.bits.data.imm  := Mux(io.in.bits.ctrl.src3Type===SrcType.fp, fprSrcData(2), io.in.bits.data.imm)

  io.out.bits.cf <> io.in.bits.cf
  io.out.bits.ctrl := io.in.bits.ctrl
  io.out.bits.ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits.ctrl.isSrc2Forward := src2ForwardNextCycle

  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfData) }

  val wbClearMask = Mux(io.wb.rfWen && !isDepend(io.wb.rfDest, io.forward.wb.rfDest, forwardRfWen), sb.mask(io.wb.rfDest), 0.U(NRReg.W))
  val isuFireSetMask = Mux(io.out.fire() && io.in.bits.ctrl.rfWen, sb.mask(rfDest), 0.U)
  when (io.flush) { sb.update(0.U, Fill(NRReg, 1.U(1.W))) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in.ready := !io.in.valid || io.out.fire()

  // read after write
  BoringUtils.addSource(io.in.valid && !io.out.valid, "perfCntCondMrawStall")
  BoringUtils.addSource(io.out.valid && !io.out.fire(), "perfCntCondMexuBusy")

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(VecInit((0 until NRReg).map(i => rf.read(i.U))), "difftestRegs")
  }
}
