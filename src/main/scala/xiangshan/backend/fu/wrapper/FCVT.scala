package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.VfpuType
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.util._


class FCVT(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // io alias
  private val opcode = fuOpType(8, 0)
  private val src0 = inData.src(0)
  private val sew = fp_fmt

  private val isRtz = opcode(2) & opcode(1)
  private val isRod = opcode(2) & !opcode(1) & opcode(0)
  private val isFrm = !isRtz && !isRod
  private val vfcvtRm = Mux1H(
    Seq(isRtz, isRod, isFrm),
    Seq(1.U, 6.U, rm)
  )

  val widen = opcode(4, 3) // 0->single 1->widen 2->norrow => width of result
  val isSingleCvt = !widen(1) & !widen(0)
  val isWidenCvt = !widen(1) & widen(0)
  val isNarrowCvt = widen(1) & !widen(0)
  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // output width 8， 16， 32， 64
  val output1H = Wire(UInt(4.W))
  output1H := chisel3.util.experimental.decode.decoder(
    widen ## sew,
    TruthTable(
      Seq(
        BitPat("b00_01") -> BitPat("b0010"), // 16
        BitPat("b00_10") -> BitPat("b0100"), // 32
        BitPat("b00_11") -> BitPat("b1000"), // 64

        BitPat("b01_00") -> BitPat("b0010"), // 16
        BitPat("b01_01") -> BitPat("b0100"), // 32
        BitPat("b01_10") -> BitPat("b1000"), // 64

        BitPat("b10_00") -> BitPat("b0001"), // 8
        BitPat("b10_01") -> BitPat("b0010"), // 16
        BitPat("b10_10") -> BitPat("b0100"), // 32
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(output1H)
  }
  val outputWidth1H = output1H
  val outIs32bits = RegNext(RegNext(outputWidth1H(2)))
  val outIsInt = !outCtrl.fuOpType(6)
  val outIsMvInst = outCtrl.fuOpType(8)

  // modules
  val fcvt = Module(new VectorCvt(XLEN))
  fcvt.io.fire := fire
  fcvt.io.src := src0
  fcvt.io.opType := opcode(7, 0)
  fcvt.io.sew := sew
  fcvt.io.rm := vfcvtRm
  fcvt.io.isFpToVecInst := true.B


  //cycle2
  val isNarrowCycle2 = RegEnable(RegEnable(isNarrowCvt, fire), fireReg)
  val outputWidth1HCycle2 = RegEnable(RegEnable(outputWidth1H, fire), fireReg)

  val fcvtResult = Mux(isNarrowCycle2, fcvt.io.result.tail(32), fcvt.io.result)

  val fcvtFflags = Mux1H(outputWidth1HCycle2, Seq(
    fcvt.io.fflags,
    Mux(isNarrowCycle2, fcvt.io.fflags.tail(10), fcvt.io.fflags),
    Mux(isNarrowCycle2, fcvt.io.fflags(4,0), fcvt.io.fflags.tail(10)),
    fcvt.io.fflags(4,0)
  ))

  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fcvtFflags)

  // for scalar f2i cvt inst
  val isFpToInt32 = outIs32bits && outIsInt
  // for f2i mv inst
  val result = Mux(outIsMvInst, RegNext(RegNext(src0)), fcvtResult)

  io.out.bits.res.data := Mux(isFpToInt32,
    Fill(32, result(31)) ## result(31, 0),
    result
  )
}
