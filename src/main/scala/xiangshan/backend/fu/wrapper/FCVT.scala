package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.FuOpType
import yunsuan.{VfcvtType, VfpuType}
import yunsuan.scalar.FPCVT
import yunsuan.util._


class FCVT(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // io alias
  private val opcode = fuOpType(8, 0)
  private val src0 = inData.src(0)
  private val sew = fp_fmt

  private val isFround  = opcode === VfcvtType.fround
  private val isFoundnx = opcode === VfcvtType.froundnx
  private val isFcvtmod = opcode === VfcvtType.fcvtmod_w_d

  private val isRtz = opcode(2) & opcode(1) | isFcvtmod
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

        BitPat("b11_01") -> BitPat("b1000"), // f16->f64/i64/ui64
        BitPat("b11_11") -> BitPat("b0010"), // f64->f16
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(output1H)
  }
  val outputWidth1H = output1H
  val outIs16bits = RegNext(RegNext(outputWidth1H(1)))
  val outIs32bits = RegNext(RegNext(outputWidth1H(2)))
  val outIsInt = !outCtrl.fuOpType(6)
  val outIsMvInst = outCtrl.fuOpType === FuOpType.FMVXF

  // modules
  val fcvt = Module(new FPCVT(XLEN))
  fcvt.io.fire := fire
  fcvt.io.src := src0
  fcvt.io.opType := opcode(7, 0)
  fcvt.io.sew := sew
  fcvt.io.rm := vfcvtRm
  fcvt.io.isFpToVecInst := true.B
  fcvt.io.isFround := Cat(isFoundnx, isFround)
  fcvt.io.isFcvtmod := isFcvtmod


  //cycle2
  val isNarrowCycle2 = RegEnable(RegEnable(isNarrowCvt, fire), fireReg)
  val outputWidth1HCycle2 = RegEnable(RegEnable(outputWidth1H, fire), fireReg)

  val fcvtResult = fcvt.io.result
  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fcvt.io.fflags)

  //fmv box
  val result_fmv = Mux1H(Seq(
    (sew === VSew.e8) -> Fill(56, src0(7)) ## src0(7, 0),
    (sew === VSew.e16) -> Fill(48, src0(15)) ## src0(15, 0),
    (sew === VSew.e32) -> Fill(32, src0(31)) ## src0(31, 0),
    (sew === VSew.e64) -> src0,
  ))
  // for scalar f2i cvt inst
  val isFpToInt32 = outIs32bits && outIsInt
  // for f2i mv inst
  val result = Mux(outIsMvInst, RegEnable(RegEnable(result_fmv, fire), fireReg),
    // for scalar fp32 fp16 result
    Mux(
      outIs32bits && !outIsInt,
      Cat(Fill(32, 1.U), fcvtResult(31,0)),
      Mux(outIs16bits && !outIsInt, Cat(Fill(48, 1.U), fcvtResult(15,0)), fcvtResult)
    )
  )

  io.out.bits.res.data := Mux(isFpToInt32,
    Fill(32, result(31)) ## result(31, 0),
    result
  )
}
