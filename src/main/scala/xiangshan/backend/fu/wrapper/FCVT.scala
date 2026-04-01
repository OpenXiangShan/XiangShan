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
import yunsuan.VfpuType
import yunsuan.scalar.FPCVT
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes._
import yunsuan.vector.Common._


class FCVT(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // io alias
  private val src0 = inData.src(0)
  private val fcvtRm = rm

  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // input width 8, 16, 32, 64
  val inSew1H = Wire(Sew())
  inSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getInputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b00") -> BitPat("b0001"), // 8
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )

  // output width 8， 16， 32， 64
  val outSew1H = Wire(Sew())
  outSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getOutputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b00") -> BitPat("b0001"), // 8
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(inSew1H)
    dontTouch(outSew1H)
  }
  val outIs16bits = RegNext(RegNext(outSew1H(1)))
  val outIs32bits = RegNext(RegNext(outSew1H(2)))
  val outIsInt = FCvtOpcode.outIsInt(outCtrl.fuOpType)
  val isMvFmt  = fuOpType(8, 7)
  val outIsMvInst = FCvtOpcode.isFmvF2I(outCtrl.fuOpType)

  // modules
  val fcvt = Module(new FPCVT(XLEN, isI2F = false))
  fcvt.io.fire     := fire
  fcvt.io.src      := src0
  fcvt.io.opType   := fuOpType
  fcvt.io.rm       := fcvtRm
  fcvt.io.inSew1H  := inSew1H
  fcvt.io.outSew1H := outSew1H


  //cycle2
  val fcvtResult = fcvt.io.result
  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fcvt.io.fflags)

  //fmv box
  val result_fmv = Mux1H(Seq(
    (isMvFmt === "b01".U) -> Fill(48, src0(15)) ## src0(15, 0),
    (isMvFmt === "b10".U) -> Fill(32, src0(31)) ## src0(31, 0),
    (isMvFmt === "b11".U) -> src0,
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
