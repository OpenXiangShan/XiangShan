package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import utility._
import yunsuan.scalar.FPCVT
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

class I2F(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {

  // io alias
  private val src1 = inData.src(0)
  private val fpRm = rm // todo

  private val fire = io.in.valid
  private val fireReg = GatedValidRegNext(fire)

  // input width 16, 32, 64
  val inSew1H = Wire(Sew())
  inSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getInputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )
  // output width 16， 32， 64
  val outSew1H = Wire(Sew())
  outSew1H := chisel3.util.experimental.decode.decoder(
    FCvtOpcode.getOutputDataWidth(fuOpType),
    TruthTable(
      Seq(
        BitPat("b01") -> BitPat("b0010"), // 16
        BitPat("b10") -> BitPat("b0100"), // 32
        BitPat("b11") -> BitPat("b1000"), // 64
      ),
      BitPat.N(4)
    )
  )
  if (backendParams.debugEn) {
    dontTouch(inSew1H)
    dontTouch(outSew1H)
  }

  val outIsMvInst = FCvtOpcode.getCvtSign(fuOpType).andR

  // modules
  val fcvt = Module(new FPCVT(XLEN, isI2F = true))
  fcvt.io.fire          := fire
  fcvt.io.src           := src1
  fcvt.io.opType        := fuOpType
  fcvt.io.rm            := fpRm
  fcvt.io.inSew1H       := inSew1H
  fcvt.io.outSew1H      := outSew1H

  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fcvt.io.fflags)

  val result = Mux(
    RegEnable(RegEnable(outIsMvInst, fire), fireReg),
    RegEnable(RegEnable(src1, fire), fireReg),
    fcvt.io.result
  )
  // box
  val res1H = RegEnable(RegEnable(outSew1H(3, 1), fire), fireReg)
  io.out.bits.res.data := Mux1H(
    res1H,
    Seq(
      Fill(48, true.B) ## result(15, 0),
      Fill(32, true.B) ## result(31, 0),
      result,
    )
  )
}
