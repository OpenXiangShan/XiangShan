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

class I2F(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfcvt OpType not supported")

  // io alias
  private val opcode = fuOpType(8, 0)
  private val src0 = inData.src(0)
  private val sew = fp_fmt
  private val vfcvtRm = rm

  val widen = opcode(4, 3) // 0: single 1: widen 2: norrow  3: 16->64/64->16
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

        BitPat("b11_01") -> BitPat("b1000"),
        BitPat("b11_11") -> BitPat("b0010"), // e64->e16
      ),
      BitPat.N(4)
    )
  )
  if(backendParams.debugEn) {
    dontTouch(output1H)
  }
  
  val outIsMvInst = outCtrl.fuOpType(8).asBool
  val outSew      = outCtrl.fpu.get.fmt

  // modules
  val fcvt = Module(new FPCVT(XLEN, isI2F = true))
  fcvt.io.fire := fire
  fcvt.io.src := src0
  fcvt.io.opType := opcode(7, 0)
  fcvt.io.sew := sew
  fcvt.io.rm := vfcvtRm
  fcvt.io.isFpToVecInst := true.B
  fcvt.io.isFround := 0.U
  fcvt.io.isFcvtmod := 0.U

  io.out.bits.res.fflags.get := Mux(outIsMvInst, 0.U, fcvt.io.fflags)

  val result = Mux(
    outIsMvInst, 
    RegEnable(RegEnable(src0, fire), fireReg),
    fcvt.io.result
  )
  // box
  val res1H = RegEnable(RegEnable(output1H(3, 1), fire), fireReg)
  io.out.bits.res.data := Mux1H(
    res1H,
    Seq(
      Fill(48, true.B) ## result(15, 0),
      Fill(32, true.B) ## result(31, 0),
      result,
    )
  )
}
