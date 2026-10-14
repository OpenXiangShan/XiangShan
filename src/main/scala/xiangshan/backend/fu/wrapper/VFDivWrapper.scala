package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VFDivOpcodes
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFuConfig, VecNonFixedLatFunc}
import xiangshan.backend.vector.WbFuBusyTable
import yunsuan.vector.Common.Fflags
import yunsuan.vector.VectorFloatDivider

class VFDivWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecNonFixedLatFunc(cfg) {
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vfdivs = Seq.fill(numVecModule)(Module(new VectorFloatDivider))

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val ex0NextSew = ex0NextOpcode(2, 1)
  private val sew = makePipeReg(ex0NextSew, pipeRegValids)
  private val isSqrt = makePipeReg(VFDivOpcodes.isFSqrt(ex0NextOpcode), pipeRegValids)
  private val ex0frm = in.frm.get

  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1

  private val divReady = vfdivs.map(_.io.start_ready_o).reduce(_ && _)
  private val divInFire = ex(0).valid && divReady
  private val divFlush = nonFixedLatOutCtrl.robIdx.needFlush(in.flush)
  private val divResultValid = vfdivs.map(_.io.finish_valid_o).reduce(_ && _)
  private val divFflags = vfdivs.flatMap(_.io.fflags_o.asTypeOf(Vec(4, Fflags())).toSeq)
  private val divFflagsE8 = Wire(Vec(dataWidth / 8, Fflags()))

  for (i <- 0 until numVecModule; byte <- 0 until dataWidthOfDataModule / 8) {
    val fflagsIdx = i * 4 + (byte / 2)
    divFflagsE8(i * (dataWidthOfDataModule / 8) + byte) := MuxCase(
      0.U.asTypeOf(Fflags()),
      Seq(
        (sew.ex0 === 1.U) -> divFflags(fflagsIdx),
        (sew.ex0 === 2.U) -> divFflags(i * 4 + (byte / 4)),
        (sew.ex0 === 3.U) -> divFflags(i * 4),
      )
    )
  }

  latchNonFixedLatOutCtrl(divInFire)

  vfdivs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.start_valid_i := ex(0).valid
      mod.io.finish_ready_i := true.B
      mod.io.flush_i := divFlush
      mod.io.fp_format_i := sew.ex0
      mod.io.opa_i := vs2Split.io.outVec64b(i)
      mod.io.opb_i := vs1Split.io.outVec64b(i)
      mod.io.frs2_i := 0.U
      mod.io.frs1_i := 0.U
      mod.io.is_frs2_i := false.B
      mod.io.is_frs1_i := false.B
      mod.io.is_sqrt_i := isSqrt.ex0
      mod.io.rm_i := ex0frm
      mod.io.is_vec_i := true.B
      mod.io.fp_aIsFpCanonicalNAN := false.B
      mod.io.fp_bIsFpCanonicalNAN := false.B
  }

  out.ex(0).valid := divResultValid
  out.ex(0).bits.data.vec.foreach {
    case vecData =>
      vecData.normal := Cat(vfdivs.map(_.io.fpdiv_res_o).reverse)
      vecData.narrow := 0.U.asTypeOf(vecData.narrow)
      vecData.maskE8 := 0.U.asTypeOf(vecData.maskE8)
      vecData.maskE16 := 0.U.asTypeOf(vecData.maskE16)
      vecData.maskE32 := 0.U.asTypeOf(vecData.maskE32)
      vecData.maskE64 := 0.U.asTypeOf(vecData.maskE64)
      vecData.fflagsE8.get := divFflagsE8
      vecData.narrowFflagsE8.get := 0.U.asTypeOf(vecData.narrowFflagsE8.get)
  }

  // The divider has no latency output. Reserve the non-fixed-latency WB
  // entry conservatively when the request is accepted; the result wakeup
  // below releases dependent vector operations at the actual completion.
  outFuLat.valid := divInFire
  outFuLat.bits := ((1 << WbFuBusyTable.NonFixedLatencyWidth) - 1).U

  private val resultLatency = Wire(Valid(UInt(WbFuBusyTable.NonFixedLatencyWidth.W)))
  resultLatency.valid := divResultValid
  resultLatency.bits := 0.U
  connectNonFixedLatWakeUp(resultLatency, divFlush, divResultValid)
}
