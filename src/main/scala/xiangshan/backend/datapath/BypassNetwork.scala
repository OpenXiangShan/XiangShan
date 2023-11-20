package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.ZeroExt
import xiangshan.XSBundle
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuVec, ExuOutput}
import xiangshan.backend.issue.{IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.datapath.DataConfig.RegDataMaxWidth

class BypassNetworkIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

  val fromDataPath = new FromDataPath
  val toExus = new ToExus
  val fromExus = new FromExus

  class FromDataPath extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(intSchdParams.genExuInputBundle)
    val vf : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(vfSchdParams.genExuInputBundle)
    val mem: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(memSchdParams.genExuInputBundle)
  }

  class ToExus extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle
    val vf : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = vfSchdParams.genExuInputBundle
    val mem: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle
  }

  class FromExus extends Bundle {
    val int: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(intSchdParams.genExuBypassValidBundle)
    val vf : MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(vfSchdParams.genExuBypassValidBundle)
    val mem: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(memSchdParams.genExuBypassValidBundle)

    def connectExuOutput(
      getSinkVecN: FromExus => MixedVec[MixedVec[ValidIO[ExuBypassBundle]]]
    )(
      sourceVecN: MixedVec[MixedVec[DecoupledIO[ExuOutput]]]
    ): Unit = {
      getSinkVecN(this).zip(sourceVecN).foreach { case (sinkVec, sourcesVec) =>
        sinkVec.zip(sourcesVec).foreach { case (sink, source) =>
          sink.valid := source.valid
          sink.bits.pdest := source.bits.pdest
          sink.bits.data := source.bits.data
        }
      }
    }
  }
}

class BypassNetwork()(implicit p: Parameters, params: BackendParams) extends Module {
  val io: BypassNetworkIO = IO(new BypassNetworkIO)

  private val fromDPs: Seq[DecoupledIO[ExuInput]] = (io.fromDataPath.int ++ io.fromDataPath.vf ++ io.fromDataPath.mem).flatten.toSeq
  private val fromExus: Seq[ValidIO[ExuBypassBundle]] = (io.fromExus.int ++ io.fromExus.vf ++ io.fromExus.mem).flatten.toSeq
  private val toExus: Seq[DecoupledIO[ExuInput]] = (io.toExus.int ++ io.toExus.vf ++ io.toExus.mem).flatten.toSeq

  // (exuIdx, srcIdx, bypassExuIdx)
  private val forwardOrBypassValidVec3: MixedVec[Vec[Vec[Bool]]] = MixedVecInit(
    fromDPs.map(x => x.bits.l1ExuVec.getOrElse(
      VecInit(Seq.fill(x.bits.params.numRegSrc)(VecInit(Seq.fill(ExuVec.width)(false.B))))
    ))
  )

  private val forwardDataVec: Vec[UInt] = VecInit(
    fromExus.map(x => ZeroExt(x.bits.data, RegDataMaxWidth))
  )

  private val bypassDataVec = VecInit(
    fromExus.map(x => ZeroExt(RegEnable(x.bits.data, x.valid), RegDataMaxWidth))
  )

  toExus.zip(fromDPs).foreach { case (sink, source) =>
    sink <> source
  }

  toExus.zipWithIndex.foreach { case (exuInput, exuIdx) =>
    exuInput.bits.src.zipWithIndex.foreach { case (src, srcIdx) =>
      when (exuInput.bits.dataSources(srcIdx).readForward) {
        src := Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardDataVec)
      }.elsewhen (exuInput.bits.dataSources(srcIdx).readBypass) {
        src := Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassDataVec)
      }.otherwise {
        src := fromDPs(exuIdx).bits.src(srcIdx)
      }
    }
  }
}
