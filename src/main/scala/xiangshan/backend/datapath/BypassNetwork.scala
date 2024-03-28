package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.{XSBundle, XSModule}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuOH, ExuOutput, ImmInfo}
import xiangshan.backend.issue.{ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.datapath.DataConfig.RegDataMaxWidth
import xiangshan.backend.decode.ImmUnion

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
    val immInfo: Vec[ImmInfo] = Input(Vec(params.allExuParams.size, new ImmInfo))
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

class BypassNetwork()(implicit p: Parameters, params: BackendParams) extends XSModule {
  val io: BypassNetworkIO = IO(new BypassNetworkIO)

  private val fromDPs: Seq[DecoupledIO[ExuInput]] = (io.fromDataPath.int ++ io.fromDataPath.vf ++ io.fromDataPath.mem).flatten.toSeq
  private val fromExus: Seq[ValidIO[ExuBypassBundle]] = (io.fromExus.int ++ io.fromExus.vf ++ io.fromExus.mem).flatten.toSeq
  private val toExus: Seq[DecoupledIO[ExuInput]] = (io.toExus.int ++ io.toExus.vf ++ io.toExus.mem).flatten.toSeq
  private val immInfo = io.fromDataPath.immInfo

  // (exuIdx, srcIdx, bypassExuIdx)
  private val forwardOrBypassValidVec3: MixedVec[Vec[UInt]] = MixedVecInit(
    fromDPs.map { (x: DecoupledIO[ExuInput]) =>
      println(s"[tmp-BypassNetwork] ${x.bits.params.name} numRegSrc: ${x.bits.params.numRegSrc}")
      x.bits.l1ExuOH.getOrElse(
        // TODO: remove tmp max 1 for fake HYU1
        VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(0.U(ExuOH.width.W)))
      )
    }
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
      val imm = ImmExtractor(
        immInfo(exuIdx).imm,
        immInfo(exuIdx).immType,
        exuInput.bits.params.dataBitsMax,
        exuInput.bits.params.immType.map(_.litValue)
      )
      val immLoadSrc0 = SignExt(ImmUnion.U.toImm32(immInfo(exuIdx).imm(immInfo(exuIdx).imm.getWidth - 1, ImmUnion.I.len)), XLEN)
      val exuParm = exuInput.bits.params
      val isIntScheduler = exuParm.schdType.isInstanceOf[IntScheduler]
      val dataSource = exuInput.bits.dataSources(srcIdx)
      val isWakeUpSink = params.allIssueParams.filter(_.exuBlockParams.contains(exuParm)).head.exuBlockParams.map(_.isIQWakeUpSink).reduce(_ || _)
      val readForward = if (isWakeUpSink) dataSource.readForward else false.B
      val readBypass = if (isWakeUpSink) dataSource.readBypass else false.B
      val readZero = if (isIntScheduler) dataSource.readZero else false.B
      val readAnotherReg = if (isIntScheduler && exuParm.numRegSrc == 2 && srcIdx==1) dataSource.readAnotherReg else false.B
      val readRegOH = exuInput.bits.dataSources(srcIdx).readRegOH
      val readImm = if (exuParm.immType.nonEmpty || exuParm.hasLoadFu || exuParm.hasHyldaFu) exuInput.bits.dataSources(srcIdx).readImm else false.B
      src := Mux1H(
        Seq(
          readForward    -> Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardDataVec),
          readBypass     -> Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassDataVec),
          readZero       -> 0.U,
//          readAnotherReg -> fromDPs(exuIdx).bits.src(0),
          readRegOH      -> fromDPs(exuIdx).bits.src(srcIdx),
          readImm        -> (if (exuInput.bits.params.hasLoadExu && srcIdx == 0) immLoadSrc0 else imm)
        )
      )
    }
  }
}
