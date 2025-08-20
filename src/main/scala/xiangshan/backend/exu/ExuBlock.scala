package xiangshan.backend.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.fu.{CSRFileIO, FenceIO}
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.{HasXSParameter, Redirect, Resolve, XSBundle}
import utility._
import xiangshan.backend.fu.FuConfig.{AluCfg, BrhCfg, FcmpCfg, I2fCfg}
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}

class ExuBlock(params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val exus: Seq[ExeUnit] = params.issueBlockParams.flatMap(_.exuBlockParams.map(x => LazyModule(x.genExuModule)))

  lazy val module = new ExuBlockImp(this)(p, params)
}

class ExuBlockImp(
  override val wrapper: ExuBlock
)(implicit
  p: Parameters,
  params: SchdBlockParams
) extends LazyModuleImp(wrapper) with HasCriticalErrors {
  val io = IO(new ExuBlockIO)

  private val exus = wrapper.exus.map(_.module)

  private val ins: collection.IndexedSeq[DecoupledIO[ExuInput]] = io.in.flatten
  private val outs: collection.IndexedSeq[DecoupledIO[ExuOutput]] = io.out.flatten

  (ins zip exus zip outs).foreach { case ((input, exu), output) =>
    exu.io.flush <> io.flush
    exu.io.csrio.foreach(exuio => io.csrio.get <> exuio)
    exu.io.csrin.foreach(exuio => io.csrin.get <> exuio)
    exu.io.I2FDataIn.foreach(exuio => io.I2FDataIn.get <> exuio)
    exu.io.F2IDataIn.foreach(exuio => io.F2IDataIn.get <> exuio)
    exu.io.fenceio.foreach(exuio => io.fenceio.get <> exuio)
    exu.io.frm.foreach(exuio => exuio := RegNext(io.frm.get))  // each vf exu pipe frm from csr
    exu.io.vxrm.foreach(exuio => io.vxrm.get <> exuio)
    exu.io.vlIsZero.foreach(exuio => io.vlIsZero.get := exuio)
    exu.io.vlIsVlmax.foreach(exuio => io.vlIsVlmax.get := exuio)
    exu.io.vtype.foreach(exuio => io.vtype.get := exuio)
    exu.io.in <> input
    output <> exu.io.out
    io.csrToDecode.foreach(toDecode => exu.io.csrToDecode.foreach(exuOut => toDecode := exuOut))
//    if (exu.wrapper.exuParams.fuConfigs.contains(AluCfg) || exu.wrapper.exuParams.fuConfigs.contains(BrhCfg)){
//      XSPerfAccumulate(s"${(exu.wrapper.exuParams.name)}_fire_cnt", PopCount(exu.io.in.fire))
//    }
    XSPerfAccumulate(s"${(exu.wrapper.exuParams.name)}_fire_cnt", PopCount(exu.io.in.fire))
  }
  if (params.isIntSchd) {
    val bjuExus = exus.filter(_.wrapper.exuParams.hasBrhFu).map(_.wrapper.module)
    assert(bjuExus.size == io.toFrontendBJUResolve.get.size, "bju num is different")
    val fromBJUResolve = bjuExus.map(_.io.toFrontendBJUResolve.get)
    io.toFrontendBJUResolve.get := fromBJUResolve
  }
  io.I2FWakeupOut.foreach{ x =>
    val exuI2FIn = exus.filter(x => x.wrapper.exuParams.fuConfigs.contains(I2fCfg)).head.io.in
    x := 0.U.asTypeOf(x)
    x.valid := exuI2FIn.valid && exuI2FIn.bits.fpWen.get
    x.bits.fpWen := exuI2FIn.bits.fpWen.get
    x.bits.pdest := exuI2FIn.bits.pdest
  }
  io.F2IWakeupOut.foreach { x =>
    val exuF2IIn = exus.filter(x => x.wrapper.exuParams.fuConfigs.contains(FcmpCfg)).head.io.in
    x := 0.U.asTypeOf(x)
    x.valid := exuF2IIn.valid && exuF2IIn.bits.rfWen.get
    x.bits.rfWen := exuF2IIn.bits.rfWen.get
    x.bits.pdest := exuF2IIn.bits.pdest
  }
  io.uncertainWakeupOut.foreach{ x =>
    x.zip(exus.filter(exu => exu.io.uncertainWakeupOut.nonEmpty).map(_.io.uncertainWakeupOut.get)).map{ case (sink, source) =>
      sink <> source
    }
  }
  val fpDataIdx = 2
  io.I2FDataOut.foreach { x =>
    val i2fFuOut = exus.filter(exu => exu.wrapper.exuParams.hasi2fFu).head.io.out
    x.valid := i2fFuOut.valid && i2fFuOut.bits.fpWen.get
    x.bits := i2fFuOut.bits.data(fpDataIdx)
  }
  val intDataIdx = 1
  io.F2IDataOut.foreach { x =>
    val f2iFuOut = exus.filter(exu => exu.wrapper.exuParams.hasf2iFu).head.io.out
    x.valid := f2iFuOut.valid && f2iFuOut.bits.intWen.get
    x.bits := f2iFuOut.bits.data(intDataIdx)
  }
  exus.find(_.io.csrio.nonEmpty).map(_.io.csrio.get).foreach { csrio =>
    exus.map(_.io.instrAddrTransType.foreach(_ := csrio.instrAddrTransType))
  }
  val aluFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(AluCfg)).map(_.io.in.fire)
  for (i <- 0 until (aluFireSeq.size + 1)){
    XSPerfAccumulate(s"alu_fire_${i}_cnt", PopCount(aluFireSeq) === i.U)
  }
  val brhFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(BrhCfg)).map(_.io.in.fire)
  for (i <- 0 until (brhFireSeq.size + 1)) {
    XSPerfAccumulate(s"brh_fire_${i}_cnt", PopCount(brhFireSeq) === i.U)
  }
  val criticalErrors = exus.filter(_.wrapper.exuParams.needCriticalErrors).flatMap(exu => exu.getCriticalErrors)
  generateCriticalErrors()
}

class ExuBlockIO(implicit p: Parameters, params: SchdBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // in(i)(j): issueblock(i), exu(j)
  val in: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(params.genExuInputCopySrcBundle)
  // out(i)(j): issueblock(i), exu(j).
  val out: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = params.genExuOutputDecoupledBundle
  val uncertainWakeupOut = Option.when(params.issueBlockParams.map(_.needUncertainWakeupFromExu).reduce(_ ||_))(params.genExuWakeUpOutValidBundle)
  val I2FWakeupOut = Option.when(params.isIntSchd)(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxI2F, params.backendParam)))
  val I2FDataOut = Option.when(params.isIntSchd)(ValidIO(UInt(XLEN.W)))
  val I2FDataIn= Option.when(params.isFpSchd)(Flipped(ValidIO(UInt(XLEN.W))))
  val F2IWakeupOut = Option.when(params.isFpSchd)(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxF2I, params.backendParam)))
  val F2IDataOut = Option.when(params.isFpSchd)(ValidIO(UInt(XLEN.W)))
  val F2IDataIn = Option.when(params.isIntSchd)(Flipped(ValidIO(UInt(XLEN.W))))
  val toFrontendBJUResolve = Option.when(params.isIntSchd)(Vec(backendParams.BrhCnt, Valid(new Resolve)))
  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))

  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVConfig)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVConfig)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVConfig)(Output(Bool()))
}
