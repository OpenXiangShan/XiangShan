package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.v2backend.Bundles.{ExuOutput, WriteBackBundle}
import xiangshan.{Redirect, XSBundle, XSModule}

class WbArbiterIO()(implicit p: Parameters, params: WbArbiterParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val in: MixedVec[DecoupledIO[WriteBackBundle]] = Flipped(params.genInput)
  val out: MixedVec[ValidIO[WriteBackBundle]] = params.genOutput

  def inGroup: Map[Int, IndexedSeq[DecoupledIO[WriteBackBundle]]] = in.groupBy(_.bits.params.port)
}

class WbArbiter(params: WbArbiterParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new WbArbiterIO()(p, params))

  private val inGroup: Map[Int, IndexedSeq[DecoupledIO[WriteBackBundle]]] = io.inGroup

  private val arbiters: Seq[Option[Arbiter[WriteBackBundle]]] = Seq.tabulate(params.numOut) { x => {
    if (inGroup.contains(x)) {
      Some(Module(new Arbiter(new WriteBackBundle(inGroup.values.head.head.bits.params), inGroup(x).length)))
    } else {
      None
    }
  }}

  arbiters.zipWithIndex.foreach { case (arb, i) =>
    if (arb.nonEmpty) {
      arb.get.io.in.zip(inGroup(i)).foreach { case (arbIn, wbIn) =>
        arbIn <> wbIn
      }
    }
  }

  io.out.zip(arbiters).foreach { case (wbOut, arb) =>
    if (arb.nonEmpty) {
      val arbOut = arb.get.io.out
      arbOut.ready := true.B
      wbOut.valid := arbOut.valid
      wbOut.bits := arbOut.bits
    } else {
      wbOut := 0.U.asTypeOf(wbOut)
    }
  }

  def getInOutMap: Map[Int, Int] = {
    (params.wbCfgs.indices zip params.wbCfgs.map(_.port)).toMap
  }
}

class WbDataPathIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))

  val fromIntExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.intSchdParams.get.genExuOutputDecoupledBundle)

  val fromVfExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.vfSchdParams.get.genExuOutputDecoupledBundle)

  val fromMemExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.memSchdParams.get.genExuOutputDecoupledBundle)

  val toIntPreg = Flipped(MixedVec(Vec(params.intPregParams.numWrite,
    new RfWritePortWithConfig(params.intPregParams.dataCfg, params.intPregParams.addrWidth))))

  val toVfPreg = Flipped(MixedVec(Vec(params.vfPregParams.numWrite,
    new RfWritePortWithConfig(params.vfPregParams.dataCfg, params.vfPregParams.addrWidth))))

  val toCtrlBlock = new Bundle {
    val writeback: MixedVec[ValidIO[ExuOutput]] = params.genWrite2CtrlBundles
  }
}

class WbDataPath(params: BackendParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new WbDataPathIO()(p, params))

  // alias
  val intArbiterInputs = (io.fromIntExu ++ io.fromVfExu ++ io.fromMemExu).flatten.filter(_.bits.params.writeIntRf)
  val vfArbiterInputs = (io.fromIntExu ++ io.fromVfExu ++ io.fromMemExu).flatten.filter(_.bits.params.writeVfRf)
  println(s"[WbDataPath] write int preg: " +
    s"IntExu(${io.fromIntExu.flatten.count(_.bits.params.writeIntRf)}) " +
    s"VfExu(${io.fromVfExu.flatten.count(_.bits.params.writeIntRf)}) " +
    s"MemExu(${io.fromMemExu.flatten.count(_.bits.params.writeIntRf)})"
  )
  println(s"[WbDataPath] write vf preg: " +
    s"IntExu(${io.fromIntExu.flatten.count(_.bits.params.writeVfRf)}) " +
    s"VfExu(${io.fromVfExu.flatten.count(_.bits.params.writeVfRf)}) " +
    s"MemExu(${io.fromMemExu.flatten.count(_.bits.params.writeVfRf)})"
  )

  // modules
  private val intWbArbiter = Module(new WbArbiter(params.getIntWbArbiterParams))
  private val vfWbArbiter = Module(new WbArbiter(params.getVfWbArbiterParams))
  println(s"[WbDataPath] int preg write back port num: ${intWbArbiter.io.out.size}, active port: ${intWbArbiter.io.inGroup.keys.toSeq.sorted}")
  println(s"[WbDataPath] vf preg write back port num: ${vfWbArbiter.io.out.size}, active port: ${vfWbArbiter.io.inGroup.keys.toSeq.sorted}")

  // module assign
  intWbArbiter.io.flush <> io.flush
  require(intWbArbiter.io.in.size == intArbiterInputs.size, s"intWbArbiter input size: ${intWbArbiter.io.in.size}, all vf wb size: ${intArbiterInputs.size}")
  intWbArbiter.io.in.zip(intArbiterInputs).foreach { case (arbiterIn, in) =>
    arbiterIn.valid := in.valid
    in.ready := arbiterIn.ready
    arbiterIn.bits.fromExuOutput(in.bits)
  }
  private val intWbArbiterOut = intWbArbiter.io.out

  vfWbArbiter.io.flush <> io.flush
  require(vfWbArbiter.io.in.size == vfArbiterInputs.size, s"vfWbArbiter input size: ${vfWbArbiter.io.in.size}, all vf wb size: ${vfArbiterInputs.size}")
  vfWbArbiter.io.in.zip(vfArbiterInputs).foreach { case (arbiterIn, in) =>
    arbiterIn.valid := in.valid
    in.ready := arbiterIn.ready
    arbiterIn.bits.fromExuOutput(in.bits)
  }

  private val vfWbArbiterOut = vfWbArbiter.io.out

  private val intExuInputs = io.fromIntExu.flatten
  private val intExuWBs = WireInit(MixedVecInit(io.fromIntExu.flatten))
  private val vfExuInputs = io.fromVfExu.flatten
  private val vfExuWBs = WireInit(MixedVecInit(io.fromVfExu.flatten))
  private val memExuInputs = io.fromMemExu.flatten
  private val memExuWBs = WireInit(MixedVecInit(io.fromMemExu.flatten))

  // only fired port can write back to ctrl block
  (intExuWBs zip intExuInputs).foreach { case (wb, input) => wb.valid := input.fire }
  (vfExuWBs zip vfExuInputs).foreach { case (wb, input) => wb.valid := input.fire }
  (memExuWBs zip memExuInputs).foreach { case (wb, input) => wb.valid := input.fire }

  // the ports not writting back pregs are always ready
  (intExuInputs ++ vfExuInputs ++ memExuInputs).foreach( x =>
    if (x.bits.params.hasNoDataWB) x.ready := true.B
  )

  // io assign
  private val toIntPreg: MixedVec[RfWritePortWithConfig] = MixedVecInit(intWbArbiterOut.map(x => x.bits.asIntRfWriteBundle(x.fire)))
  private val toVfPreg: MixedVec[RfWritePortWithConfig] = MixedVecInit(vfWbArbiterOut.map(x => x.bits.asVfRfWriteBundle(x.fire)))

  private val wb2Ctrl = intExuWBs ++ vfExuWBs ++ memExuWBs

  io.toIntPreg := toIntPreg
  io.toVfPreg := toVfPreg
  io.toCtrlBlock.writeback.zip(wb2Ctrl).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := true.B
  }
}




