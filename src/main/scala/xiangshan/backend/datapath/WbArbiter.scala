package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DiffFpWriteback, DiffIntWriteback, DifftestModule}
import utils.XSError
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuOutput, WriteBackBundle}
import xiangshan.backend.datapath.DataConfig.{IntData, VecData}
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.{Redirect, XSBundle, XSModule}

class WbArbiterDispatcherIO[T <: Data](private val gen: T, n: Int) extends Bundle {
  val in = Flipped(DecoupledIO(gen))

  val out = Vec(n, DecoupledIO(gen))
}

class WbArbiterDispatcher[T <: Data](private val gen: T, n: Int, acceptCond: T => Seq[Bool])
                           (implicit p: Parameters)
  extends Module {

  val io = IO(new WbArbiterDispatcherIO(gen, n))

  private val acceptVec: Vec[Bool] = VecInit(acceptCond(io.in.bits))

  XSError(io.in.valid && PopCount(acceptVec) > 1.U, s"[ExeUnit] accept vec should no more than 1, ${Binary(acceptVec.asUInt)} ")

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.valid := acceptVec(i) && io.in.valid
    out.bits := io.in.bits
  }

  io.in.ready := Cat(io.out.zip(acceptVec).map{ case(out, canAccept) => out.ready && canAccept}).orR
}

class WbArbiterIO()(implicit p: Parameters, params: WbArbiterParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val in: MixedVec[DecoupledIO[WriteBackBundle]] = Flipped(params.genInput)
  val out: MixedVec[ValidIO[WriteBackBundle]] = params.genOutput

  def inGroup: Map[Int, Seq[DecoupledIO[WriteBackBundle]]] = in.groupBy(_.bits.params.port).map(x => (x._1, x._2.sortBy(_.bits.params.priority).toSeq))
}

class WbArbiter(params: WbArbiterParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new WbArbiterIO()(p, params))

  private val inGroup: Map[Int, Seq[DecoupledIO[WriteBackBundle]]] = io.inGroup

  private val arbiters: Seq[Option[Arbiter[WriteBackBundle]]] = Seq.tabulate(params.numOut) { x => {
    if (inGroup.contains(x)) {
      Some(Module(new Arbiter(new WriteBackBundle(inGroup.values.head.head.bits.params, backendParams), inGroup(x).length)))
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

  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }

  val fromIntExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.intSchdParams.get.genExuOutputDecoupledBundle)

  val fromVfExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.vfSchdParams.get.genExuOutputDecoupledBundle)

  val fromMemExu: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = Flipped(params.memSchdParams.get.genExuOutputDecoupledBundle)

  val toIntPreg = Flipped(MixedVec(Vec(params.numPregWb(IntData()),
    new RfWritePortWithConfig(params.intPregParams.dataCfg, params.intPregParams.addrWidth))))

  val toVfPreg = Flipped(MixedVec(Vec(params.numPregWb(VecData()),
    new RfWritePortWithConfig(params.vfPregParams.dataCfg, params.vfPregParams.addrWidth))))

  val toCtrlBlock = new Bundle {
    val writeback: MixedVec[ValidIO[ExuOutput]] = params.genWrite2CtrlBundles
  }
}

class WbDataPath(params: BackendParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new WbDataPathIO()(p, params))

  // alias
  val fromExu = (io.fromIntExu ++ io.fromVfExu ++ io.fromMemExu).flatten.toSeq
  val intArbiterInputsWire = WireInit(MixedVecInit(fromExu))
  val intArbiterInputsWireY = intArbiterInputsWire.filter(_.bits.params.writeIntRf)
  val intArbiterInputsWireN = intArbiterInputsWire.filterNot(_.bits.params.writeIntRf)
  val vfArbiterInputsWire = WireInit(MixedVecInit(fromExu))
  val vfArbiterInputsWireY = vfArbiterInputsWire.filter(_.bits.params.writeVfRf)
  val vfArbiterInputsWireN = vfArbiterInputsWire.filterNot(_.bits.params.writeVfRf)

  def acceptCond(exuOutput: ExuOutput): Seq[Bool] = {
    val intWen = if(exuOutput.intWen.isDefined) exuOutput.intWen.get else false.B
    val fpwen  = if(exuOutput.fpWen.isDefined) exuOutput.fpWen.get else false.B
    val vecWen = if(exuOutput.vecWen.isDefined) exuOutput.vecWen.get else false.B
    Seq(intWen, fpwen || vecWen)
  }

  fromExu.zip(intArbiterInputsWire.zip(vfArbiterInputsWire))map{
    case (exuOut, (intArbiterInput, vfArbiterInput)) =>
      val regfilesTypeNum = params.pregParams.size
      val in1ToN = Module(new WbArbiterDispatcher(new ExuOutput(exuOut.bits.params), regfilesTypeNum, acceptCond))
      in1ToN.io.in.valid := exuOut.valid
      in1ToN.io.in.bits := exuOut.bits
      exuOut.ready := in1ToN.io.in.ready
      in1ToN.io.out.zip(MixedVecInit(intArbiterInput, vfArbiterInput)).foreach { case (source, sink) =>
        sink.valid := source.valid
        sink.bits := source.bits
        source.ready := sink.ready
      }
  }
  intArbiterInputsWireN.foreach(_.ready := false.B)
  vfArbiterInputsWireN.foreach(_.ready := false.B)

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
  require(intWbArbiter.io.in.size == intArbiterInputsWireY.size, s"intWbArbiter input size: ${intWbArbiter.io.in.size}, all vf wb size: ${intArbiterInputsWireY.size}")
  intWbArbiter.io.in.zip(intArbiterInputsWireY).foreach { case (arbiterIn, in) =>
    arbiterIn.valid := in.valid && in.bits.intWen.get
    in.ready := arbiterIn.ready
    arbiterIn.bits.fromExuOutput(in.bits)
  }
  private val intWbArbiterOut = intWbArbiter.io.out

  vfWbArbiter.io.flush <> io.flush
  require(vfWbArbiter.io.in.size == vfArbiterInputsWireY.size, s"vfWbArbiter input size: ${vfWbArbiter.io.in.size}, all vf wb size: ${vfArbiterInputsWireY.size}")
  vfWbArbiter.io.in.zip(vfArbiterInputsWireY).foreach { case (arbiterIn, in) =>
    arbiterIn.valid := in.valid && (in.bits.fpWen.getOrElse(false.B) || in.bits.vecWen.getOrElse(false.B))
    in.ready := arbiterIn.ready
    arbiterIn.bits.fromExuOutput(in.bits)
  }

  private val vfWbArbiterOut = vfWbArbiter.io.out

  private val intExuInputs = io.fromIntExu.flatten.toSeq
  private val intExuWBs = WireInit(MixedVecInit(intExuInputs))
  private val vfExuInputs = io.fromVfExu.flatten.toSeq
  private val vfExuWBs = WireInit(MixedVecInit(vfExuInputs))
  private val memExuInputs = io.fromMemExu.flatten.toSeq
  private val memExuWBs = WireInit(MixedVecInit(memExuInputs))

  // only fired port can write back to ctrl block
  (intExuWBs zip intExuInputs).foreach { case (wb, input) => wb.valid := input.fire }
  (vfExuWBs zip vfExuInputs).foreach { case (wb, input) => wb.valid := input.fire }
  (memExuWBs zip memExuInputs).foreach { case (wb, input) => wb.valid := input.fire }

  // the ports not writting back pregs are always ready
  // the ports set highest priority are always ready
  (intExuInputs ++ vfExuInputs ++ memExuInputs).foreach( x =>
    if (x.bits.params.hasNoDataWB || x.bits.params.isHighestWBPriority) x.ready := true.B
  )

  // io assign
  private val toIntPreg: MixedVec[RfWritePortWithConfig] = MixedVecInit(intWbArbiterOut.map(x => x.bits.asIntRfWriteBundle(x.fire)).toSeq)
  private val toVfPreg: MixedVec[RfWritePortWithConfig] = MixedVecInit(vfWbArbiterOut.map(x => x.bits.asVfRfWriteBundle(x.fire)).toSeq)

  private val wb2Ctrl = intExuWBs ++ vfExuWBs ++ memExuWBs

  io.toIntPreg := toIntPreg
  io.toVfPreg := toVfPreg
  io.toCtrlBlock.writeback.zip(wb2Ctrl).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := true.B
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    intWbArbiterOut.foreach(out => {
      val difftest = DifftestModule(new DiffIntWriteback(IntPhyRegs))
      difftest.coreid := io.fromTop.hartId
      difftest.valid := out.fire && out.bits.rfWen
      difftest.address := out.bits.pdest
      difftest.data := out.bits.data
    })
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    vfWbArbiterOut.foreach(out => {
      val difftest = DifftestModule(new DiffFpWriteback(VfPhyRegs))
      difftest.coreid := io.fromTop.hartId
      difftest.valid := out.fire // all fp instr will write fp rf
      difftest.address := out.bits.pdest
      difftest.data := out.bits.data
    })
  }

}




