package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState, DifftestArchVecRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan.v2backend.Bundles._
import xiangshan.v2backend.issue._
import xiangshan.{HasXSParameter, SrcType, XSBundle}

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: BackendParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntExu) = (io.fromIntIQ, io.toIntExu)
  private val (fromVfIQ, toVfExu) = (io.fromVfIQ, io.toFpExu)

  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  private val numIntR = intSchdParams.numRfRead + memSchdParams.numTotalIntRfRead
  println(s"number of total read req of int regfile: ${numIntR}")
//  private val numVfR = vfSchdParams.numRfRead + memSchdParams.numTotalVfRfRead

  private val intRfRaddr = Wire(Vec(numIntR, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfRdata = Wire(Vec(numIntR, UInt(intSchdParams.rfDataWidth.W)))
  private val intRfWen = Wire(Vec(io.fromIntWb.length, Bool()))
  private val intRfWaddr = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfWdata = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.rfDataWidth.W)))

//  private val vfRfSplitNum = VLEN / XLEN
//  private val vfRfRaddr = Wire(Vec(, UInt(vfSchdParams.pregIdxWidth.W)))
//  private val vfRfRdata = Wire(Vec(numIntR, UInt(vfSchdParams.rfDataWidth.W)))
//  private val vfRfWen = Wire(Vec(vfRfSplitNum, Vec(io.fromIntWb.length, Bool())))
//  private val vfRfWaddr = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.pregIdxWidth.W)))
//  private val vfRfWdata = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.rfDataWidth.W)))

  private val intDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W))))
    } else { None }
//  private val vfDebugRead: Option[(Vec[UInt], Vec[UInt])] =
//    if (env.AlwaysBasicDiff || env.EnableDifftest) {
//      Some(Wire(Vec(32, UInt(vfSchdParams.pregIdxWidth.W))), Wire(Vec(32 + 32, UInt(VLEN.W))))
//    } else { None }
  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata,
    debugReadAddr = intDebugRead.map(_._1), debugReadData = intDebugRead.map(_._2))
//  VfRegFile("VfRegFile", vfSchdParams.numPregs, vfRfSplitNum, vfRfRaddr, vfRfRdata, vfRfWen, vfRfWaddr, vfRfWdata,
//    debugReadAddr = vfDebugRead.map(_._1), debugReadData = vfDebugRead.map(_._2))

  intRfWaddr := io.fromIntWb.map(_.addr)
  intRfWdata := io.fromIntWb.map(_.data)
  intRfWen := io.fromIntWb.map(_.wen)

  io.connectWithIntRfRaddrVec(intRfRaddr)
  io.connectWithExuIntRdataVec(intRfRdata)

  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat
  }

//  vfDebugRead.foreach { case (addr, _) =>
//    addr := io.debugFpRat ++ io.debugVecRat
//  }
  println(s"intDebugRead: ${intDebugRead}")

  toIntExu.zip(fromIntIQ).foreach { case (sinkVec, sourceVec) =>
    sinkVec.zip(sourceVec).foreach { case (sink, source) =>
      sink.valid := source.valid // Todo: read arbiter
      sink.bits.fromIssueBundle(source.bits) // don't connect src data here
      source.ready := sink.ready
      // imm extract
      if (sink.bits.params.immType.nonEmpty) {
        // rs1 is always int reg, rs2 may be imm
        when (SrcType.isImm(source.bits.srcType(1))) {
          sink.bits.src(1) := ImmExtractor(
            source.bits.common.imm,
            source.bits.immType,
            sink.bits.DataBits,
            sink.bits.params.immType.map(_.litValue)
          )
        }
      }
      if (sink.bits.params.hasJmpFu) {
        when (SrcType.isPc(source.bits.srcType(0))) {
          sink.bits.src(0) := SignExt(source.bits.jmp.get.pc, XLEN)
        }
      }
    }
  }

  toVfExu.zip(fromVfIQ).foreach { case (sinkVec, sourceVec) =>
    sinkVec.zip(sourceVec).foreach { case (sink, source) =>
      sink.valid := source.valid // Todo: read arbiter
      sink.bits.fromIssueBundle(source.bits)
      source.ready := sink.ready
    }
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val delayedCnt = 2  // Todo: check it
    val difftestArchIntRegState = Module(new DifftestArchIntRegState)
    difftestArchIntRegState.io.clock := clock
    difftestArchIntRegState.io.coreid := io.hartId
    difftestArchIntRegState.io.gpr := DelayN(intDebugRead.get._2, delayedCnt)

    val difftestArchFpRegState = Module(new DifftestArchFpRegState)
    difftestArchFpRegState.io.clock := clock
    difftestArchFpRegState.io.coreid := io.hartId
    difftestArchFpRegState.io.fpr := 0.U.asTypeOf(difftestArchFpRegState.io.fpr) // DelayN(vfDebugRead.get._2, delayedCnt)

    val difftestArchVecRegState = Module(new DifftestArchVecRegState)
    difftestArchVecRegState.io.clock := clock
    difftestArchVecRegState.io.coreid := io.hartId
    difftestArchVecRegState.io.vpr := 0.U.asTypeOf(difftestArchVecRegState.io.vpr) // DelayN(vfDebugRead.get._2, delayedCnt)
  }
}

class DataPathIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  // bundles
  val hartId = Input(UInt(8.W))

  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromMemIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(memSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVfIQ = Flipped(MixedVec(vfSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val toIntExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle

  val toFpExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(vfSchdParams.genExuInputBundle)

  val fromIntWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genIntWriteBackBundle)

  val fromFpWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVfWriteBackBundle)

  val debugIntRat = Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))
  val debugFpRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))
  val debugVecRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))

  println(this.intSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.memSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.vfSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))

  def connectWithIntRfRaddrVec(rfRaddr: Vec[UInt]): Unit = {
    val addrFromIntIQ = fromIntIQ.flatMap(_.map(_.bits.getIntRfReadBundle)).flatten.map(_.addr)
    val addrFromMemIQ = fromMemIQ.flatMap(_.map(_.bits.getIntRfReadBundle)).flatten.map(_.addr)
    val addrFromIQ = WireInit(VecInit(addrFromIntIQ ++ addrFromMemIQ))
    println(s"number of read ports of int regfile: ${rfRaddr.length}")
    println(s"number of read request from iq: ${addrFromIQ.length}")
    println(s"number of read request from int iq: ${addrFromIntIQ.length}")
    println(s"number of read request from mem: ${addrFromMemIQ.length}")
    rfRaddr := addrFromIQ
  }

  def connectWithExuIntRdataVec(rfRdata: Vec[UInt]): Unit = {
    toIntExu.flatten.flatMap(_.bits.src).zip(rfRdata).foreach { case (exuSrc, rdata) =>
      exuSrc := rdata
    }
  }

  def connectWithVfRfRaddrVec(rfRaddr: Vec[UInt]): Unit = {
    val addrFromIntIQ = fromIntIQ.flatMap(_.map(_.bits.getFpRfReadBundle)).flatten.map(_.addr)
    val addrFromMemIQ = fromMemIQ.flatMap(_.map(_.bits.getFpRfReadBundle)).flatten.map(_.addr)
    val addrFromIQ = WireInit(VecInit(addrFromIntIQ ++ addrFromMemIQ))
    println(s"number of read ports of int regfile: ${rfRaddr.length}")
    println(s"number of read request from iq: ${addrFromIQ.length}")
    println(s"number of read request from int iq: ${addrFromIntIQ.length}")
    println(s"number of read request from mem: ${addrFromMemIQ.length}")
    rfRaddr := addrFromIQ
  }

  def connectWithExuVfRdataVec(rfRdata: Vec[UInt]): Unit = {
    toFpExu.flatten.flatMap(_.bits.src).zip(rfRdata).foreach { case (exuSrc, rdata) =>
      exuSrc := rdata
    }
  }
}
