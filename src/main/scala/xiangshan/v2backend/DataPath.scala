package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState, DifftestArchVecRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan.v2backend.Bundles._
import xiangshan.v2backend.issue._
import xiangshan.{HasXSParameter, Redirect, SrcType, XSBundle}

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: BackendParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntExu) = (io.fromIntIQ, io.toIntExu)
  private val (fromVfIQ, toVfExu) = (io.fromVfIQ, io.toFpExu)
  private val fromMemIQ = io.fromMemIQ

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

  private val addrFromIntIQ = fromIntIQ.map(_.map(_.bits.getIntRfReadBundle.map(_.addr)))
  private val addrFromMemIQ = fromMemIQ.map(_.map(_.bits.getIntRfReadBundle.map(_.addr)))
  private val addrFromIQ: IndexedSeq[IndexedSeq[Seq[UInt]]] = addrFromIntIQ ++ addrFromMemIQ
  private val fromIQFire: IndexedSeq[IndexedSeq[Bool]] = fromIntIQ.map(_.map(_.fire)) ++ fromMemIQ.map(_.map(_.fire))
  // hold read addr until new fromIQ.fire
  addrFromIQ.zip(fromIQFire).map { case (addrVec2, fireVec) =>
    addrVec2.zip(fireVec).map { case (addrVec, fire) =>
      addrVec.map(addr => (addr, fire))
    }
  }.flatten.flatten.zip(intRfRaddr).foreach { case ((source, fire), sink) => sink := DataHoldBypass(source, fire)}

  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat
  }

//  vfDebugRead.foreach { case (addr, _) =>
//    addr := io.debugFpRat ++ io.debugVecRat
//  }
  println(s"intDebugRead: ${intDebugRead}")

  val s1_toIntExuValid: MixedVec[MixedVec[Bool]] = Reg(MixedVec(toIntExu.map(x => MixedVec(x.map(_.valid.cloneType)))))
  val s1_toIntExuData: MixedVec[MixedVec[ExuInput]] = Reg(MixedVec(toIntExu.map(x => MixedVec(x.map(_.bits.cloneType)))))
  val s1_toIntExuReady = Wire(MixedVec(toIntExu.map(x => MixedVec(x.map(_.ready.cloneType)))))
  val s1_srcType: MixedVec[MixedVec[Vec[UInt]]] = MixedVecInit(fromIntIQ.map(x => MixedVecInit(x.map(xx => RegEnable(xx.bits.srcType, xx.fire)))))
  val s1_pregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toIntExu.map(x => MixedVec(x.map(_.bits.src.cloneType)))))
  s1_pregRData.flatten.flatten.zip(intRfRdata).foreach { case (sink, source) => sink := source }

  for (i <- fromIntIQ.indices) {
    for (j <- fromIntIQ(i).indices) {
      val s1_valid = s1_toIntExuValid(i)(j)
      val s1_ready = s1_toIntExuReady(i)(j)
      val s1_data = s1_toIntExuData(i)(j)
      val s0 = fromIntIQ(i)(j) // s0
      val block = false.B // Todo: read arbiter
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      when (s0.fire && !s1_flush && !block) {
        s1_valid := s0.valid
        s1_data.fromIssueBundle(s0.bits) // no src data here
      }.elsewhen (s1_ready) {
        s1_valid := false.B
      }

      s0.ready := (s1_ready || !s1_valid) && !block

      // imm extract
      when (s0.fire && !s1_flush && !block) {
        if (s1_data.params.immType.nonEmpty) {
          // rs1 is always int reg, rs2 may be imm
          when(SrcType.isImm(s0.bits.srcType(1))) {
            s1_data.src(1) := ImmExtractor(
              s0.bits.common.imm,
              s0.bits.immType,
              s1_data.DataBits,
              s1_data.params.immType.map(_.litValue)
            )
          }
        }
        if (s1_data.params.hasJmpFu) {
          when(SrcType.isPc(s0.bits.srcType(0))) {
            s1_data.src(0) := SignExt(s0.bits.jmp.get.pc, XLEN)
          }
        }
      }
    }
  }

//  // over write data
//  io.connectWithExuIntRdataVec(intRfRdata)

  Queue
  for (i <- toIntExu.indices) {
    for (j <- toIntExu(i).indices) {
      val sinkData = toIntExu(i)(j).bits
      toIntExu(i)(j).valid := s1_toIntExuValid(i)(j)
      s1_toIntExuReady(i)(j) := toIntExu(i)(j).ready // wire assign
      sinkData := s1_toIntExuData(i)(j)
      // preg read data
      sinkData.src := s1_pregRData(i)(j)

      // extracted imm and pc
      if (sinkData.params.immType.nonEmpty) {
        when(SrcType.isImm(s1_srcType(i)(j)(1))) {
          sinkData.src(1) := s1_toIntExuData(i)(j).src(1)
        }
      }
      if (sinkData.params.hasJmpFu) {
        when(SrcType.isPc(s1_srcType(i)(j)(0))) {
          sinkData.src(0) := s1_toIntExuData(i)(j).src(0)
        }
      }
    }
  }

  val s1_toVfExuValid: MixedVec[MixedVec[Bool]] = Reg(MixedVec(toVfExu.map(x => MixedVec(x.map(_.valid.cloneType)))))
  val s1_toVfExuData: MixedVec[MixedVec[ExuInput]] = Reg(MixedVec(toVfExu.map(x => MixedVec(x.map(_.bits.cloneType)))))
  val s1_toVfExuReady: MixedVec[MixedVec[Bool]] = Reg(MixedVec(toVfExu.map(x => MixedVec(x.map(_.ready.cloneType)))))

  for (i <- fromVfIQ.indices) {
    for (j <- fromVfIQ(i).indices) {
      val sinkValid = s1_toVfExuValid(i)(j)
      val sinkReady = s1_toVfExuReady(i)(j)
      val sinkData = s1_toVfExuData(i)(j)
      val source = fromVfIQ(i)(j)
      sinkValid := source.valid && source.bits.common.robIdx.needFlush(io.flush) // Todo: read arbiter
      sinkData.fromIssueBundle(source.bits)
      source.ready := sinkReady
    }
  }

  for (i <- toVfExu.indices) {
    for (j <- toVfExu(i).indices) {
      toVfExu(i)(j).valid := s1_toVfExuValid(i)(j)
      toVfExu(i)(j).bits := s1_toVfExuData(i)(j)
      s1_toVfExuReady(i)(j) := toVfExu(i)(j).ready
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

  val flush: ValidIO[Redirect] = Flipped(ValidIO(new Redirect))

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
