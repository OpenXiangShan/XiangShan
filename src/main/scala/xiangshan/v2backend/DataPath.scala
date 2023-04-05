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
  private val (fromMemIQ, toMemExu) = (io.fromMemIQ, io.toMemExu)
  private val (fromVfIQ, toVfExu) = (io.fromVfIQ, io.toFpExu)

  println(s"[DataPath] IntIQ(${fromIntIQ.size}), MemIQ(${fromMemIQ.size})")
  println(s"[DataPath] IntExu(${fromIntIQ.map(_.size).sum}), MemExu(${fromMemIQ.map(_.size).sum})")

  // just refences for convience
  private val fromIQ = fromIntIQ ++ fromMemIQ ++ fromVfIQ
  private val fromMemIntIQ = fromIntIQ ++ fromMemIQ
  private val fromMemVfIQ = fromVfIQ ++ fromMemIQ
  private val toExu = toIntExu ++ toMemExu ++ toVfExu
  private val toMemIntExu = toIntExu ++ toMemExu
  private val toMemVfExu = toVfExu ++ toMemExu

  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

  private val numIntRfReadByExu = intSchdParams.numIntRfReadByExu + memSchdParams.numIntRfReadByExu
  private val numVfRfReadByExu = vfSchdParams.numVfRfReadByExu + memSchdParams.numVfRfReadByExu
  // Todo: limit read port
  private val numIntR = numIntRfReadByExu
  private val numVfR = numVfRfReadByExu
  println(s"RegFile read req needed by Exu: Int(${numIntRfReadByExu}), Vf(${numVfRfReadByExu})")
  println(s"RegFile read port: Int(${numIntR}), Vf(${numVfR})")

  private val intRfRaddr = Wire(Vec(numIntR, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfRdata = Wire(Vec(numIntR, UInt(intSchdParams.rfDataWidth.W)))
  private val intRfWen = Wire(Vec(io.fromIntWb.length, Bool()))
  private val intRfWaddr = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfWdata = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.rfDataWidth.W)))

  private val vfRfSplitNum = VLEN / XLEN
  private val vfRfRaddr = Wire(Vec(numVfR, UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfRdata = Wire(Vec(numVfR, UInt(vfSchdParams.rfDataWidth.W)))
  private val vfRfWen = Wire(Vec(vfRfSplitNum, Vec(io.fromVfWb.length, Bool())))
  private val vfRfWaddr = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfWdata = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.rfDataWidth.W)))

  private val intDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W))))
    } else { None }
  private val vfDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32 + 32, UInt(vfSchdParams.pregIdxWidth.W))), Wire(Vec(32 + 32, UInt(VLEN.W))))
    } else { None }

  private val fpDebugReadData: Option[Vec[UInt]] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32, UInt(XLEN.W))))
    } else { None }
  private val vecDebugReadData: Option[Vec[UInt]] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(64, UInt(64.W)))) // v0 = Cat(Vec(1), Vec(0))
    } else { None }

  fpDebugReadData.foreach(_ := vfDebugRead
    .get._2
    .slice(0, 32)
    .map(_(63, 0))
  ) // fp only used [63, 0]
  vecDebugReadData.foreach(_ := vfDebugRead
    .get._2
    .slice(32, 64)
    .map(x => Seq(x(63, 0), x(127, 64))).flatten
  )

  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata,
    debugReadAddr = intDebugRead.map(_._1),
    debugReadData = intDebugRead.map(_._2))
  VfRegFile("VfRegFile", vfSchdParams.numPregs, vfRfSplitNum, vfRfRaddr, vfRfRdata, vfRfWen, vfRfWaddr, vfRfWdata,
    debugReadAddr = vfDebugRead.map(_._1),
    debugReadData = vfDebugRead.map(_._2))

  intRfWaddr := io.fromIntWb.map(_.addr)
  intRfWdata := io.fromIntWb.map(_.data)
  intRfWen := io.fromIntWb.map(_.wen)

  vfRfWaddr := io.fromVfWb.map(_.addr)
  vfRfWdata := io.fromVfWb.map(_.data)
  vfRfWen.foreach(_.zip(io.fromVfWb.map(_.wen)).foreach { case (wenSink, wenSource) => wenSink := wenSource } )// Todo: support fp multi-write

  // fromIQFire(i): flattened the i-th deq port fired
  private val fromIQFire: IndexedSeq[Bool] = fromIQ.flatten.map(_.fire)

  // intAddrFromIQ(i)(j): the j-th src addr of the i-th deq port
  private val intAddrFromIQ: IndexedSeq[Seq[UInt]] = fromIQ.flatten.map(_.bits.getIntRfReadBundle.map(_.addr))
  // hold read addr until new fromIQ.fire
  intAddrFromIQ.zip(fromIQFire).flatMap { case (addrVec: Seq[UInt], fire: Bool) =>
    addrVec.map(addr => (addr, fire))
  }.zip(intRfRaddr).foreach { case ((source, fire), sink) => sink := DataHoldBypass(source, fire)}

  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat
  }

  // vfAddrFromIQ(i)(j): the j-th src addr of the i-th deq port
  private val vfAddrFromIQ: IndexedSeq[Seq[UInt]] = fromIQ.flatten.map(_.bits.getFpRfReadBundle.map(_.addr))
  vfAddrFromIQ.zip(fromIQFire).flatMap { case (addrVec: Seq[UInt], fire: Bool) =>
    addrVec.map(addr => (addr, fire))
  }.zip(vfRfRaddr).foreach { case ((source, fire), sink) => sink := DataHoldBypass(source, fire) }

  vfDebugRead.foreach { case (addr, _) =>
    addr := io.debugFpRat ++ io.debugVecRat
  }
  println(s"[DataPath] " +
    s"has intDebugRead: ${intDebugRead.nonEmpty}, " +
    s"has vfDebugRead: ${vfDebugRead.nonEmpty}")

  val s1_toExuValid: MixedVec[MixedVec[Bool]] = Reg(MixedVec(
    toExu.map(x => MixedVec(x.map(_.valid.cloneType)))
  ))
  val s1_toExuData: MixedVec[MixedVec[ExuInput]] = Reg(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.cloneType)))))
  val s1_toExuReady = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.ready.cloneType))))) // Todo
  val s1_srcType: MixedVec[MixedVec[Vec[UInt]]] = MixedVecInit(fromIQ.map(x => MixedVecInit(x.map(xx => RegEnable(xx.bits.srcType, xx.fire)))))

  val s1_intPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType)))))
  val s1_vfPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType)))))

  println(s"[DataPath] s1_intPregRData.flatten.flatten.size: ${s1_intPregRData.flatten.flatten.size}, intRfRdata.size: ${intRfRdata.size}")
  println(s"[DataPath] s1_vfPregRData.flatten.flatten.size: ${s1_vfPregRData.flatten.flatten.size}, vfRfRdata.size: ${vfRfRdata.size}")

  var intRfRdataIdx = 0
  var vfRfRdataIdx = 0
  for (iqIdx <- toExu.indices) {
    for (exuIdx <- toExu(iqIdx).indices) {
      for (srcIdx <- toExu(iqIdx)(exuIdx).bits.src.indices) {
        val readDataCfgSet = toExu(iqIdx)(exuIdx).bits.params.getSrcDataType(srcIdx)
        // need read int reg
        if (readDataCfgSet.intersect(IntRegSrcDataSet).nonEmpty) {
          println(s"[DataPath] (iqIdx, exuIdx, srcIdx): ($iqIdx, $exuIdx, $srcIdx)")
          s1_intPregRData(iqIdx)(exuIdx)(srcIdx) := intRfRdata(intRfRdataIdx)
          intRfRdataIdx += 1
        } else {
          // better for debug, should never assigned to other bundles
          s1_intPregRData(iqIdx)(exuIdx)(srcIdx) := "hdead_beef_dead_beef".U
        }
        // need read vf reg
        if (readDataCfgSet.intersect(VfRegSrcDataSet).nonEmpty) {
          s1_vfPregRData(iqIdx)(exuIdx)(srcIdx) := vfRfRdata(vfRfRdataIdx)
          vfRfRdataIdx += 1
        } else {
          // better for debug, should never assigned to other bundles
          s1_vfPregRData(iqIdx)(exuIdx)(srcIdx) := "hdead_beef_dead_beef_dead_beef_dead_beef".U
        }
      }
    }
  }

  println(s"[DataPath] assigned RegFile Rdata: int(${intRfRdataIdx}), vf(${vfRfRdataIdx})")

  for (i <- fromIQ.indices) {
    for (j <- fromIQ(i).indices) {
      // IQ(s0) --[Ctrl]--> s1Reg ---------- begin
      // refs
      val s1_valid = s1_toExuValid(i)(j)
      val s1_ready = s1_toExuReady(i)(j)
      val s1_data = s1_toExuData(i)(j)
      val s0 = fromIQ(i)(j)
      val block = false.B // Todo: read arbiter
      // assign
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      when (s0.fire && !s1_flush && !block) {
        s1_valid := s0.valid
        s1_data.fromIssueBundle(s0.bits) // no src data here
      }.elsewhen (s1_ready) {
        s1_valid := false.B
      }

      s0.ready := (s1_ready || !s1_valid) && !block
      // IQ(s0) --[Ctrl]--> s1Reg ---------- end

      // IQ(s0) --[Data]--> s1Reg ---------- begin
      // imm extract
      when (s0.fire && !s1_flush && !block) {
        if (s1_data.params.immType.nonEmpty && s1_data.src.size > 1) {
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
      // IQ(s0) --[Data]--> s1Reg ---------- end
    }
  }

  for (i <- toExu.indices) {
    for (j <- toExu(i).indices) {
      // s1Reg --[Ctrl]--> exu(s1) ---------- begin
      // refs
      val sinkData = toExu(i)(j).bits
      // assign
      toExu(i)(j).valid := s1_toExuValid(i)(j)
      s1_toExuReady(i)(j) := toExu(i)(j).ready
      sinkData := s1_toExuData(i)(j)
      // s1Reg --[Ctrl]--> exu(s1) ---------- end

      // s1Reg --[Data]--> exu(s1) ---------- begin
      // data source1: preg read data
      for (k <- sinkData.src.indices) {
        println(s"[DataPath] (iqIdx, exuIdx, srcIdx): ($i, $j, $k)")
        val srcDataTypeSet: Set[DataConfig] = sinkData.params.getSrcDataType(k)

        val readRfMap: Seq[(Bool, UInt)] = (Seq(None) :+
          (if (s1_intPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(IntRegSrcDataSet).nonEmpty)
            Some(SrcType.isXp(s1_srcType(i)(j)(k)) -> s1_intPregRData(i)(j)(k))
          else None) :+
          (if (s1_vfPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(VfRegSrcDataSet).nonEmpty)
            Some(SrcType.isVfp(s1_srcType(i)(j)(k))-> s1_vfPregRData(i)(j)(k))
          else None)
        ).filter(_.nonEmpty).map(_.get)
        println(readRfMap)
        if (readRfMap.nonEmpty)
          sinkData.src(k) := Mux1H(readRfMap)
      }

      // data source2: extracted imm and pc saved in s1Reg
      if (sinkData.params.immType.nonEmpty && sinkData.src.size > 1) {
        when(SrcType.isImm(s1_srcType(i)(j)(1))) {
          sinkData.src(1) := s1_toExuData(i)(j).src(1)
        }
      }
      if (sinkData.params.hasJmpFu) {
        when(SrcType.isPc(s1_srcType(i)(j)(0))) {
          sinkData.src(0) := s1_toExuData(i)(j).src(0)
        }
      }
      // s1Reg --[Data]--> exu(s1) ---------- end
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
    difftestArchFpRegState.io.fpr := DelayN(fpDebugReadData.get, delayedCnt)

    val difftestArchVecRegState = Module(new DifftestArchVecRegState)
    difftestArchVecRegState.io.clock := clock
    difftestArchVecRegState.io.coreid := io.hartId
    difftestArchVecRegState.io.vpr := DelayN(vecDebugReadData.get, delayedCnt)
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

  val toMemExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle

  val fromIntWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genIntWriteBackBundle)

  val fromVfWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVfWriteBackBundle)

  val debugIntRat = Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))
  val debugFpRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))
  val debugVecRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))

  println(this.intSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.memSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
  println(this.vfSchdParams.issueBlockParams.map(_.exuBlockParams.map(_.getRfReadDataCfgSet)))
}
