package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState, DifftestArchVecRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils.SeqUtils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.{ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.regfile._

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)

  println(s"[DataPath] Preg Params: ")
  println(s"[DataPath]   Int R(${params.getRfReadSize(IntData())}), W(${params.getRfWriteSize(IntData())}) ")
  println(s"[DataPath]   Vf R(${params.getRfReadSize(VecData())}), W(${params.getRfWriteSize(VecData())}) ")
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: BackendParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  private val VCONFIG_PORT = params.vconfigPort

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntIQ, toIntExu) = (io.fromIntIQ, io.toIntIQ, io.toIntExu)
  private val (fromMemIQ, toMemIQ, toMemExu) = (io.fromMemIQ, io.toMemIQ, io.toMemExu)
  private val (fromVfIQ , toVfIQ , toVfExu ) = (io.fromVfIQ , io.toVfIQ , io.toFpExu)
  private val (fromIntExus, fromVfExus) = (io.fromIntExus, io.fromVfExus)

  println(s"[DataPath] IntIQ(${fromIntIQ.size}), MemIQ(${fromMemIQ.size})")
  println(s"[DataPath] IntExu(${fromIntIQ.map(_.size).sum}), MemExu(${fromMemIQ.map(_.size).sum})")

  // just refences for convience
  private val fromIQ = fromIntIQ ++ fromVfIQ ++ fromMemIQ

  private val toIQs = toIntIQ ++ toVfIQ ++ toMemIQ

  private val toExu = toIntExu ++ toVfExu ++ toMemExu

  private val fromExus = fromIntExus ++ fromVfExus

  private val fromFlattenIQ: Seq[DecoupledIO[IssueQueueIssueBundle]] = fromIQ.flatten

  private val toFlattenExu: Seq[DecoupledIO[ExuInput]] = toExu.flatten

  private val intWbBusyArbiter = Module(new IntRFWBCollideChecker(backendParams))
  private val vfWbBusyArbiter = Module(new VfRFWBCollideChecker(backendParams))
  private val intRFReadArbiter = Module(new IntRFReadArbiter(backendParams))
  private val vfRFReadArbiter = Module(new VfRFReadArbiter(backendParams))

  private val og0FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool()))))
  private val og1FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool()))))

  // port -> win
  private val intRdArbWinner: Seq2[MixedVec[Bool]] = intRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready))))
  private val vfRdArbWinner: Seq2[MixedVec[Bool]] = vfRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready))))
  private val intWbNotBlock: Seq[MixedVec[Bool]] = intWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready)))
  private val vfWbNotBlock: Seq[MixedVec[Bool]] = vfWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready)))

  private val intRdNotBlock: Seq2[Bool] = intRdArbWinner.map(_.map(_.asUInt.andR))
  private val vfRdNotBlock: Seq2[Bool] = vfRdArbWinner.map(_.map(_.asUInt.andR))

  private val intRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getIntRfReadValidBundle(xx.valid)))

  intRFReadArbiter.io.in.zip(intRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(IntData())
      for (srcIdx <- 0 until fromIQ(iqIdx)(exuIdx).bits.exuParams.numRegSrc) {
        if (srcIndices.contains(srcIdx) && inRFReadReqSeq.isDefinedAt(srcIdx)) {
          arbInSeq(srcIdx).valid := inRFReadReqSeq(srcIdx).valid
          arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
        } else {
          arbInSeq(srcIdx).valid := false.B
          arbInSeq(srcIdx).bits.addr := 0.U
        }
      }
    }
  }

  private val vfRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getVfRfReadValidBundle(xx.valid)))

  vfRFReadArbiter.io.in.zip(vfRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = VfRegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
      for (srcIdx <- 0 until fromIQ(iqIdx)(exuIdx).bits.exuParams.numRegSrc) {
        if (srcIndices.contains(srcIdx) && inRFReadReqSeq.isDefinedAt(srcIdx)) {
          arbInSeq(srcIdx).valid := inRFReadReqSeq(srcIdx).valid
          arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
        } else {
          arbInSeq(srcIdx).valid := false.B
          arbInSeq(srcIdx).bits.addr := 0.U
        }
      }
    }
  }

  private val intRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.rfWen.getOrElse(false.B)))
  private val vfRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.getVfWen.getOrElse(false.B)))

  intWbBusyArbiter.io.in.zip(intRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  vfWbBusyArbiter.io.in.zip(vfRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

  private val numIntRfReadByExu = intSchdParams.numIntRfReadByExu + memSchdParams.numIntRfReadByExu
  private val numVfRfReadByExu = vfSchdParams.numVfRfReadByExu + memSchdParams.numVfRfReadByExu
  // Todo: limit read port
  private val numIntR = numIntRfReadByExu
  private val numVfR = numVfRfReadByExu
  println(s"[DataPath] RegFile read req needed by Exu: Int(${numIntRfReadByExu}), Vf(${numVfRfReadByExu})")
  println(s"[DataPath] RegFile read port: Int(${numIntR}), Vf(${numVfR})")

  private val schdParams = params.allSchdParams

  private val intRfRaddr = Wire(Vec(params.numPregRd(IntData()), UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfRdata = Wire(Vec(params.numPregRd(IntData()), UInt(intSchdParams.rfDataWidth.W)))
  private val intRfWen = Wire(Vec(io.fromIntWb.length, Bool()))
  private val intRfWaddr = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfWdata = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.rfDataWidth.W)))

  private val vfRfSplitNum = VLEN / XLEN
  private val vfRfRaddr = Wire(Vec(params.numPregRd(VecData()), UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfRdata = Wire(Vec(params.numPregRd(VecData()), UInt(vfSchdParams.rfDataWidth.W)))
  private val vfRfWen = Wire(Vec(vfRfSplitNum, Vec(io.fromVfWb.length, Bool())))
  private val vfRfWaddr = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfWdata = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.rfDataWidth.W)))

  private val intDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W))))
    } else { None }
  private val vfDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32 + 32 + 1, UInt(vfSchdParams.pregIdxWidth.W))), Wire(Vec(32 + 32 + 1, UInt(VLEN.W))))
    } else { None }

  private val fpDebugReadData: Option[Vec[UInt]] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(32, UInt(XLEN.W))))
    } else { None }
  private val vecDebugReadData: Option[Vec[UInt]] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(Vec(64, UInt(64.W)))) // v0 = Cat(Vec(1), Vec(0))
    } else { None }
  private val vconfigDebugReadData: Option[UInt] =
    if (env.AlwaysBasicDiff || env.EnableDifftest) {
      Some(Wire(UInt(64.W)))
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
  vconfigDebugReadData.foreach(_ := vfDebugRead
    .get._2(64)(63, 0)
  )

  io.debugVconfig := vconfigDebugReadData.get

  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata,
    debugReadAddr = intDebugRead.map(_._1),
    debugReadData = intDebugRead.map(_._2))
  VfRegFile("VfRegFile", vfSchdParams.numPregs, vfRfSplitNum, vfRfRaddr, vfRfRdata, vfRfWen, vfRfWaddr, vfRfWdata,
    debugReadAddr = vfDebugRead.map(_._1),
    debugReadData = vfDebugRead.map(_._2))

  intRfWaddr := io.fromIntWb.map(_.addr)
  intRfWdata := io.fromIntWb.map(_.data)
  intRfWen := io.fromIntWb.map(_.wen)

  for (portIdx <- intRfRaddr.indices) {
    if (intRFReadArbiter.io.out.isDefinedAt(portIdx))
      intRfRaddr(portIdx) := intRFReadArbiter.io.out(portIdx).bits.addr
    else
      intRfRaddr(portIdx) := 0.U
  }

  vfRfWaddr := io.fromVfWb.map(_.addr)
  vfRfWdata := io.fromVfWb.map(_.data)
  vfRfWen.foreach(_.zip(io.fromVfWb.map(_.wen)).foreach { case (wenSink, wenSource) => wenSink := wenSource } )// Todo: support fp multi-write

  for (portIdx <- vfRfRaddr.indices) {
    if (vfRFReadArbiter.io.out.isDefinedAt(portIdx))
      vfRfRaddr(portIdx) := vfRFReadArbiter.io.out(portIdx).bits.addr
    else
      vfRfRaddr(portIdx) := 0.U
  }

  vfRfRaddr(VCONFIG_PORT) := io.vconfigReadPort.addr
  io.vconfigReadPort.data := vfRfRdata(VCONFIG_PORT)

  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat
  }

  vfDebugRead.foreach { case (addr, _) =>
    addr := io.debugFpRat ++ io.debugVecRat :+ io.debugVconfigRat
  }
  println(s"[DataPath] " +
    s"has intDebugRead: ${intDebugRead.nonEmpty}, " +
    s"has vfDebugRead: ${vfDebugRead.nonEmpty}")

  val s1_addrOHs = Reg(MixedVec(
    fromIQ.map(x => MixedVec(x.map(_.bits.addrOH.cloneType)))
  ))
  val s1_toExuValid: MixedVec[MixedVec[Bool]] = Reg(MixedVec(
    toExu.map(x => MixedVec(x.map(_.valid.cloneType)))
  ))
  val s1_toExuData: MixedVec[MixedVec[ExuInput]] = Reg(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.cloneType)))))
  val s1_toExuReady = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.ready.cloneType))))) // Todo
  val s1_srcType: MixedVec[MixedVec[Vec[UInt]]] = MixedVecInit(fromIQ.map(x => MixedVecInit(x.map(xx => RegEnable(xx.bits.srcType, xx.fire)))))

  val s1_intPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType)))))
  val s1_vfPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType)))))

  val rfrPortConfigs = schdParams.map(_.issueBlockParams).flatten.map(_.exuBlockParams.map(_.rfrPortConfigs))

  println(s"[DataPath] s1_intPregRData.flatten.flatten.size: ${s1_intPregRData.flatten.flatten.size}, intRfRdata.size: ${intRfRdata.size}")
  s1_intPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_intPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        val realIuCfg = iuCfg.map(x => if(x.size > 1) x.filter(_.isInstanceOf[IntRD]) else x).flatten
        assert(iuRdata.size == realIuCfg.size, "iuRdata.size != realIuCfg.size")
        iuRdata.zip(realIuCfg)
          .filter { case (_, rfrPortConfig) => rfrPortConfig.isInstanceOf[IntRD] }
          .foreach { case (sink, cfg) => sink := intRfRdata(cfg.port) }
      }
  }

  println(s"[DataPath] s1_vfPregRData.flatten.flatten.size: ${s1_vfPregRData.flatten.flatten.size}, vfRfRdata.size: ${vfRfRdata.size}")
  s1_vfPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_vfPregRData.zip(rfrPortConfigs).foreach{ case(iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach{ case(iuRdata, iuCfg) =>
        val realIuCfg = iuCfg.map(x => if(x.size > 1) x.filter(_.isInstanceOf[VfRD]) else x).flatten
        assert(iuRdata.size == realIuCfg.size, "iuRdata.size != realIuCfg.size")
        iuRdata.zip(realIuCfg)
          .filter { case (_, rfrPortConfig) => rfrPortConfig.isInstanceOf[VfRD] }
          .foreach { case (sink, cfg) => sink := vfRfRdata(cfg.port) }
      }
  }

  for (i <- fromIQ.indices) {
    for (j <- fromIQ(i).indices) {
      // IQ(s0) --[Ctrl]--> s1Reg ---------- begin
      // refs
      val s1_valid = s1_toExuValid(i)(j)
      val s1_ready = s1_toExuReady(i)(j)
      val s1_data = s1_toExuData(i)(j)
      val s1_addrOH = s1_addrOHs(i)(j)
      val s0 = fromIQ(i)(j) // s0
      val notBlock = intRdNotBlock(i)(j) && intWbNotBlock(i)(j) && vfRdNotBlock(i)(j) && vfWbNotBlock(i)(j)
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      val s1_cancel = og1FailedVec2(i)(j)
      when (s0.fire && !s1_flush && notBlock && !s1_cancel) {
        s1_valid := s0.valid
        s1_data.fromIssueBundle(s0.bits) // no src data here
        s1_addrOH := s0.bits.addrOH
      }.otherwise {
        s1_valid := false.B
      }
      s0.ready := (s1_ready || !s1_valid) && notBlock
      // IQ(s0) --[Ctrl]--> s1Reg ---------- end

      // IQ(s0) --[Data]--> s1Reg ---------- begin
      // imm extract
      when (s0.fire && !s1_flush && notBlock) {
        if (s1_data.params.immType.nonEmpty && s1_data.src.size > 1) {
          // rs1 is always int reg, rs2 may be imm
          when(SrcType.isImm(s0.bits.srcType(1))) {
            s1_data.src(1) := ImmExtractor(
              s0.bits.common.imm,
              s0.bits.immType,
              s1_data.params.dataBitsMax,
              s1_data.params.immType.map(_.litValue)
            )
          }
        }
        if (s1_data.params.hasJmpFu) {
          when(SrcType.isPc(s0.bits.srcType(0))) {
            s1_data.src(0) := SignExt(s0.bits.jmp.get.pc, XLEN)
          }
        } else if (s1_data.params.hasVecFu) {
          // Fuck off riscv vector imm!!! Why not src1???
          when(SrcType.isImm(s0.bits.srcType(0))) {
            s1_data.src(0) := ImmExtractor(
              s0.bits.common.imm,
              s0.bits.immType,
              s1_data.params.dataBitsMax,
              s1_data.params.immType.map(_.litValue)
            )
          }
        }
      }
      // IQ(s0) --[Data]--> s1Reg ---------- end
    }
  }

  private val fromIQFire = fromIQ.map(_.map(_.fire))
  private val toExuFire = toExu.map(_.map(_.fire))
  toIQs.zipWithIndex.foreach {
    case(toIQ, iqIdx) =>
      toIQ.zipWithIndex.foreach {
        case (toIU, iuIdx) =>
          // IU: issue unit
          val og0resp = toIU.og0resp
          og0FailedVec2(iqIdx)(iuIdx) := fromIQ(iqIdx)(iuIdx).valid && (!fromIQFire(iqIdx)(iuIdx))
          og0resp.valid := og0FailedVec2(iqIdx)(iuIdx)
          og0resp.bits.respType := RSFeedbackType.rfArbitFail
          og0resp.bits.robIdx := fromIQ(iqIdx)(iuIdx).bits.common.robIdx
          og0resp.bits.rfWen := fromIQ(iqIdx)(iuIdx).bits.common.rfWen.getOrElse(false.B)
          og0resp.bits.fuType := fromIQ(iqIdx)(iuIdx).bits.common.fuType

          val og1resp = toIU.og1resp
          og1FailedVec2(iqIdx)(iuIdx) := s1_toExuValid(iqIdx)(iuIdx) && !toExuFire(iqIdx)(iuIdx)
          og1resp.valid := s1_toExuValid(iqIdx)(iuIdx)
          og1resp.bits.respType := Mux(!og1FailedVec2(iqIdx)(iuIdx),
            if (toIU.issueQueueParams.isMemAddrIQ) RSFeedbackType.fuUncertain else RSFeedbackType.fuIdle,
            RSFeedbackType.fuBusy)
          og1resp.bits.robIdx := s1_toExuData(iqIdx)(iuIdx).robIdx
          og1resp.bits.rfWen := s1_toExuData(iqIdx)(iuIdx).rfWen.getOrElse(false.B)
          og1resp.bits.fuType := s1_toExuData(iqIdx)(iuIdx).fuType
      }
  }

  io.og0CancelVec.zip(io.og1CancelVec).zipWithIndex.foreach { case ((og0Cancel, og1Cancel), i) =>
    og0Cancel := fromFlattenIQ(i).valid && !fromFlattenIQ(i).fire
    og1Cancel := toFlattenExu(i).valid && !toFlattenExu(i).fire
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
        val srcDataTypeSet: Set[DataConfig] = sinkData.params.getSrcDataType(k)

        val readRfMap: Seq[(Bool, UInt)] = (Seq(None) :+
          (if (s1_intPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(IntRegSrcDataSet).nonEmpty)
            Some(SrcType.isXp(s1_srcType(i)(j)(k)) -> s1_intPregRData(i)(j)(k))
          else None) :+
          (if (s1_vfPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(VfRegSrcDataSet).nonEmpty)
            Some(SrcType.isVfp(s1_srcType(i)(j)(k))-> s1_vfPregRData(i)(j)(k))
          else None)
        ).filter(_.nonEmpty).map(_.get)
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
      } else if (sinkData.params.hasVecFu) {
        when(SrcType.isImm(s1_srcType(i)(j)(0))) {
          sinkData.src(0) := s1_toExuData(i)(j).src(0)
        }
      }
      // s1Reg --[Data]--> exu(s1) ---------- end
    }
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val delayedCnt = 2
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
  private val exuParams = params.allExuParams
  // bundles
  val hartId = Input(UInt(8.W))

  val flush: ValidIO[Redirect] = Flipped(ValidIO(new Redirect))

  // Todo: check if this can be removed
  val vconfigReadPort = new RfReadPort(XLEN, PhyRegIdxWidth)

  val wbConfictRead = Input(MixedVec(params.allSchdParams.map(x => MixedVec(x.issueBlockParams.map(x => x.genWbConflictBundle())))))

  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromMemIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(memSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVfIQ = Flipped(MixedVec(vfSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val toIntIQ = MixedVec(intSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toMemIQ = MixedVec(memSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toVfIQ = MixedVec(vfSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val og0CancelVec = Output(ExuVec(backendParams.numExu))

  val og1CancelVec = Output(ExuVec(backendParams.numExu))

  val toIntExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle

  val toFpExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(vfSchdParams.genExuInputBundle)

  val toMemExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle

  val fromIntWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genIntWriteBackBundle)

  val fromVfWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVfWriteBackBundle)

  val fromIntExus = Flipped(intSchdParams.genExuOutputValidBundle)

  val fromVfExus = Flipped(intSchdParams.genExuOutputValidBundle)

  val debugIntRat = Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))
  val debugFpRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))
  val debugVecRat = Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))
  val debugVconfigRat = Input(UInt(vfSchdParams.pregIdxWidth.W))
  val debugVconfig = Output(UInt(XLEN.W))

}
