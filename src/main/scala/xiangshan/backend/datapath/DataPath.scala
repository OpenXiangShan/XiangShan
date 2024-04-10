package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DiffArchFpRegState, DiffArchIntRegState, DiffArchVecRegState, DifftestModule}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils.SeqUtils._
import utils.{XSPerfAccumulate, XSPerfHistogram}
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.{ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.regfile._
import xiangshan.backend.PcToDataPathIO

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)

  println(s"[DataPath] Preg Params: ")
  println(s"[DataPath]   Int R(${params.getRfReadSize(IntData())}), W(${params.getRfWriteSize(IntData())}) ")
  println(s"[DataPath]   Vf R(${params.getRfReadSize(VecData())}), W(${params.getRfWriteSize(VecData())}) ")
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: BackendParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntIQ, toIntExu) = (io.fromIntIQ, io.toIntIQ, io.toIntExu)
  private val (fromMemIQ, toMemIQ, toMemExu) = (io.fromMemIQ, io.toMemIQ, io.toMemExu)
  private val (fromVfIQ , toVfIQ , toVfExu ) = (io.fromVfIQ , io.toVfIQ , io.toFpExu)

  println(s"[DataPath] IntIQ(${fromIntIQ.size}), MemIQ(${fromMemIQ.size})")
  println(s"[DataPath] IntExu(${fromIntIQ.map(_.size).sum}), MemExu(${fromMemIQ.map(_.size).sum})")

  // just refences for convience
  private val fromIQ: Seq[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = (fromIntIQ ++ fromVfIQ ++ fromMemIQ).toSeq

  private val toIQs = toIntIQ ++ toVfIQ ++ toMemIQ

  private val toExu: Seq[MixedVec[DecoupledIO[ExuInput]]] = (toIntExu ++ toVfExu ++ toMemExu).toSeq

  private val fromFlattenIQ: Seq[DecoupledIO[IssueQueueIssueBundle]] = fromIQ.flatten

  private val toFlattenExu: Seq[DecoupledIO[ExuInput]] = toExu.flatten

  private val intWbBusyArbiter = Module(new IntRFWBCollideChecker(backendParams))
  private val vfWbBusyArbiter = Module(new VfRFWBCollideChecker(backendParams))
  private val intRFReadArbiter = Module(new IntRFReadArbiter(backendParams))
  private val vfRFReadArbiter = Module(new VfRFReadArbiter(backendParams))

  private val og0FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool())).toSeq))
  private val og1FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool())).toSeq))

  // port -> win
  private val intRdArbWinner: Seq2[MixedVec[Bool]] = intRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val vfRdArbWinner: Seq2[MixedVec[Bool]] = vfRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val intWbNotBlock: Seq[MixedVec[Bool]] = intWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val vfWbNotBlock: Seq[MixedVec[Bool]] = vfWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq

  private val intRdNotBlock: Seq2[Bool] = intRdArbWinner.map(_.map(_.asUInt.andR))
  private val vfRdNotBlock: Seq2[Bool] = vfRdArbWinner.map(_.map(_.asUInt.andR))

  private val intRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getIntRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val vfRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getVfRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val allDataSources: Seq[Seq[Vec[DataSource]]] = fromIQ.map(x => x.map(xx => xx.bits.common.dataSources).toSeq)
  private val allNumRegSrcs: Seq[Seq[Int]] = fromIQ.map(x => x.map(xx => xx.bits.exuParams.numRegSrc).toSeq)

  intRFReadArbiter.io.in.zip(intRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(IntData())
      for (srcIdx <- 0 until fromIQ(iqIdx)(exuIdx).bits.exuParams.numRegSrc) {
        if (srcIndices.contains(srcIdx) && inRFReadReqSeq.isDefinedAt(srcIdx)) {
          arbInSeq(srcIdx).valid := inRFReadReqSeq(srcIdx).valid && allDataSources(iqIdx)(exuIdx)(srcIdx).readReg
          arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
//          if (allNumRegSrcs(iqIdx)(exuIdx) == 2) {
//            val src0Req = inRFReadReqSeq(0).valid && allDataSources(iqIdx)(exuIdx)(0).readReg
//            val src1Req = inRFReadReqSeq(1).valid && allDataSources(iqIdx)(exuIdx)(1).readReg
//            if (srcIdx == 0) {
//              arbInSeq(srcIdx).valid := src0Req || src1Req
//              arbInSeq(srcIdx).bits.addr := Mux(src1Req && !src0Req, inRFReadReqSeq(1).bits.addr,inRFReadReqSeq(0).bits.addr)
//            } else {
//              arbInSeq(srcIdx).valid := src0Req && src1Req
//              arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
//            }
//          } else {
//            arbInSeq(srcIdx).valid := inRFReadReqSeq(srcIdx).valid && allDataSources(iqIdx)(exuIdx)(srcIdx).readReg
//            arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
//          }
        } else {
          arbInSeq(srcIdx).valid := false.B
          arbInSeq(srcIdx).bits.addr := 0.U
        }
      }
    }
  }

  vfRFReadArbiter.io.in.zip(vfRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = VfRegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
      for (srcIdx <- 0 until fromIQ(iqIdx)(exuIdx).bits.exuParams.numRegSrc) {
        if (srcIndices.contains(srcIdx) && inRFReadReqSeq.isDefinedAt(srcIdx)) {
          arbInSeq(srcIdx).valid := inRFReadReqSeq(srcIdx).valid && allDataSources(iqIdx)(exuIdx)(srcIdx).readReg
          arbInSeq(srcIdx).bits.addr := inRFReadReqSeq(srcIdx).bits.addr
        } else {
          arbInSeq(srcIdx).valid := false.B
          arbInSeq(srcIdx).bits.addr := 0.U
        }
      }
    }
  }

  private val intRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.rfWen.getOrElse(false.B)).toSeq).toSeq
  private val vfRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.getVfWen.getOrElse(false.B)).toSeq).toSeq

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

  private val pcReadValid = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathValid))
  private val pcReadFtqPtr = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathFtqPtr))
  private val pcReadFtqOffset = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathFtqOffset))
  private val targetPCRdata = io.fromPcTargetMem.toDataPathTargetPC
  private val pcRdata = io.fromPcTargetMem.toDataPathPC
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

  val pcReadFtqPtrFormIQ = fromIntIQ.flatten.filter(x => x.bits.exuParams.needPc)
  assert(pcReadFtqPtrFormIQ.size == pcReadFtqPtr.size, s"pcReadFtqPtrFormIQ.size ${pcReadFtqPtrFormIQ.size} not equal pcReadFtqPtr.size ${pcReadFtqPtr.size}")
  pcReadValid.zip(pcReadFtqPtrFormIQ.map(_.valid)).map(x => x._1 := x._2)
  pcReadFtqPtr.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqIdx.get)).map(x => x._1 := x._2)
  pcReadFtqOffset.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqOffset.get)).map(x => x._1 := x._2)
  io.fromPcTargetMem.fromDataPathValid := pcReadValid
  io.fromPcTargetMem.fromDataPathFtqPtr := pcReadFtqPtr
  io.fromPcTargetMem.fromDataPathFtqOffset := pcReadFtqOffset

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

  io.debugVconfig.foreach(_ := vconfigDebugReadData.get)

  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata,
    bankNum = 4,
    debugReadAddr = intDebugRead.map(_._1),
    debugReadData = intDebugRead.map(_._2))
  VfRegFile("VfRegFile", vfSchdParams.numPregs, vfRfSplitNum, vfRfRaddr, vfRfRdata, vfRfWen, vfRfWaddr, vfRfWdata,
    debugReadAddr = vfDebugRead.map(_._1),
    debugReadData = vfDebugRead.map(_._2))

  intRfWaddr := io.fromIntWb.map(_.addr).toSeq
  intRfWdata := io.fromIntWb.map(_.data).toSeq
  intRfWen := io.fromIntWb.map(_.wen).toSeq

  for (portIdx <- intRfRaddr.indices) {
    if (intRFReadArbiter.io.out.isDefinedAt(portIdx))
      intRfRaddr(portIdx) := intRFReadArbiter.io.out(portIdx).bits.addr
    else
      intRfRaddr(portIdx) := 0.U
  }

  vfRfWaddr := io.fromVfWb.map(x => RegEnable(x.addr, x.wen)).toSeq
  vfRfWdata := io.fromVfWb.map(x => RegEnable(x.data, x.wen)).toSeq
  vfRfWen.foreach(_.zip(io.fromVfWb.map(x => RegNext(x.wen))).foreach { case (wenSink, wenSource) => wenSink := wenSource } )// Todo: support fp multi-write

  for (portIdx <- vfRfRaddr.indices) {
    if (vfRFReadArbiter.io.out.isDefinedAt(portIdx))
      vfRfRaddr(portIdx) := vfRFReadArbiter.io.out(portIdx).bits.addr
    else
      vfRfRaddr(portIdx) := 0.U
  }


  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat.get
  }

  vfDebugRead.foreach { case (addr, _) =>
    addr := io.debugFpRat.get ++ io.debugVecRat.get :+ io.debugVconfigRat.get
  }
  println(s"[DataPath] " +
    s"has intDebugRead: ${intDebugRead.nonEmpty}, " +
    s"has vfDebugRead: ${vfDebugRead.nonEmpty}")

  val s1_addrOHs = Reg(MixedVec(
    fromIQ.map(x => MixedVec(x.map(_.bits.addrOH.cloneType).toSeq)).toSeq
  ))
  val s1_toExuValid: MixedVec[MixedVec[Bool]] = Reg(MixedVec(
    toExu.map(x => MixedVec(x.map(_.valid.cloneType).toSeq)).toSeq
  ))
  val s1_toExuData: MixedVec[MixedVec[ExuInput]] = Reg(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.cloneType).toSeq)).toSeq))
  val s1_immInfo = Reg(MixedVec(toExu.map(x => MixedVec(x.map(x => new ImmInfo).toSeq)).toSeq))
  s1_immInfo.zip(fromIQ).map { case (s1Vec, s0Vec) =>
    s1Vec.zip(s0Vec).map { case (s1, s0) =>
      s1.imm := Mux(s0.valid, s0.bits.common.imm, s1.imm)
      s1.immType := Mux(s0.valid, s0.bits.immType, s1.immType)
    }
  }
  io.og1ImmInfo.zip(s1_immInfo.flatten).map{ case(out, reg) =>
    out := reg
  }
  val s1_toExuReady = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.ready.cloneType).toSeq))))
  val s1_srcType: MixedVec[MixedVec[Vec[UInt]]] = MixedVecInit(fromIQ.map(x => MixedVecInit(x.map(xx => RegEnable(xx.bits.srcType, xx.fire)).toSeq)))

  val s1_intPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
  val s1_vfPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))

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

  val og0_cancel_no_load = og0FailedVec2.flatten.zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1)
  val og0_cancel_delay = RegNext(VecInit(og0_cancel_no_load.toSeq))
  for (i <- fromIQ.indices) {
    for (j <- fromIQ(i).indices) {
      // IQ(s0) --[Ctrl]--> s1Reg ---------- begin
      // refs
      val s1_valid = s1_toExuValid(i)(j)
      val s1_ready = s1_toExuReady(i)(j)
      val s1_data = s1_toExuData(i)(j)
      val s1_addrOH = s1_addrOHs(i)(j)
      val s0 = fromIQ(i)(j) // s0

      val srcNotBlock = Wire(Bool())
      srcNotBlock := s0.bits.common.dataSources.zip(intRdArbWinner(i)(j) zip vfRdArbWinner(i)(j)).map { case (source, win) =>
        !source.readReg || win._1 && win._2
      }.fold(true.B)(_ && _)
//      if (fromIQ(i)(j).bits.exuParams.schdType.isInstanceOf[IntScheduler] && (fromIQ(i)(j).bits.exuParams.numRegSrc == 2)) {
//        val src0VfBlock = s0.bits.common.dataSources(0).readReg && !vfRdArbWinner(i)(j)(0)
//        val src1VfBlock = s0.bits.common.dataSources(1).readReg && !vfRdArbWinner(i)(j)(1)
//        val src1IntBlock = s0.bits.common.dataSources(0).readReg && s0.bits.common.dataSources(1).readReg && !intRdArbWinner(i)(j)(1)
//        val src0IntBlock = (s0.bits.common.dataSources(0).readReg || s0.bits.common.dataSources(1).readReg) && !intRdArbWinner(i)(j)(0)
//        srcNotBlock := !src0VfBlock && !src1VfBlock && !src1IntBlock && !src0IntBlock
//      }
      val notBlock = srcNotBlock && intWbNotBlock(i)(j) && vfWbNotBlock(i)(j)
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      val s1_cancel = og1FailedVec2(i)(j)
      val s0_cancel = Wire(Bool())
      if (s0.bits.exuParams.isIQWakeUpSink) {
        val exuOHNoLoad = s0.bits.common.l1ExuOH.get.map(x => x.asTypeOf(Vec(x.getWidth, Bool())).zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1))
        s0_cancel := exuOHNoLoad.zip(s0.bits.common.dataSources).map{
          case (exuOH, dataSource) => (VecInit(exuOH).asUInt & og0_cancel_delay.asUInt).orR && dataSource.readForward
        }.reduce(_ || _) && s0.valid
      } else s0_cancel := false.B
      val s0_ldCancel = LoadShouldCancel(s0.bits.common.loadDependency, io.ldCancel)
      when (s0.fire && !s1_flush && notBlock && !s1_cancel && !s0_ldCancel && !s0_cancel) {
        s1_valid := s0.valid
        s1_data.fromIssueBundle(s0.bits) // no src data here
//        if (fromIQ(i)(j).bits.exuParams.schdType.isInstanceOf[IntScheduler] && (fromIQ(i)(j).bits.exuParams.numRegSrc == 2)) {
//          s1_data.dataSources(1).value := Mux(!s0.bits.common.dataSources(0).readReg && s0.bits.common.dataSources(1).readReg, DataSource.anotherReg, s0.bits.common.dataSources(1).value)
//        }
        s1_addrOH := s0.bits.addrOH
      }.otherwise {
        s1_valid := false.B
      }
      s0.ready := (s1_ready || !s1_valid) && notBlock && !s1_cancel && !s0_ldCancel && !s0_cancel
      // IQ(s0) --[Ctrl]--> s1Reg ---------- end
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
          og0resp.valid                 := og0FailedVec2(iqIdx)(iuIdx)
          og0resp.bits.robIdx           := fromIQ(iqIdx)(iuIdx).bits.common.robIdx
          og0resp.bits.uopIdx.foreach(_ := fromIQ(iqIdx)(iuIdx).bits.common.vpu.get.vuopIdx)
          og0resp.bits.resp             := RespType.block
          og0resp.bits.fuType           := fromIQ(iqIdx)(iuIdx).bits.common.fuType

          val og1resp = toIU.og1resp
          og1FailedVec2(iqIdx)(iuIdx)   := s1_toExuValid(iqIdx)(iuIdx) && !toExuFire(iqIdx)(iuIdx)
          og1resp.valid                 := s1_toExuValid(iqIdx)(iuIdx)
          og1resp.bits.robIdx           := s1_toExuData(iqIdx)(iuIdx).robIdx
          og1resp.bits.uopIdx.foreach(_ := s1_toExuData(iqIdx)(iuIdx).vpu.get.vuopIdx)
          // respType:  fuIdle      ->IQ entry clear
          //            fuUncertain ->IQ entry no action
          //            fuBusy      ->IQ entry issued set false, then re-issue
          // Only hyu, lda and sta are fuUncertain at OG1 stage
          og1resp.bits.resp             := Mux(!og1FailedVec2(iqIdx)(iuIdx),
            if (toIU.issueQueueParams match { case x => x.isMemAddrIQ && !x.isVecMemIQ || x.inVfSchd}) RespType.uncertain else RespType.success,
            RespType.block
          )
          og1resp.bits.fuType           := s1_toExuData(iqIdx)(iuIdx).fuType
      }
  }

  io.og0CancelOH := VecInit(fromFlattenIQ.map(x => x.valid && !x.fire)).asUInt
  io.og1CancelOH := VecInit(toFlattenExu.map(x => x.valid && !x.fire)).asUInt

  io.cancelToBusyTable.zipWithIndex.foreach { case (cancel, i) =>
    cancel.valid := fromFlattenIQ(i).valid && !fromFlattenIQ(i).fire
    cancel.bits.rfWen := fromFlattenIQ(i).bits.common.rfWen.getOrElse(false.B)
    cancel.bits.fpWen := fromFlattenIQ(i).bits.common.fpWen.getOrElse(false.B)
    cancel.bits.vecWen := fromFlattenIQ(i).bits.common.vecWen.getOrElse(false.B)
    cancel.bits.pdest := fromFlattenIQ(i).bits.common.pdest
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
      if (sinkData.params.hasJmpFu) {
        val index = pcReadFtqPtrFormIQ.map(_.bits.exuParams).indexOf(sinkData.params)
        sinkData.pc.get := pcRdata(index)
      }
      if (sinkData.params.needTarget) {
        val index = pcReadFtqPtrFormIQ.map(_.bits.exuParams).indexOf(sinkData.params)
        sinkData.predictInfo.get.target := targetPCRdata(index)
      }
    }
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val delayedCnt = 2
    val difftestArchIntRegState = DifftestModule(new DiffArchIntRegState, delay = delayedCnt)
    difftestArchIntRegState.coreid := io.hartId
    difftestArchIntRegState.value := intDebugRead.get._2

    val difftestArchFpRegState = DifftestModule(new DiffArchFpRegState, delay = delayedCnt)
    difftestArchFpRegState.coreid := io.hartId
    difftestArchFpRegState.value := fpDebugReadData.get

    val difftestArchVecRegState = DifftestModule(new DiffArchVecRegState, delay = delayedCnt)
    difftestArchVecRegState.coreid := io.hartId
    difftestArchVecRegState.value := vecDebugReadData.get
  }

  val int_regcache_size = 48
  val int_regcache_tag = RegInit(VecInit(Seq.fill(int_regcache_size)(0.U(intSchdParams.pregIdxWidth.W))))
  val int_regcache_enqPtr = RegInit(0.U(log2Up(int_regcache_size).W))
  int_regcache_enqPtr := int_regcache_enqPtr + PopCount(intRfWen)
  for (i <- intRfWen.indices) {
    when (intRfWen(i)) {
      int_regcache_tag(int_regcache_enqPtr + PopCount(intRfWen.take(i))) := intRfWaddr(i)
    }
  }

  val vf_regcache_size = 48
  val vf_regcache_tag = RegInit(VecInit(Seq.fill(vf_regcache_size)(0.U(vfSchdParams.pregIdxWidth.W))))
  val vf_regcache_enqPtr = RegInit(0.U(log2Up(vf_regcache_size).W))
  vf_regcache_enqPtr := vf_regcache_enqPtr + PopCount(vfRfWen.head)
  for (i <- vfRfWen.indices) {
    when (vfRfWen.head(i)) {
      vf_regcache_tag(vf_regcache_enqPtr + PopCount(vfRfWen.head.take(i))) := vfRfWaddr(i)
    }
  }

  XSPerfHistogram(s"IntRegFileRead_hist", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"VfRegFileRead_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"IntRegFileWrite_hist", PopCount(intRFWriteReq.flatten), true.B, 0, 20, 1)
  XSPerfHistogram(s"VfRegFileWrite_hist", PopCount(vfRFWriteReq.flatten), true.B, 0, 20, 1)

  val int_regcache_part32 = (1 until 33).map(i => int_regcache_tag(int_regcache_enqPtr - i.U))
  val int_regcache_part24 = (1 until 24).map(i => int_regcache_tag(int_regcache_enqPtr - i.U))
  val int_regcache_part16 = (1 until 17).map(i => int_regcache_tag(int_regcache_enqPtr - i.U))
  val int_regcache_part8 = (1 until 9).map(i => int_regcache_tag(int_regcache_enqPtr - i.U))

  val int_regcache_48_hit_vec = intRFReadArbiter.io.in.flatten.flatten.map(x => x.valid && int_regcache_tag.map(_ === x.bits.addr).reduce(_ || _))
  val int_regcache_8_hit_vec = intRFReadArbiter.io.in.flatten.flatten.map(x => x.valid && int_regcache_part8.map(_ === x.bits.addr).reduce(_ || _))
  val int_regcache_16_hit_vec = intRFReadArbiter.io.in.flatten.flatten.map(x => x.valid && int_regcache_part16.map(_ === x.bits.addr).reduce(_ || _))
  val int_regcache_24_hit_vec = intRFReadArbiter.io.in.flatten.flatten.map(x => x.valid && int_regcache_part24.map(_ === x.bits.addr).reduce(_ || _))
  val int_regcache_32_hit_vec = intRFReadArbiter.io.in.flatten.flatten.map(x => x.valid && int_regcache_part32.map(_ === x.bits.addr).reduce(_ || _))
  XSPerfAccumulate("IntRegCache48Hit", PopCount(int_regcache_48_hit_vec))
  XSPerfAccumulate("IntRegCache8Hit", PopCount(int_regcache_8_hit_vec))
  XSPerfAccumulate("IntRegCache16Hit", PopCount(int_regcache_16_hit_vec))
  XSPerfAccumulate("IntRegCache24Hit", PopCount(int_regcache_24_hit_vec))
  XSPerfAccumulate("IntRegCache32Hit", PopCount(int_regcache_32_hit_vec))
  XSPerfHistogram("IntRegCache48Hit_hist", PopCount(int_regcache_48_hit_vec), true.B, 0, 16, 2)

  XSPerfAccumulate(s"IntRFReadBeforeArb", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"IntRFReadAfterArb", PopCount(intRFReadArbiter.io.out.map(_.valid)))
  XSPerfAccumulate(s"VfRFReadBeforeArb", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"VfRFReadAfterArb", PopCount(vfRFReadArbiter.io.out.map(_.valid)))
  XSPerfAccumulate(s"IntUopBeforeArb", PopCount(fromIntIQ.flatten.map(_.valid)))
  XSPerfAccumulate(s"IntUopAfterArb", PopCount(fromIntIQ.flatten.map(_.fire)))
  XSPerfAccumulate(s"MemUopBeforeArb", PopCount(fromMemIQ.flatten.map(_.valid)))
  XSPerfAccumulate(s"MemUopAfterArb", PopCount(fromMemIQ.flatten.map(_.fire)))
  XSPerfAccumulate(s"VfUopBeforeArb", PopCount(fromVfIQ.flatten.map(_.valid)))
  XSPerfAccumulate(s"VfUopAfterArb", PopCount(fromVfIQ.flatten.map(_.fire)))

  XSPerfHistogram(s"IntRFReadBeforeArb_hist", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"IntRFReadAfterArb_hist", PopCount(intRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadBeforeArb_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadAfterArb_hist", PopCount(vfRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"IntUopBeforeArb_hist", PopCount(fromIntIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"IntUopAfterArb_hist", PopCount(fromIntIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
  XSPerfHistogram(s"MemUopBeforeArb_hist", PopCount(fromMemIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"MemUopAfterArb_hist", PopCount(fromMemIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
  XSPerfHistogram(s"VfUopBeforeArb_hist", PopCount(fromVfIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"VfUopAfterArb_hist", PopCount(fromVfIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
}

class DataPathIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  // bundles
  val hartId = Input(UInt(8.W))

  val flush: ValidIO[Redirect] = Flipped(ValidIO(new Redirect))

  val wbConfictRead = Input(MixedVec(params.allSchdParams.map(x => MixedVec(x.issueBlockParams.map(x => x.genWbConflictBundle())))))

  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromMemIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(memSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVfIQ = Flipped(MixedVec(vfSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val toIntIQ = MixedVec(intSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toMemIQ = MixedVec(memSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toVfIQ = MixedVec(vfSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val og0CancelOH = Output(ExuOH(backendParams.numExu))

  val og1CancelOH = Output(ExuOH(backendParams.numExu))

  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val cancelToBusyTable = Vec(backendParams.numExu, ValidIO(new CancelSignal))

  val toIntExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle

  val toFpExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(vfSchdParams.genExuInputBundle)

  val toMemExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle

  val og1ImmInfo: Vec[ImmInfo] = Output(Vec(params.allExuParams.size, new ImmInfo))

  val fromIntWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genIntWriteBackBundle)

  val fromVfWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVfWriteBackBundle)

  val fromPcTargetMem = Flipped(new PcToDataPathIO(params))

  val debugIntRat     = if (params.debugEn) Some(Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))) else None
  val debugFpRat      = if (params.debugEn) Some(Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))) else None
  val debugVecRat     = if (params.debugEn) Some(Input(Vec(32, UInt(vfSchdParams.pregIdxWidth.W)))) else None
  val debugVconfigRat = if (params.debugEn) Some(Input(UInt(vfSchdParams.pregIdxWidth.W))) else None
  val debugVconfig    = if (params.debugEn) Some(Output(UInt(XLEN.W))) else None
}
