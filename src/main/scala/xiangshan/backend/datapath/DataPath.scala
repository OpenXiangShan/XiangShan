package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DiffArchFpRegState, DiffArchIntRegState, DiffArchVecRegState, DifftestModule}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils.SeqUtils._
import utils._
import xiangshan._
import xiangshan.backend.{BackendParams, ExcpModToVprf, PcToDataPathIO, VprfToExcpMod}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.{FpScheduler, ImmExtractor, IntScheduler, SchdBlockParams, VecScheduler}
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.regfile._
import xiangshan.backend.regcache._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.FuType.is0latency
import xiangshan.backend.fu.FuType.isUncertain
import xiangshan.mem.{LqPtr, SqPtr}

class DataPath(implicit p: Parameters, params: BackendParams, param: SchdBlockParams)
  extends XSModule with HasXSParameter {

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntIQ, toIntExu) = (io.fromIntIQ, io.toIntIQ, io.toIntExu)
  private val (fromFpIQ,  toFpIQ,  toFpExu)  = (io.fromFpIQ,  io.toFpIQ,  io.toFpExu)
  private val (fromVfIQ,  toVfIQ,  toVfExu ) = (io.fromVfIQ,  io.toVfIQ,  io.toVecExu)

  println(s"[${param.getName}DataPath] IntIQ(${fromIntIQ.size}), FpIQ(${fromFpIQ.size}), VecIQ(${fromVfIQ.size})")
  println(s"[${param.getName}DataPath] IntExu(${fromIntIQ.map(_.size).sum}), FpExu(${fromFpIQ.map(_.size).sum}), VecExu(${fromVfIQ.map(_.size).sum})")

  // just refences for convience
  private val fromIQ: Seq[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = (fromIntIQ ++ fromFpIQ ++ fromVfIQ).toSeq

  private val toIQs = toIntIQ ++ toFpIQ ++ toVfIQ

  private val toExu: Seq[MixedVec[DecoupledIO[ExuInput]]] = (toIntExu ++ toFpExu ++ toVfExu).toSeq

  private val fromFlattenIQ: Seq[DecoupledIO[IssueQueueIssueBundle]] = fromIQ.flatten

  private val toFlattenExu: Seq[DecoupledIO[ExuInput]] = toExu.flatten

  private val intWbBusyArbiter = Module(new IntRFWBCollideChecker(backendParams))
  private val fpWbBusyArbiter = Module(new FpRFWBCollideChecker(backendParams))
  private val vfWbBusyArbiter = Module(new VfRFWBCollideChecker(backendParams))
  private val v0WbBusyArbiter = Module(new V0RFWBCollideChecker(backendParams))
  private val vlWbBusyArbiter = Module(new VlRFWBCollideChecker(backendParams))

  private val intRFReadArbiter = Module(new IntRFReadArbiter(backendParams))
  private val fpRFReadArbiter = Module(new FpRFReadArbiter(backendParams))
  private val vfRFReadArbiter = Module(new VfRFReadArbiter(backendParams))
  private val v0RFReadArbiter = Module(new V0RFReadArbiter(backendParams))
  private val vlRFReadArbiter = Module(new VlRFReadArbiter(backendParams))

  private val og0FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool())).toSeq))
  private val og1FailedVec2: MixedVec[Vec[Bool]] = Wire(MixedVec(fromIQ.map(x => Vec(x.size, Bool())).toSeq))

  // port -> win
  private val intRdArbWinner: Seq2[MixedVec[Bool]] = intRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val fpRdArbWinner: Seq2[MixedVec[Bool]] = fpRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val vfRdArbWinner: Seq2[MixedVec[Bool]] = vfRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val v0RdArbWinner: Seq2[MixedVec[Bool]] = v0RFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq
  private val vlRdArbWinner: Seq2[Option[Bool]] = vlRFReadArbiter.io.in.map(_.map(x => x.headOption.map(_.ready)))

  private val intWbNotBlock: Seq[MixedVec[Bool]] = intWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val fpWbNotBlock: Seq[MixedVec[Bool]] = fpWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val vfWbNotBlock: Seq[MixedVec[Bool]] = vfWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val v0WbNotBlock: Seq[MixedVec[Bool]] = v0WbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val vlWbNotBlock: Seq[MixedVec[Bool]] = vlWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq

  // rdSrcsNotBlock(i)(j)(k): IQ(i).exu(j).src(k) is not blocked
  private val rdSrcsNotBlock: MixedVec[MixedVec[Vec[Bool]]] = Wire(MixedVec(
    fromIQ.map(x => MixedVec(
      x.map(xx => Vec(xx.bits.exuParams.numRegSrc, Bool()))
    ))
  ))

  // rdNotBlock(i)(j): all srcs of IQ(i).exu(j) are not blocked
  private val rdNotBlock: MixedVec[MixedVec[Bool]] = Wire(MixedVec(
    fromIQ.map(x => MixedVec(
      x.map(xx => Bool())
    ))
  ))

  private val intRdNotBlock: Seq2[Bool] = intRdArbWinner.map(_.map(_.asUInt.andR))
  private val fpRdNotBlock: Seq2[Bool] = fpRdArbWinner.map(_.map(_.asUInt.andR))
  private val vfRdNotBlock: Seq2[Bool] = vfRdArbWinner.map(_.map(_.asUInt.andR))
  private val v0RdNotBlock: Seq2[Bool] = v0RdArbWinner.map(_.map(_.asUInt.andR))

  private val intRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val fpRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val vfRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val v0RFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val vlRFReadReq: Seq2[Option[ValidIO[RfReadPortWithConfig]]] = fromIQ.map(x => x.map(xx => xx.bits.genVlRdReadValidBundle(xx.valid)).toSeq).toSeq

  private val allDataSources: Seq[Seq[Vec[DataSource]]] = fromIQ.map(x => x.map(xx => xx.bits.common.dataSources).toSeq)
  private val allNumRegSrcs: Seq[Seq[Int]] = fromIQ.map(x => x.map(xx => xx.bits.exuParams.numRegSrc).toSeq)

  intRFReadArbiter.io.in.zip(intRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(IntData())
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
  fpRFReadArbiter.io.in.zip(fpRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = FpRegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
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

  vfRFReadArbiter.io.in.zip(vfRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = VecRegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
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

  v0RFReadArbiter.io.in.zip(v0RFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = V0RegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
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

  vlRFReadArbiter.io.in.zip(vlRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq).zipWithIndex.foreach { case ((arbInSeq, inRFReadReq), exuIdx) =>
      println(s"iqIdx: $iqIdx, exuIdx: $exuIdx, arbInSeq: $arbInSeq, inRFReadReq: $inRFReadReq")
      arbInSeq.headOption.foreach(_.valid := inRFReadReq.map(_.valid).get)
      arbInSeq.headOption.foreach(_.bits.addr := inRFReadReq.map(_.bits.addr).get)
    }
  }

  private val intRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.rfWen.getOrElse(false.B)).toSeq).toSeq
  private val fpRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.fpWen.getOrElse(false.B)).toSeq).toSeq
  private val vfRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.vecWen.getOrElse(false.B)).toSeq).toSeq
  private val v0RFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.v0Wen.getOrElse(false.B)).toSeq).toSeq
  private val vlRFWriteReq: Seq2[Bool] = fromIQ.map(x => x.map(xx => xx.valid && xx.bits.common.vlWen.getOrElse(false.B)).toSeq).toSeq

  intWbBusyArbiter.io.in.zip(intRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  fpWbBusyArbiter.io.in.zip(fpRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  vfWbBusyArbiter.io.in.zip(vfRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  v0WbBusyArbiter.io.in.zip(v0RFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  vlWbBusyArbiter.io.in.zip(vlRFWriteReq).foreach { case (arbInSeq, inRFWriteReqSeq) =>
    arbInSeq.zip(inRFWriteReqSeq).foreach { case (arbIn, inRFWriteReq) =>
      arbIn.valid := inRFWriteReq
    }
  }

  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vecSchdParams = params.schdParams(VecScheduler())

  private val schdParams = params.allSchdParams

  private val pcReadValid = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathValid))
  private val pcReadFtqPtr = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathFtqPtr))
  private val pcReadFtqOffset = Wire(chiselTypeOf(io.fromPcTargetMem.fromDataPathFtqOffset))
  private val targetPCRdata = io.fromPcTargetMem.toDataPathTargetPC
  private val pcRdata = io.fromPcTargetMem.toDataPathPC
  private val intRfRdata = Option.when(param.isIntSchd)(Wire(Vec(params.numPregRd(IntData()), UInt(intSchdParams.rfDataWidth.W))))
  private val fpRfRdata = Option.when(param.isFpSchd)(Wire(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.rfDataWidth.W))))
  private val vfRfRdata = Option.when(param.isVecSchd)(Wire(Vec(params.numPregRd(VecData()), UInt(vecSchdParams.rfDataWidth.W))))
  private val v0RfRdata = Option.when(param.isVecSchd)(Wire(Vec(params.numPregRd(V0Data()), UInt(V0Data().dataWidth.W))))
  private val vlRfRdata = Option.when(param.isVecSchd)(Wire(Vec(params.numPregRd(VlData()), UInt(VlData().dataWidth.W))))

  val pcReadFtqPtrFormIQ = fromIntIQ.flatten.filter(x => x.bits.exuParams.needPc)
  assert(pcReadFtqPtrFormIQ.size == pcReadFtqPtr.size, s"pcReadFtqPtrFormIQ.size ${pcReadFtqPtrFormIQ.size} not equal pcReadFtqPtr.size ${pcReadFtqPtr.size}")
  pcReadValid.zip(pcReadFtqPtrFormIQ.map(_.valid)).map(x => x._1 := x._2)
  pcReadFtqPtr.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqIdx.get)).map(x => x._1 := x._2)
  pcReadFtqOffset.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqOffset.get)).map(x => x._1 := x._2)
  io.fromPcTargetMem.fromDataPathValid := pcReadValid
  io.fromPcTargetMem.fromDataPathFtqPtr := pcReadFtqPtr
  io.fromPcTargetMem.fromDataPathFtqOffset := pcReadFtqOffset

  private val intDiffRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(backendParams.basicDebugEn && param.isIntSchd, (Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W)))))
  private val fpDiffRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(backendParams.basicDebugEn && param.isFpSchd, (Wire(Vec(32, UInt(fpSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W)))))
  private val vfDiffRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(backendParams.basicDebugEn && param.isVecSchd, (Wire(Vec(31, UInt(vecSchdParams.pregIdxWidth.W))), Wire(Vec(31, UInt(VLEN.W)))))
  private val v0DiffRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(backendParams.basicDebugEn && param.isVecSchd, (Wire(Vec(1, UInt(log2Up(V0PhyRegs).W))), Wire(Vec(1, UInt(V0Data().dataWidth.W)))))
  private val vlDiffRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(backendParams.basicDebugEn && param.isVecSchd, (Wire(Vec(1, UInt(log2Up(VlPhyRegs).W))), Wire(Vec(1, UInt(VlData().dataWidth.W)))))

  private val fpDiffReadData: Option[Vec[UInt]] =
    OptionWrapper(backendParams.basicDebugEn && param.isFpSchd, Wire(Vec(32, UInt(XLEN.W))))
  private val vecDiffReadData: Option[Vec[UInt]] =
    OptionWrapper(backendParams.basicDebugEn && param.isVecSchd, Wire(Vec(64, UInt(64.W)))) // v0 = Cat(Vec(1), Vec(0))
  private val vlDiffReadData: Option[UInt] =
    OptionWrapper(backendParams.basicDebugEn && param.isVecSchd, Wire(UInt(VlData().dataWidth.W)))


  fpDiffReadData.foreach(_ := fpDiffRead
    .get._2
    .slice(0, 32)
    .map(_(63, 0))
  ) // fp only used [63, 0]
  vecDiffReadData.foreach(_ := 
    v0DiffRead
    .get._2
    .slice(0, 1)
    .map(x => Seq(x(63, 0), x(127, 64))).flatten ++ 
    vfDiffRead
    .get._2
    .slice(0, 31)
    .map(x => Seq(x(63, 0), x(127, 64))).flatten
  )
  vlDiffReadData.foreach(_ := vlDiffRead
    .get._2(0)
  )

  io.diffVl.foreach(_ := vlDiffReadData.get)

  io.toWakeupQueueRCIdx := 0.U.asTypeOf(io.toWakeupQueueRCIdx)
  io.toBypassNetworkRCData := 0.U.asTypeOf(io.toBypassNetworkRCData)
  val splitNum = if (backendParams.debugEn) 1 else 4
  if (param.isIntSchd) {
    val intRfRaddr = Wire(Vec(params.numPregRd(IntData()), UInt(intSchdParams.pregIdxWidth.W)))
    val intRfWen = Wire(Vec(io.fromIntWb.get.length, Bool()))
    val intRfWaddr = Wire(Vec(io.fromIntWb.get.length, UInt(intSchdParams.pregIdxWidth.W)))
    val intRfWdata = Wire(Vec(io.fromIntWb.get.length, UInt(intSchdParams.rfDataWidth.W)))
    IntRegFileSplit("IntRegFile", intSchdParams.numPregs, splitNum, intRfRaddr, intRfRdata.get, intRfWen, intRfWaddr, intRfWdata,
      bankNum = 1,
      debugReadAddr = intDiffRead.map(_._1),
      debugReadData = intDiffRead.map(_._2)
    )
    intRfWaddr := io.fromIntWb.get.map(x => RegEnable(x.pdest, x.wen)).toSeq
    intRfWdata := io.fromIntWb.get.map(x => RegEnable(x.data, x.wen)).toSeq
    intRfWen := RegNext(VecInit(io.fromIntWb.get.map(_.wen).toSeq))
    for (portIdx <- intRfRaddr.indices) {
      if (intRFReadArbiter.io.out.isDefinedAt(portIdx))
        intRfRaddr(portIdx) := intRFReadArbiter.io.out(portIdx).bits.addr
      else
        intRfRaddr(portIdx) := 0.U
    }
    // regcache
    val regCache = Module(new RegCache())
    def IssueBundle2RCReadPort(issue: DecoupledIO[IssueQueueIssueBundle]): Vec[RCReadPort] = {
      val readPorts = Wire(Vec(issue.bits.exuParams.numIntSrc, new RCReadPort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth)))
      readPorts.zipWithIndex.foreach { case (r, idx) =>
        r.ren := issue.valid && issue.bits.common.dataSources(idx).readRegCache
        r.addr := issue.bits.rcIdx.get(idx)
        r.data := DontCare
      }
      readPorts
    }
    val regCacheReadReq = fromIntIQ.flatten.filter(_.bits.exuParams.numIntSrc > 0).flatMap(IssueBundle2RCReadPort(_))
    val regCacheReadData = regCache.io.readPorts.map(_.data)
    println(s"[${param.getName}DataPath] regCache readPorts size: ${regCache.io.readPorts.size}, regCacheReadReq size: ${regCacheReadReq.size}")
    require(regCache.io.readPorts.size == regCacheReadReq.size, "reg cache's readPorts size should be equal to regCacheReadReq")
    regCache.io.readPorts.zip(regCacheReadReq).foreach { case (r, req) =>
      r.ren := req.ren
      r.addr := req.addr
    }
    val s1_RCReadData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
    s1_RCReadData.foreach(_.foreach(_.foreach(_ := 0.U)))
    s1_RCReadData.zip(toExu).filter(_._2.map(_.bits.params.isIntExeUnit).reduce(_ || _)).flatMap(_._1).flatten
      .zip(regCacheReadData.take(params.getIntExuRCReadSize)).foreach { case (s1_data, rdata) =>
      s1_data := rdata
    }
    s1_RCReadData.zip(toExu).filter(_._2.map(x => x.bits.params.isMemExeUnit && x.bits.params.readIntRf).reduce(_ || _)).flatMap(_._1).flatten
      .zip(regCacheReadData.takeRight(params.getMemExuRCReadSize)).foreach { case (s1_data, rdata) =>
      s1_data := rdata
    }
    println(s"[${param.getName}DataPath] s1_RCReadData.int.size: ${s1_RCReadData.zip(toExu).filter(_._2.map(_.bits.params.isIntExeUnit).reduce(_ || _)).flatMap(_._1).flatten.size}, RCRdata.int.size: ${params.getIntExuRCReadSize}")
    println(s"[${param.getName}DataPath] s1_RCReadData.mem.size: ${s1_RCReadData.zip(toExu).filter(_._2.map(x => x.bits.params.isMemExeUnit && x.bits.params.readIntRf).reduce(_ || _)).flatMap(_._1).flatten.size}, RCRdata.mem.size: ${params.getMemExuRCReadSize}")
    io.toWakeupQueueRCIdx := regCache.io.toWakeupQueueRCIdx
    io.toBypassNetworkRCData := s1_RCReadData
    regCache.io.writePorts := io.fromBypassNetwork
    // perf counter
    val int_regcache_size = 48
    val int_regcache_tag = RegInit(VecInit(Seq.fill(int_regcache_size)(0.U(intSchdParams.pregIdxWidth.W))))
    val int_regcache_enqPtr = RegInit(0.U(log2Up(int_regcache_size).W))
    int_regcache_enqPtr := int_regcache_enqPtr + PopCount(intRfWen)
    for (i <- intRfWen.indices) {
      when(intRfWen(i)) {
        int_regcache_tag(int_regcache_enqPtr + PopCount(intRfWen.take(i))) := intRfWaddr(i)
      }
    }
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
  }
  else if (param.isFpSchd) {
    val fpRfRaddr = Wire(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.pregIdxWidth.W)))
    val fpRfWen = Wire(Vec(io.fromFpWb.get.length, Bool()))
    val fpRfWaddr = Wire(Vec(io.fromFpWb.get.length, UInt(fpSchdParams.pregIdxWidth.W)))
    val fpRfWdata = Wire(Vec(io.fromFpWb.get.length, UInt(fpSchdParams.rfDataWidth.W)))
    FpRegFileSplit("FpRegFile", fpSchdParams.numPregs, splitNum, fpRfRaddr, fpRfRdata.get, fpRfWen, fpRfWaddr, fpRfWdata,
      bankNum = 1,
      debugReadAddr = fpDiffRead.map(_._1),
      debugReadData = fpDiffRead.map(_._2)
    )
    fpRfWaddr := io.fromFpWb.get.map(x => RegEnable(x.pdest, x.wen)).toSeq
    fpRfWdata := io.fromFpWb.get.map(x => RegEnable(x.data, x.wen)).toSeq
    fpRfWen := RegNext(VecInit(io.fromFpWb.get.map(_.wen).toSeq))
    for (portIdx <- fpRfRaddr.indices) {
      if (fpRFReadArbiter.io.out.isDefinedAt(portIdx))
        fpRfRaddr(portIdx) := fpRFReadArbiter.io.out(portIdx).bits.addr
      else
        fpRfRaddr(portIdx) := 0.U
    }
  }
  else {
    val vfRfRaddr = Wire(Vec(params.numPregRd(VecData()), UInt(vecSchdParams.pregIdxWidth.W)))
    val vfRfWen = Wire(Vec(splitNum, Vec(io.fromVfWb.get.length, Bool())))
    val vfRfWaddr = Wire(Vec(io.fromVfWb.get.length, UInt(vecSchdParams.pregIdxWidth.W)))
    val vfRfWdata = Wire(Vec(io.fromVfWb.get.length, UInt(vecSchdParams.rfDataWidth.W)))
    val v0RfSplitNum = VLEN / XLEN
    val v0RfRaddr = Wire(Vec(params.numPregRd(V0Data()), UInt(log2Up(V0PhyRegs).W)))
    val v0RfWen = Wire(Vec(v0RfSplitNum, Vec(io.fromV0Wb.get.length, Bool())))
    val v0RfWaddr = Wire(Vec(io.fromV0Wb.get.length, UInt(log2Up(V0PhyRegs).W)))
    val v0RfWdata = Wire(Vec(io.fromV0Wb.get.length, UInt(V0Data().dataWidth.W)))
    val vlRfRaddr = Wire(Vec(params.numPregRd(VlData()), UInt(log2Up(VlPhyRegs).W)))
    val vlRfWen = Wire(Vec(io.fromVlWb.get.length, Bool()))
    val vlRfWaddr = Wire(Vec(io.fromVlWb.get.length, UInt(log2Up(VlPhyRegs).W)))
    val vlRfWdata = Wire(Vec(io.fromVlWb.get.length, UInt(VlData().dataWidth.W)))
    VfRegFile("VfRegFile", vecSchdParams.numPregs, splitNum, vfRfRaddr, vfRfRdata.get, vfRfWen, vfRfWaddr, vfRfWdata,
      debugReadAddr = vfDiffRead.map(_._1),
      debugReadData = vfDiffRead.map(_._2)
    )
    VfRegFile("V0RegFile", V0PhyRegs, v0RfSplitNum, v0RfRaddr, v0RfRdata.get, v0RfWen, v0RfWaddr, v0RfWdata,
      debugReadAddr = v0DiffRead.map(_._1),
      debugReadData = v0DiffRead.map(_._2)
    )
    FpRegFile("VlRegFile", VlPhyRegs, vlRfRaddr, vlRfRdata.get, vlRfWen, vlRfWaddr, vlRfWdata,
      bankNum = 1,
      isVlRegfile = true,
      debugReadAddr = vlDiffRead.map(_._1),
      debugReadData = vlDiffRead.map(_._2)
    )
    vfRfWaddr := io.fromVfWb.get.map(x => RegEnable(x.pdest, x.wen)).toSeq
    vfRfWdata := io.fromVfWb.get.map(x => RegEnable(x.data, x.wen)).toSeq
    vfRfWen.foreach(_.zip(io.fromVfWb.get.map(x => RegNext(x.wen))).foreach { case (wenSink, wenSource) => wenSink := wenSource })
    for (portIdx <- vfRfRaddr.indices) {
      if (vfRFReadArbiter.io.out.isDefinedAt(portIdx))
        vfRfRaddr(portIdx) := vfRFReadArbiter.io.out(portIdx).bits.addr
      else
        vfRfRaddr(portIdx) := 0.U
    }
    v0RfWaddr := io.fromV0Wb.get.map(x => RegEnable(x.pdest, x.wen)).toSeq
    v0RfWdata := io.fromV0Wb.get.map(x => RegEnable(x.data, x.wen)).toSeq
    v0RfWen.foreach(_.zip(io.fromV0Wb.get.map(x => RegNext(x.wen))).foreach { case (wenSink, wenSource) => wenSink := wenSource })
    for (portIdx <- v0RfRaddr.indices) {
      if (v0RFReadArbiter.io.out.isDefinedAt(portIdx))
        v0RfRaddr(portIdx) := v0RFReadArbiter.io.out(portIdx).bits.addr
      else
        v0RfRaddr(portIdx) := 0.U
    }
    vlRfWaddr := io.fromVlWb.get.map(x => RegEnable(x.pdest, x.wen)).toSeq
    vlRfWdata := io.fromVlWb.get.map(x => RegEnable(x.data, x.wen)).toSeq
    vlRfWen := io.fromVlWb.get.map(x => RegNext(x.wen)).toSeq
    for (portIdx <- vlRfRaddr.indices) {
      if (vlRFReadArbiter.io.out.isDefinedAt(portIdx))
        vlRfRaddr(portIdx) := vlRFReadArbiter.io.out(portIdx).bits.addr
      else
        vlRfRaddr(portIdx) := 0.U
    }

    val vecExcpUseVecRdPorts = Seq(6, 7, 8, 9, 10, 11, 0, 1)
    val vecExcpUseVecWrPorts = Seq(0, 1, 2, 3)
    val vecExcpUseV0RdPorts = Seq(2, 3)
    val vecExcpUsev0WrPorts = Seq(0)
    var v0RdPortsIter: Iterator[Int] = vecExcpUseV0RdPorts.iterator
    val v0WrPortsIter: Iterator[Int] = vecExcpUsev0WrPorts.iterator
    val (fromVecExcp, toVecExcp) = (io.fromVecExcpMod.get, io.toVecExcpMod.get)
    for (i <- fromVecExcp.r.indices) {
      when(fromVecExcp.r(i).valid && !fromVecExcp.r(i).bits.isV0) {
        vfRfRaddr(vecExcpUseVecRdPorts(i)) := fromVecExcp.r(i).bits.addr
      }
      if (i % maxMergeNumPerCycle == 0) {
        val v0RdPort = v0RdPortsIter.next()
        when(fromVecExcp.r(i).valid && fromVecExcp.r(i).bits.isV0) {
          v0RfRaddr(v0RdPort) := fromVecExcp.r(i).bits.addr
        }
      }
    }
    for (i <- fromVecExcp.w.indices) {
      when(fromVecExcp.w(i).valid && !fromVecExcp.w(i).bits.isV0) {
        val vecWrPort = vecExcpUseVecWrPorts(i)
        vfRfWen.foreach(_(vecWrPort) := true.B)
        vfRfWaddr(vecWrPort) := fromVecExcp.w(i).bits.newVdAddr
        vfRfWdata(vecWrPort) := fromVecExcp.w(i).bits.newVdData
      }
      if (i % maxMergeNumPerCycle == 0) {
        when(fromVecExcp.w(i).valid && fromVecExcp.w(i).bits.isV0) {
          val v0WrPort = v0WrPortsIter.next()
          v0RfWen.foreach(_(v0WrPort) := true.B)
          v0RfWaddr(v0WrPort) := fromVecExcp.w(i).bits.newVdAddr
          v0RfWdata(v0WrPort) := fromVecExcp.w(i).bits.newVdData
        }
      }
    }
    v0RdPortsIter = vecExcpUseV0RdPorts.iterator
    for (i <- toVecExcp.rdata.indices) {
      toVecExcp.rdata(i).valid := RegNext(fromVecExcp.r(i).valid)
      toVecExcp.rdata(i).bits := Mux(
        RegEnable(!fromVecExcp.r(i).bits.isV0, fromVecExcp.r(i).valid),
        vfRfRdata.get(vecExcpUseVecRdPorts(i)),
        if (i % maxMergeNumPerCycle == 0) v0RfRdata.get(v0RdPortsIter.next()) else 0.U,
      )
    }
  }

  intDiffRead.foreach { case (addr, _) =>
    addr := io.diffIntRat.get
  }

  fpDiffRead.foreach { case (addr, _) =>
    addr := io.diffFpRat.get
  }

  vfDiffRead.foreach { case (addr, _) =>
    addr := io.diffVecRat.get
  }
  v0DiffRead.foreach { case (addr, _) =>
    addr := io.diffV0Rat.get
  }
  vlDiffRead.foreach { case (addr, _) =>
    addr := io.diffVlRat.get
  }

  println(s"[${param.getName}DataPath] " +
    s"has intDiffRead: ${intDiffRead.nonEmpty}, " +
    s"has fpDiffRead: ${fpDiffRead.nonEmpty}, " +
    s"has vecDiffRead: ${vfDiffRead.nonEmpty}, " +
    s"has v0DiffRead: ${v0DiffRead.nonEmpty}, " +
    s"has vlDiffRead: ${vlDiffRead.nonEmpty}")

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
  val s1_fpPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
  val s1_vfPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
  val s1_v0PregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
  // one uop only use one vl at most
  val s1_vlPregRData: MixedVec[MixedVec[UInt]] = Wire(MixedVec(toExu.map(x => MixedVec(x.flatMap(_.bits.vl.map(chiselTypeOf(_)).toSeq)))))

  val rfrPortConfigs = schdParams.map(_.issueBlockParams).flatten.map(_.exuBlockParams.map(_.rfrPortConfigs))
  // (i)(j): IQ(i), EXU(j)
  val vlRdPortConfigs: Seq[Seq[VlRD]] = schdParams.flatMap(_.issueBlockParams).map(_.exuBlockParams.map(_.vlRD))

  val allRData = (s1_intPregRData ++ s1_fpPregRData ++ s1_vfPregRData ++ s1_v0PregRData)
  allRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  if (param.isIntSchd) {
    s1_intPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[IntRD]) > 0 }
          .foreach { case (sink, cfg) => sink := intRfRdata.get(cfg.find(_.isInstanceOf[IntRD]).get.port) }
      }
    }
    s1_vlPregRData.foreach(_.foreach(_ := 0.U))
  }
  if (param.isIntSchd || param.isFpSchd) {
    val fpRfRdataFinal = if (param.isIntSchd) io.fpRfRdataIn.get else fpRfRdata.get
    io.fpRfRdataOut.foreach(_ := fpRfRdata.get)
    s1_fpPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
    s1_fpPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[FpRD]) > 0 }
          .foreach { case (sink, cfg) => sink := fpRfRdataFinal(cfg.find(_.isInstanceOf[FpRD]).get.port) }
      }
    }
    s1_vlPregRData.foreach(_.foreach(_ := 0.U))
  }
  if (param.isVecSchd) {
    s1_vfPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[VfRD]) > 0 }
          .foreach { case (sink, cfg) => sink := vfRfRdata.get(cfg.find(_.isInstanceOf[VfRD]).get.port) }
      }
    }
    s1_v0PregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[V0RD]) > 0 }
          .foreach { case (sink, cfg) => sink := v0RfRdata.get(cfg.find(_.isInstanceOf[V0RD]).get.port) }
      }
    }
    s1_vlPregRData.zip(vlRdPortConfigs).foreach { case (iqRdata, iqCfg: Seq[VlRD]) =>
      iqRdata.zip(iqCfg).foreach { case (exuRData: UInt, exuCfg: VlRD) =>

        exuRData := vlRfRdata.get(exuCfg.port)
      }
    }
  }


  val og0_cancel_no_load = VecInit(og0FailedVec2.flatten.zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1).toSeq)
  val exuParamsNoLoad = fromIQ.flatten.zip(params.allExuParams).filter(!_._2.hasLoadFu)
  val is_0latency = Wire(Vec(og0_cancel_no_load.size, Bool()))
  is_0latency := exuParamsNoLoad.map(x => is0latency(x._1.bits.common.fuType))
  val og0_cancel_delay = RegNext(VecInit(og0_cancel_no_load.zip(is_0latency).map(x => x._1 && x._2)))
  val flushReg = RegNextWithEnable(io.flush)
  for (i <- fromIQ.indices) {
    for (j <- fromIQ(i).indices) {
      // IQ(s0) --[Ctrl]--> s1Reg ---------- begin
      // refs
      val s1_valid = s1_toExuValid(i)(j)
      val s1_ready = s1_toExuReady(i)(j)
      val s1_data = s1_toExuData(i)(j)
      val s1_addrOH = s1_addrOHs(i)(j)
      val s0 = fromIQ(i)(j) // s0
      PerfCCT.updateInstPos(s0.bits.common.debug_seqNum, PerfCCT.InstPos.AtIssueArb.id.U, s0.valid, clock, reset)
      PerfCCT.updateInstPos(s1_data.debug_seqNum, PerfCCT.InstPos.AtIssueReadReg.id.U, s1_valid, clock, reset)

      for (k <- s0.bits.common.dataSources.indices) {
        rdSrcsNotBlock(i)(j)(k) := intRdArbWinner(i)(j)(k) && fpRdArbWinner(i)(j)(k) && vfRdArbWinner(i)(j)(k) && v0RdArbWinner(i)(j)(k)
      }
      rdNotBlock(i)(j) := (s0.bits.common.dataSources zip rdSrcsNotBlock(i)(j)).map {
        case (source, srcNotBlock) =>
          !source.readReg || srcNotBlock && vlRdArbWinner(i)(j).getOrElse(true.B)
      }.fold(true.B)(_ && _)
      val notBlock = rdNotBlock(i)(j) && intWbNotBlock(i)(j) && fpWbNotBlock(i)(j) && vfWbNotBlock(i)(j) && v0WbNotBlock(i)(j) && vlWbNotBlock(i)(j)
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, flushReg))
      val s1_cancel = og1FailedVec2(i)(j)
      val s0_cancel = Wire(Bool())
      if (s0.bits.exuParams.isIQWakeUpSink) {
        val exuOHNoLoad = s0.bits.common.exuSources.get.map(x => x.toExuOH(s0.bits.exuParams).zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1))
        s0_cancel := exuOHNoLoad.zip(s0.bits.common.dataSources).map{
          case (exuOH, dataSource) => (VecInit(exuOH).asUInt & og0_cancel_delay.asUInt).orR && dataSource.readForward
        }.reduce(_ || _) && s0.valid
      } else s0_cancel := false.B
      val s0_ldCancel = LoadShouldCancel(s0.bits.common.loadDependency, io.ldCancel)
      when (s0.fire && !s1_flush && !s0_ldCancel) {
        s1_valid := true.B
      }.otherwise {
        s1_valid := false.B
      }
      when (s0.valid) {
        s1_data.fromIssueBundle(s0.bits) // no src data here
        s1_addrOH := s0.bits.addrOH
      }
      s0.ready := notBlock && !s0_cancel
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
          og0FailedVec2(iqIdx)(iuIdx)   := fromIQ(iqIdx)(iuIdx).valid && !fromIQ(iqIdx)(iuIdx).ready
          og0resp.valid                 := og0FailedVec2(iqIdx)(iuIdx)
          og0resp.bits.robIdx           := fromIQ(iqIdx)(iuIdx).bits.common.robIdx
          og0resp.bits.uopIdx.foreach(_ := fromIQ(iqIdx)(iuIdx).bits.common.vpu.get.vuopIdx)
          og0resp.bits.sqIdx.foreach(_ := 0.U.asTypeOf(new SqPtr))
          og0resp.bits.lqIdx.foreach(_ := 0.U.asTypeOf(new LqPtr))
          og0resp.bits.resp             := RespType.block
          og0resp.bits.fuType           := fromIQ(iqIdx)(iuIdx).bits.common.fuType

          val og1resp = toIU.og1resp
          val hasUncertain = s1_toExuData(iqIdx)(iuIdx).params.needUncertainWakeup
          val lastUncertainFire = RegNext(toExu(iqIdx)(iuIdx).valid && isUncertain(s1_toExuData(iqIdx)(iuIdx).fuType) && s1_toExuReady(iqIdx)(iuIdx))
          if (hasUncertain){
            og1FailedVec2(iqIdx)(iuIdx) := s1_toExuValid(iqIdx)(iuIdx) && isUncertain(s1_toExuData(iqIdx)(iuIdx).fuType) && (!s1_toExuReady(iqIdx)(iuIdx) || s1_toExuReady(iqIdx)(iuIdx) && lastUncertainFire)
          }
          else{
            og1FailedVec2(iqIdx)(iuIdx) := s1_toExuValid(iqIdx)(iuIdx) && !s1_toExuReady(iqIdx)(iuIdx)
          }
          og1resp.valid                 := s1_toExuValid(iqIdx)(iuIdx)
          og1resp.bits.robIdx           := s1_toExuData(iqIdx)(iuIdx).robIdx
          og1resp.bits.uopIdx.foreach(_ := s1_toExuData(iqIdx)(iuIdx).vpu.get.vuopIdx)
          og1resp.bits.sqIdx.foreach(_ :=  0.U.asTypeOf(new SqPtr))
          og1resp.bits.lqIdx.foreach(_ :=  0.U.asTypeOf(new LqPtr))
          // respType:  success    -> IQ entry clear
          //            uncertain  -> IQ entry no action
          //            block      -> IQ entry issued set false, then re-issue
          // hyu, lda and sta are uncertain at OG1 stage
          // and all vector arith exu should check success in og2 stage
          og1resp.bits.resp             := Mux(og1FailedVec2(iqIdx)(iuIdx),
            RespType.block,
            if (toIU.issueQueueParams match { case x => x.isLdAddrIQ || x.isStAddrIQ || x.isHyAddrIQ || x.isVecLduIQ || x.isVecStuIQ || x.inVfSchd})
              RespType.uncertain
            else
              RespType.success,
          )
          og1resp.bits.fuType           := s1_toExuData(iqIdx)(iuIdx).fuType
      }
  }

  io.og0Cancel := og0FailedVec2.flatten.zip(params.allExuParams).map{ case (cancel, params) => 
                    if (params.isIQWakeUpSource && params.wakeUpFuLatancySet.contains(0)) cancel else false.B
                  }.toSeq
  io.og1Cancel := toFlattenExu.map(x => x.valid && !x.fire)


  if (backendParams.debugEn){
    dontTouch(og0_cancel_no_load)
    dontTouch(is_0latency)
    dontTouch(og0_cancel_delay)
  }
  for (i <- toExu.indices) {
    for (j <- toExu(i).indices) {
      // s1Reg --[Ctrl]--> exu(s1) ---------- begin
      // refs
      val sinkData = toExu(i)(j).bits
      // assign
      toExu(i)(j).valid := s1_toExuValid(i)(j) && !og1FailedVec2(i)(j)
      s1_toExuReady(i)(j) := toExu(i)(j).ready
      sinkData := s1_toExuData(i)(j)
      // s1Reg --[Ctrl]--> exu(s1) ---------- end

      // s1Reg --[Data]--> exu(s1) ---------- begin
      // data source1: preg read data
      for (k <- sinkData.src.indices) {
        val srcDataTypeSet: Set[DataConfig] = sinkData.params.getSrcDataType(k)
        val readRfMap: Seq[(Bool, UInt)] = (
          if (k == 3) {(
            Seq(None)
            :+
            OptionWrapper(s1_v0PregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(V0RegSrcDataSet).nonEmpty, 
              (SrcType.isV0(s1_srcType(i)(j)(k)) -> s1_v0PregRData(i)(j)(k)))
          )}
          else {(
            Seq(None)
            :+
            OptionWrapper(s1_intPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(IntRegSrcDataSet).nonEmpty, 
              (SrcType.isXp(s1_srcType(i)(j)(k)) -> s1_intPregRData(i)(j)(k)))
            :+
            OptionWrapper(s1_vfPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(VecRegSrcDataSet).nonEmpty,
              (SrcType.isVp(s1_srcType(i)(j)(k)) -> s1_vfPregRData(i)(j)(k)))
            :+
            OptionWrapper(s1_fpPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(FpRegSrcDataSet).nonEmpty, 
              (SrcType.isFp(s1_srcType(i)(j)(k)) -> s1_fpPregRData(i)(j)(k)))
          )}
        ).filter(_.nonEmpty).map(_.get)

        if (readRfMap.nonEmpty)
          sinkData.src(k) := Mux1H(readRfMap)
      }
      // There is no Mux1H in vl read data, since it always use seperated data path
      sinkData.vl.foreach {
        x => x := s1_vlPregRData(i)(j)
      }
      if (sinkData.params.hasJmpFu || sinkData.params.hasLoadFu) {
        val index = pcReadFtqPtrFormIQ.map(_.bits.exuParams).indexOf(sinkData.params)
        sinkData.pc.get := pcRdata(index)
        val aluSinkData = toExu(i)(0).bits
        aluSinkData.pc.foreach(_ := pcRdata(index))
      }
      if (sinkData.params.needTarget) {
        val index = pcReadFtqPtrFormIQ.map(_.bits.exuParams).indexOf(sinkData.params)
        sinkData.predictInfo.get.target := targetPCRdata(index)
      }
    }
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val delayedCnt = 2
    if (param.isIntSchd) {
      val difftestArchIntRegState = DifftestModule(new DiffArchIntRegState, delay = delayedCnt)
      difftestArchIntRegState.coreid := io.hartId
      difftestArchIntRegState.value := intDiffRead.get._2
    }
    if (param.isFpSchd) {
      val difftestArchFpRegState = DifftestModule(new DiffArchFpRegState, delay = delayedCnt)
      difftestArchFpRegState.coreid := io.hartId
      difftestArchFpRegState.value := fpDiffReadData.get
    }
    if (param.isVecSchd) {
      val difftestArchVecRegState = DifftestModule(new DiffArchVecRegState, delay = delayedCnt)
      difftestArchVecRegState.coreid := io.hartId
      difftestArchVecRegState.value := vecDiffReadData.get
    }
  }

  XSPerfHistogram(s"IntRegFileRead_hist", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"FpRegFileRead_hist", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"VfRegFileRead_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"IntRegFileWrite_hist", PopCount(intRFWriteReq.flatten), true.B, 0, 20, 1)
  XSPerfHistogram(s"FpRegFileWrite_hist", PopCount(fpRFWriteReq.flatten), true.B, 0, 20, 1)
  XSPerfHistogram(s"VfRegFileWrite_hist", PopCount(vfRFWriteReq.flatten), true.B, 0, 20, 1)

  XSPerfAccumulate(s"IntRFReadBeforeArb", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"IntRFReadAfterArb", PopCount(intRFReadArbiter.io.out.map(_.valid)))
  XSPerfAccumulate(s"FpRFReadBeforeArb", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"FpRFReadAfterArb", PopCount(fpRFReadArbiter.io.out.map(_.valid)))
  XSPerfAccumulate(s"VfRFReadBeforeArb", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"VfRFReadAfterArb", PopCount(vfRFReadArbiter.io.out.map(_.valid)))
  XSPerfAccumulate(s"IntUopBeforeArb", PopCount(fromIntIQ.flatten.map(_.valid)))
  XSPerfAccumulate(s"IntUopAfterArb", PopCount(fromIntIQ.flatten.map(_.fire)))
  XSPerfAccumulate(s"VfUopBeforeArb", PopCount(fromVfIQ.flatten.map(_.valid)))
  XSPerfAccumulate(s"VfUopAfterArb", PopCount(fromVfIQ.flatten.map(_.fire)))

  XSPerfHistogram(s"IntRFReadBeforeArb_hist", PopCount(intRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"IntRFReadAfterArb_hist", PopCount(intRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"FpRFReadBeforeArb_hist", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"FpRFReadAfterArb_hist", PopCount(fpRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadBeforeArb_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadAfterArb_hist", PopCount(vfRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"IntUopBeforeArb_hist", PopCount(fromIntIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"IntUopAfterArb_hist", PopCount(fromIntIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
  XSPerfHistogram(s"VfUopBeforeArb_hist", PopCount(fromVfIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"VfUopAfterArb_hist", PopCount(fromVfIQ.flatten.map(_.fire)), true.B, 0, 8, 2)

  // datasource perf counter (after arbiter)
  fromIQ.foreach(iq => iq.foreach{exu => 
    val exuParams = exu.bits.exuParams
    if (exuParams.isIntExeUnit) {
      for (i <- 0 until 2) {
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_forward",  exu.fire && exu.bits.common.dataSources(i).readForward)
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_bypass",   exu.fire && exu.bits.common.dataSources(i).readBypass)
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_regcache", exu.fire && exu.bits.common.dataSources(i).readRegCache)
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_reg",      exu.fire && exu.bits.common.dataSources(i).readReg)
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_zero",     exu.fire && exu.bits.common.dataSources(i).readZero)
        XSPerfAccumulate(s"INT_ExuId${exuParams.exuIdx}_src${i}_dataSource_imm",      exu.fire && exu.bits.common.dataSources(i).readImm)
      }
    }
    if (exuParams.isMemExeUnit && exuParams.readIntRf) {
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_forward",  exu.fire && exu.bits.common.dataSources(0).readForward)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_bypass",   exu.fire && exu.bits.common.dataSources(0).readBypass)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_regcache", exu.fire && exu.bits.common.dataSources(0).readRegCache)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_reg",      exu.fire && exu.bits.common.dataSources(0).readReg)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_zero",     exu.fire && exu.bits.common.dataSources(0).readZero)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_imm",      exu.fire && exu.bits.common.dataSources(0).readImm)
    }
  })

  // Top-Down
  val IQsFire = fromFlattenIQ.map(_.fire)
  val uopsIssued = IQsFire.reduce(_ || _)  
  val uopsIssuedCnt = PopCount(IQsFire)

  val noStoreIssued = !fromIntIQ.flatten.filter(memIq => memIq.bits.exuParams.fuConfigs.contains(FuConfig.StaCfg) ||
                                                         memIq.bits.exuParams.fuConfigs.contains(FuConfig.StdCfg)
  ).map(_.fire).reduce(_ || _)

  io.uopTopDown.uopsIssued := uopsIssued
  io.uopTopDown.uopsIssuedCnt := uopsIssuedCnt
  io.uopTopDown.noStoreIssued := noStoreIssued
}

class DataPathIO()(implicit p: Parameters, params: BackendParams, param: SchdBlockParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vecSchdParams = params.schdParams(VecScheduler())
  // bundles
  val hartId = Input(UInt(8.W))

  val flush: ValidIO[Redirect] = Flipped(ValidIO(new Redirect))

  val wbConfictRead = Input(MixedVec(params.allSchdParams.map(x => MixedVec(x.issueBlockParams.map(x => x.genWbConflictBundle())))))

  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromFpIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(fpSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVfIQ = Flipped(MixedVec(vecSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVecExcpMod = Option.when(param.isVecSchd)(Input(new ExcpModToVprf(maxMergeNumPerCycle * 2, maxMergeNumPerCycle)))

  val toIntIQ = MixedVec(intSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toFpIQ = MixedVec(fpSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toVfIQ = MixedVec(vecSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toVecExcpMod = Option.when(param.isVecSchd)(Output(new VprfToExcpMod(maxMergeNumPerCycle * 2)))

  val fpRfRdataIn = Option.when(param.isIntSchd)(Input(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.rfDataWidth.W))))

  val fpRfRdataOut = Option.when(param.isFpSchd)(Output(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.rfDataWidth.W))))

  val og0Cancel = Output(ExuVec())

  val og1Cancel = Output(ExuVec())

  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val toIntExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle

  val toFpExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(fpSchdParams.genExuInputBundle)

  val toVecExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(vecSchdParams.genExuInputBundle)

  val og1ImmInfo: Vec[ImmInfo] = Output(Vec(params.allExuParams.size, new ImmInfo))

  val fromIntWb = Option.when(param.isIntSchd)(Input(params.genIntWriteBackBundle))

  val fromFpWb = Option.when(param.isFpSchd)(Input(params.genFpWriteBackBundle))

  val fromVfWb = Option.when(param.isVecSchd)(Input(params.genVfWriteBackBundle))

  val fromV0Wb = Option.when(param.isVecSchd)(Input(params.genV0WriteBackBundle))

  val fromVlWb = Option.when(param.isVecSchd)(Input(params.genVlWriteBackBundle))

  val fromPcTargetMem = Flipped(new PcToDataPathIO(params))

  val fromBypassNetwork: Vec[RCWritePort] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn)
  )

  val toBypassNetworkRCData: MixedVec[MixedVec[Vec[UInt]]] = MixedVec(
    Seq(intSchdParams, fpSchdParams, vecSchdParams).map(schd => schd.issueBlockParams.map(iq =>
      MixedVec(iq.exuBlockParams.map(exu => Output(Vec(exu.numRegSrc, UInt(exu.srcDataBitsMax.W)))))
    )).flatten
  )

  val toWakeupQueueRCIdx: Vec[UInt] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    Output(UInt(RegCacheIdxWidth.W))
  )

  val diffIntRat = if (params.basicDebugEn && param.isIntSchd) Some(Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))) else None
  val diffFpRat  = if (params.basicDebugEn && param.isFpSchd) Some(Input(Vec(32, UInt(fpSchdParams.pregIdxWidth.W)))) else None
  val diffVecRat = if (params.basicDebugEn && param.isVecSchd) Some(Input(Vec(31, UInt(vecSchdParams.pregIdxWidth.W)))) else None
  val diffV0Rat  = if (params.basicDebugEn && param.isVecSchd) Some(Input(Vec(1, UInt(log2Up(V0PhyRegs).W)))) else None
  val diffVlRat  = if (params.basicDebugEn && param.isVecSchd) Some(Input(Vec(1, UInt(log2Up(VlPhyRegs).W)))) else None
  val diffVl     = if (params.basicDebugEn && param.isVecSchd) Some(Output(UInt(VlData().dataWidth.W))) else None

  val uopTopDown = new UopTopDown
}
