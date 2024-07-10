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
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.{FpScheduler, ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.regfile._
import xiangshan.backend.regcache._
import xiangshan.backend.PcToDataPathIO
import xiangshan.backend.fu.FuType.is0latency
import xiangshan.mem.{SqPtr, LqPtr}

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)

  println(s"[DataPath] Preg Params: ")
  println(s"[DataPath]   Int R(${params.getRfReadSize(IntData())}), W(${params.getRfWriteSize(IntData())}) ")
  println(s"[DataPath]   Fp R(${params.getRfReadSize(FpData())}), W(${params.getRfWriteSize(FpData())}) ")
  println(s"[DataPath]   Vf R(${params.getRfReadSize(VecData())}), W(${params.getRfWriteSize(VecData())}) ")
  println(s"[DataPath]   V0 R(${params.getRfReadSize(V0Data())}), W(${params.getRfWriteSize(V0Data())}) ")
  println(s"[DataPath]   Vl R(${params.getRfReadSize(VlData())}), W(${params.getRfWriteSize(VlData())}) ")
}

class DataPathImp(override val wrapper: DataPath)(implicit p: Parameters, params: BackendParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val io = IO(new DataPathIO())

  private val (fromIntIQ, toIntIQ, toIntExu) = (io.fromIntIQ, io.toIntIQ, io.toIntExu)
  private val (fromFpIQ,  toFpIQ,  toFpExu)  = (io.fromFpIQ,  io.toFpIQ,  io.toFpExu)
  private val (fromMemIQ, toMemIQ, toMemExu) = (io.fromMemIQ, io.toMemIQ, io.toMemExu)
  private val (fromVfIQ,  toVfIQ,  toVfExu ) = (io.fromVfIQ,  io.toVfIQ,  io.toVecExu)

  println(s"[DataPath] IntIQ(${fromIntIQ.size}), FpIQ(${fromFpIQ.size}), VecIQ(${fromVfIQ.size}), MemIQ(${fromMemIQ.size})")
  println(s"[DataPath] IntExu(${fromIntIQ.map(_.size).sum}), FpExu(${fromFpIQ.map(_.size).sum}), VecExu(${fromVfIQ.map(_.size).sum}), MemExu(${fromMemIQ.map(_.size).sum})")

  // just refences for convience
  private val fromIQ: Seq[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = (fromIntIQ ++ fromFpIQ ++ fromVfIQ ++ fromMemIQ).toSeq

  private val toIQs = toIntIQ ++ toFpIQ ++ toVfIQ ++ toMemIQ

  private val toExu: Seq[MixedVec[DecoupledIO[ExuInput]]] = (toIntExu ++ toFpExu ++ toVfExu ++ toMemExu).toSeq

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
  private val vlRdArbWinner: Seq2[MixedVec[Bool]] = vlRFReadArbiter.io.in.map(_.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq).toSeq

  private val intWbNotBlock: Seq[MixedVec[Bool]] = intWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val fpWbNotBlock: Seq[MixedVec[Bool]] = fpWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val vfWbNotBlock: Seq[MixedVec[Bool]] = vfWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val v0WbNotBlock: Seq[MixedVec[Bool]] = v0WbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq
  private val vlWbNotBlock: Seq[MixedVec[Bool]] = vlWbBusyArbiter.io.in.map(x => MixedVecInit(x.map(_.ready).toSeq)).toSeq

  private val intRdNotBlock: Seq2[Bool] = intRdArbWinner.map(_.map(_.asUInt.andR))
  private val fpRdNotBlock: Seq2[Bool] = fpRdArbWinner.map(_.map(_.asUInt.andR))
  private val vfRdNotBlock: Seq2[Bool] = vfRdArbWinner.map(_.map(_.asUInt.andR))
  private val v0RdNotBlock: Seq2[Bool] = v0RdArbWinner.map(_.map(_.asUInt.andR))
  private val vlRdNotBlock: Seq2[Bool] = vlRdArbWinner.map(_.map(_.asUInt.andR))

  private val intRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val fpRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val vfRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val v0RFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq
  private val vlRFReadReq: Seq3[ValidIO[RfReadPortWithConfig]] = fromIQ.map(x => x.map(xx => xx.bits.getRfReadValidBundle(xx.valid)).toSeq).toSeq

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

  vlRFReadArbiter.io.in.zip(vlRFReadReq).zipWithIndex.foreach { case ((arbInSeq2, inRFReadReqSeq2), iqIdx) =>
    arbInSeq2.zip(inRFReadReqSeq2).zipWithIndex.foreach { case ((arbInSeq, inRFReadReqSeq), exuIdx) =>
      val srcIndices: Seq[Int] = VlRegSrcDataSet.flatMap(data => fromIQ(iqIdx)(exuIdx).bits.exuParams.getRfReadSrcIdx(data)).toSeq.sorted
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
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())

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

  private val fpRfRaddr = Wire(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.pregIdxWidth.W)))
  private val fpRfRdata = Wire(Vec(params.numPregRd(FpData()), UInt(fpSchdParams.rfDataWidth.W)))
  private val fpRfWen = Wire(Vec(io.fromFpWb.length, Bool()))
  private val fpRfWaddr = Wire(Vec(io.fromFpWb.length, UInt(fpSchdParams.pregIdxWidth.W)))
  private val fpRfWdata = Wire(Vec(io.fromFpWb.length, UInt(fpSchdParams.rfDataWidth.W)))

  private val vfRfSplitNum = VLEN / XLEN
  private val vfRfRaddr = Wire(Vec(params.numPregRd(VecData()), UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfRdata = Wire(Vec(params.numPregRd(VecData()), UInt(vfSchdParams.rfDataWidth.W)))
  private val vfRfWen = Wire(Vec(vfRfSplitNum, Vec(io.fromVfWb.length, Bool())))
  private val vfRfWaddr = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfWdata = Wire(Vec(io.fromVfWb.length, UInt(vfSchdParams.rfDataWidth.W)))

  private val v0RfSplitNum = VLEN / XLEN
  private val v0RfRaddr = Wire(Vec(params.numPregRd(V0Data()), UInt(log2Up(V0PhyRegs).W)))
  private val v0RfRdata = Wire(Vec(params.numPregRd(V0Data()), UInt(V0Data().dataWidth.W)))
  private val v0RfWen = Wire(Vec(v0RfSplitNum, Vec(io.fromV0Wb.length, Bool())))
  private val v0RfWaddr = Wire(Vec(io.fromV0Wb.length, UInt(log2Up(V0PhyRegs).W)))
  private val v0RfWdata = Wire(Vec(io.fromV0Wb.length, UInt(V0Data().dataWidth.W)))

  private val vlRfRaddr = Wire(Vec(params.numPregRd(VlData()), UInt(log2Up(VlPhyRegs).W)))
  private val vlRfRdata = Wire(Vec(params.numPregRd(VlData()), UInt(VlData().dataWidth.W)))
  private val vlRfWen = Wire(Vec(io.fromVlWb.length, Bool()))
  private val vlRfWaddr = Wire(Vec(io.fromVlWb.length, UInt(log2Up(VlPhyRegs).W)))
  private val vlRfWdata = Wire(Vec(io.fromVlWb.length, UInt(VlData().dataWidth.W)))

  val pcReadFtqPtrFormIQ = fromIntIQ.flatten.filter(x => x.bits.exuParams.needPc)
  assert(pcReadFtqPtrFormIQ.size == pcReadFtqPtr.size, s"pcReadFtqPtrFormIQ.size ${pcReadFtqPtrFormIQ.size} not equal pcReadFtqPtr.size ${pcReadFtqPtr.size}")
  pcReadValid.zip(pcReadFtqPtrFormIQ.map(_.valid)).map(x => x._1 := x._2)
  pcReadFtqPtr.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqIdx.get)).map(x => x._1 := x._2)
  pcReadFtqOffset.zip(pcReadFtqPtrFormIQ.map(_.bits.common.ftqOffset.get)).map(x => x._1 := x._2)
  io.fromPcTargetMem.fromDataPathValid := pcReadValid
  io.fromPcTargetMem.fromDataPathFtqPtr := pcReadFtqPtr
  io.fromPcTargetMem.fromDataPathFtqOffset := pcReadFtqOffset

  private val intDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, (Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W)))))
  private val fpDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, (Wire(Vec(32, UInt(fpSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W)))))
  private val vfDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, (Wire(Vec(31, UInt(vfSchdParams.pregIdxWidth.W))), Wire(Vec(31, UInt(VLEN.W)))))
  private val v0DebugRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, (Wire(Vec(1, UInt(log2Up(V0PhyRegs).W))), Wire(Vec(1, UInt(V0Data().dataWidth.W)))))
  private val vlDebugRead: Option[(Vec[UInt], Vec[UInt])] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, (Wire(Vec(1, UInt(log2Up(VlPhyRegs).W))), Wire(Vec(1, UInt(VlData().dataWidth.W)))))

  private val fpDebugReadData: Option[Vec[UInt]] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, Wire(Vec(32, UInt(XLEN.W))))
  private val vecDebugReadData: Option[Vec[UInt]] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, Wire(Vec(64, UInt(64.W)))) // v0 = Cat(Vec(1), Vec(0))
  private val vlDebugReadData: Option[UInt] =
    OptionWrapper(env.AlwaysBasicDiff || env.EnableDifftest, Wire(UInt(VlData().dataWidth.W)))


  fpDebugReadData.foreach(_ := fpDebugRead
    .get._2
    .slice(0, 32)
    .map(_(63, 0))
  ) // fp only used [63, 0]
  vecDebugReadData.foreach(_ := 
    v0DebugRead
    .get._2
    .slice(0, 1)
    .map(x => Seq(x(63, 0), x(127, 64))).flatten ++ 
    vfDebugRead
    .get._2
    .slice(0, 31)
    .map(x => Seq(x(63, 0), x(127, 64))).flatten
  )
  vlDebugReadData.foreach(_ := vlDebugRead
    .get._2(0)
  )

  io.debugVl.foreach(_ := vlDebugReadData.get)

  IntRegFile("IntRegFile", intSchdParams.numPregs, intRfRaddr, intRfRdata, intRfWen, intRfWaddr, intRfWdata,
    bankNum = 1,
    debugReadAddr = intDebugRead.map(_._1),
    debugReadData = intDebugRead.map(_._2)
  )
  FpRegFile("FpRegFile", fpSchdParams.numPregs, fpRfRaddr, fpRfRdata, fpRfWen, fpRfWaddr, fpRfWdata,
    bankNum = 1,
    debugReadAddr = fpDebugRead.map(_._1),
    debugReadData = fpDebugRead.map(_._2)
  )
  VfRegFile("VfRegFile", vfSchdParams.numPregs, vfRfSplitNum, vfRfRaddr, vfRfRdata, vfRfWen, vfRfWaddr, vfRfWdata,
    debugReadAddr = vfDebugRead.map(_._1),
    debugReadData = vfDebugRead.map(_._2)
  )
  VfRegFile("V0RegFile", V0PhyRegs, v0RfSplitNum, v0RfRaddr, v0RfRdata, v0RfWen, v0RfWaddr, v0RfWdata,
    debugReadAddr = v0DebugRead.map(_._1),
    debugReadData = v0DebugRead.map(_._2)
  )
  FpRegFile("VlRegFile", VlPhyRegs, vlRfRaddr, vlRfRdata, vlRfWen, vlRfWaddr, vlRfWdata,
    bankNum = 1,
    debugReadAddr = vlDebugRead.map(_._1),
    debugReadData = vlDebugRead.map(_._2)
  )

  intRfWaddr := io.fromIntWb.map(x => RegEnable(x.addr, x.wen)).toSeq
  intRfWdata := io.fromIntWb.map(x => RegEnable(x.data, x.wen)).toSeq
  intRfWen := RegNext(VecInit(io.fromIntWb.map(_.wen).toSeq))

  for (portIdx <- intRfRaddr.indices) {
    if (intRFReadArbiter.io.out.isDefinedAt(portIdx))
      intRfRaddr(portIdx) := intRFReadArbiter.io.out(portIdx).bits.addr
    else
      intRfRaddr(portIdx) := 0.U
  }

  fpRfWaddr := io.fromFpWb.map(x => RegEnable(x.addr, x.wen)).toSeq
  fpRfWdata := io.fromFpWb.map(x => RegEnable(x.data, x.wen)).toSeq
  fpRfWen := RegNext(VecInit(io.fromFpWb.map(_.wen).toSeq))

  for (portIdx <- fpRfRaddr.indices) {
    if (fpRFReadArbiter.io.out.isDefinedAt(portIdx))
      fpRfRaddr(portIdx) := fpRFReadArbiter.io.out(portIdx).bits.addr
    else
      fpRfRaddr(portIdx) := 0.U
  }

  vfRfWaddr := io.fromVfWb.map(x => RegEnable(x.addr, x.wen)).toSeq
  vfRfWdata := io.fromVfWb.map(x => RegEnable(x.data, x.wen)).toSeq
  vfRfWen.foreach(_.zip(io.fromVfWb.map(x => RegNext(x.wen))).foreach { case (wenSink, wenSource) => wenSink := wenSource } )

  for (portIdx <- vfRfRaddr.indices) {
    if (vfRFReadArbiter.io.out.isDefinedAt(portIdx))
      vfRfRaddr(portIdx) := vfRFReadArbiter.io.out(portIdx).bits.addr
    else
      vfRfRaddr(portIdx) := 0.U
  }

  v0RfWaddr := io.fromV0Wb.map(_.addr).toSeq
  v0RfWdata := io.fromV0Wb.map(_.data).toSeq
  v0RfWen.foreach(_.zip(io.fromV0Wb.map(_.wen)).foreach { case (wenSink, wenSource) => wenSink := wenSource } )

  for (portIdx <- v0RfRaddr.indices) {
    if (v0RFReadArbiter.io.out.isDefinedAt(portIdx))
      v0RfRaddr(portIdx) := v0RFReadArbiter.io.out(portIdx).bits.addr
    else
      v0RfRaddr(portIdx) := 0.U
  }

  vlRfWaddr := io.fromVlWb.map(_.addr).toSeq
  vlRfWdata := io.fromVlWb.map(_.data).toSeq
  vlRfWen := io.fromVlWb.map(_.wen).toSeq

  for (portIdx <- vlRfRaddr.indices) {
    if (vlRFReadArbiter.io.out.isDefinedAt(portIdx))
      vlRfRaddr(portIdx) := vlRFReadArbiter.io.out(portIdx).bits.addr
    else
      vlRfRaddr(portIdx) := 0.U
  }


  intDebugRead.foreach { case (addr, _) =>
    addr := io.debugIntRat.get
  }

  fpDebugRead.foreach { case (addr, _) =>
    addr := io.debugFpRat.get
  }

  vfDebugRead.foreach { case (addr, _) =>
    addr := io.debugVecRat.get
  }
  v0DebugRead.foreach { case (addr, _) =>
    addr := io.debugV0Rat.get
  }
  vlDebugRead.foreach { case (addr, _) =>
    addr := io.debugVlRat.get
  }

  println(s"[DataPath] " +
    s"has intDebugRead: ${intDebugRead.nonEmpty}, " +
    s"has fpDebugRead: ${fpDebugRead.nonEmpty}, " +
    s"has vecDebugRead: ${vfDebugRead.nonEmpty}, " +
    s"has v0DebugRead: ${v0DebugRead.nonEmpty}, " +
    s"has vlDebugRead: ${vlDebugRead.nonEmpty}")

  // regcache
  private val regCache = Module(new RegCache())

  def IssueBundle2RCReadPort(issue: DecoupledIO[IssueQueueIssueBundle]): Vec[RCReadPort] = {
    val readPorts = Wire(Vec(issue.bits.exuParams.numIntSrc, new RCReadPort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth)))
    readPorts.zipWithIndex.foreach{ case (r, idx) =>
      r.ren  := issue.valid && issue.bits.common.dataSources(idx).readRegCache
      r.addr := issue.bits.rcIdx.get(idx)
      r.data := DontCare
    }
    readPorts
  }

  private val regCacheReadReq = fromIntIQ.flatten.filter(_.bits.exuParams.numIntSrc > 0).flatMap(IssueBundle2RCReadPort(_)) ++ 
                                fromMemIQ.flatten.filter(_.bits.exuParams.numIntSrc > 0).flatMap(IssueBundle2RCReadPort(_))
  private val regCacheReadData = regCache.io.readPorts.map(_.data)

  println(s"[DataPath] regCache readPorts size: ${regCache.io.readPorts.size}, regCacheReadReq size: ${regCacheReadReq.size}")
  require(regCache.io.readPorts.size == regCacheReadReq.size, "reg cache's readPorts size should be equal to regCacheReadReq")

  regCache.io.readPorts.zip(regCacheReadReq).foreach{ case (r, req) => 
    r.ren := req.ren
    r.addr := req.addr
  }

  val s1_RCReadData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))
  s1_RCReadData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_RCReadData.zip(toExu).filter(_._2.map(_.bits.params.isIntExeUnit).reduce(_ || _)).flatMap(_._1).flatten
    .zip(regCacheReadData.take(params.getIntExuRCReadSize)).foreach{ case (s1_data, rdata) => 
      s1_data := rdata
    }
  s1_RCReadData.zip(toExu).filter(_._2.map(x => x.bits.params.isMemExeUnit && x.bits.params.readIntRf).reduce(_ || _)).flatMap(_._1).flatten
    .zip(regCacheReadData.takeRight(params.getMemExuRCReadSize)).foreach{ case (s1_data, rdata) => 
      s1_data := rdata
    }

  println(s"[DataPath] s1_RCReadData.int.size: ${s1_RCReadData.zip(toExu).filter(_._2.map(_.bits.params.isIntExeUnit).reduce(_ || _)).flatMap(_._1).flatten.size}, RCRdata.int.size: ${params.getIntExuRCReadSize}")
  println(s"[DataPath] s1_RCReadData.mem.size: ${s1_RCReadData.zip(toExu).filter(_._2.map(x => x.bits.params.isMemExeUnit && x.bits.params.readIntRf).reduce(_ || _)).flatMap(_._1).flatten.size}, RCRdata.mem.size: ${params.getMemExuRCReadSize}")

  io.toWakeupQueueRCIdx := regCache.io.toWakeupQueueRCIdx
  io.toBypassNetworkRCData := s1_RCReadData
  regCache.io.writePorts := io.fromBypassNetwork

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
  val s1_vlPregRData: MixedVec[MixedVec[Vec[UInt]]] = Wire(MixedVec(toExu.map(x => MixedVec(x.map(_.bits.src.cloneType).toSeq))))

  val rfrPortConfigs = schdParams.map(_.issueBlockParams).flatten.map(_.exuBlockParams.map(_.rfrPortConfigs))

  println(s"[DataPath] s1_intPregRData.flatten.flatten.size: ${s1_intPregRData.flatten.flatten.size}, intRfRdata.size: ${intRfRdata.size}")
  s1_intPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_intPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[IntRD]) > 0 }
          .foreach { case (sink, cfg) => sink := intRfRdata(cfg.find(_.isInstanceOf[IntRD]).get.port) }
      }
  }

  println(s"[DataPath] s1_fpPregRData.flatten.flatten.size: ${s1_fpPregRData.flatten.flatten.size}, fpRfRdata.size: ${fpRfRdata.size}")
  s1_fpPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_fpPregRData.zip(rfrPortConfigs).foreach { case (iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach { case (iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[FpRD]) > 0 }
          .foreach { case (sink, cfg) => sink := fpRfRdata(cfg.find(_.isInstanceOf[FpRD]).get.port) }
      }
  }

  println(s"[DataPath] s1_vfPregRData.flatten.flatten.size: ${s1_vfPregRData.flatten.flatten.size}, vfRfRdata.size: ${vfRfRdata.size}")
  s1_vfPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_vfPregRData.zip(rfrPortConfigs).foreach{ case(iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach{ case(iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[VfRD]) > 0 }
          .foreach { case (sink, cfg) => sink := vfRfRdata(cfg.find(_.isInstanceOf[VfRD]).get.port) }
      }
  }

  println(s"[DataPath] s1_v0PregRData.flatten.flatten.size: ${s1_v0PregRData.flatten.flatten.size}, v0RfRdata.size: ${v0RfRdata.size}")
  s1_v0PregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_v0PregRData.zip(rfrPortConfigs).foreach{ case(iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach{ case(iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[V0RD]) > 0 }
          .foreach { case (sink, cfg) => sink := v0RfRdata(cfg.find(_.isInstanceOf[V0RD]).get.port) }
      }
  }

  println(s"[DataPath] s1_vlPregRData.flatten.flatten.size: ${s1_vlPregRData.flatten.flatten.size}, vlRfRdata.size: ${vlRfRdata.size}")
  s1_vlPregRData.foreach(_.foreach(_.foreach(_ := 0.U)))
  s1_vlPregRData.zip(rfrPortConfigs).foreach{ case(iqRdata, iqCfg) =>
      iqRdata.zip(iqCfg).foreach{ case(iuRdata, iuCfg) =>
        iuRdata.zip(iuCfg)
          .filter { case (_, cfg) => cfg.count(_.isInstanceOf[VlRD]) > 0 }
          .foreach { case (sink, cfg) => sink := vlRfRdata(cfg.find(_.isInstanceOf[VlRD]).get.port) }
      }
  }

  val og0_cancel_no_load = VecInit(og0FailedVec2.flatten.zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1).toSeq)
  val exuParamsNoLoad = fromIQ.flatten.zip(params.allExuParams).filter(!_._2.hasLoadFu)
  val is_0latency = Wire(Vec(og0_cancel_no_load.size, Bool()))
  is_0latency := exuParamsNoLoad.map(x => is0latency(x._1.bits.common.fuType))
  val og0_cancel_delay = RegNext(VecInit(og0_cancel_no_load.zip(is_0latency).map(x => x._1 && x._2)))
  val isVfScheduler = VecInit(exuParamsNoLoad.map(x => x._2.schdType.isInstanceOf[VfScheduler].B))
  val og0_cancel_delay_for_mem = VecInit(og0_cancel_delay.zip(isVfScheduler).map(x => x._1 && !x._2))
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
      srcNotBlock := s0.bits.common.dataSources.zip(intRdArbWinner(i)(j) zip fpRdArbWinner(i)(j) zip vfRdArbWinner(i)(j) zip v0RdArbWinner(i)(j) zip vlRdArbWinner(i)(j)).map {
        case (source, ((((win_int, win_fp), win_vf), win_v0), win_vl)) =>
        !source.readReg || win_int && win_fp && win_vf && win_v0 && win_vl
      }.fold(true.B)(_ && _)
      val notBlock = srcNotBlock && intWbNotBlock(i)(j) && fpWbNotBlock(i)(j) && vfWbNotBlock(i)(j) && v0WbNotBlock(i)(j) && vlWbNotBlock(i)(j)
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      val s1_cancel = og1FailedVec2(i)(j)
      val s0_cancel = Wire(Bool())
      val og0_cancel_delay_need = if (s0.bits.exuParams.schdType.isInstanceOf[MemScheduler]) og0_cancel_delay_for_mem else og0_cancel_delay
      if (s0.bits.exuParams.isIQWakeUpSink) {
        val exuOHNoLoad = s0.bits.common.l1ExuOH.get.map(x => x.asTypeOf(Vec(x.getWidth, Bool())).zip(params.allExuParams).filter(!_._2.hasLoadFu).map(_._1))
        s0_cancel := exuOHNoLoad.zip(s0.bits.common.dataSources).map{
          case (exuOH, dataSource) => (VecInit(exuOH).asUInt & og0_cancel_delay_need.asUInt).orR && dataSource.readForward
        }.reduce(_ || _) && s0.valid
      } else s0_cancel := false.B
      val s0_ldCancel = LoadShouldCancel(s0.bits.common.loadDependency, io.ldCancel)
      when (s0.fire && !s1_flush && notBlock && !s1_cancel && !s0_ldCancel && !s0_cancel) {
        s1_valid := s0.valid
      }.otherwise {
        s1_valid := false.B
      }
      when (s0.valid) {
        s1_data.fromIssueBundle(s0.bits) // no src data here
        s1_addrOH := s0.bits.addrOH
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
          og0resp.bits.sqIdx.foreach(_ := 0.U.asTypeOf(new SqPtr))
          og0resp.bits.lqIdx.foreach(_ := 0.U.asTypeOf(new LqPtr))
          og0resp.bits.resp             := RespType.block
          og0resp.bits.fuType           := fromIQ(iqIdx)(iuIdx).bits.common.fuType

          val og1resp = toIU.og1resp
          og1FailedVec2(iqIdx)(iuIdx)   := s1_toExuValid(iqIdx)(iuIdx) && !toExuFire(iqIdx)(iuIdx)
          og1resp.valid                 := s1_toExuValid(iqIdx)(iuIdx)
          og1resp.bits.robIdx           := s1_toExuData(iqIdx)(iuIdx).robIdx
          og1resp.bits.uopIdx.foreach(_ := s1_toExuData(iqIdx)(iuIdx).vpu.get.vuopIdx)
          og1resp.bits.sqIdx.foreach(_ :=  0.U.asTypeOf(new SqPtr))
          og1resp.bits.lqIdx.foreach(_ :=  0.U.asTypeOf(new LqPtr))
          // respType:  fuIdle      ->IQ entry clear
          //            fuUncertain ->IQ entry no action
          //            fuBusy      ->IQ entry issued set false, then re-issue
          // hyu, lda and sta are fuUncertain at OG1 stage
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
                    if (params.isIQWakeUpSource && params.latencyCertain && params.wakeUpFuLatancySet.contains(0)) cancel else false.B
                  }.toSeq
  io.og1Cancel := toFlattenExu.map(x => x.valid && !x.fire)


  if (backendParams.debugEn){
    dontTouch(og0_cancel_no_load)
    dontTouch(is_0latency)
    dontTouch(og0_cancel_delay)
    dontTouch(isVfScheduler)
    dontTouch(og0_cancel_delay_for_mem)
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
        val readRfMap: Seq[(Bool, UInt)] = (
          if (k == 3) {(
            Seq(None)
            :+
            OptionWrapper(s1_v0PregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(V0RegSrcDataSet).nonEmpty, 
              (SrcType.isV0(s1_srcType(i)(j)(k)) -> s1_v0PregRData(i)(j)(k)))
          )}
          else if (k == 4) {(
            Seq(None)
            :+
            OptionWrapper(s1_vlPregRData(i)(j).isDefinedAt(k) && srcDataTypeSet.intersect(VlRegSrcDataSet).nonEmpty, 
              (SrcType.isVp(s1_srcType(i)(j)(k)) -> s1_vlPregRData(i)(j)(k)))
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
  XSPerfHistogram(s"FpRegFileRead_hist", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"VfRegFileRead_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 20, 1)
  XSPerfHistogram(s"IntRegFileWrite_hist", PopCount(intRFWriteReq.flatten), true.B, 0, 20, 1)
  XSPerfHistogram(s"FpRegFileWrite_hist", PopCount(fpRFWriteReq.flatten), true.B, 0, 20, 1)
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
  XSPerfAccumulate(s"FpRFReadBeforeArb", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)))
  XSPerfAccumulate(s"FpRFReadAfterArb", PopCount(fpRFReadArbiter.io.out.map(_.valid)))
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
  XSPerfHistogram(s"FpRFReadBeforeArb_hist", PopCount(fpRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"FpRFReadAfterArb_hist", PopCount(fpRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadBeforeArb_hist", PopCount(vfRFReadArbiter.io.in.flatten.flatten.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"VfRFReadAfterArb_hist", PopCount(vfRFReadArbiter.io.out.map(_.valid)), true.B, 0, 16, 2)
  XSPerfHistogram(s"IntUopBeforeArb_hist", PopCount(fromIntIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"IntUopAfterArb_hist", PopCount(fromIntIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
  XSPerfHistogram(s"MemUopBeforeArb_hist", PopCount(fromMemIQ.flatten.map(_.valid)), true.B, 0, 8, 2)
  XSPerfHistogram(s"MemUopAfterArb_hist", PopCount(fromMemIQ.flatten.map(_.fire)), true.B, 0, 8, 2)
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
      }
    }
    if (exuParams.isMemExeUnit && exuParams.readIntRf) {
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_forward",  exu.fire && exu.bits.common.dataSources(0).readForward)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_bypass",   exu.fire && exu.bits.common.dataSources(0).readBypass)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_regcache", exu.fire && exu.bits.common.dataSources(0).readRegCache)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_reg",      exu.fire && exu.bits.common.dataSources(0).readReg)
      XSPerfAccumulate(s"MEM_ExuId${exuParams.exuIdx}_src0_dataSource_zero",     exu.fire && exu.bits.common.dataSources(0).readZero)
    }
  })
}

class DataPathIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())
  private val memSchdParams = params.schdParams(MemScheduler())
  // bundles
  val hartId = Input(UInt(8.W))

  val flush: ValidIO[Redirect] = Flipped(ValidIO(new Redirect))

  val wbConfictRead = Input(MixedVec(params.allSchdParams.map(x => MixedVec(x.issueBlockParams.map(x => x.genWbConflictBundle())))))

  val fromIntIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(intSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromFpIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(fpSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromMemIQ: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] =
    Flipped(MixedVec(memSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromVfIQ = Flipped(MixedVec(vfSchdParams.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val toIntIQ = MixedVec(intSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toFpIQ = MixedVec(fpSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toMemIQ = MixedVec(memSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val toVfIQ = MixedVec(vfSchdParams.issueBlockParams.map(_.genOGRespBundle))

  val og0Cancel = Output(ExuVec())

  val og1Cancel = Output(ExuVec())

  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val toIntExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputBundle

  val toFpExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(fpSchdParams.genExuInputBundle)

  val toVecExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(vfSchdParams.genExuInputBundle)

  val toMemExu: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = memSchdParams.genExuInputBundle

  val og1ImmInfo: Vec[ImmInfo] = Output(Vec(params.allExuParams.size, new ImmInfo))

  val fromIntWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genIntWriteBackBundle)

  val fromFpWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genFpWriteBackBundle)

  val fromVfWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVfWriteBackBundle)

  val fromV0Wb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genV0WriteBackBundle)

  val fromVlWb: MixedVec[RfWritePortWithConfig] = MixedVec(params.genVlWriteBackBundle)

  val fromPcTargetMem = Flipped(new PcToDataPathIO(params))

  val fromBypassNetwork: Vec[RCWritePort] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn)
  )

  val toBypassNetworkRCData: MixedVec[MixedVec[Vec[UInt]]] = MixedVec(
    Seq(intSchdParams, fpSchdParams, vfSchdParams, memSchdParams).map(schd => schd.issueBlockParams.map(iq => 
      MixedVec(iq.exuBlockParams.map(exu => Output(Vec(exu.numRegSrc, UInt(exu.srcDataBitsMax.W)))))
    )).flatten
  )

  val toWakeupQueueRCIdx: Vec[UInt] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    Output(UInt(RegCacheIdxWidth.W))
  )

  val debugIntRat  = if (params.debugEn) Some(Input(Vec(32, UInt(intSchdParams.pregIdxWidth.W)))) else None
  val debugFpRat   = if (params.debugEn) Some(Input(Vec(32, UInt(fpSchdParams.pregIdxWidth.W)))) else None
  val debugVecRat  = if (params.debugEn) Some(Input(Vec(31, UInt(vfSchdParams.pregIdxWidth.W)))) else None
  val debugV0Rat   = if (params.debugEn) Some(Input(Vec(1, UInt(log2Up(V0PhyRegs).W)))) else None
  val debugVlRat   = if (params.debugEn) Some(Input(Vec(1, UInt(log2Up(VlPhyRegs).W)))) else None
  val debugVl      = if (params.debugEn) Some(Output(UInt(VlData().dataWidth.W))) else None
}
