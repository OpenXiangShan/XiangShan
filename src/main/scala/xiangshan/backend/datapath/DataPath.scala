package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Data, _}
import chisel3.util._
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState, DifftestArchVecRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.{ImmExtractor, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.Bundles._
import xiangshan.backend.regfile._
import xiangshan.backend.datapath.WbConfig.{IntWB, PregWB, VfWB}

class WbBusyArbiterIO(inPortSize: Int, outPortSize: Int)(implicit p: Parameters) extends XSBundle {
  val in = Vec(inPortSize, Flipped(DecoupledIO(new Bundle{}))) // TODO: remote the bool
  val flush = Flipped(ValidIO(new Redirect))
}

class WbBusyArbiter(isInt: Boolean)(implicit p: Parameters) extends XSModule {
  val allExuParams = backendParams.allExuParams

  val portConfigs = allExuParams.flatMap(_.wbPortConfigs).filter{
    wbPortConfig =>
      if(isInt){
        wbPortConfig.isInstanceOf[IntWB]
      }
      else{
        wbPortConfig.isInstanceOf[VfWB]
      }
  }

  val numRfWrite = if (isInt) backendParams.numIntWb else backendParams.numVfWb

  val io = IO(new WbBusyArbiterIO(portConfigs.size, numRfWrite))
  // inGroup[port -> Bundle]
  val inGroup = io.in.zip(portConfigs).groupBy{ case(port, config) => config.port}
  // sort by priority
  val inGroupSorted = inGroup.map{
    case(key, value) => (key -> value.sortBy{ case(port, config) => config.asInstanceOf[PregWB].priority})
  }

  private val arbiters = Seq.tabulate(numRfWrite) { x => {
    if (inGroupSorted.contains(x)) {
      Some(Module(new Arbiter( new Bundle{} ,n = inGroupSorted(x).length)))
    } else {
      None
    }
  }}

  arbiters.zipWithIndex.foreach { case (arb, i) =>
    if (arb.nonEmpty) {
      arb.get.io.in.zip(inGroupSorted(i).map(_._1)).foreach { case (arbIn, addrIn) =>
        arbIn <> addrIn
      }
    }
  }

  arbiters.foreach(_.foreach(_.io.out.ready := true.B))
}

class RFArbiterBundle(addrWidth: Int)(implicit p: Parameters) extends XSBundle {
  val addr = UInt(addrWidth.W)
}

class RFReadArbiterIO(inPortSize: Int, outPortSize: Int, pregWidth: Int)(implicit p: Parameters) extends XSBundle {
  val in = Vec(inPortSize, Flipped(DecoupledIO(new RFArbiterBundle(pregWidth))))
  val out = Vec(outPortSize, Valid(new RFArbiterBundle(pregWidth)))
  val flush = Flipped(ValidIO(new Redirect))
}

class RFReadArbiter(isInt: Boolean)(implicit p: Parameters) extends XSModule {
  val allExuParams = backendParams.allExuParams

  val portConfigs: Seq[RdConfig] = allExuParams.map(_.rfrPortConfigs.flatten).flatten.filter{
    rfrPortConfigs =>
      if(isInt){
        rfrPortConfigs.isInstanceOf[IntRD]
      }
      else{
        rfrPortConfigs.isInstanceOf[VfRD]
      }
  }

  private val moduleName = this.getClass.getName + (if (isInt) "Int" else "Vf")

  println(s"[$moduleName] ports(${portConfigs.size})")
  for (portCfg <- portConfigs) {
    println(s"[$moduleName] port: ${portCfg.port}, priority: ${portCfg.priority}")
  }

  val pregParams = if(isInt) backendParams.intPregParams else backendParams.vfPregParams

  val io = IO(new RFReadArbiterIO(portConfigs.size, backendParams.numRfRead, pregParams.addrWidth))
  // inGroup[port -> Bundle]
  val inGroup: Map[Int, IndexedSeq[(DecoupledIO[RFArbiterBundle], RdConfig)]] = io.in.zip(portConfigs).groupBy{ case(port, config) => config.port}
  // sort by priority
  val inGroupSorted: Map[Int, IndexedSeq[(DecoupledIO[RFArbiterBundle], RdConfig)]] = inGroup.map{
    case(key, value) => (key -> value.sortBy{ case(port, config) => config.priority})
  }

  private val arbiters: Seq[Option[Arbiter[RFArbiterBundle]]] = Seq.tabulate(backendParams.numRfRead) { x => {
    if (inGroupSorted.contains(x)) {
      Some(Module(new Arbiter(new RFArbiterBundle(pregParams.addrWidth), inGroupSorted(x).length)))
    } else {
      None
    }
  }}

  arbiters.zipWithIndex.foreach { case (arb, i) =>
    if (arb.nonEmpty) {
      arb.get.io.in.zip(inGroupSorted(i).map(_._1)).foreach { case (arbIn, addrIn) =>
        arbIn <> addrIn
      }
    }
  }

  io.out.zip(arbiters).foreach { case (addrOut, arb) =>
    if (arb.nonEmpty) {
      val arbOut = arb.get.io.out
      arbOut.ready := true.B
      addrOut.valid := arbOut.valid
      addrOut.bits := arbOut.bits
    } else {
      addrOut := 0.U.asTypeOf(addrOut)
    }
  }
}

class DataPath(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  private implicit val dpParams: BackendParams = params
  lazy val module = new DataPathImp(this)
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

  private val intWbBusyArbiter = Module(new WbBusyArbiter(true))
  private val vfWbBusyArbiter = Module(new WbBusyArbiter(false))
  private val intRFReadArbiter = Module(new RFReadArbiter(true))
  private val vfRFReadArbiter = Module(new RFReadArbiter(false))

  private val issuePortsIn = fromIQ.flatten
  private val intNotBlocksW = fromIQ.map { case iq => Wire(Vec(iq.size, Bool())) }
  private val intNotBlocksSeqW = intNotBlocksW.flatten
  private val vfNotBlocksW = fromIQ.map { case iq => Wire(Vec(iq.size, Bool())) }
  private val vfNotBlocksSeqW = vfNotBlocksW.flatten
  private val intBlocks = fromIQ.map{ case iq => Wire(Vec(iq.size, Bool())) }
  private val intBlocksSeq = intBlocks.flatten
  private val vfBlocks = fromIQ.map { case iq => Wire(Vec(iq.size, Bool())) }
  private val vfBlocksSeq = vfBlocks.flatten
  private val intWbConflictReads = io.wbConfictRead.flatten.flatten.map(_.intConflict)
  private val vfWbConflictReads = io.wbConfictRead.flatten.flatten.map(_.vfConflict)

  val intWbBusyInSize = issuePortsIn.map(issuePortIn => issuePortIn.bits.getIntWbBusyBundle.size).scan(0)(_ + _)
  val intReadPortInSize: IndexedSeq[Int] = issuePortsIn.map(issuePortIn => issuePortIn.bits.getIntRfReadBundle.size).scan(0)(_ + _)
  issuePortsIn.zipWithIndex.foreach{
    case (issuePortIn, idx) =>
      val wbBusyIn: Seq[Bool] = issuePortIn.bits.getIntWbBusyBundle
      val lw = intWbBusyInSize(idx)
      val rw = intWbBusyInSize(idx + 1)
      val arbiterInW = intWbBusyArbiter.io.in.slice(lw, rw)
      arbiterInW.zip(wbBusyIn).foreach {
        case (sink, source) =>
          sink.bits := DontCare
          sink.valid := issuePortIn.valid && source
      }
       val notBlockFlag = if (rw > lw) {
        val arbiterRes = arbiterInW.zip(wbBusyIn).map {
          case (sink, source) => sink.ready
        }.reduce(_ & _)
        if (intWbConflictReads(idx).isDefined) {
          Mux(intWbConflictReads(idx).get, arbiterRes, true.B)
        } else arbiterRes
      } else true.B
      intNotBlocksSeqW(idx) := notBlockFlag
      val readPortIn = issuePortIn.bits.getIntRfReadBundle
      val l = intReadPortInSize(idx)
      val r = intReadPortInSize(idx + 1)
      val arbiterIn = intRFReadArbiter.io.in.slice(l, r)
      arbiterIn.zip(readPortIn).foreach{
        case(sink, source) =>
          sink.bits.addr := source.addr
          sink.valid := issuePortIn.valid && SrcType.isXp(source.srcType)
      }
      if(r > l){
        intBlocksSeq(idx) := !arbiterIn.zip(readPortIn).map {
          case (sink, source) => Mux(SrcType.isXp(source.srcType), sink.ready, true.B)
        }.reduce(_ & _)
      }
      else{
        intBlocksSeq(idx) := false.B
      }
  }
  intWbBusyArbiter.io.flush := io.flush
  intRFReadArbiter.io.flush := io.flush

  val vfWbBusyInSize = issuePortsIn.map(issuePortIn => issuePortIn.bits.getVfWbBusyBundle.size).scan(0)(_ + _)
  val vfReadPortInSize: IndexedSeq[Int] = issuePortsIn.map(issuePortIn => issuePortIn.bits.getVfRfReadBundle.size).scan(0)(_ + _)
  println(s"vfReadPortInSize: $vfReadPortInSize")

  issuePortsIn.zipWithIndex.foreach {
    case (issuePortIn, idx) =>
      val wbBusyIn = issuePortIn.bits.getVfWbBusyBundle
      val lw = vfWbBusyInSize(idx)
      val rw = vfWbBusyInSize(idx + 1)
      val arbiterInW = vfWbBusyArbiter.io.in.slice(lw, rw)
      arbiterInW.zip(wbBusyIn).foreach {
        case (sink, source) =>
          sink.bits := DontCare
          sink.valid := issuePortIn.valid && source
      }
      val notBlockFlag = if (rw > lw){
        val arbiterRes = arbiterInW.zip(wbBusyIn).map {
          case (sink, source) => sink.ready
        }.reduce(_ & _)
        if(vfWbConflictReads(idx).isDefined) {
          Mux(vfWbConflictReads(idx).get, arbiterRes, true.B)
        }else arbiterRes
      }else true.B
      vfNotBlocksSeqW(idx) := notBlockFlag

      val readPortIn = issuePortIn.bits.getVfRfReadBundle
      val l = vfReadPortInSize(idx)
      val r = vfReadPortInSize(idx + 1)
      val arbiterIn = vfRFReadArbiter.io.in.slice(l, r)
      arbiterIn.zip(readPortIn).foreach {
        case (sink, source) =>
          sink.bits.addr := source.addr
          sink.valid := issuePortIn.valid && SrcType.isVfp(source.srcType)
      }
      if (r > l) {
        vfBlocksSeq(idx) := !arbiterIn.zip(readPortIn).map {
          case (sink, source) => Mux(SrcType.isVfp(source.srcType), sink.ready, true.B)
        }.reduce(_ & _)
      }
      else {
        vfBlocksSeq(idx) := false.B
      }
  }
  vfWbBusyArbiter.io.flush := io.flush
  vfRFReadArbiter.io.flush := io.flush

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

  private val intRfRaddr = Wire(Vec(params.numRfRead, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfRdata = Wire(Vec(params.numRfRead, UInt(intSchdParams.rfDataWidth.W)))
  private val intRfWen = Wire(Vec(io.fromIntWb.length, Bool()))
  private val intRfWaddr = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.pregIdxWidth.W)))
  private val intRfWdata = Wire(Vec(io.fromIntWb.length, UInt(intSchdParams.rfDataWidth.W)))

  private val vfRfSplitNum = VLEN / XLEN
  private val vfRfRaddr = Wire(Vec(params.numRfRead, UInt(vfSchdParams.pregIdxWidth.W)))
  private val vfRfRdata = Wire(Vec(params.numRfRead, UInt(vfSchdParams.rfDataWidth.W)))
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

  intRFReadArbiter.io.out.map(_.bits.addr).zip(intRfRaddr).foreach{ case(source, sink) => sink := source }

  vfRfWaddr := io.fromVfWb.map(_.addr)
  vfRfWdata := io.fromVfWb.map(_.data)
  vfRfWen.foreach(_.zip(io.fromVfWb.map(_.wen)).foreach { case (wenSink, wenSource) => wenSink := wenSource } )// Todo: support fp multi-write

  vfRFReadArbiter.io.out.map(_.bits.addr).zip(vfRfRaddr).foreach{ case(source, sink) => sink := source }
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
      val block = (intBlocks(i)(j) || !intNotBlocksW(i)(j)) || (vfBlocks(i)(j) || !vfNotBlocksW(i)(j))
      val s1_flush = s0.bits.common.robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      when (s0.fire && !s1_flush && !block) {
        s1_valid := s0.valid
        s1_data.fromIssueBundle(s0.bits) // no src data here
        s1_addrOH := s0.bits.addrOH
      }.otherwise {
        s1_valid := false.B
      }
      dontTouch(block)
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
          og0resp.valid := fromIQ(iqIdx)(iuIdx).valid && (!fromIQFire(iqIdx)(iuIdx))
          og0resp.bits.respType := RSFeedbackType.rfArbitFail
          og0resp.bits.addrOH := fromIQ(iqIdx)(iuIdx).bits.addrOH
          og0resp.bits.rfWen := fromIQ(iqIdx)(iuIdx).bits.common.rfWen.getOrElse(false.B)
          og0resp.bits.fuType := fromIQ(iqIdx)(iuIdx).bits.common.fuType

          val og1resp = toIU.og1resp
          og1resp.valid := s1_toExuValid(iqIdx)(iuIdx)
          og1resp.bits.respType := Mux(toExuFire(iqIdx)(iuIdx),
            if (toIU.issueQueueParams.isMemAddrIQ) RSFeedbackType.fuUncertain else RSFeedbackType.fuIdle,
            RSFeedbackType.fuBusy)
          og1resp.bits.addrOH := s1_addrOHs(iqIdx)(iuIdx)
          og1resp.bits.rfWen := s1_toExuData(iqIdx)(iuIdx).rfWen.getOrElse(false.B)
          og1resp.bits.fuType := s1_toExuData(iqIdx)(iuIdx).fuType
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
