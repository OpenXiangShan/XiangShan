/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import difftest.{DiffPhyVecRegState, DifftestModule}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig.RdConfig
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.Vxrm
import xiangshan.backend.regfile.{FpRegFile, PregParams, VfRegFile}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.VecIssueQueue.WakeUpBundle
import xiangshan.backend.vector.VecRegionModule._
import xiangshan.backend.{ExcpModToVprf, VprfToExcpMod}

class VecRegionModule(val regionParam: RegionParam)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  implicit val param: RegionParam = regionParam

  lazy val module = new VecRegionImp(this)

  val issueQueues: Seq[VecIssueQueue.LazyMod] = param.issueParams.map(issueParam =>
    LazyModule(new VecIssueQueue.LazyMod()(p, issueParam))
  )

  val issuePipes: Seq[Seq[IssuePipe.LazyMod]] = param.issueParams.map { case issueParam =>
    issueParam.exuParams.map { case exuParam =>
      LazyModule(new IssuePipe.LazyMod(exuParam))
    }
  }
}

class VecRegionImp(
  override val wrapper: VecRegionModule
)(
  implicit
  p: Parameters, param: RegionParam
) extends LazyModuleImp(wrapper) with HasXSParameter {
  val debugEn = backendParams.debugEn
  val basicDebugEn = backendParams.basicDebugEn

  val numVp = backendParams.vfPregParams.numEntries
  val numVpWritePort = backendParams.getVfRfWriteSize
  val numVpReadPort = backendParams.getVfRfReadSize
  val numVl = backendParams.vlPregParams.numEntries
  val numVlWritePort = backendParams.getVlRfWriteSize
  val numVlReadPort = param.getVlReadCfgs.flatten.count(_ != null)
  val vlWidth = VlData().dataWidth

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  dontTouch(in)
  dontTouch(out)

  val issueQueues: Seq[VecIssueQueue] = wrapper.issueQueues.map(x => x.module.suggestName(x.param.getInstanceNameOfIQ))
  val issuePipes: Seq[Seq[IssuePipe]] = wrapper.issuePipes.map(_.map(x => x.module.suggestName(x.param.getInstanceNameOfPipe)))

  val vpWbDataPath = Module(new WbDataPath(backendParams.vfPregParams))
  val vlWbDataPath = Module(new WbDataPath(backendParams.vlPregParams))
  val v0WbDataPath = Module(new WbDataPath(backendParams.v0PregParams))

  val vpRaddr = Wire(Vec(numVpReadPort, UInt(VfPhyRegIdxWidth.W)))
  val vpRdata = Wire(Vec(numVpReadPort, UInt(VLEN.W)))
  val vpWen   = Wire(Vec(numVpWritePort, Bool()))
  val vpWaddr = Wire(Vec(numVpWritePort, UInt(VfPhyRegIdxWidth.W)))
  val vpWdata = Wire(Vec(numVpWritePort, UInt(VLEN.W)))
  val vpDiffReadData: Option[Vec[UInt]] = Option.when(basicDebugEn)(Wire(Vec(numVp, UInt(VLEN.W))))

  val vlRaddr = Wire(Vec(numVlReadPort, UInt(VlPhyRegIdxWidth.W)))
  val vlRdata = Wire(Vec(numVlReadPort, UInt(vlWidth.W)))
  val vlWen   = Wire(Vec(numVlWritePort, Bool()))
  val vlWaddr = Wire(Vec(numVlWritePort, UInt(VlPhyRegIdxWidth.W)))
  val vlWdata = Wire(Vec(numVlWritePort, UInt(vlWidth.W)))
  val vlDiffReadAddr: Option[Vec[UInt]] = Option.when(basicDebugEn)(Wire(Vec(1, UInt(VlPhyRegIdxWidth.W))))
  val vlDiffReadData: Option[Vec[UInt]] = Option.when(basicDebugEn)(Wire(Vec(1, UInt(vlWidth.W))))

  VfRegFile("VfRegFile", VfPhyRegs, 1, vpRaddr, vpRdata, Seq(vpWen), vpWaddr, vpWdata,
    debugAllRData = vpDiffReadData
  )

  FpRegFile("VlRegFile", VlPhyRegs, vlRaddr, vlRdata, vlWen, vlWaddr, vlWdata,
    bankNum = 1,
    isVlRegfile = true,
    debugReadAddr = vlDiffReadAddr,
    debugReadData = vlDiffReadData,
  )

  issueQueues.filterNot(_.param.hasVStd).zipWithIndex.foreach {
    case (iq, i) =>
      iq.in.flush := in.flush
      iq.in.enq := in.fromDispatch.uops(i)
  }

  issueQueues.filter(_.param.hasVStd).zipWithIndex.foreach {
    case (iq, i) =>
      iq.in.flush := in.flush
      iq.in.enq := in.fromIntRegion.vstdUops(i)
  }

  issueQueues.zipWithIndex.foreach {
    case (iq, i) =>
      iq.in.resps.is0 := issuePipes(i).map(_.out.is0Resp)
      iq.in.resps.is1 := issuePipes(i).map(_.out.is1Resp)
      iq.in.wakeup.gpWbVec := in.fromIntRegion.gpWbWakeUp
      iq.in.wakeup.fpWbVec := in.fromFltRegion.fpWbWakeUp
      iq.in.wakeup.vlWbVec.foreach(_ := in.fromIntRegion.vlWbWakeUp)
      iq.in.wakeup.vpWbVec.zipWithIndex.foreach { case (wakeup, i) =>
        wakeup.wen := vpWbDataPath.out.toRf(i).wen
        wakeup.pdest := vpWbDataPath.out.toRf(i).pdest
        wakeup.delay := 0.U // Todo
      }
      iq.in.wakeup.v0WbVec.foreach(
        _.zipWithIndex.foreach { case (wakeup, i) =>
          wakeup.wen := v0WbDataPath.out.toRf(i).wen
          wakeup.pdest := v0WbDataPath.out.toRf(i).pdest
          wakeup.delay := 0.U // Todo
        }
      )
  }


  (issueQueues lazyZip issuePipes).zipWithIndex.foreach { case ((iq, pipes), iqIdx) =>
    pipes.zipWithIndex.foreach { case (pipe, pipeIdx) =>
      pipe.in.flush := in.flush
      pipe.in.is0Next := iq.out.deq(pipeIdx)
      pipe.in.is0RdFail := false.B // Todo
      pipe.in.is0WtFail := false.B // Todo
      pipe.in.is0LdCancel := false.B // Todo
      pipe.in.is1VpRdDataNext.foreach { case rdata =>
        rdata.data := vpRdata(rdata.rdConfig.port)
      }
      pipe.in.is1VlRdDataNext.foreach { case rdata =>
        rdata.data := vlRdata(rdata.rdConfig.port)
      }
      pipe.in.is2GpRdDataNext := RegNext(in.fromIntRegion.is1GpRdDataNext(iqIdx)(pipeIdx))
      pipe.in.is2FpRdDataNext := RegNext(in.fromFltRegion.is1FpRdDataNext(iqIdx)(pipeIdx))
      pipe.in.is0GpRdFailNext := in.fromIntRegion.is0GpRdDataFail(iqIdx)(pipeIdx)
      pipe.in.is0FpRdFailNext := in.fromFltRegion.is0FpRdDataFail(iqIdx)(pipeIdx)

      pipe.in.frm.foreach(_ := in.fromCSR.frm)
      pipe.in.vxrm.foreach(_ := in.fromCSR.vxrm)
    }
  }

  println(s"[tmp-${this.getClass}] " +
    s"vpWbDataPath.in.fromExus: ${vpWbDataPath.in.fromExus.map(_.map(_.size))} " +
    s"issuePipes vpWbNext: ${issuePipes.map(_.map(_.out.vpWbNext).collect { case x if x.nonEmpty => x.get }.size)}" +
    s"in.fromMem.vpWb: ${in.fromMem.vpWb.map(_.size)}"
  )

  require(
    vpWbDataPath.in.fromExus.flatten.flatten.size ==
      (issuePipes.map(_.map(_.out.vpWbNext).collect { case x if x.nonEmpty => x.get }) ++
        in.fromMem.vpWb).flatten.size
  )

  vpWbDataPath.in.fromExus.flatten.flatten
    .zip(Seq(
      issuePipes.map(_.map(_.out.vpWbNext).collect { case x if x.nonEmpty => x.get }),
      in.fromMem.vpWb,
    ).flatten.flatten)
    .foreach { case (sink, source) => sink := source }

  println(s"[tmp-${this.getClass}] " +
    s"vlWbDataPath.in.fromExus: ${vlWbDataPath.in.fromExus.map(_.map(_.size))} " +
    s"issuePipes vlWbNext: ${issuePipes.map(_.map(_.out.vlWbNext).collect { case x if x.nonEmpty => x.get })}" +
    s"in.fromIntRegion.vlWb: ${in.fromIntRegion.vlWb.size}"
  )

  require(
    vlWbDataPath.in.fromExus.flatten.flatten.size == (
      issuePipes.flatMap(_.map(_.out.vlWbNext).collect { case x if x.nonEmpty => x.get }) ++
      in.fromIntRegion.vlWb
    ).size
  )

  vlWbDataPath.in.fromExus.flatten.flatten
    .zip(Seq(
      issuePipes.flatMap(_.map(_.out.vlWbNext).collect { case x if x.nonEmpty => x.get }),
      in.fromIntRegion.vlWb,
    ).flatten)
    .foreach { case (sink, source) => sink := source }


  v0WbDataPath.in.fromExus.flatten.flatten
    .zip(Seq(
      issuePipes.map(_.map(_.out.v0WbNext).collect { case x if x.nonEmpty => x.get }),
      in.fromMem.v0Wb,
    ).flatten.flatten)
    .foreach { case (sink, source) => sink := source }

  vpWen := vpWbDataPath.out.toRf.map(_.wen)
  vpWaddr := vpWbDataPath.out.toRf.map(_.pdest)
  vpWdata := vpWbDataPath.out.toRf.map(_.data)
  vpRaddr := issuePipes
    .flatMap(_.flatMap(_.out.is0VpRdAddrNext))
    .groupBy(_.rdConfig.port)
    .toSeq
    .sortBy { case (port, raddr) => port }
    .map { case (port, raddrSeq) => raddrSeq.ensuring(_.size == 1).head.addr }

  vlWen := in.fromIntRegion.vlWb.map(_.wen)
  vlWaddr := in.fromIntRegion.vlWb.map(_.pdest)
  vlWdata := in.fromIntRegion.vlWb.map(_.data)
  vlRaddr := issuePipes
    .flatMap(_.flatMap(_.out.is0VlRdAddrNext))
    .groupBy(_.rdConfig.port)
    .toSeq
    .sortBy { case (port, raddr) => port }
    .map { case (port, raddrSeq) => raddrSeq.ensuring(_.size == 1).head.addr }

  out.toDispatch.IQValidNumVec := issueQueues.filterNot(_.param.hasVStd).map(_.out.validNum)
  out.toDispatch.debug.foreach(_.IQValidNumVec := issueQueues.filterNot(_.param.hasVStd).map(_.out.validNum))
  out.toDispatch.debug.foreach(_.IQEnqHasIssuedVec.foreach(_ := 0.U) )
  for ((iq, i) <- issueQueues.filterNot(_.param.hasVStd).zipWithIndex) {
    out.toDispatch.canAccept(i).foreach(_ := iq.out.canAccept)
  }
  for ((iq, i) <- issueQueues.filter(_.param.hasVStd).zipWithIndex) {
    out.toIntRegion.vstdCanAccept(i).foreach(_ := iq.out.canAccept)
  }

  require(
    out.toRob.writeback.flatten.size == issuePipes.flatMap(_.map(_.out.robWbNext)).size,
    s"out.toRob.writeback: ${out.toRob.writeback.map(_.size)}" +
      s"issuePipes: ${issuePipes.map(_.map(_.out.robWbNext).size)}"
  )

  out.toRob.writeback.flatten zip issuePipes.flatMap(_.map(_.out.robWbNext)) foreach {
    case (sink: ValidIO[Exu.ToRob], source: ValidIO[Exu.ToRob]) => sink := source
  }

  out.toMem.ex0 zip issuePipes.map(_.filter(_.param.hasVStd).map(_.out.ex0)) foreach {
    case (sink: MixedVec[ValidIO[Exu.InUop]], source: Seq[ValidIO[Exu.InUop]]) => sink := source
  }

  for (i <- out.gpWbNext.indices) {
    out.gpWbNext(i) := issuePipes(i).map(_.out.gpWbNext).filter(_.nonEmpty).map(_.get)
    out.fpWbNext(i) := issuePipes(i).map(_.out.fpWbNext).filter(_.nonEmpty).map(_.get)
    out.vpWbNext(i) := issuePipes(i).map(_.out.vpWbNext).filter(_.nonEmpty).map(_.get)

    out.v0WbNext(i) := issuePipes(i).map(_.out.v0WbNext).filter(_.nonEmpty).map(_.get)
    out.vlWbNext(i) := issuePipes(i).map(_.out.vlWbNext).filter(_.nonEmpty).map(_.get)
  }

  out.vpWb := vpWbDataPath.out.toRf
  out.v0Wb := v0WbDataPath.out.toRf
  out.vlWb := vlWbDataPath.out.toRf

  out.toVecExcpMod := 0.U.asTypeOf(out.toVecExcpMod)

  out.diff.foreach(_.diffVl := vlDiffReadData.get.head)
  vlDiffReadAddr.foreach(_ := in.diff.get.diffVlRat)

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val diffNumWidth = 64
    val vecDiffNumPregs = VLEN / diffNumWidth * backendParams.vfPregParams.numEntries
    val vecDiffReadData: Option[Vec[UInt]] =
      Option.when(backendParams.basicDebugEn)(Wire(Vec(vecDiffNumPregs, UInt(diffNumWidth.W))))

    vecDiffReadData.foreach(_ :=
      vpDiffReadData
        .get
        .flatMap(x => Seq(x(63, 0), x(127, 64)))
    )

    val difftest = DifftestModule(new DiffPhyVecRegState(vecDiffNumPregs), delay = 2)
    difftest.coreid := in.fromTop.hartId
    difftest.value := vecDiffReadData.get
  }

}

object VecRegionModule {
  class In(implicit p: Parameters, param: RegionParam) extends XSBundle {
    val intRegion = backendParams.getIntRegionParam
    val fltRegion = backendParams.getFltRegionParam

    val fpSchdParam = backendParams.fpSchdParams.get

    val fromTop = new Bundle {
      val hartId = UInt(8.W)
    }
    val flush = ValidIO(new Redirect)
    val fromDispatch = new Bundle {
      val uops: MixedVec[Vec[ValidIO[VecIssueQueue.Enq]]] = MixedVec(
        param.issueParams.filterNot(_.hasVStd).map(x => Vec(x.numEnq, ValidIO(new VecIssueQueue.Enq()(p, x))))
      )
    }
    val fromMem = new Bundle {
      val wakeUp = Vec(backendParams.LdExuCnt, Valid(new MemWakeUpBundle))
      val vpWb: MixedVec[MixedVec[Exu.ToRf]] = intRegion.genExuToRfBundle(backendParams.vfPregParams)
      val v0Wb: MixedVec[MixedVec[Exu.ToRf]] = intRegion.genExuToRfBundle(backendParams.v0PregParams)
      val vstdWb = param.genExuToRobBundle(ValidIO(_), _.hasVStd)
    }

    val fromIntRegion = new Bundle {
      val vstdUops: MixedVec[Vec[ValidIO[VecIssueQueue.Enq]]] = MixedVec(
        param.issueParams.filter(_.numVStd > 0).map(x => Flipped(Vec(x.numEnq, ValidIO(new VecIssueQueue.Enq()(p, x)))))
      )
      val gpWbWakeUp = Vec(backendParams.getIntRfWriteSize, new WakeUpBundle(IntPhyRegIdxWidth, IntData()))
      val vlWbWakeUp = Vec(backendParams.getVlRfWriteSize, new WakeUpBundle(VlPhyRegIdxWidth, VlData()))
      val vlWb: MixedVec[Exu.ToRf] = MixedVec(intRegion.genExuToRfBundle(backendParams.vlPregParams).flatten)
      val is0GpRdDataFail: MixedVec[MixedVec[Vec[Bool]]] = param.genRfRdFailBundle(backendParams.intPregParams)
      val is1GpRdDataNext: MixedVec[MixedVec[MixedVec[IssuePipe.RfReadDataBundle]]] = param.genRfRdDataBundle(backendParams.intPregParams)
      // Todo: bypass data
    }

    val fromFltRegion = new Bundle {
      val is0FpRdDataFail: MixedVec[MixedVec[Vec[Bool]]] = param.genRfRdFailBundle(backendParams.fpPregParams)
      val is1FpRdDataNext: MixedVec[MixedVec[MixedVec[IssuePipe.RfReadDataBundle]]] = param.genRfRdDataBundle(backendParams.fpPregParams)
      val fpWbWakeUp = Vec(backendParams.getFpRfWriteSize, new WakeUpBundle(FpPhyRegIdxWidth, FpData()))
      // Todo: bypass data
    }

    val fromCSR = new Bundle {
      val vxrm = Vxrm()
      val frm = Frm()
    }

    val fromVecExcpMod = new ExcpModToVprf(maxMergeNumPerCycle * 2, maxMergeNumPerCycle)

    val diff = Option.when(backendParams.basicDebugEn)(new DiffIn)
  }

  class Out(implicit p: Parameters, param: RegionParam) extends XSBundle {
    val numDeq: Int = param.issueParams.map(_.numDeq).sum
    val numEntry: Int = param.issueParams.map(_.numEntry).max
    val numIQ: Int = param.issueParams.count(x => !x.hasVStd)

    val toDispatch = new Bundle {
      val IQValidNumVec: Vec[UInt] = Vec(
        param.issueParams.filterNot(_.hasVStd).map(_.numDeq).sum,
        UInt(numEntry.U.getWidth.W)
      )

      val canAccept: MixedVec[Vec[Bool]] = MixedVec(
        param.issueParams.filterNot(_.hasVStd).map(x => Flipped(Vec(x.numEnq, Bool())))
      )

      val debug = Option.when(backendParams.debugEn)(new Bundle {
        val IQValidNumVec = Vec(numIQ, UInt(numEntry.U.getWidth.W))
        val IQEnqHasIssuedVec = Vec(numIQ, Output(Bool()))
      })
    }

    val toIntRegion = new Bundle {
      val vstdCanAccept: MixedVec[Vec[Bool]] = MixedVec(
        param.issueParams.filter(_.hasVStd).map(x => Vec(x.numEnq, Bool()))
      )
    }

    val toMem = new Bundle {
      val ex0: MixedVec[MixedVec[ValidIO[Exu.InUop]]] = param.genExuInputBundle(ValidIO(_), _.hasVStd)
    }

    val toRob = new Bundle {
      val writeback: MixedVec[MixedVec[ValidIO[Exu.ToRob]]] = param.genExuToRobBundle(ValidIO(_))
    }
    val gpWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.intPregParams)
    val fpWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.fpPregParams)
    val vpWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.vfPregParams)
    val v0WbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.v0PregParams)
    val vlWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.vlPregParams)

    /**
     * [[vpWb]] is after WbDataPath while [[vpWbNext]] is before WbDataPath
     */
    val vpWb: Vec[RfWriteBundle] = Vec(
      backendParams.getRfWriteSize(backendParams.vfPregParams.dataCfg),
      new RfWriteBundle(backendParams.vfPregParams)
    )

    val v0Wb: Vec[RfWriteBundle] = Vec(
      backendParams.getRfWriteSize(backendParams.v0PregParams.dataCfg),
      new RfWriteBundle(backendParams.v0PregParams)
    )

    val vlWb: Vec[RfWriteBundle] = Vec(
      backendParams.getRfWriteSize(backendParams.vlPregParams.dataCfg),
      new RfWriteBundle(backendParams.vlPregParams)
    )

    val toVecExcpMod = new VprfToExcpMod(maxMergeNumPerCycle * 2)

    val diff = Option.when(backendParams.basicDebugEn)(new DiffOut)
  }

  class DiffIn(implicit p: Parameters) extends XSBundle {
    val diffVlRat = Vec(1, UInt(log2Up(VlPhyRegs).W))
  }

  class DiffOut(implicit p: Parameters) extends XSBundle {
    val diffVl = UInt(VlData().dataWidth.W)
  }

  class RfWriteBundle(val pregParams: PregParams) extends Bundle {
    private val dataWidth = pregParams.dataCfg.dataWidth
    private val addrWidth = pregParams.addrWidth

    val wen = Bool()
    val pdest = UInt(addrWidth.W)
    val data = UInt(dataWidth.W)
  }

  class RfArbiterBundle(val rdConfig: RdConfig)(implicit p: Parameters) extends XSBundle {
    val ren = Bool()
    val addr = UInt(PhyRegIdxWidth.W)
    val robIdx = new RobPtr()
  }

  object RfArbiterBundle {
    def apply(rdConfig: RdConfig)(ren: Bool, addr: UInt, robIdx: RobPtr)(implicit p: Parameters): RfArbiterBundle = {
      val rfArbiterBundle = Wire(new RfArbiterBundle(rdConfig))
      rfArbiterBundle.ren := ren
      rfArbiterBundle.addr := addr
      rfArbiterBundle.robIdx := robIdx
      rfArbiterBundle
    }
  }

  class DebugBundle(implicit p: Parameters) extends XSBundle {
    val debug          = new xiangshan.DebugBundle
    val perfDebugInfo  = new PerfDebugInfo()
    val seqNum         = InstSeqNum()
  }
}
