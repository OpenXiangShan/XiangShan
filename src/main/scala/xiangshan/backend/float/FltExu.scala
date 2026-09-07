package xiangshan.backend.float

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig.WbConfig
import xiangshan.backend.decode.opcode.{Latency, Opcode}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm, _}
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{Frm => VecFrm}
import xiangshan.backend.vector.VecIssueQueue.{BypassDelay, BypassSource, Deq, WakeUpBundle}
import xiangshan.backend.vector.VecRegionModule.DebugBundle
import xiangshan.backend.vector.fu._
import xiangshan.backend.vector.ExuParam
import xiangshan.backend.vector.Exu
import xiangshan.backend.vector.Exu.ToRf
import xiangshan.backend.float.FltIssueQueue.FltWakeUpBundle
import xiangshan.mem.{SqPtr, StoreQueueDataWrite}
import yunsuan.vector.Common.{SewOH, VSew, _}
import yunsuan.vector.v2.MergeUnit

class FltExu(val param: ExuParam)(implicit val p: Parameters) extends Module with HasXSParameter {
  override def desiredName: String = param.name

  val latencyMax: Int = param.fuConfigs.map(_.latency).max
  val vlenb = VLEN / 8
  // The width of the number of e8 elem in VLEN bits
  val byteElemWidth = log2Ceil(vlenb)
  private val numOfMgu = if (param.hasVStd) 0 else latencyMax + 1
  private val numOfEx = latencyMax + 1

  val in = IO(Input(new FltExu.In(param)))
  val out = IO(Output(new FltExu.Out(param)))

  val bypass: FltBypassNetwork = Module(new FltBypassNetwork()(param, p))
  val fus: Seq[Func] = param.fuConfigs.map(cfg => cfg.fuGen2(p, cfg))
  private val nonFixedLatFus: Seq[FltNonFixedLatFunc] = fus.collect { case fu: FltNonFixedLatFunc => fu }

  val ex: Vec[ValidIO[Exu.ExStage]] = RegInit(
    VecInit.fill(numOfEx)(ValidIO(new Exu.ExStage(param)).Lit(_.valid -> false.B))
  )

  val inEx = Wire(ValidIO(new Exu.ExStage(param)))
  inEx.valid := in.uop.valid
  inEx.bits :<#= in.uop.bits
  inEx.bits.fuSel := VecInit(param.fuConfigs.map(_.fuSel2(in.uop.bits)))

  ex.zip(inEx +: ex).zipWithIndex.foreach {
    case ((sink: ValidIO[Exu.ExStage], source: ValidIO[Exu.ExStage]), stageIdx) =>
      sink.valid := source.valid && !source.bits.ctrl.robIdx.needFlush(in.flush)
      when(source.valid) {
        sink.bits := source.bits
        if (stageIdx == 0) {
          sink.bits.data.src := bypass.out.src
        }
      }
  }

  val exFadd: Vec[ValidIO[Exu.ExStage]] = RegInit(
    VecInit.fill(numOfEx)(ValidIO(new Exu.ExStage(param)).Lit(_.valid -> false.B))
  )
  dontTouch(exFadd)
  val fmulToFadd = fus.find(_.cfg.isFmul).get.out.FmulToFadd.get
  exFadd(0).valid := fmulToFadd.valid || inEx.valid && FuType.isFalu(inEx.bits.ctrl.fuType)
  when(fmulToFadd.valid){
    exFadd(0).bits.data.src(0) := fmulToFadd.fpA
    exFadd(0).bits.data.src(1) := fmulToFadd.src2
    exFadd(0).bits.ctrl := ex(1).ctrl
    exFadd(0).bits.fuSel := VecInit(param.fuConfigs.map(_.isFAlu.B))
    exFadd(0).bits.ctrl.latency := 1.U
  }.elsewhen(inEx.valid) {
    exFadd(0).bits := inEx.bits
    exFadd(0).bits.data.src := bypass.out.src
  }
  exFadd(1).valid := exFadd(0).valid && !exFadd(0).bits.ctrl.robIdx.needFlush(in.flush)
  when(exFadd(0).valid) {
    exFadd(1).bits := exFadd(0).bits
  }

  bypass.in.sewOH := SewOH(in.uop.ctrl.vtype.map(_.vsew).getOrElse(VSew.e8))
  bypass.in.bypassCtrl := in.uop.bypassCtrl
  bypass.in.fpRdData := in.fpRdData
  bypass.in.fpWb0Next := in.fpWb0Next
  bypass.in.fpWb0 := in.fpWb0
  bypass.in.fpWb1 := in.fpWb1

  fus.map(_.in.ex0Next).zipWithIndex.foreach {
    case (sink: ValidIO[Func.InUop], i) =>
      sink.valid := inEx.valid && inEx.bits.fuSel(i)
      sink.bits <#=: inEx.bits
  }

  fus.map(_.in.ex).zipWithIndex.foreach {
    case (sink, i) =>
      sink.zip(ex).foreach {
        case (fuInN: ValidIO[Func.InUop], exN) =>
          fuInN.valid := exN.valid && exN.bits.fuSel(i)
          fuInN.bits <#=: exN.bits
      }
  }

  fus.foreach {
    case fu =>
      fu.in.flush := in.flush
      if (fu.in.frm.nonEmpty) require(in.frm.nonEmpty, s"${fu.name} needs frm input, but it's not provided by exu")
      fu.in.frm.foreach(_ := 0.U) // frm is in ctrl and in is1Next
      if (fu.cfg.isFAlu) {
        val fmulToFadd = fus.find(_.cfg.isFmul).get.out.FmulToFadd.get
        fu.in.FmulToFadd.get.valid := RegNext(fmulToFadd.valid)
        fu.in.FmulToFadd.get.bits := RegNext(Mux(fmulToFadd.valid, fmulToFadd.bits, 0.U.asTypeOf(fmulToFadd.bits)))
        fu.in.ex.zip(exFadd).foreach { case (sink, soure) =>
          sink.valid := soure.valid
          sink.bits <#=: soure.bits
        }
      }
      fu.in.busyTableEmpty.foreach(_ := in.busyTableEmpty.get)
  }

  val outFuUopEx = Wire(Vec(latencyMax + 1, ValidIO(new Exu.OutUop(param))))
  outFuUopEx.zipWithIndex.foreach {
    case (out: ValidIO[Exu.OutUop], i) =>
      val fuOuts: Seq[ValidIO[Func.OutUop]] = fus.flatMap(_.out.ex.lift(i))
      out.valid := fuOuts.map(_.valid).orR
      out.bits :<#= fuOuts
  }

  val fuOutValidOH: Seq[Bool] = fus.filterNot(_.cfg.isFdiv).flatMap(_.out.ex.map(_.valid))
  val simultaneousOutCnt = PopCount(fuOutValidOH)

  assert(
    simultaneousOutCnt <= 1.U,
    s"${param.name} produced multiple Exu outputs in one cycle"
  )
  dontTouch(outFuUopEx)
  if(out.outFuWakeUp.nonEmpty){
    println(s"${param.name}'s out.outFuWakeUp is nonEmpty, nonFixedLatFus.size = ${nonFixedLatFus.size}")
    dontTouch(out.outFuWakeUp.get)
  }
  else {
    println(s"${param.name}'s out.outFuWakeUp is Empty, nonFixedLatFus.size = ${nonFixedLatFus.size}")
  }
  println(s"nonFixedLatFus.size = ${nonFixedLatFus.size}")
  out.uop.valid := Cat(outFuUopEx.map(_.valid)).orR
  out.uop.bits := Mux1H(outFuUopEx.map(x => x.valid -> x.bits))
  out.outFuLat.foreach { sink =>
    sink.zip(nonFixedLatFus).foreach { case (toWbBusyTable, fu) =>
      toWbBusyTable := 0.U.asTypeOf(toWbBusyTable)
    }
  }
  out.outFuWakeUp.foreach { sink =>
    sink.zip(nonFixedLatFus).foreach { case (wakeup, fu) =>
      wakeup := fu.outFuWakeUp
    }
  }
  out.outFuBusy.foreach { sink =>
    sink.zip(nonFixedLatFus).foreach { case (wakeup, fu) =>
      wakeup := fu.outFuBusy
    }
  }

  def elemIdxMapVdIdx(elemIdx: UInt, eewOH: UInt) = {
    require(elemIdx.getWidth >= log2Up(VLEN))
    // 3 = log2(8)
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => elemIdx(byteElemWidth - x + 3, byteElemWidth - x)))
  }

  def elemIdxMapElemE8Idx(elemIdx: UInt, eewOH: UInt) = {
    // eewOH(0) -> Cat(elemIdx(byteElemWidth - 1, 0), 0.U(0.W)),
    // eewOH(1) -> Cat(elemIdx(byteElemWidth - 2, 0), 0.U(1.W)),
    // eewOH(2) -> Cat(elemIdx(byteElemWidth - 3, 0), 0.U(2.W)),
    // eewOH(3) -> Cat(elemIdx(byteElemWidth - 4, 0), 0.U(3.W)),
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => Cat(elemIdx.take(byteElemWidth - x), 0.U(x.W))))
  }
}

object FltExu {
  class In(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val uop = ValidIO(new Exu.InUop(param))
    val frm = Option.when(param.readFrm)(Frm())
    val fpRdData = Vec(param.numRegSrc, UInt(XLEN.W))
    val fpWb0Next = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb0 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb1 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val busyTableEmpty = Option.when(param.fuConfigs.find(_.isFdiv).nonEmpty)(Bool())
  }

  class Out(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val uop = ValidIO(new Exu.OutUop(param))
    val outFuLat = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, Valid(UInt(WbFuBusyTable.NonFixedLatencyWidth.W))))
    val outFuWakeUp = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, new FltWakeUpBundle(backendParams.fpPregParams)))
    val outFuBusy = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, Bool()))
  }

  class ExStage(param: ExuParam)(implicit p: Parameters) extends InUop(param) {
    val fuSel = Vec(param.fuConfigs.size,Bool())

    def :<#=(source: InUop): Unit = {
      this.ctrl := source.ctrl
      this.data := source.data
      this.bypassCtrl := source.bypassCtrl
      this.debug.foreach(_ := source.debug.get)
    }
  }

  class InUop(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val ctrl = new InCtrl(param)
    val data = new InData(param)
    val bypassCtrl = new InBypassCtrl(param)
    val debug = Option.when(backendParams.debugEn)(new DebugBundle())

    def toOldExuInput: Bundles.ExuInput = {
      val exuInput = Wire(new Bundles.ExuInput(param.getExeUnitParams()))
      exuInput.fuType := this.ctrl.fuType
      exuInput.fuOpType := this.ctrl.opcode
      exuInput.src := this.data.src
      exuInput.v0.foreach(_ := this.data.v0.get)
      exuInput.vl.foreach(_ := this.data.vl.get)
      exuInput.is0Lat.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.copySrc.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.selImm := 0.U
      exuInput.imm := this.data.imm.getOrElse(0.U)
      exuInput.nextPcOffset.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.robIdx := this.ctrl.robIdx
      exuInput.iqIdx := 0.U
      exuInput.isFirstIssue := false.B
      exuInput.pdestCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.rfWenCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.fpWenCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.vecWenCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.v0WenCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.vlWenCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.loadDependencyCopy.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.pdest := this.ctrl.pdest
      exuInput.pdestV0.foreach(_ := this.ctrl.pdestV0.get)
      exuInput.pdestVl.foreach(_ := this.ctrl.pdestVl.get)
      exuInput.rfWen.foreach(_ := this.ctrl.gpWen.get)
      exuInput.fpWen.foreach(_ := this.ctrl.fpWen.get)
      exuInput.vecWen.foreach(_ := this.ctrl.vpWen.get)
      exuInput.v0Wen.foreach(_ := this.ctrl.v0Wen.get)
      exuInput.vlWen.foreach(_ := this.ctrl.vlWen.get)
      exuInput.oldVType.foreach(_ := this.ctrl.oldVType.get)
      exuInput.vtype.foreach(_ := this.ctrl.vtype.get)
      exuInput.flushPipe.foreach(_ := this.ctrl.flushPipe.get)
      exuInput.rasAction.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.pc.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.isRVC.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.ftqIdx.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.ftqOffset.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.predictInfo.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.loadWaitBit.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.waitForRobIdx.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.storeSetHit.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.loadWaitStrict.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.ssid.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.lqIdx.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.sqIdx.foreach(x => x := this.ctrl.sqIdx.get)
      exuInput.dataSources.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.exuSources.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.loadDependency.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.perfDebugInfo.foreach(x => x := this.ctrl.debug.get.perfDebugInfo)
      exuInput.debug_seqNum.foreach(x => x := this.ctrl.debug.get.seqNum)

      exuInput
    }

    def <#=:(sink: Func.InCtrl): Unit = {
      sink.opcode                      := this.ctrl.opcode
      sink.latency                     := this.ctrl.latency
      sink.robIdx                      := this.ctrl.robIdx
      sink.uopIdx                      := this.ctrl.uopIdx
      sink.pdest                       := this.ctrl.pdest
      sink.pdestV0     .foreach(x => x := this.ctrl.pdestV0.get)
      sink.pdestVl     .foreach(x => x := this.ctrl.pdestVl.get)
      sink.rfWen       .foreach(x => x := this.ctrl.gpWen.get)
      sink.fpWen       .foreach(x => x := this.ctrl.fpWen.get)
      sink.vecWen      .foreach(x => x := this.ctrl.vpWen.get)
      sink.v0Wen       .foreach(x => x := this.ctrl.v0Wen.get)
      sink.vlWen       .foreach(x => x := this.ctrl.vlWen.get)
      sink.flushPipe   .foreach(x => x := this.ctrl.flushPipe.get)
      sink.fflagsWen   .foreach(x => x := this.ctrl.fflagsWen.get)
      sink.sqIdx       .foreach(x => x := this.ctrl.sqIdx.get)
      sink.vtype       .foreach(x => x := this.ctrl.vtype.get)
      sink.oldVType    .foreach(x => x := this.ctrl.oldVType.get)
      sink.vm          .foreach(x => x := this.ctrl.vm.get)
      sink.frm         .foreach(x => x := this.ctrl.frm.get)
    }

    def <#=:(sink: Func.InData): Unit = {
      sink.src                         := this.data.src
      sink.vl          .foreach(x => x := this.data.vl.get)
      sink.v0          .foreach(x => x := this.data.v0.get)
      sink.pc          .foreach(x => x := this.data.pc.get)
      sink.imm                         := this.data.imm.getOrElse(0.U)
    }

    def <#=:(sink: Func.InUop) : Unit = {
      sink.data <#=: this
      sink.ctrl <#=: this
      sink.debug.foreach(x => x := this.debug.get)
    }
  }

  class OutUop(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val toRob = new ToRob(param)
    val toGpRf = param.getGpWriteCfg.map(new ToRf(_, backendParams.intPregParams))
    val toFpRf = param.getFpWriteCfg.map(new ToRf(_, backendParams.fpPregParams))
    val toVpRf = param.getVpWriteCfg.map(new ToRf(_, backendParams.vfPregParams))
    val toV0Rf = param.getV0WriteCfg.map(new ToRf(_, backendParams.v0PregParams))
    val toVlRf = param.getVlWriteCfg.map(new ToRf(_, backendParams.vlPregParams))
    val toSQ = Option.when(param.hasVStd)(new StoreQueueDataWrite)

    def :<#=(fuOuts: Seq[ValidIO[Func.OutUop]]): Unit = {
      val fuOutValidOH: Vec[Bool] = VecInit(fuOuts.map(_.valid))

      this.toRob.robIdx := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.robIdx))
      this.toRob.flushPipe.foreach(_ := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.flushPipe.getOrElse(false.B))))
      this.toRob.replay.foreach(_ := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.replay.getOrElse(false.B))))
      this.toRob.redirect.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.redirect.getOrElse(0.U.asTypeOf(x)))))
//      this.toRob.fflags.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.fflags.getOrElse(0.U.asTypeOf(x)))))
//      this.toRob.vxsat.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.vxsat.getOrElse(0.U.asTypeOf(x)))))
      this.toRob.exceptionVec := ExceptSparseVec.mux1h(fuOutValidOH, fuOuts.map(_.bits.ctrl.exceptionVec))
      this.toRob.debug.foreach(_ := Mux1H(fuOutValidOH, fuOuts.map(_.bits.debug.get)))

      this.toGpRf.foreach { case x =>
        x.wen := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.rfWen.getOrElse(false.B)))
        x.pdest := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.pdest))
        x.data := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.int.getOrElse(0.U)))
      }

      this.toFpRf.foreach { case x =>
        x.wen := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.fpWen.getOrElse(false.B)))
        x.pdest := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.pdest))
        x.data := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.fp.getOrElse(0.U)))
      }

      this.toVpRf.foreach { case x =>
        x.wen := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.vecWen.getOrElse(false.B)))
        x.pdest := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.pdest))
      }

      this.toV0Rf.foreach { case x =>
        x.wen := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.v0Wen.getOrElse(false.B)))
        x.pdest := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.pdestV0.getOrElse(0.U)))
        x.data := 0.U
      }

      this.toVlRf.foreach { case x =>
        x.wen := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.vlWen.getOrElse(false.B)))
        x.pdest := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.pdestVl.getOrElse(0.U)))
        x.data := 0.U
      }

      this.toSQ.foreach { x =>
        x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.vstd.getOrElse(0.U.asTypeOf(chiselTypeOf(x)))))
      }
    }
  }

  class InCtrl(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val robIdx    = new RobPtr
    val uopIdx    = UopIdx()

    val fuType    = FuType()
    val opcode    = Opcode()
    val latency   = Latency()

    val gpWen     = Option.when(param.needGpWen)(Bool())
    val fpWen     = Option.when(param.needFpWen)(Bool())
    val vpWen     = Option.when(param.needVpWen)(Bool())
    val pdest     = UInt(PhyRegIdxWidth.W)

    val v0Wen     = Option.when(param.needV0Wen)(Bool())
    val vlWen     = Option.when(param.needVlWen)(Bool())
    val pdestV0   = Option.when(param.needV0Wen)(UInt(V0PhyRegIdxWidth.W))
    val pdestVl   = Option.when(param.needVlWen)(UInt(VlPhyRegIdxWidth.W))

    val fflagsWen = Option.when(param.needFFlagsWen)(Bool())
    val vxsatWen  = Option.when(param.needVxsatWen)(Bool())

    val flushPipe = Option.when(param.needFlushPipe)(Bool())

    val sqIdx     = Option.when(param.needSqIdx)(new SqPtr)

    val frm       = Option.when(param.readFrm)(Frm())
    val vm        = Option.when(param.needVM)(Bool())
    val vtype     = Option.when(param.readVType)(VType())
    val oldVType  = Option.when(param.readOldVType)(VType())

    val debug     = Option.when(backendParams.debugEn)(new DebugBundle)

    def fromIssueDeq(deq: Deq): Unit = {
      this.robIdx := deq.robIdx
      this.uopIdx := deq.uopIdx
      this.fuType := deq.fuType
      this.opcode := deq.opcode
      this.latency := deq.latency

      this.gpWen.foreach(_ := deq.gpWen)
      this.fpWen.foreach(_ := deq.fpWen)
      this.vpWen.foreach(_ := deq.vpWen)
      this.pdest := deq.pdest
      this.v0Wen.foreach(_ := deq.v0Wen)
      this.vlWen.foreach(_ := deq.vlWen)
      this.pdestV0.foreach(_ := deq.pdestV0.get)
      this.pdestVl.foreach(_ := deq.pdestVl.get)

      this.fflagsWen.foreach(_ := deq.fflagsWen.get)
      this.vxsatWen.foreach(_ := deq.vxsatWen.get)

      this.flushPipe.foreach(_ := deq.flushPipe.get)

      this.sqIdx.foreach(_ := deq.sqIdx.get)
      this.vm.foreach(_ := deq.vm.get)
      this.vtype.foreach(_ := deq.vtype.get)
      this.oldVType.foreach(_ := deq.oldVType.get)

      this.debug.foreach(_ := deq.debug.get)
    }
  }

  class InData(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val src = Vec(param.numRegSrc, UInt(param.srcDataBitsMax.W))
    val v0  = Option.when(param.readV0Rf)(V0())
    val vl  = Option.when(param.readVlRf)(Vl())
    val imm = Option.when(param.needImm)(UInt(param.immWidth.W))
    val pc  = Option.when(param.needPc)(UInt(VAddrData().dataWidth.W))
  }

  class InBypassCtrl(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val gpRen = Vec(param.numRegSrc, Bool())
    val fpRen = Vec(param.numRegSrc, Bool())
    val vpRen = Vec(param.numRegSrc, Bool())
    val bypassSource = Vec(param.numRegSrc, new BypassSource)
    val bypassDelay  = Vec(param.numRegSrc, BypassDelay())

    def fromIssueDeq(deq: Deq): Unit = {
      this.gpRen := deq.gpRen
      this.fpRen := deq.fpRen
      this.vpRen := deq.vpRen
      this.bypassSource := deq.bypassSource
      this.bypassDelay := deq.bypassDelay
    }
  }

  class BypassCtrl(val param: ExuParam, val pregParams: PregParams)(implicit p: Parameters) extends XSBundle {
    private val sourceWidth = log2Up(pregParams.getNumWrite(backendParams))

    val source = UInt(sourceWidth.W)
    val delay = BypassDelay()
  }

  class ToRob(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val robIdx        = new RobPtr()(p)
    val flushPipe     = Option.when(param.needFlushPipe)(Bool())
    val replay        = Option.when(param.needReplay)(Bool())
    val redirect      = Option.when(param.needRedirect)(ValidIO(new Redirect))
    val fflags        = Option.when(param.needFFlagsWen)(Fflags())
    val vxsat         = Option.when(param.needVxsatWen)(Bool())
    val exceptionVec  = ExceptSparseVec(param.exceptionOut)
    val trigger       = Option.when(param.needTrigger)(TriggerAction())
    val debug         = Option.when(backendParams.debugEn)(new DebugBundle)

    def :<#=(source: Exu.InUop): Unit = {
      this.robIdx              := source.ctrl.robIdx
      this.flushPipe   .foreach(_ := source.ctrl.flushPipe.get)
      this.replay      .foreach(_ := false.B)
      this.redirect    .foreach(x => x := 0.U.asTypeOf(x))
      this.fflags      .foreach(x => x := 0.U.asTypeOf(x))
      this.vxsat       .foreach(x => x := 0.U.asTypeOf(x))
      this.exceptionVec.zeroInit()
      this.trigger     .foreach(x => x := 0.U.asTypeOf(x))
      this.debug       .foreach(x => x := source.ctrl.debug.get)
    }
  }
}
