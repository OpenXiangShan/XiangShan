package xiangshan.backend.float

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.vector.fu._
import xiangshan.backend.vector.ExuParam
import xiangshan.backend.vector.Exu
import xiangshan.backend.float.FltIssueQueue.FltWakeUpBundle
import utility.XSPerfAccumulate
import yunsuan.vector.Common.validAutoUnwrap

class FltExu(val param: ExuParam)(implicit val p: Parameters) extends Module with HasXSParameter {
  override def desiredName: String = param.name

  val latencyMax: Int = param.fuConfigs.map(_.latency).max
  private val numOfEx = latencyMax + 1

  val in = IO(Input(new FltExu.In(param)))
  val out = IO(Output(new FltExu.Out(param)))

  val bypass: FltBypassNetwork = Module(new FltBypassNetwork()(param, p))
  val fus: Seq[Func] = param.fuConfigs.map(cfg => cfg.fuGen2(p, cfg))
  private val nonFixedLatFus: Seq[FltNonFixedLatFunc] = fus.collect { case fu: FltNonFixedLatFunc => fu }

  val ex: Vec[ValidIO[Exu.ExStage]] = RegInit(
    VecInit.fill(numOfEx)(ValidIO(new Exu.ExStage(param)).Lit(_.valid -> false.B))
  )

  // A load-M4 consumer can already be inside this execution pipe when the
  // load reaches s3 and asserts ldCancel.  Keep this cancellation path
  // separate from the ordinary FMA/FMUL M4 confirmation protocol: only a
  // source explicitly tagged as load-produced M4 is killed here.
  private val ldWBPort = param.backendParams.getIntRegionParam.issueParams
    .filter(_.hasLdu).map(_.fpWbPortIds.head)
  private def loadM4Cancel(uop: Exu.InUop): Bool = {
    // The marker survives the ordinary load M2 wakeup.  fmaSrc3Wait is only
    // the FMUL1 bypass selector and may already be zero when s3 cancels.
    val tagged = uop.bypassCtrl.fmaSrc3LoadM4.get
    val matches = ldWBPort.zip(in.ldCancel).map { case (wbIdx, cancel) =>
      uop.bypassCtrl.bypassSource(2).idx === wbIdx.U && cancel
    }
    tagged && (if (matches.nonEmpty) matches.reduce(_ || _) else false.B)
  }

  val inEx = Wire(ValidIO(new Exu.ExStage(param)))
  inEx.valid := in.uop.valid
  inEx.bits :<#= in.uop.bits
  inEx.bits.fuSel := VecInit(param.fuConfigs.map(_.fuSel2(in.uop.bits)))

  ex.zip(inEx +: ex).zipWithIndex.foreach {
    case ((sink: ValidIO[Exu.ExStage], source: ValidIO[Exu.ExStage]), stageIdx) =>
      sink.valid := source.valid && !source.bits.ctrl.robIdx.needFlush(in.flush)
      if (stageIdx == 1) {
        sink.valid := source.valid && !source.bits.ctrl.robIdx.needFlush(in.flush) && !(loadM4Cancel(source.bits) && source.bits.bypassCtrl.fmaSrc3Wait.get === 1.U)
      }
      else if (stageIdx == 2) {
        sink.valid := source.valid && !source.bits.ctrl.robIdx.needFlush(in.flush) && !(loadM4Cancel(source.bits) && source.bits.bypassCtrl.fmaSrc3Wait.get === 2.U)
      }
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
  val fmul1Src3Wait = ex(1).bits.bypassCtrl.fmaSrc3Wait.get
  val fmul1Src3Port = ex(1).bits.bypassCtrl.bypassSource(2).idx(log2Ceil(backendParams.getFpRfWriteSize) - 1, 0)
  // An M4-issued consumer reaches fmul1 alongside the producer's wb0Next.
  // Issuing one cycle later instead uses wb0; ordinary issues retain src2S1.
  val fmul1Src3 = Mux(fmul1Src3Wait === 2.U, in.fpWb0Next(fmul1Src3Port),
    Mux(fmul1Src3Wait === 1.U, in.fpWb0(fmul1Src3Port), fmulToFadd.src2))
  XSPerfAccumulate("fma_src3_fmul1_bypass", fmulToFadd.valid && fmul1Src3Wait =/= 0.U)
  val fmulToFaddLoadM4Cancel = ex(1).valid && loadM4Cancel(ex(1).bits) && ex(1).bits.bypassCtrl.fmaSrc3Wait.get === 2.U
  val fmulToFaddValid = fmulToFadd.valid && !ex(1).bits.ctrl.robIdx.needFlush(in.flush) && !fmulToFaddLoadM4Cancel
  exFadd(0).valid := fmulToFaddValid || inEx.valid && FuType.isFalu(inEx.bits.ctrl.fuType)
  when(fmulToFaddValid){
    exFadd(0).bits.data.src(0) := fmulToFadd.fpA
    exFadd(0).bits.data.src(1) := fmul1Src3
    exFadd(0).bits.ctrl := ex(1).ctrl
    exFadd(0).bits.bypassCtrl := ex(1).bypassCtrl
    exFadd(0).bits.fuSel := VecInit(param.fuConfigs.map(_.isFAlu.B))
    exFadd(0).bits.ctrl.latency := 1.U
    exFadd(0).bits.debug.foreach(_ := ex(1).bits.debug.get)
  }.elsewhen(inEx.valid) {
    exFadd(0).bits := inEx.bits
    exFadd(0).bits.data.src := bypass.out.src
  }
  exFadd(1).valid := exFadd(0).valid && !exFadd(0).bits.ctrl.robIdx.needFlush(in.flush)
  when(exFadd(0).valid) {
    exFadd(1).bits := exFadd(0).bits
  }

  bypass.in.bypassCtrl := in.uop.bypassCtrl
  bypass.in.fpRdData := in.fpRdData
  bypass.in.fpWb0Next := in.fpWb0Next
  bypass.in.fpWb0 := in.fpWb0

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
        fu.in.FmulToFadd.get.valid := RegNext(fmulToFaddValid)
        fu.in.FmulToFadd.get.bits := RegNext(Mux(fmulToFaddValid, fmulToFadd.bits, 0.U.asTypeOf(fmulToFadd.bits)))
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
    sink.zip(nonFixedLatFus).foreach { case (toWbBusyTable, _) =>
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
}

object FltExu {
  class In(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val uop = ValidIO(new Exu.InUop(param))
    // Raw load-cancel pulse.  Unlike the delayed IQ cancel, this is needed
    // while a load-M4 consumer is already in the execution pipe.
    val ldCancel = Vec(backendParams.LdExuCnt, Bool())
    val frm = Option.when(param.readFrm)(Frm())
    val fpRdData = Vec(param.numRegSrc, UInt(XLEN.W))
    val fpWb0Next = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb0 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val busyTableEmpty = Option.when(param.fuConfigs.find(_.isFdiv).nonEmpty)(Bool())
  }

  class Out(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val uop = ValidIO(new Exu.OutUop(param))
    val outFuLat = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, Valid(UInt(WbFuBusyTable.NonFixedLatencyWidth.W))))
    val outFuWakeUp = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, new FltWakeUpBundle(backendParams.fpPregParams)))
    val outFuBusy = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, Bool()))
  }
}
