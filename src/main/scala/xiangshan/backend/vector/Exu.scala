package xiangshan.backend.vector

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.Bundles
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig.WbConfig
import xiangshan.backend.decode.opcode.{Latency, Opcode}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{Frm => VecFrm}
import xiangshan.backend.fu.vector.Bundles.{V0, VType, Vl, Vxrm, Vxsat}
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.VecRegionModule.DebugBundle
import xiangshan.backend.vector.fu._
import xiangshan.mem.SqPtr
import xiangshan._
import yunsuan.vector.Common._
import yunsuan.vector.v2.MergeUnit

class Exu(val param: ExuParam)(implicit val p: Parameters) extends Module with HasXSParameter {
  override def desiredName: String = param.name

  val latencyMax: Int = param.fuConfigs.map(_.latency).max
  val vlenb = VLEN / 8
  // The width of the number of e8 elem in VLEN bits
  val byteElemWidth = log2Ceil(vlenb)

  val in = IO(Input(new Exu.In(param)))
  val out = IO(Output(new Exu.Out(param)))

  println(s"[tmp-$getClass] ${param.getInstanceNameOfPipe} ${param.fuConfigs.map(cfg => cfg.name -> cfg.fuGen2)}")
  val fus: Seq[Func] = param.fuConfigs.map(cfg => cfg.fuGen2(p, cfg))
  val mgus: Seq[MergeUnit] = Seq.fill(latencyMax + 1)(Module(new MergeUnit()))

  val ex: Vec[ValidIO[Exu.ExStage]] = RegInit(
    VecInit.fill(latencyMax + 1)(ValidIO(new Exu.ExStage(param)).Lit(_.valid -> false.B))
  )

  val inEx = Wire(ValidIO(new Exu.ExStage(param)))
  inEx.valid := in.uop.valid
  inEx.bits :<#= in.uop.bits
  inEx.bits.fuSel := VecInit(param.fuConfigs.map(_.fuSel2(in.uop.bits)))

  ex zip (inEx +: ex) foreach {
    case (sink: ValidIO[Exu.ExStage], source: ValidIO[Exu.ExStage]) =>
      sink.valid := source.valid && !source.bits.ctrl.robIdx.needFlush(in.flush)
      when(source.valid) {
        sink.bits := source.bits
      }
  }

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
      val effectiveFrm = ex.head.bits.ctrl.frm.map(instFrm =>
        Mux(instFrm === VecFrm.DYN, in.frm.get, instFrm)
      )
      fu.in.flush := in.flush
      (fu.in.frm zip effectiveFrm).foreach { case (fuFrm, frm) => fuFrm := frm }
      fu.in.vxrm.foreach(_ := in.vxrm.get)
  }


  mgus.zipWithIndex.foreach {
    case (mgu, i) =>
      val vl = ex(i).bits.data.vl.get.suggestName(s"ex${i}_vl")
      // Todo: widen uop should use 2x value
      val eewOH = UIntToOH(ex(i).bits.ctrl.vtype.get.vsew, SewOH.width).suggestName(s"ex${i}_eewOH")
      val vdIdx = ex(i).bits.ctrl.uopIdx // Todo: may by wrong for some kind of uops
      val vlMapVdIdx = elemIdxMapVdIdx(vl, eewOH)(3, 0) // 4 bits 0~8
      val end = elemIdxMapElemE8Idx(vl, eewOH)
      val vd = Mux1H(fus.flatMap(_.out.ex.lift(i)).map(validIO =>
        validIO.valid -> validIO.bits.data.vec.get.normal
      )).suggestName(s"ex${i}_vd")

      mgu.in.ctrl.vma := ex(i).bits.ctrl.vtype.get.vma
      mgu.in.ctrl.vta := ex(i).bits.ctrl.vtype.get.vta
      mgu.in.data.mask := Fill(vlenb, ex(i).bits.ctrl.vm.get) | ex(i).bits.data.v0.get // Todo: use vlenb v0
      // since vstart is always 0 for vector arith instruction, begin is always 0
      mgu.in.data.begin := 0.U
      mgu.in.data.end := Mux1H(Seq(
        (vdIdx > vlMapVdIdx) -> 0.U,
        (vdIdx === vlMapVdIdx) -> end,
        (vdIdx < vlMapVdIdx) -> vlenb.U,
      ))
      mgu.in.data.oldVd := ex(i).bits.data.src(2).toByteVec
      mgu.in.data.vd := vd.toByteVec
  }

  val outFuUopEx = Wire(Vec(latencyMax + 1, ValidIO(new Exu.OutUop(param))))
  outFuUopEx.zipWithIndex.foreach {
    case (out: ValidIO[Exu.OutUop], i) =>
      val fuOuts: Seq[ValidIO[Func.OutUop]] = fus.flatMap(_.out.ex.lift(i))
      out.valid := fuOuts.map(_.valid).orR
      out.bits :<#= fuOuts
      out.bits.toVpRf.foreach(_.data := mgus(i).out.res.asUInt)

      out.bits.toRob.vxsat.foreach {
        case x =>
          val vxsat: Vec[UInt] = Mux1H(
            fus.flatMap(_.out.ex.lift(i)).map(
              validIO => validIO.valid -> validIO.bits.data.vec.get.vxsatE8.getOrElse(0.U.asTypeOf(Vec(vlenb, Vxsat())))
            )
          ).suggestName(s"ex${i}_vxsat")

          x := Mux1H(mgus(i).out.activeEn, vxsat)
      }

      out.bits.toRob.fflags.foreach {
        case x =>
          val fflags: Vec[UInt] = Mux1H(
            fus.flatMap(_.out.ex.lift(i)).map(
              validIO => validIO.valid -> validIO.bits.data.vec.get.fflagsE8.getOrElse(0.U.asTypeOf(Vec(vlenb, Fflags())))
            )
          ).suggestName(s"ex${i}_fflags")

          x := Mux1H(mgus(i).out.activeEn, fflags)
      }
  }

  val fuOutValidOH: Seq[Bool] = fus.flatMap(_.out.ex.map(_.valid))
  val simultaneousOutCnt = PopCount(outFuUopEx.map(_.valid))

  assert(
    simultaneousOutCnt <= 1.U,
    s"${param.name} produced multiple Exu outputs in one cycle"
  )

  out.uop.valid := Cat(outFuUopEx.map(_.valid)).orR
  out.uop.bits := Mux1H(outFuUopEx.map(x => x.valid -> x.bits))

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

object Exu {
  class In(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val uop = ValidIO(new Exu.InUop(param))
    val frm = Option.when(param.readFrm)(Frm())
    val vxrm = Option.when(param.readVxrm)(Vxrm())
  }

  class Out(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val uop = ValidIO(new Exu.OutUop(param))
  }

  class ExStage(param: ExuParam)(implicit p: Parameters) extends InUop(param) {
    val fuSel = Vec(param.fuConfigs.size,Bool())

    def :<#=(source: InUop): Unit = {
      this.ctrl := source.ctrl
      this.data := source.data
      this.debug.foreach(_ := source.debug.get)
    }
  }

  class InUop(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val ctrl = new InCtrl(param)
    val data = new InData(param)
    val debug = Option.when(backendParams.debugEn)(new DebugBundle())

    def fromIssueDeq(deq: VecIssueQueue.Deq): Unit = {
      this.ctrl.fromIssueDeq(deq)
    }

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
      exuInput.vpu.foreach(x => x := 0.U.asTypeOf(x))
      exuInput.oldVType.foreach(_ := this.ctrl.oldVType.get)
      exuInput.vtype.foreach(_ := this.ctrl.vtype.get)
      exuInput.vialuCtrl.foreach(x => x := 0.U.asTypeOf(x))
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
      exuInput.numLsElem.foreach(x => x := 0.U.asTypeOf(x))
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
      sink.vtype       .foreach(x => x := this.ctrl.vtype.get)
      sink.oldVType    .foreach(x => x := this.ctrl.oldVType.get)
      sink.vm          .foreach(x => x := this.ctrl.vm.get)
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
    val toGpRf = param.getGpWbPort.map(new ToRf(_, backendParams.intPregParams))
    val toFpRf = param.getFpWbPort.map(new ToRf(_, backendParams.fpPregParams))
    val toVpRf = param.getVpWbPort.map(new ToRf(_, backendParams.vfPregParams))
    val toV0Rf = param.getV0WbPort.map(new ToRf(_, backendParams.v0PregParams))
    val toVlRf = param.getVlWbPort.map(new ToRf(_, backendParams.vlPregParams))

    def :<#=(fuOuts: Seq[ValidIO[Func.OutUop]]): Unit = {
      val fuOutValidOH: Vec[Bool] = VecInit(fuOuts.map(_.valid))

      this.toRob.robIdx := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.robIdx))
      this.toRob.flushPipe.foreach(_ := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.flushPipe.getOrElse(false.B))))
      this.toRob.replay.foreach(_ := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.replay.getOrElse(false.B))))
      this.toRob.redirect.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.redirect.getOrElse(0.U.asTypeOf(x)))))
//      this.toRob.fflags.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.fflags.getOrElse(0.U.asTypeOf(x)))))
//      this.toRob.vxsat.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.data.vxsat.getOrElse(0.U.asTypeOf(x)))))
      this.toRob.exceptionVec.foreach(x => x := Mux1H(fuOutValidOH, fuOuts.map(_.bits.ctrl.exceptionVec.getOrElse(0.U.asTypeOf(x)))))
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

    def fromIssueDeq(deq: VecIssueQueue.Deq): Unit = {
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

      this.frm.foreach(_ := deq.frm.get)
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

  class ToRf(val wbCfg: WbConfig, val pregParams: PregParams) extends Bundle {
    private val dataWidth = pregParams.dataCfg.dataWidth
    private val addrWidth = pregParams.addrWidth

    val wen = Bool()
    val pdest = UInt(addrWidth.W)
    val data = UInt(dataWidth.W)
  }

  class ToRob(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val robIdx        = new RobPtr()(p)
    val flushPipe     = Option.when(param.needFlushPipe)(Bool())
    val replay        = Option.when(param.needReplay)(Bool())
    val redirect      = Option.when(param.needRedirect)(ValidIO(new Redirect))
    val fflags        = Option.when(param.needFFlagsWen)(Fflags())
    val vxsat         = Option.when(param.needVxsatWen)(Bool())
    val exceptionVec  = Option.when(param.exceptionOut.nonEmpty)(ExceptionVec())
    val trigger       = Option.when(param.needTrigger)(TriggerAction())
    val debug         = Option.when(backendParams.debugEn)(new DebugBundle)

    def toOldExuOutput(implicit p: Parameters): xiangshan.backend.Bundles.ExuOutput = {
      val exuOutput = Wire(new Bundles.ExuOutput(param.getExeUnitParams()))

      exuOutput.data.foreach(_ := 0.U)
      exuOutput.pdest := 0.U
      exuOutput.pdestV0.foreach(_ := 0.U)
      exuOutput.pdestVl.foreach(_ := 0.U)
      exuOutput.robIdx := this.robIdx
      exuOutput.intWen.foreach(_ := false.B)
      exuOutput.fpWen.foreach(_ := false.B)
      exuOutput.vecWen.foreach(_ := false.B)
      exuOutput.v0Wen.foreach(_ := false.B)
      exuOutput.vlWen.foreach(_ := false.B)
      exuOutput.redirect.foreach(_ := this.redirect.get)
      exuOutput.fflagsWen.foreach(_ := this.fflags.get.orR)
      exuOutput.fflags.foreach(_ := this.fflags.get)
      exuOutput.vxsat.foreach(_ := this.vxsat.get)
      exuOutput.exceptionVec.foreach(_ := this.exceptionVec.get)
      exuOutput.flushPipe.foreach(_ := this.flushPipe.get)
      exuOutput.replay.foreach(_ := this.replay.get)
      exuOutput.lqIdx.foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.sqIdx.foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.trigger.foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.isRVC.foreach(_ := false.B)
      exuOutput.vls.foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.isFromLoadUnit.foreach(_ := false.B)
      exuOutput.debug := this.debug.map(_.debug).getOrElse(0.U.asTypeOf(exuOutput.debug))
      exuOutput.perfDebugInfo.foreach(_ := this.debug.get.perfDebugInfo)
      exuOutput.debug_seqNum.foreach(_ := this.debug.get.seqNum)

      exuOutput
    }

    def toNewExuOutput(valid: Bool)(implicit p: Parameters): xiangshan.backend.Bundles.NewExuOutput = {
      val exuOutput = Wire(new Bundles.NewExuOutput(param.getExeUnitParams()))

      exuOutput.toRob.valid := valid
      exuOutput.toRob.bits.robIdx := this.robIdx
      exuOutput.toRob.bits.fflags      .foreach(_ := this.fflags.get)
      exuOutput.toRob.bits.fflagsWen   .foreach(_ := this.fflags.get.orR)
      exuOutput.toRob.bits.vxsat       .foreach(_ := this.vxsat.get)
      exuOutput.toRob.bits.exceptionVec.foreach(_ := this.exceptionVec.get)
      exuOutput.toRob.bits.flushPipe   .foreach(_ := this.flushPipe.get)
      exuOutput.toRob.bits.trigger     .foreach(_ := this.trigger.get)
      exuOutput.toRob.bits.vxsat       .foreach(_ := this.vxsat.get)
      exuOutput.toRob.bits.lqIdx       .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toRob.bits.sqIdx       .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toRob.bits.vls         .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.pdest                                  := 0.U
      exuOutput.pdestV0                .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.pdestVl                .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toIntRf                .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toFpRf                 .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toVecRf                .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toV0Rf                 .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.toVlRf                 .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.redirect               .foreach(_ := this.redirect.get)
      exuOutput.isFromLoadUnit         .foreach(x => x := 0.U.asTypeOf(x))
      exuOutput.debug                             := this.debug.map(_.debug).getOrElse(0.U.asTypeOf(exuOutput.debug))
      exuOutput.perfDebugInfo          .foreach(_ := this.debug.get.perfDebugInfo)
      exuOutput.debug_seqNum           .foreach(_ := this.debug.get.seqNum)

      exuOutput
    }

    def toWriteBackRobBundle(implicit p: Parameters): xiangshan.backend.Bundles.WriteBackRobBundle = {
      val toRob = Wire(new Bundles.WriteBackRobBundle(param.getExeUnitParams(), backendParams))

      toRob.robIdx        := this.robIdx
      toRob.flushPipe     .foreach(_ := this.flushPipe.get)
      toRob.replay        .foreach(_ := this.replay.get)
      toRob.redirect      .foreach(_ := this.redirect.get)
      toRob.fflags        .foreach(_ := this.fflags.get)
      toRob.fflagsWen     .foreach(_ := this.fflags.get.orR)
      toRob.vxsat         .foreach(_ := this.vxsat.get)
      toRob.exceptionVec  .foreach(_ := this.exceptionVec.get)
      toRob.lqIdx         .foreach(x => x := 0.U.asTypeOf(x))
      toRob.sqIdx         .foreach(x => x := 0.U.asTypeOf(x))
      toRob.trigger       .foreach(x => x := 0.U.asTypeOf(x))
      toRob.vls           .foreach(x => x := 0.U.asTypeOf(x))
      toRob.data          := 0.U
      toRob.pdest         := 0.U
      toRob.vecWen        .foreach(_ := false.B)
      toRob.v0Wen         .foreach(_ := false.B)
      toRob.debug                    := this.debug.map(_.debug).getOrElse(0.U.asTypeOf(toRob.debug))
      toRob.perfDebugInfo .foreach(_ := this.debug.get.perfDebugInfo)
      toRob.debug_seqNum  .foreach(_ := this.debug.get.seqNum)

      toRob
    }

    def fromOldExuOutput(source: xiangshan.backend.Bundles.ExuOutput): Unit = {
      this.robIdx := source.robIdx
      this.flushPipe   .foreach(_ := source.flushPipe   .get)
      this.replay      .foreach(_ := source.replay      .get)
      this.redirect    .foreach(_ := source.redirect    .get)
      this.fflags      .foreach(_ := source.fflags      .get)
      this.vxsat       .foreach(_ := source.vxsat       .get)
      this.exceptionVec.foreach(_ := source.exceptionVec.get)
      this.trigger     .foreach(_ := source.trigger     .get)
      this.debug.foreach { case x =>
        x.debug         := source.debug
        x.perfDebugInfo := source.perfDebugInfo.get
        x.seqNum        := source.debug_seqNum.get
      }
    }

    def fromOldExuOutput(source: xiangshan.backend.Bundles.NewExuOutput): Unit = {
      this.robIdx                 := source.toRob.bits.robIdx
      this.flushPipe   .foreach(_ := source.toRob.bits.flushPipe   .get)
      this.replay      .foreach(_ := source.toRob.bits.replay      .get)
      this.redirect    .foreach(_ := source.redirect               .get)
      this.fflags      .foreach(_ := source.toRob.bits.fflags      .get)
      this.vxsat       .foreach(_ := source.toRob.bits.vxsat       .get)
      this.exceptionVec.foreach(_ := source.toRob.bits.exceptionVec.get)
      this.trigger     .foreach(_ := source.toRob.bits.trigger     .get)
      this.debug.foreach { case x =>
        x.debug         := source.debug
        x.perfDebugInfo := source.perfDebugInfo.get
        x.seqNum        := source.debug_seqNum.get
      }
    }

    def :<#=(source: Exu.InUop): Unit = {
      this.robIdx              := source.ctrl.robIdx
      this.flushPipe   .foreach(_ := source.ctrl.flushPipe.get)
      this.replay      .foreach(_ := false.B)
      this.redirect    .foreach(x => x := 0.U.asTypeOf(x))
      this.fflags      .foreach(x => x := 0.U.asTypeOf(x))
      this.vxsat       .foreach(x => x := 0.U.asTypeOf(x))
      this.exceptionVec.foreach(x => x := 0.U.asTypeOf(x))
      this.trigger     .foreach(x => x := 0.U.asTypeOf(x))
      this.debug       .foreach(x => x := source.ctrl.debug.get)
    }
  }
}
