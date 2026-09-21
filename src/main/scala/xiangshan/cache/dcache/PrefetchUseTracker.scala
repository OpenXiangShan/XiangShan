package xiangshan.cache

import chisel3._
import chisel3.reflect.DataMirror.isVisible
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore, tapAndRead}
import org.chipsalliance.cde.config.Parameters
import utility._

object PrefetchPerfBoundary {
  def apply(name: String, value: UInt)(implicit p: Parameters): Unit = {
    if (p(PerfCounterOptionsKey).enablePerfPrint) {
      val origin = chisel3.XSCompatibility.currentModule
      XSLog.registerCaller { ctrl =>
        val live = if (isVisible(value)) value
          else if (p(LogUtilsOptionsKey).enableXMR) tapAndRead(value) else bore(value)
        val start = RegInit(0.U(value.getWidth.W))
        when (ctrl.clean) { start := live }
        XSPerfPrint(origin)(ctrl.dump, p"${name}_window_start, ${start}\n")
        XSPerfPrint(origin)(ctrl.dump, p"${name}_window_end, ${live}\n")
      }
    }
  }
}

class PrefetchUseAccess(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way = UInt(log2Ceil(nWays).W)
}

class PrefetchUseInstall(implicit p: Parameters) extends PrefetchUseAccess {
  val source = UInt(L1PfSourceBits.W)
  val used = Bool()
}

class PrefetchUseTracker(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val load = Input(Vec(LoadPipelineWidth, Valid(new PrefetchUseAccess)))
    val install = Input(Valid(new PrefetchUseInstall))
    val invalidate = Input(Valid(new PrefetchUseAccess))
    val firstUse = Output(Vec(LoadPipelineWidth, Valid(UInt(L1PfSourceBits.W))))
    val unusedExit = Output(Valid(UInt(L1PfSourceBits.W)))
  })
  val source = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U(L1PfSourceBits.W))))))
  val used = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))
  def same(a: PrefetchUseAccess, b: PrefetchUseAccess): Bool = a.set === b.set && a.way === b.way
  for (lane <- 0 until LoadPipelineWidth) {
    val access = io.load(lane)
    val duplicate = io.load.take(lane).map(prior => prior.valid && same(prior.bits, access.bits)).foldLeft(false.B)(_ || _)
    io.firstUse(lane).valid := access.valid && !duplicate && !used(access.bits.set)(access.bits.way) &&
      isFromL1Prefetch(source(access.bits.set)(access.bits.way))
    io.firstUse(lane).bits := source(access.bits.set)(access.bits.way)
    when (access.valid) { used(access.bits.set)(access.bits.way) := true.B }
  }
  val exit = Wire(new PrefetchUseAccess)
  exit.set := Mux(io.install.valid, io.install.bits.set, io.invalidate.bits.set)
  exit.way := Mux(io.install.valid, io.install.bits.way, io.invalidate.bits.way)
  val exitUsedNow = io.load.map(a => a.valid && same(a.bits, exit)).reduce(_ || _)
  io.unusedExit.valid := (io.install.valid || io.invalidate.valid) &&
    isFromL1Prefetch(source(exit.set)(exit.way)) && !used(exit.set)(exit.way) && !exitUsedNow
  io.unusedExit.bits := source(exit.set)(exit.way)
  when (io.invalidate.valid) {
    source(io.invalidate.bits.set)(io.invalidate.bits.way) := 0.U
    used(io.invalidate.bits.set)(io.invalidate.bits.way) := false.B
  }
  when (io.install.valid) {
    source(io.install.bits.set)(io.install.bits.way) := io.install.bits.source
    used(io.install.bits.set)(io.install.bits.way) := io.install.bits.used
  }
  XSPerfAccumulate("first_load_use", PopCount(io.firstUse.map(_.valid)))
  XSPerfAccumulate("unused_exit", io.unusedExit.valid)
  PrefetchPerfBoundary("unused_prefetch_occupancy", PopCount((0 until nSets).flatMap(set =>
    (0 until nWays).map(way => isFromL1Prefetch(source(set)(way)) && !used(set)(way)))))
}
