package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import top.Generator
import utility._
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey

/** Both the isolated policy and native Stream integration are driven by the C++ test. */
class StreamHistoryTestTop(implicit p: Parameters) extends Module {
  val c       = p(StreamHistoryKey).get
  val history = Module(new StreamHistory(c, 50))
  val io = IO(new Bundle {
    val train = Flipped(Valid(new Bundle {
      val pc       = UInt(50.W)
      val vaddr    = UInt(50.W)
      val eligible = Bool()
    }))
    val flush       = Input(Bool())
    val candidates  = Input(Vec(3, new StreamHistoryCandidate(50)))
    val allow       = Output(Vec(3, Bool()))
    val inspect     = Output(new StreamHistoryEntry(c, 50))
    val nativeTrain = Input(Bool())
    val nativePC    = Input(UInt(50.W))
    val nativeVA    = Input(UInt(50.W))
    val nativeReady = Output(Bool())
    val filtered    = Output(Vec(3, Valid(new StreamPrefetchReqBundle)))
    val original    = Output(Vec(3, Valid(new StreamPrefetchReqBundle)))
  })
  history.io.train <> io.train
  history.io.flush      := io.flush
  history.io.candidates := io.candidates
  io.allow              := history.io.allow
  io.inspect            := history.io.inspect
  val native   = Module(new StreamBitVectorArray()(p.alterPartial { case StreamHistoryKey => None }))
  val filtered = Module(new StreamBitVectorArray)
  for (stream <- Seq(native, filtered)) {
    stream.io.enable                  := true.B
    stream.io.fdbkDegree              := 1.U
    stream.io.flush                   := io.flush
    stream.io.historyFlush            := io.flush
    stream.io.dynamic_depth           := 16.U
    stream.io.confidence              := 1.U
    stream.io.train_req.valid         := io.nativeTrain
    stream.io.train_req.bits          := 0.U.asTypeOf(new TrainReqBundle)
    stream.io.train_req.bits.pc       := io.nativePC
    stream.io.train_req.bits.vaddr    := io.nativeVA
    stream.io.train_req.bits.miss     := true.B
    stream.io.stream_lookup_req.valid := false.B
    stream.io.stream_lookup_req.bits  := 0.U
  }
  assert(native.io.train_req.ready === filtered.io.train_req.ready)
  io.nativeReady := filtered.io.train_req.ready
  Seq(native, filtered).zip(Seq(io.original, io.filtered)).foreach { case (stream, output) =>
    output(0) := stream.io.l1_prefetch_req
    output(1) := stream.io.l2_prefetch_req
    output(2) := stream.io.l3_prefetch_req
  }
  XSLog.collect(GTimer(), false.B, false.B, false.B)
}

object StreamHistoryTestMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  val core                              = config.alterPartial { case XSCoreParamsKey => config(XSTileKey).head }
  ChiselDB.init(false)
  Constantin.init(false)
  Generator.execute(firrtlOpts, new StreamHistoryTestTop()(core), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  val target = args.sliding(2).collectFirst { case Array("--target-dir", dir) => dir }.getOrElse("build")
  FileRegisters.write(fileDir = target)
}
