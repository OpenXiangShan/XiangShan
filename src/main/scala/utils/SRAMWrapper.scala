package utils

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.util._
import firrtl.annotations.Annotation
import freechips.rocketchip.transforms.naming.OverrideDesiredNameAnnotation

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: T, setIdx: UInt, waymask: UInt) = {
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
}

abstract class SRAMTemplate extends Module {
  def read(addr: UInt, ren: Bool): Vec[UInt]
  def write(addr: UInt, wen: Bool, wdata: UInt, wmask: UInt): Unit
}

class SinglePortSRAM(set: Int, way: Int, width: Int) extends SRAMTemplate {
  val io = IO(new Bundle() {
    val addr = Input(UInt(log2Up(set).W))
    val ren = Input(Bool())
    val rdata = Output(Vec(way, UInt(width.W)))
    val wdata = Input(UInt(width.W))
    val wen = Input(Bool())
    val wmask = Input(UInt(way.W))
  })
  val mem = SyncReadMem(set, Vec(way, UInt(width.W)))
  io.rdata := mem.read(io.addr, io.ren)
  when(io.wen){
    mem.write(io.addr, VecInit(Seq.fill(way)(io.wdata)), io.wmask.asBools())
  }

  override def read(addr: UInt, ren: Bool): Vec[UInt] = {
    io.addr := addr
    io.ren := ren
    io.rdata
  }

  override def write(addr: UInt, wen: Bool, wdata: UInt, wmask: UInt): Unit = {
    io.addr := addr
    io.wen := wen
    io.wdata := wdata
    io.wmask := wmask
  }
}
class DualPortSRAM(set: Int, way: Int, width: Int) extends SRAMTemplate {
  val io = IO(new Bundle() {
    val raddr = Input(UInt(log2Up(set).W))
    val ren = Input(Bool())
    val rdata = Output(Vec(way, UInt(width.W)))
    val waddr = Input(UInt(log2Up(set).W))
    val wdata = Input(UInt(width.W))
    val wen = Input(Bool())
    val wmask = Input(UInt(way.W))
  })
  val mem = SyncReadMem(set, Vec(way, UInt(width.W)))
  io.rdata := mem.read(io.raddr, io.ren)
  when(io.wen){
    mem.write(io.waddr, VecInit(Seq.fill(way)(io.wdata)), io.wmask.asBools())
  }

  override def read(addr: UInt, ren: Bool): Vec[UInt] = {
    io.raddr := addr
    io.ren := ren
    io.rdata
  }

  override def write(addr: UInt, wen: Bool, wdata: UInt, wmask: UInt): Unit = {
    io.waddr := addr
    io.wen := wen
    io.wdata := wdata
    io.wmask := wmask
  }
}

class SRAMWrapper[T <: Data]
(
  sramName: String,
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  singlePort: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
//  val array = SyncReadMem(set, Vec(way, wordType))
  val array: SRAMTemplate = if(singlePort) {
    Module(new SinglePortSRAM(set, way, gen.getWidth))
  } else {
    Module(new DualPortSRAM(set, way, gen.getWidth))
  }
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet,
    if(singlePort) Mux(io.w.req.valid, io.w.req.bits.setIdx, io.r.req.bits.setIdx)
    else io.w.req.bits.setIdx
  )
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  array.write(setIdx, wen, wdataword, waymask)

  val rdataWire = if(singlePort) array.read(setIdx, realRen) else array.read(io.r.req.bits.setIdx, realRen)

  val rdata = (if(holdRead) HoldUnless(rdataWire, RegNext(realRen)) else rdataWire).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  val prefix = if(singlePort) "SinglePortSRAM_" else "DualPortSRAM_"
  annotate(new ChiselAnnotation {
    override def toFirrtl: Annotation = OverrideDesiredNameAnnotation(s"$prefix$sramName", array.toAbsoluteTarget)
  })
}

class SRAMTemplateWithArbiter[T <: Data](sramName: String, nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMWrapper(sramName, gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map { r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}
