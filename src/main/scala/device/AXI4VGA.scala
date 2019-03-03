package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

trait HasVGAConst {
  // these are only fit for 800x600
  val ScreenW = 800
  val ScreenH = 600

  val HFrontPorch = 56
  val HActive = HFrontPorch + 120
  val HBackPorch = HActive + ScreenW
  val HTotal = HBackPorch + 64
  val VFrontPorch = 37
  val VActive = VFrontPorch + 6
  val VBackPorch = VActive + ScreenH
  val VTotal = VBackPorch + 23

  val FBWidth = ScreenW / 2
  val FBHeight = ScreenH / 2
  val FBPixels = FBWidth * FBHeight
}

class VGABundle extends Bundle {
  val r = Output(UInt(4.W))
  val g = Output(UInt(4.W))
  val b = Output(UInt(4.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
}

class VGACtrl extends AXI4SlaveModule(new AXI4Lite) with HasVGAConst {
  // actually this is a constant
  val fbSizeReg = Cat(FBWidth.U(16.W), FBHeight.U(16.W))
  // we always return fbSizeReg to axi4lite
  in.r.bits.data := fbSizeReg
  val sync = in.aw.fire()
}

class AXI4VGA extends Module with HasVGAConst {
  // need a 50MHz clock
  val io = IO(new Bundle {
    val in = new Bundle {
      val fb = Flipped(new AXI4)
      val ctrl = Flipped(new AXI4Lite)
    }
    val vga = new VGABundle
  })

  val ctrl = Module(new VGACtrl)
  io.in.ctrl <> ctrl.io.in
  val fb = Module(new AXI4RAM(memByte = FBPixels * 4))
  // writable by axi4lite
  // but it only readable by the internel controller
  fb.io.in.aw <> io.in.fb.aw
  fb.io.in.w <> io.in.fb.w
  io.in.fb.b <> fb.io.in.b
  io.in.fb.ar.ready := true.B
  io.in.fb.r.bits.data := 0.U
  io.in.fb.r.bits.resp := AXI4Parameters.RESP_OKAY
  io.in.fb.r.valid := BoolStopWatch(io.in.fb.ar.fire(), io.in.fb.r.fire(), startHighPriority = true)
  io.in.fb.r.bits.id := io.in.fb.ar.bits.id
  io.in.fb.r.bits.user := io.in.fb.ar.bits.user
  io.in.fb.r.bits.last := true.B

  def inRange(x: UInt, start: Int, end: Int) = (x >= start.U) && (x < end.U)

  val (hCounter, hFinish) = Counter(true.B, HTotal)
  val (vCounter, vFinish) = Counter(hFinish, VTotal)

  io.vga.hsync := hCounter >= HFrontPorch.U
  io.vga.vsync := vCounter >= VFrontPorch.U

  val hInRange = inRange(hCounter, HActive, HBackPorch)
  val vInRange = inRange(vCounter, VActive, VBackPorch)
  val videoValid = hInRange && vInRange

  val hCounterIsOdd = hCounter(0)
  val vCounterIsOdd = vCounter(0)
  // there is 1 cycle latency to read block memory,
  // so we should issue the read request 1 cycle eariler
  val nextPixel = inRange(hCounter, HActive - 1, HBackPorch - 1) && vInRange && hCounterIsOdd
  val fbPixelAddrV0 = Counter(nextPixel && !vCounterIsOdd, FBPixels)._1
  val fbPixelAddrV1 = Counter(nextPixel &&  vCounterIsOdd, FBPixels)._1

  // each pixel is 4 bytes
  fb.io.in.ar := DontCare
  fb.io.in.ar.bits.addr := Cat(Mux(vCounterIsOdd, fbPixelAddrV1, fbPixelAddrV0), 0.U(2.W))
  fb.io.in.ar.valid := nextPixel

  fb.io.in.r.ready := true.B
  val color = fb.io.in.r.bits.data
  io.vga.r := Mux(videoValid, color(23, 20), 0.U)
  io.vga.g := Mux(videoValid, color(15, 12), 0.U)
  io.vga.b := Mux(videoValid, color(7, 4), 0.U)
}
