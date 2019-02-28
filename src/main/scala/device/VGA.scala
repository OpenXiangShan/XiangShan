package device

import chisel3._
import chisel3.util._

import bus.axi4.{AXI4Lite, AXI4Parameters}

trait HasVGAConst {
  // these are only fit for 800x600
  val HFrontPorch = 56
  val HActive = HFrontPorch + 120
  val HBackPorch = HActive + 800
  val HTotal = HBackPorch + 64
  val VFrontPorch = 37
  val VActive = VFrontPorch + 6
  val VBackPorch = VActive + 600
  val VTotal = VBackPorch + 23

  val FBWidth = 320
  val FBHeight = 200
}

class VGABundle extends Bundle {
  val r = Output(UInt(4.W))
  val g = Output(UInt(4.W))
  val b = Output(UInt(4.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
}

class VGA extends Module with HasVGAConst {
  // need a 50MHz clock
  val io = IO(new VGABundle)

  def inRange(x: UInt, start: Int, end: Int) = (x >= start.U) && (x < end.U)

  val (hCounter, hFinish) = Counter(true.B, HTotal)
  val (vCounter, vFinish) = Counter(hFinish, VTotal)

  io.hsync := hCounter >= HFrontPorch.U
  io.vsync := vCounter >= VFrontPorch.U

  val videoValid = inRange(hCounter, HActive, HBackPorch) && inRange(vCounter, VActive, VBackPorch)

  val colorGenerator = Counter(true.B, 0xfff)._1
  io.r := Mux(videoValid, colorGenerator(11, 8), 0.U)
  io.g := Mux(videoValid, colorGenerator(7, 4), 0.U)
  io.b := Mux(videoValid, colorGenerator(3, 0), 0.U)
}
