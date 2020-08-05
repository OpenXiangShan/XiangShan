package top

import system.XSSoc
import device.{AXI4Flash, AXI4Timer, AXI4VGA}
import gpu._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation

class Top extends Module {
  val io = IO(new Bundle{})
  val xsSoc = Module(new XSSoc())
  val timer = Module(new AXI4Timer)
  val vga = Module(new AXI4VGA)
  val flash = Module(new AXI4Flash)
//  val gpu = Module(new AXI4GPU)

  xsSoc.io := DontCare
  timer.io := DontCare
  vga.io := DontCare
  flash.io := DontCare
//  gpu.io := DontCare
  dontTouch(xsSoc.io)
  dontTouch(timer.io)
  dontTouch(vga.io)
  dontTouch(flash.io)
//  dontTouch(gpu.io)
}

object TopMain extends App {
  (new chisel3.stage.ChiselStage).execute(
    args,
    Seq(ChiselGeneratorAnnotation(() => new Top))
  )
}
