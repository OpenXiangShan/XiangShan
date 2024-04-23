package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRConfig._

class IprioModule extends Module {
  val io = IO(new IprioIO)

  val miselect = io.in.miselect
  val siselect = io.in.siselect

  val iprio0 = RegInit(1.U(XLEN.W))
  val iprio2 = RegInit(0.U(XLEN.W))
  val iprio4 = RegInit(0.U(XLEN.W))
  val iprio6 = RegInit(0.U(XLEN.W))
  val iprio8 = RegInit(0.U(XLEN.W))
  val iprio10 = RegInit(0.U(XLEN.W))
  val iprio12 = RegInit(0.U(XLEN.W))
  val iprio14 = RegInit(0.U(XLEN.W))

  val iprios: Seq[UInt] = Seq(
    iprio14,
    iprio12,
    iprio10,
    iprio8,
    iprio6,
    iprio4,
    iprio2,
    iprio0,
  )

  io.out.iprios := Cat(iprios)

}

class IprioIO extends Bundle {
  val in = Input(new Bundle {
    val miselect = UInt(8.W)
    val siselect = UInt(8.W)
    val vsiselect = UInt(12.W)
  })
  val out = Output(new Bundle {
    val iprios = UInt((64*8).W)
  })
}