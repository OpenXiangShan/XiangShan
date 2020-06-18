package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._

class DecodeBuffer extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val in  = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    val out = Vec(RenameWidth, DecoupledIO(new CfCtrl))
  })
}
