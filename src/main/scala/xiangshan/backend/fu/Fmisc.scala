package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._

import xiangshan.backend.fu.FunctionUnit._

class Fmisc extends FunctionUnit(fmiscCfg){
  val io = IO(new Bundle() {})
  override def toString: String = "Fmisc"
}
